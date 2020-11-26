/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.io;

import static com.globalmentor.io.IOStreams.DEFAULT_BUFFER_SIZE;
import static com.globalmentor.java.Conditions.*;

import java.io.*;

/**
 * A reader that buffers data and allows this data to be processed before handing it to the consumer. This class also allows marking and resetting, as well as
 * peeking upcoming buffered characters.
 * 
 * <p>
 * The class maintains an internal buffer, which is of size <var>bufferSize</var>, part of which is used for the undo buffer of size <var>undoBufferSize</var>.
 * The following indexes are maintained in the buffer:
 * </p>
 * <dl>
 * <dt><var>readIndex</var></dt>
 * <dd>The index of the next character to be read.</dd>
 * <dt><var>peekIndex</var></dt>
 * <dd>The index of the next character to be peeked. After each read this is set to equal <var>readIndex</var>.</dd>
 * <dt><var>bufferEndIndex</var></dt>
 * <dd>The index directly after the last character in the buffer.</dd>
 * <dt><var>fetchBufferIndex</var></dt>
 * <dd>The index where a new buffer should be read. The default implementation is for <var>fetchBufferIndex</var> to equal <var>bufferEndIndex</var>, but
 * processing of a stream may determine that rebuffering should occur earlier -- if a "\r\n" is split across buffers, for instance.</dd>
 * </dl>
 * @author Garret Wilson
 * @see Reader
 */
public class ProcessingBufferedReader extends Reader {

	//TODO maybe implement locking behavior like java.io.BufferedReader

	/**
	 * The value which indicates the end of the data has been reached.
	 * @see #read()
	 */
	public static final int END_VALUE = -1;

	/** The default size of the buffer for unreading data. */
	public static final int DEFAULT_UNDO_BUFFER_SIZE = 256;

	/**
	 * The size of buffered data, including undo buffer space.
	 * @see #undoBufferSize
	 */
	private int bufferSize = DEFAULT_BUFFER_SIZE;

	/**
	 * Returns the size allocated to the buffer.
	 * @implSpec By default this method returns {@value IOStreams#DEFAULT_BUFFER_SIZE}.
	 * @return The size of buffered data, including undo buffer space.
	 * @see #getUndoBufferSize()
	 */
	public int getBufferSize() {
		return bufferSize;
	}

	/** The size of the extra buffer for unreading data. */
	private int undoBufferSize = DEFAULT_UNDO_BUFFER_SIZE;

	/** @return The size of the extra buffer for unreading data. */
	public int getUndoBufferSize() {
		return undoBufferSize;
	}

	/** The internal buffer used to hold characters. */
	private char[] Buffer = null;

	/** @return The internal buffer used to hold characters. */
	protected char[] getBuffer() {
		return Buffer;
	}

	/**
	 * Sets the buffer to be used to hold characters.
	 * @param buffer The new buffer.
	 */
	private void setBuffer(final char[] buffer) {
		this.Buffer = buffer; //set the buffer
		if(Buffer != null) { //if a real buffer was set
			initializeIndexes(); //initialize the indexes, such as the read and peek indexes
		}
	}

	/** Whether or not we've read the last buffer. */
	private boolean lastBuffer = false;

	/**
	 * @return Whether or not we've read the last buffer. Technically the last buffer could have been read, but it might have been filled completely, in which
	 *         case this function would still return <code>false</code>.
	 */
	protected boolean isLastBuffer() {
		return lastBuffer;
	}

	/**
	 * Sets whether or not the previously-read buffer was the last.
	 * @param lastBuffer Whether the last buffer has been read.
	 */
	protected void setLastBuffer(final boolean lastBuffer) {
		this.lastBuffer = lastBuffer;
	}

	/** The index of the next character to be read. */
	private int readIndex;

	/** @return The index of the next character to be read. */
	protected int getReadIndex() {
		return readIndex;
	}

	/**
	 * Sets the index of the next character to be read and checks to see if the end of the file has been reached. Resets peeking.
	 * @param readIndex The index of the next character to be read.
	 * @throws IOException if an I/O error occurs.
	 * @see #updateEnd()
	 */
	protected void setReadIndex(final int readIndex) throws IOException {
		this.readIndex = readIndex; //set the read index
		resetPeek(); //reset peeking
		updateEnd(); //make sure we haven't reached the end of the data yet
	}

	/** The index of the next character to be peeked. After each read this is set to equal ReadIndex. */
	private int peekIndex;

	/** @return The index of the next character to be peeked. After each read this is set to equal ReadIndex. */
	protected int getPeekIndex() {
		return peekIndex;
	}

	/**
	 * Sets the index of the next character to be peeked.
	 * @param peekIndex The index of the next character to be peeked.
	 */
	protected void setPeekIndex(final int peekIndex) {
		this.peekIndex = peekIndex;
	}

	/** The index directly after the end of the last character in the buffer. */
	private int bufferEndIndex = 0;

	/** @return The index directly after the end of the last character in the buffer. */
	protected int getBufferEndIndex() {
		return bufferEndIndex;
	}

	/**
	 * Sets the index directly after the end of the last character in the buffer.
	 * @param bufferEndIndex The index directly after the end of the last character in the buffer.
	 */
	protected void setBufferEndIndex(final int bufferEndIndex) {
		this.bufferEndIndex = bufferEndIndex;
	}

	/**
	 * The index where a new buffer should be read.
	 * @see ProcessingBufferedReader#bufferEndIndex
	 */
	private int fetchBufferIndex = 0;

	/**
	 * @return The index where a new buffer should be read. Usually equals <code>BufferEndIndex</code>.
	 * @see ProcessingBufferedReader#getBufferEndIndex
	 */
	protected int getFetchBufferIndex() {
		return fetchBufferIndex;
	}

	/**
	 * Sets the index where a new buffer should be read.
	 * @param fetchBufferIndex The index where a new buffer should be read.
	 * @see ProcessingBufferedReader#setBufferEndIndex
	 */
	protected void setFetchBufferIndex(final int fetchBufferIndex) {
		this.fetchBufferIndex = fetchBufferIndex;
	}

	/** Whether or not the end of the data has been reached. */
	private boolean eof = false;

	/** @return Whether or not the end of the data has been reached. */
	public boolean isEnd() {
		return eof;
	}

	/**
	 * Sets whether the end of the data has been reached.
	 * @param isEnd Whether the end of the data has been reached.
	 */
	protected void setEnd(final boolean isEnd) {
		this.eof = isEnd;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation returns true if there are characters already in the buffer that would be used without fetching another buffer, or if the underlying
	 * reader is ready.
	 * </p>
	 */
	public boolean ready() throws IOException {
		if(isEnd()) { //if we're at the end, we never block
			return true;
		}
		return getReadIndex() < getFetchBufferIndex() || getReader().ready();
	}

	/** {@inheritDoc} */
	@Override
	public boolean markSupported() {
		return true;
	}

	/** The index in the buffer of the mark, which will be negative if the reader is not marked. */
	private int markIndex = -1;

	/** @return The index in the buffer of the mark, which will be negative if the reader is not marked. */
	protected int getMarkIndex() {
		return markIndex;
	}

	/**
	 * {@inheritDoc}
	 * @throws IllegalArgumentException if the given read-ahead limit is greater than {@link #getUndoBufferSize()}.
	 * @see #getUndoBufferSize()
	 */
	@Override
	public void mark(final int readAheadLimit) throws IOException {
		checkArgument(readAheadLimit <= getUndoBufferSize(),
				"Mark read-ahead limit of " + readAheadLimit + " not supported; maximum read-ahead limit is " + getUndoBufferSize() + ".");
		this.markIndex = getReadIndex(); //mark the current location
		//		this.readAheadLimit=readAheadLimit;
	}

	/** Removes the current mark. */
	/*TODO del
		protected void unmark()
		{
			markIndex=-1;
			readAheadLimit=0;
		}
	*/

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation resets peeking.
	 * </p>
	 * <p>
	 * This implementation does not support resetting without first marking.
	 * </p>
	 * @throws IOException if the reader is not currently marked.
	 * @see #resetPeek()
	 */
	public void reset() throws IOException {
		if(markIndex < 0) {
			throw new IOException("Resetting an unmarked reader is not supported.");
		}
		setReadIndex(markIndex); //move the read index back to the mark location
		resetPeek(); //reset peeking
	}

	/** The reader from which we extract the information. */
	private Reader reader = null;

	/** @return The reader from which we extract the information. */
	protected Reader getReader() {
		return reader;
	}

	/**
	 * Constructor to create a {@link ProcessingBufferedReader} from another reader.
	 * @param reader The reader that contains the data.
	 */
	public ProcessingBufferedReader(final Reader reader) {
		this.reader = reader; //store the reader they passed us
		createBuffer(); //create our buffer
	}

	/**
	 * Constructor to create a {@link ProcessingBufferedReader} from another reader, along with several characters that have already been read.
	 * <var>prereadCharacters</var> must be less than or equal to the length of the buffer.
	 * @param reader The reader that contains the data.
	 * @param prereadCharacters The characters that have already been read.
	 * @throws IOException if <var>prereadCharacters</var> is too long for the buffer.
	 */
	public ProcessingBufferedReader(final Reader reader, final CharSequence prereadCharacters) throws IOException {
		this(reader); //do the default construction
		loadBuffer(prereadCharacters); //load the specified characters into the buffer
	}

	/**
	 * Constructor to create a reader from the contents of a character sequence.
	 * @param characters The string that should be used for input.
	 * @throws IOException if an I/O error occurs.
	 */
	public ProcessingBufferedReader(final CharSequence characters) throws IOException {
		reader = null; //show that we will not have an input reader to read from; this will be fine, because we'll never have have to fetch any other buffers
		setBuffer(characters.toString().toCharArray()); //use the characters from the char sequence
		setBufferEndIndex(characters.length()); //show that the buffer is as long as the string
		setFetchBufferIndex(getBufferEndIndex()); //we'll never need to fetch another buffer, but here is where we would need to if we were going to
		setLastBuffer(true); //show that this is the last buffer we have, so that fetchBuffer() will not try to fetch another buffer
		processBufferedData(0); //process the string data before we start reading from it
	}

	/**
	 * Creates a new buffer and initializes buffer indexes. Any data in previous buffers will be erased.
	 */
	protected void createBuffer() {
		setBuffer(new char[Math.max(getBufferSize(), getUndoBufferSize())]); //create a new buffer large enough to hold buffered characters and undo characters
		setBufferEndIndex(0); //show that we're already at the end of this buffer
		setFetchBufferIndex(0); //show that we should fetch another buffer before getting any information
	}

	/**
	 * Loads the buffer with the specified characters. <var>bufferCharacters</var> must be less than or equal to the room left in the buffer.
	 * @param bufferCharacters The characters which should be placed in the buffer.
	 * @throws IOException if <var>bufferCharacters</var> is too long for the buffer.
	 */
	protected void loadBuffer(final CharSequence bufferCharacters) throws IOException {
		final int charactersLength = bufferCharacters.length(); //find out how many characters there are
		final int destIndex = getBufferEndIndex(); //we'll start putting new data at the end of the buffer
		final int roomLeft = getBufferSize() - destIndex; //find out how many characters can physically fit into our buffer
		if(roomLeft < charactersLength) { //if there isn't enough room for our characters
			throw new IOException("Not enough room in buffer for loading specified characters."); //throw an exception
		}
		System.arraycopy(bufferCharacters.toString().toCharArray(), 0, getBuffer(), destIndex, charactersLength); //copy the characters to the buffer
		setBufferEndIndex(getBufferEndIndex() + charactersLength); //update the end of the buffer
		setFetchBufferIndex(getBufferEndIndex()); //we'll fetch a new buffer when we reach the end of the buffer
		processBufferedData(destIndex); //do whatever processing we need to do with the newly fetched data
	}

	/**
	 * Initializes relevant buffer indexes when, for example, the buffer is first created. This implementation sets the read index and the peek index; the other
	 * buffer end and fetch buffer indexes will be set by methods that set the buffer itself, such as {@link #createBuffer()}. This method is called from within
	 * {@link #setBuffer(char[])}.
	 * @see #createBuffer()
	 * @see #setBuffer(char[])
	 */
	protected void initializeIndexes() {
		readIndex = 0; //we'll start reading at the first character next time (don't use the setReadIndex() function, because that will try to update the EOF property before all the other index are initialized)
		peekIndex = getReadIndex(); //we'll start peeking at same place
	}

	/**
	 * Adjusts all indexes by a certain amount. This method is currently called by {@link #fetchBuffer()} after moving data in the buffer.
	 * <p>
	 * The mark index is adjusted as well. If adjusting the indexes moves the mark index below the bottom of the buffer, the reader has effectively become
	 * unmarked.
	 * </p>
	 * @param moveDelta The number of characters to move the indexes, positive for forwards, negative for backwards.
	 * @see #fetchBuffer()
	 */
	protected void adjustIndexes(final int moveDelta) {
		readIndex += moveDelta; //readjust the read index; do not use setReadIndex() as that will try to check for EOF before all our indexes are updated
		peekIndex += moveDelta; //readjust the peek index
		bufferEndIndex += moveDelta; //readjust the end of the buffer
		fetchBufferIndex += moveDelta; //readjust the index of the location to fetch a new buffer
		if(markIndex >= 0) { //if the mark is still valid 
			markIndex += moveDelta; //move it back as well
		}
	}

	/**
	 * Moves a portion of the buffer. This method is called from {@link #fetchBuffer()} before fetching data.
	 * @param sourceIndex The index in the buffer to be moved.
	 * @param destIndex The destination in the buffer for the data being moved.
	 * @param len The number of characters to move.
	 * @see #fetchBuffer()
	 */
	protected void moveBuffer(final int sourceIndex, final int destIndex, final int len) {
		System.arraycopy(getBuffer(), sourceIndex, getBuffer(), destIndex, len); //move the data in the buffer
	}

	/**
	 * Fills a buffer with new information.
	 * <p>
	 * This method will read as much data as it can without blocking, and return the number of characters read.
	 * </p>
	 * <p>
	 * If the current buffer is full, the last section of the buffer will be moved to the front to make room for more data; otherwise, the data will simply be
	 * appended to the buffer.
	 * </p>
	 * <p>
	 * This method does not change the EOF status.
	 * </p>
	 * @return The number of characters fetched, or {@value #END_VALUE} if the end of the file was reached.
	 * @throws IOException if an I/O error occurs.
	 * @see #isLastBuffer()
	 */
	protected int fetchBuffer() throws IOException {
		//TODO should we check to make sure that we have a valid reader?
		if(!isEnd() && !isLastBuffer()) { //if we're not out of data and we haven't already read the last buffer of information (meaning all previous buffers were completely full)
			if(getBufferSize() - getBufferEndIndex() == 0) { //if there is no room for more data (the last fetchBuffer() filled the entire buffer), we'll shift the buffer contents down
				//first, move any unread characters and undo buffer space to the beginning of the array
				final int sourceBegin = Math.max(getReadIndex() - getUndoBufferSize(), 0); //reserve a certain amount of space for pushing back, unless we haven't read anything to push back
				if(sourceBegin > 0) { //if we have anything to move back
					final int sourceEnd = getBufferEndIndex(); //we're going to copy everything up to the end of the buffer
					final int moveDelta = -sourceBegin; //find how much we're going to move the data
					int destIndex = 0; //we'll copy all of this to the beginning of the array
					moveBuffer(sourceBegin, destIndex, sourceEnd - sourceBegin); //move the data back to have room for more data
					adjustIndexes(moveDelta); //adjust the indexes since we've moved the data
				}
			}
			final int destIndex = getBufferEndIndex(); //we'll start putting new data at the end of the buffer
			final int numCharsToRead = getBufferSize() - destIndex; //find out how many characters can physically fit into our buffer TODO this currently doesn't check to see if the undo buffer was larger than the buffer size
			if(numCharsToRead > 0) { //if we have any room left in our buffer
				final int numCharactersRead = getReader().read(getBuffer(), destIndex, numCharsToRead); //read characters into our array and find out how many characters we read
				if(numCharactersRead != END_VALUE) { //if we read any characters at all
					final int newDataBeginIndex = getFetchBufferIndex(); //we'll count everything after the "fetch buffer" marker as new data, even though it may be data that was read last time
					setBufferEndIndex(getBufferEndIndex() + numCharactersRead); //update the end of the buffer
					setFetchBufferIndex(getBufferEndIndex()); //we'll fetch a new buffer when we reach the end of the buffer
					processBufferedData(newDataBeginIndex); //do whatever processing we need to do with the newly fetched data
					return numCharactersRead; //return the number of characters read
				} else { //if there were no more characters to read
					setLastBuffer(true); //show that this was the last buffer read (we can't get any more characters from our input reader)
					return 0; //show that we couldn't fetch any characters
				}
			} else
				//if there was no room left in the buffer TODO we'll probably want to enlarge the buffer here
				return 0; //show that we didn't read any characters; this doesn't mean that we've reached the end of the file; maybe they tried to fetch the buffer twice in a row
		}
		return END_VALUE; //if we reach here, we either were already at the end of the file or we couldn't fetch any more characters
	}

	/**
	 * Provides away to do initial processing on newly-buffered data. The fetch index may be modified.
	 * @param newDataBeginIndex The starting index of the newly fetched data.
	 * @throws IOException if an I/O error occurs.
	 * @see #getFetchBufferIndex()
	 */
	protected void processBufferedData(final int newDataBeginIndex) throws IOException {
	}

	/**
	 * Checks to see if the end of the data has been reached, and updates the EOF property if so. If the EOF property is already set to <code>true</code>, this
	 * function does nothing.
	 * @return Whether or not the end of the file has been reached.
	 * @throws IOException if an I/O error occurs.
	 * @see #isEnd()
	 */
	protected boolean updateEnd() throws IOException {
		if(!isEnd()) { //if we don't think we've hit the end of the file, yet, we'll make sure
			if(getReadIndex() >= getFetchBufferIndex()) { //if we're at a place where we should fetch a new buffer of information (we should never be *over* this amount, but >= is used just to be redundantly safe)
				if(fetchBuffer() == END_VALUE) //fetch another buffer of information; if we were not able to get more information
					setEnd(true); //show that we're at the end of the file
			}
		}
		return isEnd(); //return whether or not we're at the end of the file (this should be a definite answer)
	}

	/**
	 * Read a single character. This method will block until a character is available, an I/O error occurs, or the end of the stream is reached.
	 * @return The character read, as an integer in the range 0 to 16383 (0x00-0xffff), or {@value #END_VALUE} if the end of the stream has been reached.
	 * @throws IOException if an I/O error occurs.
	 */
	public int read() throws IOException {
		//TODO fix		resetPeek();	//reset our peek buffer in case any characters were peeked so that the next character read will be accurate
		if(!isEnd()) { //if we're not at the end of the file, yet
			if(getReadIndex() >= getFetchBufferIndex()) { //if we're at a place where we should fetch a new buffer of information (we should never be *over* this amount, but >= is used just to be redundantly safe)
				if(fetchBuffer() == END_VALUE) //fetch another buffer of information; if we were not able to get more information
					setEnd(true); //show that we're at the end of the file
			}
			if(!isEnd()) { //fetching another buffer could have alerted us to being at the end of the file even though we didn't know it before; if we're not at the end of the file, yet
				final char readChar = getBuffer()[getReadIndex()]; //get the character at our current read position
				setReadIndex(getReadIndex() + 1); //advance the read index
				return readChar; //return the character we read
			}
		}
		//if we're at the end of the file
		resetPeek(); //reset peeking just because this function always does, even at the end of the file
		return END_VALUE; //show that we reached the end of the file
	}

	/**
	 * Read characters into a portion of an array. This method will block until some input is available, an I/O error occurs, or the end of the stream is reached.
	 * Resets peeking.
	 * @param cbuf Destination buffer.
	 * @param off Offset at which to start storing characters.
	 * @param len Maximum number of characters to read.
	 * @return The number of characters read, or {@value #END_VALUE} if the end of the stream has been reached.
	 * @throws IOException if an I/O error occurs.
	 */
	public int read(final char[] cbuf, int off, final int len) throws IOException {
		if(!isEnd()) { //if we're not at the end of the file
			int numCharsRead = 0; //this will store the number of characters actually read; right now, we don't know if we're going to read any characters
			while(numCharsRead < len) { //keep reading characters until we've read as many as they want
				final int numCharsInBuffer = getFetchBufferIndex() - getReadIndex(); //find out how many characters are left in the buffer
				final int numCharsToReadThisTime = Math.min(len - numCharsRead, numCharsInBuffer); //read the number of characters we need to, or however many are left in the buffer this time
				final int sourceBegin = getReadIndex(); //find out where to start copying
				System.arraycopy(getBuffer(), sourceBegin, cbuf, off, numCharsToReadThisTime); //copy the characters, or all the characters in this buffer, whichever is less
				numCharsRead += numCharsToReadThisTime; //show that we read at least a little more
				off += numCharsToReadThisTime; //show that our destination offset also increased
				setReadIndex(getReadIndex() + numCharsToReadThisTime); //move the read position ahead the number of characters that we're going to read; only do this *after* we've copied in case that setReadIndex() fetches another buffer
				if(numCharsRead < len) { //if we still have more characters to read
					if(!isEnd()) //if we're not at the end of the file, yet (the call to setReadIndex(), above, will have checked)
						fetchBuffer(); //fetch another buffer of information (actually, going to the end of the buffer would automatically have fetched another buffer, above, so this is redundant but won't considerably hurt performance, because fetchBuffer() will quickly return when it sees the buffer is full) TODO maybe take this out and put a comment about setReadIndex() fetching another buffer if needed
					else
						//if we are at the end of the file
						break; //show that there's nothing left to read
				}
			}
			resetPeek(); //reset peeking, as this function always does, whether or not we were able to read any characters
			return numCharsRead; //return the number of characters we were able to read before reaching the end of the file
		} else { //if we've reached the end of the file
			resetPeek(); //reset peeking, as this function always does, whether or not we were able to read any characters
			return END_VALUE; //show that we're at the end of the data
		}
	}

	/**
	 * Pushes back a single character. Resets peeking.
	 * @param c The character to push back.
	 * @throws IOException if there is no room to push back characters (the undo buffer is full) or some other I/O error occurs.
	 */
	public void unread(int c) throws IOException {
		unskip(1); //back up one position
		getBuffer()[getReadIndex()] = (char)c; //put the character they provided us in that location
	}

	/**
	 * Pushes back a portion of an array of characters. Resets peeking.
	 * @param cbuf The array of characters to push back.
	 * @param off The offset of the first character to push back.
	 * @param len The number of characters to push back.
	 * @throws IOException if there is no room to push back characters (the undo buffer is full) or some other I/O error occurs.
	 */
	public void unread(char[] cbuf, int off, int len) throws IOException {
		unskip(len); //back up the correct number of positions
		System.arraycopy(cbuf, off, getBuffer(), getReadIndex(), len); //copy the characters into the buffer
	}

	/**
	 * Pushes back an array of characters. Resets peeking.
	 * @param cbuf The array of characters to push back.
	 * @throws IOException if there is no room to push back characters (the undo buffer is full) or some other I/O error occurs.
	 */
	public void unread(char[] cbuf) throws IOException {
		unread(cbuf, 0, cbuf.length); //unread all the characters in the array, starting at the first character
	}

	/**
	 * Close the stream. Once a stream has been closed, further read(), ready(), mark(), or reset() invocations will throw an IOException.
	 * <p>
	 * Closing a previously-closed stream, however, has no effect.
	 * </p>
	 * @throws IOException if an I/O error occurs.
	 */
	public void close() throws IOException {
		if(getReader() != null) { //if we have a reader
			getReader().close();
		}
		//TODO we need to decide how to close when we're reading from a string, so that further calls to read() and such will throw an exception
		setBuffer(null); //release our buffer so that it can be garbage collected immediately
	}

	/**
	 * Peeks at the next character to be read. Each successive peek will return a successive character.
	 * <p>
	 * This function is reset with every call to #read().
	 * </p>
	 * <p>
	 * Since every peek reads a successive character, {@link #resetPeek()} should be called if it must be ensured that the next character peeked is the next
	 * character to be read.
	 * @throws IOException if an I/O error occurs.
	 * @return The next character that will be read after the character retrieved in the last {@link #read()} or {@link #peek()}, or {@value #END_VALUE} if there
	 *         are no characters left to read.
	 * @see #read()
	 * @see #resetPeek()
	 */
	public int peek() throws IOException {
		if(!isEnd()) { //if we're not at the end of the file, yet
			if(getPeekIndex() >= getFetchBufferIndex()) { //if we're peeking at a place where we should fetch a new buffer of information (we should never be *over* this amount, but >= is used just to be redundantly safe)
				if(fetchBuffer() == END_VALUE) //fetch another buffer of information; if we couldn't fetch any more information
					return END_VALUE; //show that we can't peek anymore (but don't set the EOF variable, because our read position is not necessarily at the end of the file)
			}
			final char peekedChar = getBuffer()[getPeekIndex()]; //get the character at our current peek position
			setPeekIndex(getPeekIndex() + 1); //advance the peek index
			return peekedChar; //return the character we peeked
		} else { //if we are at the end of the file
			return END_VALUE; //show that we can't peek anymore, because we're at the end of the file
		}
	}

	/**
	 * Peeks a specified number of characters into a portion of an array. This method will block until some input is available, an I/O error occurs, or the end of
	 * the stream is reached.
	 * <p>
	 * Resets peeking.
	 * </p>
	 * @param cbuf Destination buffer.
	 * @param off Offset at which to start storing characters.
	 * @param len Maximum number of characters to peek.
	 * @return The number of characters peeked, or 0 if the end of the stream has been reached.
	 * @throws IOException if an I/O error occurs.
	 */
	public int peek(char[] cbuf, int off, int len) throws IOException {
		int numCharsPeeked = 0; //this will store the number of characters actually peeked; right now, we don't know if we're going to peek any characters
		if(!isEnd()) { //if we're not at the end of the file
			while(numCharsPeeked < len) { //keep peeking characters until we've peeked as many as they want
				final int numCharsInBuffer = getFetchBufferIndex() - getPeekIndex(); //find out how many characters are left in the buffer
				final int numCharsToPeekThisTime = Math.min(len - numCharsPeeked, numCharsInBuffer); //peek the number of characters we need to, or however many are left in the buffer this time
				final int sourceBegin = getPeekIndex(); //find out where to start copying
				setPeekIndex(getPeekIndex() + numCharsToPeekThisTime); //move the peek position ahead the number of characters that we're going to peek
				System.arraycopy(getBuffer(), sourceBegin, cbuf, off, numCharsToPeekThisTime); //copy the characters, or all the characters in this buffer, whichever is less
				numCharsPeeked += numCharsToPeekThisTime; //show that we peeked at least a little more
				off += numCharsToPeekThisTime; //show that our destination offset also increased
				if(numCharsPeeked < len) { //if we still have more characters to peek
					//TODO probably move this clause (and all similar ones) to the front of the loop
					if(getPeekIndex() >= getFetchBufferIndex() && !isLastBuffer()) { //if we've peeked to the end of the buffer, and this isn't the last buffer
						fetchBuffer(); //fetch the next buffer TODO note that if they peek *way* ahead of the read index, the fetchBuffer() won't read anymore, and we'll go into an endless loop; check the result of fetchBuffer() here
					} else
						//if we haven't peeked to the end of the buffer, and we still haven't peeked all we need to
						break; //show that there's nothing left to peek
				}
			}
			if(numCharsPeeked == 0) { //if no characters were peeked
				setEnd(true); //show that we've reached the end of the file
			}
		}
		return numCharsPeeked; //return the number of characters we were able to peek before reaching the end of the file
	}

	/**
	 * Resets any characters we've peeked so that the next peek will reflect the next character which will be read.
	 * <p>
	 * Does not affect the next character to be read.
	 * </p>
	 */
	public void resetPeek() {
		setPeekIndex(getReadIndex()); //make the peek index the same as the read index
	}

	/**
	 * Skips the specified number of characters. This method will block until some characters are available, an I/O error occurs, or the end of the stream is
	 * reached.
	 * <p>
	 * Resets peeking.
	 * </p>
	 * @param n The number of characters to skip.
	 * @return The number of characters actually skipped.
	 * @throws IOException if an I/O error occurs.
	 */
	public long skip(long n) throws IOException {
		long numCharsSkipped = 0; //this will store the number of characters actually skipped; right now, we don't know if we're going to skip any characters
		if(!isEnd()) { //if we're not at the end of the file
			while(numCharsSkipped < n) { //keep skipping characters until we've skipped enough
				final int numCharsInBuffer = getFetchBufferIndex() - getReadIndex(); //find out how many characters are left in the buffer
				final int numCharsToSkipThisTime = n - numCharsSkipped < numCharsInBuffer ? (int)(n - numCharsSkipped) : numCharsInBuffer; //skip the amount of characters we need to, or however many is left in the buffer this time
				setReadIndex(getReadIndex() + numCharsToSkipThisTime); //move the read position ahead the number of characters that we're going to skip
				numCharsSkipped += numCharsToSkipThisTime; //show that we skipped at least a little more
				if(numCharsSkipped < n) { //if we still have more characters to skip
					if(!isEnd()) //if we're not at the end of the file, yet (the call to setReadIndex(), above, will have checked)
						fetchBuffer(); //fetch another buffer of information (actually, going to the end of the buffer would automatically have fetched another buffer, above, so this is redundant but won't considerably hurt performance, because fetchBuffer() will quickly return when it sees the buffer is full) TODO maybe remove this and put a comment about setReadIndex() fetching another buffer if needed
					else
						//if we are at the end of the file
						break; //show that there's nothing left to skip
				}
			}
		}
		resetPeek(); //reset peeking, as this function always does, whether or not we were able to skip any characters
		return numCharsSkipped; //return the number of characters we were able to skip before reaching the end of the file
	}

	/**
	 * Reverses the reading position to the previous characters read. Functions similar to unread() except that the unread characters will always be the
	 * characters that were previously read.
	 * <p>
	 * Resets peeking.
	 * </p>
	 * @param n The number of characters to unskip.
	 * @throws IOException if there is no room to push back characters (the undo buffer is full) or some other I/O error occurs.
	 * @see #unread(char[])
	 */
	public void unskip(final int n) throws IOException {
		if(getReadIndex() - n >= 0) { //if we have enough room to back up the requested number of characters
			setReadIndex(getReadIndex() - n); //back the read index up the specified number of characters, which will reset peeking as well
		} else
			//if we can't push back more characters TODO we could copy characters forward to make room
			throw new IOException("Not enough room to push back " + n + " character(s); only room for " + getReadIndex() + "."); //show we had a buffer underrun
	}

}
