package com.garretwilson.io;

import java.io.*;
import com.garretwilson.util.Debug;
//G***del if we don't need import java.net.URL;
//G***del if we don't need import java.net.MalformedURLException;
//G***del if we don't need import com.garretwilson.util.StringManipulator;

/**Class for reading and decoding characters from bytes in an <code>InputStream</code>.
Functions similar to <code>java.io.InputStreamReader</code> except that the class
allows prereading of bytes, changing of character encoding a wider variety of character encodings su
buffering and retrieving characters from an input stream.
The <code>BufferedPushbackReader</code> class maintains an internal buffer, which is of
size <code>BufferSize</code>, part of which is used for the pushback buffer of
size <code>PushbackBufferSize</code>. The following
indexes are maintained in the buffer:
<ul>
	<li><code>ReadIndex</code>The index of the next character to be read.</li>
	<li><code>PeekIndex</code>The index of the next character to be peeked. After each
		read this is set to equal ReadIndex.</li>
	<li><code>BufferEndIndex</code>The index directly after the end of the last character
		in the buffer.</li>
	<li><code>FetchBufferIndex</code>The index where a new buffer should be read. The
		default implementation is for <code>FetchBufferIndex</code> to equal
		<code>BufferEndIndex</code>, but processing of a stream may determine that
		rebuffering should occur earlier -- if a "\r\n" is split across buffers, for
		instance.</li>
</ul>
@author Garret Wilson
@see Reader
*/
//G***decide if we want to implement unread or not

//G***maybe implement locking behavior like java.io.BufferedReader

public class BufferedPushbackReader extends Reader
{

	/**The value which indicates the end of the data has been reached.
	@see BufferedPushbackReader#read*/
	public final static int EOF_VALUE=-1;

	/**The default size of the buffer for undreading data.*/
	public final static int DEFAULT_PUSHBACK_BUFFER_SIZE=256;

	/**The default size of buffered data.*/
	public final static int DEFAULT_BUFFER_SIZE=8192;

	/**The size of buffered data, including pushback buffer space.
	@see BufferedPushbackReader#PushbackBufferSize*/
	private int BufferSize=DEFAULT_BUFFER_SIZE;

		/**@return The size of buffered data, including pushback buffer space.
		@see BufferedPushbackReader#getPushbackBufferSize*/
		public int getBufferSize() {return BufferSize;}

	/**The size of the extra buffer for undreading data.*/
	private int PushbackBufferSize=DEFAULT_PUSHBACK_BUFFER_SIZE;

		/**@return The size of the extra buffer for undreading data.*/
		public int getPushbackBufferSize() {return PushbackBufferSize;}

	/**The internal buffer used to hold characters.*/
	private char[] Buffer=null;

		/**@return The internal buffer used to hold characters.*/
		protected char[] getBuffer() {return Buffer;}

		/**Sets the buffer to be used to hold characters.
		@param buffer The new buffer.
		*/
		private void setBuffer(final char[] buffer)
		{
			Buffer=buffer;	//set the buffer
			if(Buffer!=null)	//if a real buffer was set
				initializeIndexes();	//initialize the indexes, such as the read and peek indexes
		}

	/**Whether or not we've read the last buffer.*/
	private boolean LastBuffer=false;

		/**@return Whether or not we've read the last buffer. Technically the last
			buffer could have been read, but it might have been filled completely,
			in which case this function would still return <code>false</code>.*/
		protected boolean isLastBuffer() {return LastBuffer;}

		/**Sets whether or not the previously-read buffer was the last.
		@param lastBuffer Whether the last buffer has been read.*/
		protected void setLastBuffer(final boolean lastBuffer) {LastBuffer=lastBuffer;}

	/**The index of the next character to be read.*/
	private int ReadIndex;

		/**@return The index of the next character to be read.*/
		protected int getReadIndex() {return ReadIndex;}

		/**Sets the index of the next character to be read and checks to see if the
			end of the file has been reached.
			Resets peeking.
		@param readIndex The index of the next character to be read.
		@except IOException Thrown when an I/O error occurs.
		@see BufferedPushbackReader#updateEOF
		*/
		protected void setReadIndex(final int readIndex) throws IOException
		{
			ReadIndex=readIndex;	//set the read index
			resetPeek();	//reset peeking
			updateEOF();	//make sure we haven't reached the end of the data yet
//G***del System.out.println("Finished updating EOF");	//G***del
//G***del System.out.println("updated EOF");	//G***del
		}

	/**The index of the next character to be peeked. After each read this is set to equal ReadIndex.*/
	private int PeekIndex;

		/**@return The index of the next character to be peeked. After each read this is set to equal ReadIndex.*/
		protected int getPeekIndex() {return PeekIndex;}

		/**Sets the index of the next character to be peeked.
		@param peekIndex The index of the next character to be peeked.*/
		protected void setPeekIndex(final int peekIndex) {PeekIndex=peekIndex;}

	/**The index directly after the end of the last character in the buffer.*/
	private int BufferEndIndex=0;

		/**@return The index directly after the end of the last character in the buffer.*/
		protected int getBufferEndIndex() {return BufferEndIndex;}

		/**Sets the index directly after the end of the last character in the buffer.
		@param bufferEndIndex The index directly after the end of the last character in the buffer.*/
		protected void setBufferEndIndex(final int bufferEndIndex) {BufferEndIndex=bufferEndIndex;}

	/**The index where a new buffer should be read.
	@see BufferedPushbackReader#BufferEndIndex*/
	private int FetchBufferIndex=0;

		/**@return The index where a new buffer should be read. Usually equals <code>BufferEndIndex</code>.
		@see BufferedPushbackReader#getBufferEndIndex*/
		protected int getFetchBufferIndex() {return FetchBufferIndex;}

		/**Sets the index where a new buffer should be read.
		@param fetchBufferIndex The index where a new buffer should be read.
		@see BufferedPushbackReader#setBufferEndIndex*/
		protected void setFetchBufferIndex(final int fetchBufferIndex) {FetchBufferIndex=fetchBufferIndex;}

	/**Whether or not the end of the data has been reached.*/
	private boolean EOF=false;

		/**@return Whether or not the end of the data has been reached.*/
		public boolean isEOF() {return EOF;}

		/**Sets whether the end of the data has been reached.
		@param eof Whether the end of the data has been reached.*/
		protected void setEOF(final boolean eof)
		{
			EOF=eof;
//G***del System.out.println("Just set EOF true");	//G***del
		}

	/**A carriage return character.*/	//G***do we want these here?
//G***del	public final static char CR_CHAR='\r';
	/**A linefeed character.*/
//G***del	public final static char LF_CHAR='\n';

	/**Creates an input stream from the specified location name.
	@param context The context in which to parse the specification, or
		<code>null</code> if there is no context.
	@param spec A String representation of a URL or a filename.
	@except IOException Thrown when an I/O error occurs.
	@return An input stream based on the specified context and specification.
	*/
	//G***maybe put this in a separate com.garretwilson.io.Reader class
	//G***what about storing the URL so we can use it as a context elsewhere?
/*G***fix
	public static InputStream createInputStream(final URL context, final String spec) throws IOException
	{
		InputStream inputStream;	//we don't know where our input stream is coming from, yet
		try
		{
			inputStream=new URL(context, spec).openConnection().getInputStream();	//assume the location is an Internet URL and try to open a connection to it
		}
		catch(MalformedURLException e)	//if the location isn't a valid URL
		{
			inputStream=new FileInputStream(spec);	//assume it's a file and try to open it
		}
		return inputStream;	//return the input stream we constructed
	}
*/

//G***fix	/**The input stream from which we'll read the data.*/
//G***fix	private InputStream InStream;

//G***for now, we'll cheat and use another reader which we'll create to do most of our work; later, we may want to do all the extraction and encoding ourselves

	/**The reader from which we extract the information.*/
	private Reader InReader=null;	//G***del when we use our own routines

		/**@return The reader from which we extract the information.*/
		protected Reader getInReader() {return InReader;}

	/**The index of the line of the current character being processed.*/
//G***fix	private long CurrentLineIndex;

		/**@return The index of the line of the current character being processed.
		@see getCurrentCharIndex
		*/
//G***del		public long getCurrentLineIndex() {return 0;} //G***fix {return CurrentLineIndex;}

		/**Changes the current line index.
		@param newLineIndex The new line index.
		@see setCurentCharIndex
		*/
//G***del		public void setCurrentLineIndex(final long newLineIndex) {} //G***fix {CurrentLineIndex=newLineIndex;}

	/**The index of the current character being processed.*/
//G***fix	private long CurrentCharIndex;

		/**@return The index of the current character being processed.
		@see getCurentLineIndex
		*/
//G***del		public long getCurrentCharIndex() {return 0;} //G***fix {return CurrentCharIndex;}

		/**Changes the current character index.
		@param newLineIndex The new character index.
		@see setCurrentLineIndex
		*/
//G***del		public void setCurrentCharIndex(final long newCharIndex) {} //G***fix {CurrentCharIndex=newCharIndex;}

	/**The string of characters we've peeked since the last read(). On the next call to read(), these characters will first be unread.
	@see peekChar
	@see readChar
	*/
//G***fix	private String PeekBuffer="";

	/**The string of characters we've peeked or unread since the last read(). On the next call to read(), these characters will first be unread.
	@see peek
	@see read
	@see unread
	G***later use this buffer for buffering
	*/
//G***fix later	private String Buffer="";

	/*Constructor to create a <code>BufferedPushbackReader</code> from another reader.
	@param inReader The reader that contains the data.
	*/
	public BufferedPushbackReader(final Reader inReader)
	{
//G***del System.out.println("Creating a BufferedPushbackReader.");	//G***del
		InReader=inReader;	//store the reader they passed us
		createBuffer();	//create our buffer
//G***fix		CurrentLineIndex=0;		//show that we're starting on the first line
//G***fix		CurrentCharIndex=-1;	//show that we haven't read any characters, yet
	}
//G***del */

	/*Constructor to create a <code>BufferedPushbackReader</code> from another
		reader, along with several characters that have already been read.
		<code>prereadCharacters</code> must be less than or equal to the length of
		the buffer.
	@param inReader The reader that contains the data.
	@param prereadCharacters The characters that have already been read.
	@exception IOException Thrown if <code>prereadCharacters</code> is too long for the buffer.
	*/
	public BufferedPushbackReader(final Reader inReader, final StringBuffer prereadCharacters) throws IOException
	{
		this(inReader);	//do the default construction
		loadBuffer(prereadCharacters.toString());	//load the specified characters into the buffer
	}

	/**Constructor to create a <code>BufferedPushbackReader</code> from an input stream
		using the system default encoding.
	@param inStream The input stream that contains the text.
	*/
/*G***del
	public BufferedPushbackReader(final InputStream inStream)
	{
		this(new InputStreamReader(inStream));	//create an input stream reader and do the default constructing G***check; is this the type of reader we want to construct?
	}
*/

	/**Constructor to create a reader from the contents of a string.
	@param inString The string that should be used for input.
	@except IOException Thrown when an I/O error occurs.
	*/
	public BufferedPushbackReader(final String inString) throws IOException
	{
		InReader=null;	//show that we will not have an input reader to read from; this will be fine, because we'll never have have to fetch any other buffers
		setBuffer(inString.toCharArray());	//instead of creating a new buffer, just use the string's buffer
		setBufferEndIndex(inString.length());	//show that the buffer is as long as the string
		setFetchBufferIndex(getBufferEndIndex());	//we'll never need to fetch another buffer, but here is where we would need to if we were going to
		setLastBuffer(true);	//show that this is the last buffer we have, so that fetchBuffer() will not try to fetch another buffer
		processBufferedData(0);	//process the string data before we start reading from it
	}

	/**Updates all indexes by a specific amount, either positive or negative.
	@param delta The amount to update the indexes forward or backwards (for positive
		and negative values, respectively.
	*/
/*G***del if we don't need
	protected void adjustIndexes(final int delta)
	{
*/

	/**Creates a new buffer and initializes buffer indexes. Any data in previous
		buffers will be erased.
	*/
	protected void createBuffer()
	{
		setBuffer(new char[Math.max(getBufferSize(), getPushbackBufferSize())]);	//create a new buffer large enough to hold buffered characters and pushback characters
		setBufferEndIndex(0);	//show that we're already at the end of this buffer
		setFetchBufferIndex(0);	//show that we should fetch another buffer before getting any information
/*G***del
		setReadIndex(0);	//we'll start reading at the first character next time
		setPeekIndex(getReadIndex());	//we'll start peeking at same place
		setBufferEndIndex(0);	//show that we're already at the end of this buffer
		setFetchBufferIndex(0);	//show that this is where we should begin rebuffering
*/
	}

	/*Loads the buffer with the specified characters.
	<code>bufferCharacters</code> must be less than or equal to the room left in the buffer.
	@param bufferCharacters The characters which should be placed in the buffer.
	@exception IOException Thrown if <code>bufferCharacters</code> is too long for the buffer.
	*/
	protected void loadBuffer(final String bufferCharacters) throws IOException
	{
		final int charactersLength=bufferCharacters.length();	//find out how many characters there are
		final int destIndex=getBufferEndIndex();	//we'll start putting new data at the end of the buffer
		final int roomLeft=getBufferSize()-destIndex;	//find out how many characters can physically fit into our buffer
		if(roomLeft<charactersLength)	//if there isn't enough room for our characters
			throw new IOException("Not enough room in buffer for loading specified characters.");	//throw an exception
		System.arraycopy(bufferCharacters.toCharArray(), 0, getBuffer(), destIndex, charactersLength);	//copy the characters to the buffer
		setBufferEndIndex(getBufferEndIndex()+charactersLength);	//update the end of the buffer
		setFetchBufferIndex(getBufferEndIndex());	//we'll fetch a new buffer when we reach the end of the buffer
		processBufferedData(destIndex);	//do whatever processing we need to do with the newly fetched data
	}

	/**Initializes relevant buffer indexes when, for example, the buffer is first
		created. This implementation sets the read index and the peek index; the
		other buffer end and fetch buffer indexes will be	set by methods that set
		the buffer itself, such as <code>createBuffer()</code>.
		This method is called from within <code>setBuffer()</code>.
	@see #createBuffer
	@see #setBuffer
	*/
	protected void initializeIndexes()
	{
		ReadIndex=0;	//we'll start reading at the first character next time (don't use the setReadIndex() function, because that will try to update the EOF property before all the other index are initialized)
		PeekIndex=getReadIndex();	//we'll start peeking at same place
	}

	/**Adjusts all indexes by a certain amount. This method is currently called
		by <code>fetchBuffer()</code> after moving data in the buffer.
	@param The number of characters to move the indexes, positive for forwards,
		negative for backwards.
	@see #fetchBuffer
	*/
	protected void adjustIndexes(final int moveDelta)
	{
		ReadIndex=getReadIndex()+moveDelta;	//readjust the read index; do not use setReadIndex() as that will try to check for EOF before all our indexes are updated
		PeekIndex=getPeekIndex()+moveDelta;	//readjust the peek index
		BufferEndIndex=getBufferEndIndex()+moveDelta;	//readjust the end of the buffer
		FetchBufferIndex=getFetchBufferIndex()+moveDelta;	//readjust the index of the location to fetch a new buffer
	}

	/**Moves a portion of the buffer. This method is called from <code>fetchBuffer</code>
		before fetching data.
	@param sourceIndex The index in the buffer to be moved.
	@param destIndex The destination in the buffer for the data being moved.
	@param len The number of characters to move.
	@see #fetchBuffer
	*/
	protected void moveBuffer(final int sourceIndex, final int destIndex, final int len)
	{
		System.arraycopy(getBuffer(), sourceIndex, getBuffer(), destIndex, len);	//move the data in the buffer
	}

	/**Fills a buffer with new information.
		This method will read as much data as it can without blocking, and return
		the number of characters read.
		If the current buffer is full, the last section of the buffer will be moved
		to the front to make room for more data; otherwise, the data will simply be
		appended to the buffer.
		This method does not change the EOF status.
	@return The number of characters fetched, or -1 if the end of the file was reached.
	@except IOException Thrown when an I/O error occurs.
	@see BufferedPushbackReader#isLastBuffer
	*/
	protected int fetchBuffer() throws IOException
	{
		//G***should we check to make sure that we have a valid reader?

//G***del System.out.println("Getting ready to fetch a new buffer, isEOF: "+isEOF()+" isLastBuffer: "+isLastBuffer());	//G***del
//G***del System.out.println("Getting ready to fetch a new buffer.");	//G***del
		if(!isEOF() && !isLastBuffer())	//if we're not out of data and we haven't already read the last buffer of information (meaning all previous buffers were completely full)
		{
			if(getBufferSize()-getBufferEndIndex()==0)  //if there is no room for more data (the last fetchBuffer() filled the entire buffer), we'll shift the buffer contents down
		  {
	//G***del System.out.println("Inside fetchBuffer(), not EOF or last buffer");	//G***del
				//first, move any unread characters and pushback buffer space to the beginning of the array
				final int sourceBegin=Math.max(getReadIndex()-getPushbackBufferSize(), 0);	//reserve a certain amount of space for pushing back, unless we haven't read anything to pushback
				if(sourceBegin>0)	//if we have anything to move back
				{
	//G***del System.out.println("FetchBuffer() ready to move data backwards; readIndex: "+getReadIndex());	//G***del
					final int sourceEnd=getBufferEndIndex();	//we're going to copy everthing up to the end of the buffer
	//G***del System.out.println("Source begin: "+sourceBegin);	//G***del
	//G***del System.out.println("Source end: "+sourceEnd);	//G***del
					final int moveDelta=-sourceBegin;	//find how much we're going to move the data
					int sourceIndex=sourceBegin;	//we'll start reading at the start of the source
					int destIndex=0;	//we'll copy all of this to the beginning of the array
	//G***del System.out.println("fetchBuffer() ready to copy, sourceBegin: "+sourceBegin+" len: "+(sourceEnd-sourceBegin));	//G***del
					moveBuffer(sourceBegin, destIndex, sourceEnd-sourceBegin);	//move the data back to have room for more data
	//G***del if we don't need				adjustIndexes(moveDelta);	//readjust the indexes
	//G***del System.out.println("Moved data, ready for update");	//G***del
					adjustIndexes(moveDelta);	//adjust the indexes since we've moved the data
	/*G***fix
					setReadIndex(getReadIndex()+moveDelta);	//readjust the read index G***will this cause problems if setReadIndex() updates the EOF?
					setPeekIndex(getPeekIndex()+moveDelta);	//readjust the peek index
					setBufferEndIndex(getBufferEndIndex()+moveDelta);	//readjust the end of the buffer
					setFetchBufferIndex(getFetchBufferIndex()+moveDelta);	//readjust the index of the location to fetch a new buffer
	*/
				}
		  }
			final int destIndex=getBufferEndIndex();	//we'll start putting new data at the end of the buffer
			final int numCharsToRead=getBufferSize()-destIndex;	//find out how many characters can physically fit into our buffer G***this currently doesn't check to see if the pushback buffer was larger than the buffer size
			if(numCharsToRead>0)	//if we have any room left in our buffer
			{
		//G***fix		int numCharactersRead=0;	//this will keep track of the number of characters we've read
	//G***del System.out.println("FetchBuffer() ready to read new data.");	//G***del
				final int numCharactersRead=getInReader().read(getBuffer(), destIndex, numCharsToRead);	//read characters into our array and find out how many characters we read

//G***del Debug.trace("Read number of characters: ", numCharactersRead);	//G***del

	//G***del System.out.println("getInReader().read() returned this many characters: "+numCharactersRead);	//G***del
				if(numCharactersRead!=EOF_VALUE)	//if we read any characters at all
				{
//G***del Debug.trace(new String(getBuffer(), destIndex, numCharactersRead));	//G**8del
/*G**del when works; comment; not reading all the characters does *not* mean this is the last buffer
					if(numCharactersRead!=numCharsToRead)	//if we wanted more characters than we got
						setLastBuffer(true);	//show that this was the last buffer of information
*/
					final int newDataBeginIndex=getFetchBufferIndex();	//we'll count everything after the "fetch buffer" marker as new data, even though it may be data that was read last time
					setBufferEndIndex(getBufferEndIndex()+numCharactersRead);	//update the end of the buffer
					setFetchBufferIndex(getBufferEndIndex());	//we'll fetch a new buffer when we reach the end of the buffer
					processBufferedData(newDataBeginIndex);	//do whatever processing we need to do with the newly fetched data
					return numCharactersRead;	//return the number of characters read
				}
				else	//if there were no more characters to read
				{
					setLastBuffer(true);	//show that this was the last buffer read (we can't get any more characters from our input reader)
					return 0;	//show that we couldn't fetch any characters
				}
			}
			else	//if there was no room left in the buffer G***we'll probably want to enlarge the buffer here
				return 0;	//show that we didn't read any characters; this doesn't mean that we've reached the end of the file; maybe they tried to fetch the buffer twice in a row
//G***del; this is incorrect!				setLastBuffer(true);	//show that this was the last buffer read
/*G***del
			else	//if we didn't read any characters
				setEOF(true);	//show that we reached the end of the data
*/
		}
//G***del System.out.println("fetchBuffer() returning EOF_VALUE");	//G***del
		return EOF_VALUE;	//if we reach here, we either were already at the end of the file or we couldn't fetch any more characters
	}

	/**Provides away to do initial processing on newly-buffered data. The fetch
	index may be modified.
	@param newDataBeginIndex The starting index of the newly fetched data.
	@except IOException Thrown when an I/O error occurs.
	@see #getFetchBufferIndex
	*/
	protected void processBufferedData(final int newDataBeginIndex) throws IOException {}

	/**Checks to see if the end of the data has been reached, and updates the EOF
		property if so. If the EOF property is already set to <code>true</code>, this
		function does nothing.
	@return Whether or not the end of the file has been reached.
	@except IOException Thrown when an I/O error occurs.
	@see ParseReader#isEOF
	*/
	protected boolean updateEOF() throws IOException
	{
//G***del System.out.println("updateEOF() index: "+getReadIndex()+" fetch at: "+getFetchBufferIndex());	//G***del
		if(!isEOF())	//if we don't think we've hit the end of the file, yet, we'll make sure
		{
//G***del System.out.println("updateEOF() not EOF");	//G***del
			if(getReadIndex()>=getFetchBufferIndex())	//if we're at a place where we should fetch a new buffer of information (we should never be *over* this amount, but >= is used just to be redundantly safe)
			{
//G***del System.out.println("updateEOF() ready to fetch");	//G***del
//G***del System.out.println("Before FetchBuffer() in updateEOF()");	//G***del
				if(fetchBuffer()==EOF_VALUE)	//fetch another buffer of information; if we were not able to get more information
					setEOF(true);	//show that we're at the end of the file
			}
		}
		return isEOF();	//return whether or not we're at the end of the file (this should be a definite answer)
	}

	/**Reports if there are characters left to be read.
	Resets peeking.
	@return <code>true</code> if there are no characters left to read, else <code>false</code> if there are characters waiting.
	@except IOException Thrown if an I/O error occurs.
	@see TextReader#peek
	@see TextReader#resetPeek
	*/
/*G***fix
	public boolean isEOF() throws IOException
	{
		resetPeek();	//since we're checking to see if there are more characters to be *read*, not peeked, we'll need to synchronize with resetPeek()
		final boolean eof=peek()==-1;	//try to peek a character to see if it's the end of the file (we'll get -1 if it is the end of the stream)
		resetPeek();	//now that we're finished peeking, we'll reset peek so that the next character peeked will reflect the next character to be read
		return eof;	//return whether or not the end of the file has been reached
	}
*/

	/**Read a single character. This method will block until a character is
		available, an I/O error occurs, or the end of the stream is reached.
	@return The character read, as an integer in the range 0 to 16383 (0x00-0xffff),
		or -1 if the end of the stream has been reached.
	@except IOException Thrown if an I/O error occurs.
	@see readChar
	*/
	public int read() throws IOException
	{
//G***fix		resetPeek();	//reset our peek buffer in case any characters were peeked so that the next character read will be accurate
		if(!isEOF())	//if we're not at the end of the file, yet
		{
			if(getReadIndex()>=getFetchBufferIndex())	//if we're at a place where we should fetch a new buffer of information (we should never be *over* this amount, but >= is used just to be redundantly safe)
			{
//G***del System.out.println("Before FetchBuffer() in read()");	//G***del
				if(fetchBuffer()==EOF_VALUE)	//fetch another buffer of information; if we were not able to get more information
					setEOF(true);	//show that we're at the end of the file
			}
			if(!isEOF())	//fetching another buffer could have alerted us to being at the end of the file even though we didn't know it before; if we're not at the end of the file, yet
			{
				final char readChar=getBuffer()[getReadIndex()];	//get the character at our current read position
//G***del System.out.println("read() found character: "+readChar+" ASCII "+(int)readChar+" at index: "+getReadIndex());	//G***del
//G***del System.out.println("Before setReadIndex() in read()");	//G***del
				setReadIndex(getReadIndex()+1);	//advance the read index
//G***del; this is now in setReadIndex()				resetPeek();	//show that our next peek location will be at the same position -- that is, reset peeking
//G***del System.out.println("read() found character: "+readChar+" ASCII "+(int)readChar);	//G***del
//G***del Debug.trace("read() found character: "+readChar+" ASCII "+(int)readChar);	//G***del
				return readChar;	//return the character we read
			}
		}
		//if we're at the end of the file
		resetPeek();	//reset peeking just because this function always does, even at the end of the file
		return EOF_VALUE;	//show that we reached the end of the file
	}

	/**Read characters into a portion of an array. This method will block until
		some input is available, an I/O error occurs, or the end of the stream is reached.
		Resets peeking.
	@param cbuf Destination buffer.
	@param off Offset at which to start storing characters.
	@param len Maximum number of characters to read.
	@return The number of characters read, or -1 if the end of the stream has been reached.
	@exception IOException Thrown if an I/O error occurs.
	*/
	public int read(char[] cbuf, int off, int len) throws IOException
	{
		if(!isEOF())	//if we're not at the end of the file
		{
			int numCharsRead=0;	//this will store the number of characters actually read; right now, we don't know if we're going to read any characters
			while(numCharsRead<len)	//keep reading characters until we've read as many as they want
			{
				final int numCharsInBuffer=getFetchBufferIndex()-getReadIndex();	//find out how many characters are left in the buffer
				final int numCharsToReadThisTime=Math.min(len-numCharsRead, numCharsInBuffer);	//read the number of characters we need to, or however many are left in the buffer this time
				final int sourceBegin=getReadIndex();	//find out where to start copying
				System.arraycopy(getBuffer(), sourceBegin, cbuf, off, numCharsToReadThisTime);	//copy the characters, or all the characters in this buffer, whichever is less
				numCharsRead+=numCharsToReadThisTime;	//show that we read at least a little more
				off+=numCharsToReadThisTime;	//show that our destination offset also increased
				setReadIndex(getReadIndex()+numCharsToReadThisTime);	//move the read position ahead the number of characters that we're going to read; only do this *after* we've copied in case that setReadIndex() fetches another buffer
				if(numCharsRead<len)	//if we still have more characters to read
				{
					if(!isEOF())	//if we're not at the end of the file, yet (the call to setReadIndex(), above, will have checked)
						fetchBuffer();	//fetch another buffer of information (actually, going to the end of the buffer would automatically have fetched another buffer, above, so this is redundant but won't considerably hurt performance, because fetchBuffer() will quickly return when it sees the buffer is full) G***maybe take this out and put a comment about setReadIndex() fetching another buffer if needed
					else	//if we are at the end of the file
						break;	//show that there's nothing left to read
				}
			}
			resetPeek();	//reset peeking, as this function always does, whether or not we were able to read any characters
			return numCharsRead;	//return the number of characters we were able to read before reaching the end of the file
		}
		else	//if we've reached the end of the file
		{
			resetPeek();	//reset peeking, as this function always does, whether or not we were able to read any characters
			return EOF_VALUE;	//show that we're at the end of the data
		}
	}

	/**Reads characters until the end of the file is reached.
	@param delimiterChar The character which indicates reading should stop.
	@except IOException Thrown when an i/o error occurs.
	@return The characters up to the end of the file.
	*/
/**del if we don't need or want
	public String readString() throws IOException	//G***make sure this function isn't a duplication of something in the Java API
	{
		String characterString="";	//this string will receive the characters we've read
		int i=read();	//read a character
		while(i!=-1)		//while we haven't reached the end of the file
		{
			characterString+=(char)i;	//add this character to our string
			i=read();	//read another character
		}
		return characterString;	//return the characters we read
	}
*/

	/**Pushes back a single character.
		Resets peeking.
	@param c The character to push back.
	@exception IOException Thrown if there is no room to push back characters
		(the pushback buffer is full) or some other I/O error occurs.
	*/
	public void unread(int c) throws IOException
	{
		unskip(1);	//back up one position
		getBuffer()[getReadIndex()]=(char)c;			//put the character they provided us in that location
/*G***del
		if(getReadIndex()>0)	//if we're not at the beginning of the buffer
		{
//G***del System.out.println("Before setReadIndex() in unRead()");	//G***del
			setReadIndex(getReadIndex()-1);	//back the read index up one position, which will reset peeking as well
			getBuffer()[getReadIndex()]=(char)c;			//put the character they provided us in that location
//G***del			resetPeek();	//reset peeking
		}
		else	//if we can't push back more characters G***we could copy characters forward to make room
			throw new IOException("No room to push back character '"+c+"'.");	//show we had a buffer underrun
*/
	}

	/**Pushes back a portion of an array of characters.
		Resets peeking.
	@param cbuf The array of characters to push back.
	@param off The offset of the first character to push back.
	@param len The number of characters to push back.
	@exception IOException Thrown if there is no room to push back characters
		(the pushback buffer is full) or some other I/O error occurs.
	*/
	public void unread(char[] cbuf, int off, int len) throws IOException
	{
		unskip(len);	//back up the correct number of positions
		System.arraycopy(cbuf, off, getBuffer(), getReadIndex(), len);	//copy the characters into the buffer
	}

	/**Pushes back an array of characters.
		Resets peeking.
	@param cbuf The array of characters to push back.
	@exception IOException Thrown if there is no room to push back characters
		(the pushback buffer is full) or some other I/O error occurs.
	*/
	public void unread(char[] cbuf) throws IOException
	{
		unread(cbuf, 0, cbuf.length); //unread all the characters in the array, starting at the first character
	}

	/**Close the stream. Once a stream has been closed, further read(), ready(),
		mark(), or reset() invocations will throw an IOException.
	Closing a previously-closed stream, however, has no effect.
	@except IOException Thrown if an I/O error occurs.
	*/
	public void close() throws IOException
	{
		if(getInReader()!=null)	//if we have a reader
			getInReader().close();	//G***fix later with our own code
		//G***we need to decide how to close when we're reading from a string, so that further calls to read() and such will throw an exception
		setBuffer(null);	//release our buffer so that it can be garbage collected immediately
	}

		//G***what about overriding ready()?

	/**Peeks at the next character to be read. Each successive peek will return a successive character.
		This function is reset with every call to read().
		Since every peek reads a successive character, resetPeek() should be called if it must be ensured that the next character peeked is the next character to be read.
	@except IOException Thrown when an i/o error occurs.
	@return The next character that will be read after the character retrieved in the last read() or peek(), or -1 if there are no characters left to read.
	@see read
	@see resetPeek
	*/
	public int peek() throws IOException
	{
		if(!isEOF())	//if we're not at the end of the file, yet
		{
			if(getPeekIndex()>=getFetchBufferIndex())	//if we're peeking at a place where we should fetch a new buffer of information (we should never be *over* this amount, but >= is used just to be redundantly safe)
			{
//G***del System.out.println("Before FetchBuffer() in peek()");	//G***del
				if(fetchBuffer()==EOF_VALUE)	//fetch another buffer of information; if we couldn't fetch any more information
					return EOF_VALUE;	//show that we can't peek anymore (but don't set the EOF variable, because our read position is not necessarily at the end of the file)
			}
			final char peekedChar=getBuffer()[getPeekIndex()];	//get the character at our current peek position
			setPeekIndex(getPeekIndex()+1);	//advance the peekindex
			return peekedChar;	//return the character we peeked
		}
		else	//if we are at the end of the file
			return EOF_VALUE;	//show that we can't peek anymore, because we're at the end of the file
	}
/*G***del
		int peekChar;	//this will hold the character peeked
		final int i=getInReader().read();	//read a character
		switch(i)	//see which character this is
		{
			case CR_CHAR:	//if this is the carriage return character (CR==#xD=='\r') (XML only wants an LF at the end of each line)
				{
					int i2=getInReader().read();	//read another character to see if this is a CRLF combination
					if((char)i2!=LF_CHAR && i2!=-1)	//if this is *not* part of of a CRLF ("\r\n"), we'll put the character back
						getInReader().unread(i2);	//put the character back so we can read it next time (it will be the first character on the next line)
				}
				peekChar=LF_CHAR;	//we'll normalize all end-of-line characters to LF, as the XML spec requires
				break;
			case LF_CHAR:	//if this is a linefeed character (LF==#xA=='\n')
				peekChar=LF_CHAR;	//we'll normalize all end-of-line characters to LF, as the XML spec requires
				break;
			default:	//if this is not an end-of-line indicator, it's not something to process at this point; just pass it back
				peekChar=i;	//we'll just use the character we read
				break;
		}
		if(i!=-1)	//if we haven't reached the end of the file
			PeekBuffer+=(char)peekChar;	//show that there is one more character we peeked
//G***del System.out.println("Peek buffer after peeking  "+(char)i+": "+PeekBuffer+"\n");	//G***del

		return peekChar;	//return the character we peeked
}
*/

	/**Peeks a specified number of characters into a portion of an array. This
		method will block until some input is available, an I/O error occurs, or
		the end of the stream is reached.
		Resets peeking.
	@param cbuf Destination buffer.
	@param off Offset at which to start storing characters.
	@param len Maximum number of characters to peek.
	@return The number of characters peeked, or -1 if the end of the stream has been reached.
	@exception IOException Thrown if an I/O error occurs.
	*/
	public int peek(char[] cbuf, int off, int len) throws IOException
	{
//G***del System.out.println("needed: "+len);	//G***del
		if(!isEOF())	//if we're not at the end of the file
		{
			int numCharsPeeked=0;	//this will store the number of characters actually peeked; right now, we don't know if we're going to peek any characters
			while(numCharsPeeked<len)	//keep peeking characters until we've peeked as many as they want
			{
//G***del System.out.println("peeked: "+numCharsPeeked);	//G***del
				final int numCharsInBuffer=getFetchBufferIndex()-getPeekIndex();	//find out how many characters are left in the buffer
				final int numCharsToPeekThisTime=Math.min(len-numCharsPeeked, numCharsInBuffer);	//peek the number of characters we need to, or however many are left in the buffer this time
				final int sourceBegin=getPeekIndex();	//find out where to start copying
//G***del System.out.println("numCharsInBuffer: "+numCharsInBuffer+" numCharsToPeekThisTime: "+numCharsToPeekThisTime+" sourceBegin: "+sourceBegin);	//G***del
				setPeekIndex(getPeekIndex()+numCharsToPeekThisTime);	//move the peek position ahead the number of characters that we're going to peek
				System.arraycopy(getBuffer(), sourceBegin, cbuf, off, numCharsToPeekThisTime);	//copy the characters, or all the characters in this buffer, whichever is less
				numCharsPeeked+=numCharsToPeekThisTime;	//show that we peeked at least a little more
				off+=numCharsToPeekThisTime;	//show that our destination offset also increased
				if(numCharsPeeked<len)	//if we still have more characters to peek
				{
/*G***del
System.out.println("Number of characters peeked "+numCharsPeeked+" vs amount wanted: "+len);	//G***del
System.out.println("Peek index now: "+getPeekIndex()+" Read index now: "+getReadIndex());	//G***del
*/
						//G***probably move this clause (and all similar ones) to the front of the loop
					if(getPeekIndex()>=getFetchBufferIndex() && !isLastBuffer())	//if we've peeked to the end of the buffer, and this isn't the last buffer
					{
//G***del System.out.println("peek() fetching another buffer");	//G***del
						fetchBuffer();	//fetch the next buffer G***note that if they peek *way* ahead of the read index, the fetchBuffer() won't read anymore, and we'll go into an endless loop; check the result of fetchBuffer() here
					}
					else	//if we haven't peeked to the end of the buffer, and we still haven't peeked all we need to
						break;	//show that there's nothing left to peek
				}
			}
			return numCharsPeeked;	//return the number of characters we were able to peek before reaching the end of the file
		}
		else	//if we've reached the end of the file
			return EOF_VALUE;	//show that we're at the end of the data
	}

	/**Resets any characters we've peeked so that the next peek will reflect the next character which will be read.
		Does not affect the next character to be read.
	@see BufferedPushbackReader#peekChar G***fix this
	*/
	public void resetPeek()
	{
		setPeekIndex(getReadIndex());	//make the peek indes the same as the read index
	}

	/**@return A string of all the characters, if any, that have been peeked since the last time peeking was reset.
	@see TextReader#peek
	@see TextReader#resetPeek
	*/
/*G***see if we need this or not
	public String getPeekedChars()
	{
		return PeekBuffer;	//return the buffer which contains the characters peeked
	}
*/

	/**Skips the specified number of characters. This method will block until some
		characters are available, an I/O error occurs, or the end of the stream is reached.
		Resets peeking.
	@param n The number of characters to skip.
	@return The number of characters actually skipped.
	@except IOException Thrown when an I/O error occurs.
	*/
	public long skip(long n) throws IOException
	{
		long numCharsSkipped=0;	//this will store the number of characters actually skipped; right now, we don't know if we're going to skip any characters
		if(!isEOF())	//if we're not at the end of the file
		{
			while(numCharsSkipped<n)	//keep skipping characters until we've skipped enough
			{
				final int numCharsInBuffer=getFetchBufferIndex()-getReadIndex();	//find out how many characters are left in the buffer
				final int numCharsToSkipThisTime=n-numCharsSkipped<numCharsInBuffer ? (int)(n-numCharsSkipped) : numCharsInBuffer;	//skip the amount of characters we need to, or however many is left in the buffer this time
//G***del				Math.min(n, (long)numCharsInBuffer);	//
				setReadIndex(getReadIndex()+numCharsToSkipThisTime);	//move the read position ahead the number of characters that we're going to skip
				numCharsSkipped+=numCharsToSkipThisTime;	//show that we skipped at least a little more
				if(numCharsSkipped<n)	//if we still have more characters to skip
				{
					if(!isEOF())	//if we're not at the end of the file, yet (the call to setReadIndex(), above, will have checked)
						fetchBuffer();	//fetch another buffer of information (actually, going to the end of the buffer would automatically have fetched another buffer, above, so this is redundant but won't considerably hurt performance, because fetchBuffer() will quickly return when it sees the buffer is full) G***maybe remove this and put a comment about setReadIndex() fetching another buffer if needed
					else	//if we are at the end of the file
						break;	//show that there's nothing left to skip
				}
			}
/*G***del when works
				final int numCharsActuallySkipped=Math.min(n, getFetchBufferIndex()-getReadIndex());	//it's possible that, after fetching another buffer, we still don't have as many character to skip as they want, so take the minimum of how many are left and how many they want to skip

				if(numCharsInBuffer<n)	//if there are not enough characters left in the buffer to skip everything we need to
				{
					setReadIndex(getFetchBufferIndex());	//go to the end of our current buffer
					fetchBuffer();	//fetch the next buffer
				}
				else	//if there are enough characters left in the buffer to skip all we need
				{

				}

					fetchBuffer();	//try to fetch another bufferful of information
//G***fix all this so that this will work with skip amounts that are larger than our buffer
			numCharsSkipped=Math.min(n, getFetchBufferIndex()-getReadIndex());	//it's possible that, after fetching another buffer, we still don't have as many character to skip as they want, so take the minimum of how many are left and how many they want to skip
			setReadIndex(getReadIndex()+(int)numCharsSkipped);	//move the read position ahead the number of characters that we're going to skip G***fix this so that we can skip long distances
*/
		}
		resetPeek();	//reset peeking, as this function always does, whether or not we were able to skip any characters
		return numCharsSkipped;	//return the number of characters we were able to skip before reaching the end of the file
	}

	/**Skips the number of characters as the specified string. This is a convenience
		function in cases where, for example, one has peeked a string and then needs
		to skip over it. This function has the identical effect of calling skip(s.length().
		This method will block until some characters are available, an I/O error
		occurs, or the end of the stream is reached.
		Resets peeking.
	@param n The number of characters to skip.
	@return The number of characters actually skipped.
	@except IOException Thrown when an I/O error occurs.
	*/
/*G***del; it's too easy to get confused with skipExpectedString()
	public long skip(final String s) throws IOException
	{
		return skip(s.length());	//skip the specified number of characters
	}
*/

	/**Reverses the reading position to the previous characters read. Functions
		similar to unread() except that the unread characters will always be the
		characters that were previously read.
		Resets peeking.
	@param n The number of characters to unskip.
	@exception IOException Thrown if there is no room to push back characters
		(the pushback buffer is full) or some other I/O error occurs.
	@see BufferedPushbackReader#unread
	*/
	public void unskip(final int n) throws IOException
	{
		if(getReadIndex()-n>=0)	//if we have enough room to back up the requested number of characters
		{
//G***del System.out.println("Before setReadIndex() in unRead()");	//G***del
			setReadIndex(getReadIndex()-n);	//back the read index up the specified number of characters, which will reset peeking as well
		}
		else	//if we can't push back more characters G***we could copy characters forward to make room
			throw new IOException("Not enough room to push back "+n+" character(s); only room for "+getReadIndex()+".");	//show we had a buffer underrun
	}

	/**Skips every character that matches the skip characters, and returns the number of characters skipped.
	Throws an exception if the end of the file is unexpectedly reached.
	Resets peeking.
	@param skipChars The characters which should be skipped.
	@except IOException Thrown when an i/o error occurs.
	@except EOFException Thrown if the end of the file is reached unexpectedly.
	@return The number of characters skipped.
	*/
/*G***del if we don't want this in this class
	protected long skipChars(final String skipChars) throws IOException
	{
		resetPeek();	//reset our peeking so that we'll really be peeking the next character
		int numCharsSkipped=0;	//show that we haven't skipped any characters yet
		while(skipChars.indexOf(peekCharacter())!=-1)	//peek a character; if the character is one of our skipped characters
			++numCharsSkipped;	//show that we have one more character to skip
		skipCharacters(numCharsSkipped);	//now that we know how many to skip, actually skip the characters
		resetPeek();	//reset peeking so that the next character peeked will reflect the next character to be read
		return numCharsSkipped;	//return the number of characters we skipped
	}
*/















	/**Retrieves the specified number of bytes with no processing. The next call
		to <code>read()</code>, <code>peek()</code>, or other function will function
		as if no data has been retrieved. This function is similar to <code>peek()</code>
		except that no processing occurs and no buffering takes place.
		This function must be called before any data has been buffered -- that is,
		before any normal <code>read()</code> or <code>peek()</code> functions have
		been called. Otherwise, an <code>IOException</code> will be thrown.
	@param cbuf Destination buffer.
	@param off Offset at which to start storing characters.
	@param len Maximum number of characters to peek.
	@return The number of characters peeked, or -1 if the end of the stream has been reached.
	@exception IOException Thrown if an I/O error occurs or if the function is
		called after data has been buffered (that is, after a call to any other
		reading or peeking function).
	*/
/*G***fix
	public int preread(byte[] bbuf, int off, int len) throws IOException
	{
//G***del System.out.println("needed: "+len);	//G***del
		if(!isEOF())	//if we're not at the end of the file
		{





			int numBytesPreread=0;	//this will store the number of bytes actually prepread; right now, we don't know if we're going to preread any bytes
			while(numCharsPeeked<len)	//keep peeking characters until we've peeked as many as they want
			{
//G***del System.out.println("peeked: "+numCharsPeeked);	//G***del
				final int numCharsInBuffer=getFetchBufferIndex()-getPeekIndex();	//find out how many characters are left in the buffer
				final int numCharsToPeekThisTime=Math.min(len-numCharsPeeked, numCharsInBuffer);	//peek the number of characters we need to, or however many are left in the buffer this time
				final int sourceBegin=getPeekIndex();	//find out where to start copying
//G***del System.out.println("numCharsInBuffer: "+numCharsInBuffer+" numCharsToPeekThisTime: "+numCharsToPeekThisTime+" sourceBegin: "+sourceBegin);	//G***del
				setPeekIndex(getPeekIndex()+numCharsToPeekThisTime);	//move the peek position ahead the number of characters that we're going to peek
				System.arraycopy(getBuffer(), sourceBegin, cbuf, off, numCharsToPeekThisTime);	//copy the characters, or all the characters in this buffer, whichever is less
				numCharsPeeked+=numCharsToPeekThisTime;	//show that we peeked at least a little more
				off+=numCharsToPeekThisTime;	//show that our destination offset also increased
				if(numCharsPeeked<len)	//if we still have more characters to peek
				{
						//G***probably move this clause (and all similar ones) to the front of the loop
					if(getPeekIndex()>=getFetchBufferIndex() && !isLastBuffer())	//if we've peeked to the end of the buffer, and this isn't the last buffer
					{
//G***del System.out.println("peek() fetching another buffer");	//G***del
						fetchBuffer();	//fetch the next buffer G***note that if they peek *way* ahead of the read index, the fetchBuffer() won't read anymore, and we'll go into an endless loop; check the result of fetchBuffer() here
					}
					else	//if we haven't peeked to the end of the buffer, and we still haven't peeked all we need to
						break;	//show that there's nothing left to peek
				}
			}
			return numCharsPeeked;	//return the number of characters we were able to peek before reaching the end of the file
		}
		else	//if we've reached the end of the file
			return EOF_VALUE;	//show that we're at the end of the data
	}
*/

}
