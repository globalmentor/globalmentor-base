package com.garretwilson.io;

import java.io.*;
import java.net.URL;	//G***decide if we want a URL constructor or not
//G***testing import com.garretwilson.io.TextReader;
import com.garretwilson.io.BufferedPushbackReader;
//G***del if we don't need import com.garretwilson.util.StringManipulator;
import com.garretwilson.lang.*;
import com.garretwilson.util.Debug;

/**Class which provides methods for parsing data from a stream.*/
public class ParseReader extends BufferedPushbackReader
{

	/**Line numbers are zero-based.*/
//G***del	public final static long FIRST_LINE_NUMBER=0;

	/**Character numbers are zero-based.*/
//G***del	public final static long FIRST_CHARACTER_NUMBER=0;

	/**The source of the data (e.g. a String, File, or URL).*/
	private Object SourceObject=null;

		/**@return The source of the data, if one has been assigned.*/
		public Object getSourceObject() {return SourceObject;}

		/**Sets the source of the reader's data.
		@param newSourceObject The new source of the reader's data.
		*/
		public void setSourceObject(final Object newSourceObject) {SourceObject=newSourceObject;}

	/**The name of this reader, if one has been assigned.*/
	private String Name=null;

		/**Returns the name of this reader. If there is no name, and there is a
			source object, the source object in string form is returned. This function
			is guaranteed not to return <code>null</code>.
		@return The name of this reader, if one has been assigned, else the empty string.
		@see #getSourceObject
		*/
		public String getName()
		{
			if(Name!=null)	//if we have a name
				return Name;	//return the name
			else if(getSourceObject()!=null)	//if we don't have a name but have a source object
				return getSourceObject().toString();	//return the source object's string form
			else	//if we don't have a name or a source object
				return "";	//return the empty string
		}

		/**Sets the name of the reader.
		@param newName The new name of the reader.
		*/
		public void setName(final String newName) {Name=newName;}

	/**The index of the line of the character last read, or 0 if no characters
		have been read.*/
	private long LineIndex=0;

		/**@param bufferIndex The buffer index of specified character.
		@return The index of the line of the character at the specified index in the
			buffer.
		@see #getCharIndex
		*/
		public long getLineIndex(final int bufferIndex)
		{
			updatePosition(bufferIndex+1);	//make sure our position is updated
			return LineIndex;	//return the line index
		}

		/**@return The index of the line of the character last read, or 0 if no
			characters have been read.
		@see #getCharIndex
		*/
		public long getLineIndex()
		{
			return getLineIndex(getReadIndex()-1);	//get the line index of the last-read character
//G***del			updatePosition(getReadIndex());	//make sure our position is updated
//G***del			return LineIndex;	//return the line index
		}

		/**Changes the line index of the last character read.
		@param newLineIndex The new line index.
		@see #setCharIndex
		*/
		public void setLineIndex(final long newLineIndex) {LineIndex=newLineIndex;}

	/**The index of the character last read, or -1 if no characters have been read.*/
	private long CharIndex=-1;

		/**@param bufferIndex The buffer index of the specified character.
		@return The character index of the character at the specified index in the
			buffer.
		@see #getLineIndex
		*/
		public long getCharIndex(final int bufferIndex)
		{
			updatePosition(bufferIndex+1);	//make sure our position is updated
			return CharIndex;
		}

		/**@return The index of the character last read, or -1 if no characters have
			been read.
		@see #getLineIndex
		*/
		public long getCharIndex()
		{
			return getCharIndex(getReadIndex()-1);	//get the character index of the last-read character
//G***del			updatePosition(getReadIndex());	//make sure our position is updated
//G***del			return CharIndex;
		}

		/**Changes the index of the last character read.
		@param newCharIndex The new character index.
		@see #setLineIndex
		*/
		public void setCharIndex(final long newCharIndex) {CharIndex=newCharIndex;}

	/**The index of the last time the line and character indexes were updated. Since
		The position (line and character) is that of the last character read, this
		will, when updated, be one character behind the read index.
	@see #getLineIndex
	@see #getCharIndex
	@see BufferedPushbackReader#getReadIndex
	*/
	private int LastPositionIndex;

		/**@return The index of the last time the line and character indexes were updated.
		@see #getLineIndex
		@see #getCharIndex
		@see BufferedPushbackReader#getReadIndex
		*/
		protected int getLastPositionIndex() {return LastPositionIndex;}

		/**Sets the index of the last time the line and character indexes were updated.
			This is usually set to the current read index after synchronizing the position.
		@param lastPositionIndex The index of the last time the line and character indexes were updated.
		@see #getLineIndex
		@see #getCharIndex
		@see BufferedPushbackReader#getReadIndex
		*/
		protected void setLastPositionIndex(final int lastPositionIndex) {LastPositionIndex=lastPositionIndex;}

	/**The index of the line of the current character being processed.*/
//G***del	private int CurrentLineIndex;
	/**The index of the current character being processed.*/
//G***del	private int CurrentCharIndex;

	/*Constructor that specifies an input stream.
	@param inStream The input stream that contains the data.
	*/
/*G***del
	public ParseReader(final InputStream inStream)
	{
		super(inStream);	//construct the base class
//G***del System.out.println("Creating a ParseReader from an InputStream.");	//G***del
//G***del		CurrentLineIndex=0;		//show that we're starting on the first line
//G***del		CurrentCharIndex=-1;	//show that we haven't read any characters, yet
	}
*/

	/*Constructor that specifies another reader.
	@param reader The reader that contains the data.
	*/
	public ParseReader(final Reader reader)
	{
		super(reader);	//construct the base class
//G***del System.out.println("Creating a ParseReader from a Reader.");	//G***del
	}

	/*Constructor that specifies an input stream and a name.
	@param inStream The input stream that contains the data.
	@param name The name of the reader.
	*/
/*G***del
	public ParseReader(final InputStream inStream, final String name)
	{
		this(inStream);	//do the default constructing
//G***del System.out.println("Creating a ParseReader from an InputStream with name: "+name);	//G***del
		setName(name);	//set the name of the reader
	}
*/

	/*Constructor that specifies another reader and a name.
	@param reader The reader that contains the data.
	@param name The name of the reader.
	*/
	public ParseReader(final Reader reader, final String name)
	{
		this(reader);	//do the default constructing
//G***del System.out.println("Creating a ParseReader from a Reader with name: "+name);	//G***del
		setName(name);	//set the name of the reader
	}

	/**Constructor to create a reader from the contents of a string.
	@param inString The string that should be used for input.
	@param name The name of the reader.
	@except IOException Thrown when an I/O error occurs.
	*/
	public ParseReader(final String inString, final String name) throws IOException
	{
		super(inString);	//construct the parent class
		setSourceObject(inString);	//show where we're getting the data from
//G***del System.out.println("Creating a ParseReader from a string with name: "+name+" content: "+inString);	//G***del
		setName(name);	//set the name of the reader
	}

	/**Constructor to create a reader from a URL, setting the source object to be
		the URL.
	@param url The URL that should be used for input.
	@except IOException Thrown when an I/O error occurs.
	@see #setSourceObject
	*/
/*G***del if we can get away with it, because of the default input stream conversion
	public ParseReader(final URL url) throws IOException
	{
		super(url.openConnection().getInputStream());	//construct the parent class by connecting to the URL and getting an input stream
//G***del System.out.println("ParseReader(URL)");	//G***del
		setSourceObject(url);	//show where we're getting the data from, and let that represent the name as well
//G***del		setName(name);	//set the name of the reader
	}
*/

	/*Constructor that specifies an input stream and a source object.
	@param inStream The input stream that contains the data.
	@param sourceObject The source of the data (e.g. a String, File, or URL).
	*/
/*G***del
	public ParseReader(final InputStream inStream, final Object sourceObject)
	{
		this(inStream);	//do the default constructing
		setSourceObject(sourceObject);	//make a record of the object representing the location of the data
	}
*/

	/*Constructor to create a <code>ParseReader</code> from another
		reader, along with several characters that have already been read.
		<code>prereadCharacters</code> must be less than or equal to the length of
		the buffer.
	@param inReader The reader that contains the data.
	@param prereadCharacters The characters that have already been read.
	@exception IOException Thrown if <code>prereadCharacters</code> is too long for the buffer.
	@see BufferedPushbackReader
	*/
	public ParseReader(final Reader inReader, final StringBuffer prereadCharacters) throws IOException
	{
		super(inReader, prereadCharacters);	//allow the super class to do the constructing
	}

	/*Constructor to create a <code>ParseReader</code> from another
		reader, along with a source object.
		<code>prereadCharacters</code> must be less than or equal to the length of
		the buffer.
	@param inReader The reader that contains the data.
	@param sourceObject The source of the data (e.g. a String, File, or URL).
	@exception IOException Thrown if <code>prereadCharacters</code> is too long for the buffer.
	@see BufferedPushbackReader
	*/
	public ParseReader(final Reader inReader, final Object sourceObject) throws IOException
	{
		super(inReader);	//allow the super class to do the constructing
		setSourceObject(sourceObject);	//make a record of the object representing the location of the data
	}

	/*Constructor to create a <code>ParseReader</code> from another
		reader, along with several characters that have already been read and a source
		object.
		<code>prereadCharacters</code> must be less than or equal to the length of
		the buffer.
	@param inReader The reader that contains the data.
	@param prereadCharacters The characters that have already been read.
	@param sourceObject The source of the data (e.g. a String, File, or URL).
	@exception IOException Thrown if <code>prereadCharacters</code> is too long for the buffer.
	@see BufferedPushbackReader
	*/
	public ParseReader(final Reader inReader, final StringBuffer prereadCharacters, final Object sourceObject) throws IOException
	{
		super(inReader, prereadCharacters);	//allow the super class to do the constructing
		setSourceObject(sourceObject);	//make a record of the object representing the location of the data
	}

	/**Initializes relevant buffer indexes when, for example, the buffer is first
		created. The parent version of the method is first called and then the
		last position index is set.
	*/
	protected void initializeIndexes()
	{
		super.initializeIndexes();	//initialize the other indexes normally
		LastPositionIndex=getReadIndex()-1;	//synchronize the last position updated with the previous character read
	}

	/**Adjusts all indexes by a certain amount. This method calls the parent
		version of this method and then updates the last position index.
	@param The number of characters to move the indexes, positive for forwards,
		negative for backwards.
	*/
	protected void adjustIndexes(final int moveDelta)
	{
		super.adjustIndexes(moveDelta);	//move the parent indexes normally
		LastPositionIndex=getLastPositionIndex()+moveDelta;	//readjust the index where we last knew our position
	}

	/**Moves a portion of the buffer. This method updates the line and character
		positions before the data is moved.
	@param sourceIndex The index in the buffer to be moved.
	@param destIndex The destination in the buffer for the data being moved.
	@param len The number of characters to move.
	@see #getLineIndex
	@see #getCharIndex
	@see #getLastPositionIndex
	*/
	protected void moveBuffer(final int sourceIndex, final int destIndex, final int len)
	{
		updatePosition(sourceIndex);	//update our line and character indexes to one character behind the data we're moving
		super.moveBuffer(sourceIndex, destIndex, len);	//move the data normally
	}

	/**Updates the line and character indexes and sets the last position index to
		one character before the given index, which is usually the read index.
		This method counts the line breaks between the last position index and the
		read index -1 and updates the line and character indexes appropriately.
		This method is called by <code>getLineIndex()</code>, <code>getCharIndex()</code>,
		and <code>moveBuffer()</code>.
	@param updateIndex The index of the character after the position should be updated to.
	@see #getLineIndex
	@see #getCharIndex
	@see #getLastPositionIndex
	@see #moveBuffer
	@see #BufferedPushbackReader#getReadIndex
	*/
	protected void updatePosition(final int updateIndex)
	{
		final char[] buffer=getBuffer();	//get a reference to the buffer
//G***del		final int readIndex=getReadIndex();	//get the read index
//G***del		int lastPositionIndex=getLastPositionIndex();	//get the last index where we updated the position
		if(updateIndex-1>LastPositionIndex)	//if the update index is more the last place we updated our position
		{
			if(LastPositionIndex<0 && updateIndex>0)	//if we haven't read any characters yet, and the update index is valid
			{
				setCharIndex(CharIndex-LastPositionIndex);	//update our character index; in other words, before reading any characters the last position index will be negative (and so will the character index), so this will move them both forward; don't use getCharIndex(), because that will call this function again
				LastPositionIndex=0;	//show that we're ready to start processing the read characters
			}
			while(LastPositionIndex<updateIndex-1)	//while the last position read is under one minus the update index
			{
				if(buffer[LastPositionIndex]=='\n')	//if this was a new line character
				{
					setLineIndex(LineIndex+1);	//go to the next line; don't use getLineIndex() because that would call this function again
					setCharIndex(0);	//start at the first of the next line
				}
				else	//if this isn't a new line character
					setCharIndex(CharIndex+1);	//advance the character position; do not use getCharIndex() because that would call this function again G***this can probably be done more efficiently somehow
				++LastPositionIndex;	//move our last position index up one
			}
		}
		//G***fix for when they've unread characters
	}

	/**Read a single character, and throws an exception if the end of the file is reached.
	@return The character read.
	@except IOException Thrown if a general I/O error occurs.
	@except ParseEOFException Thrown if the end of the file is reached unexpectedly.
	@see TextReader#read
	*/
	public char readChar() throws IOException, ParseEOFException
	{
		final int i=read();	//read a character
		if(i!=-1)	//if we haven't reached the end of the file
			return (char)i;	//return the character
		else	//if we did reach the end of the file
			throw new ParseEOFException("End of stream reached while reading a character.", getLineIndex(), getCharIndex(), getName());	//show that we hit the end of the file G***Int
	}

	/**Peeks at the next character to be read, and throws an exception if there are no more characters to read.
	Each successive peek will return a successive character.
	This function is reset with every call to read().
	Since every peek reads a successive character, resetPeek() should be called if it must be ensured that the next character peeked is the next character to be read.
	This function really reads a character, but all characters peeked will be unread before characters are read in a call to readCharacter().
	@return The next character that will be read after the character retrieved in the last read() or peek().
	@except IOException Thrown when a general I/O error occurs.
	@except ParseEOFException Thrown if the end of the file is reached unexpectedly.
	@see TextReader#read
	@see ParseReader#readChar
	@see TextReader#resetPeek
	*/
	public char peekChar() throws IOException, ParseEOFException
	{
		final int i=peek();	//peek at a character
		if(i!=-1)	//if we haven't reached the end of the file
			return (char)i;	//return the character
		else	//if we did reach the end of the file
			throw new ParseEOFException("End of stream reached while reading a character.", getLineIndex(), getCharIndex(), getName());	//show that we hit the end of the file G***Int
	}

	/**Peeks the specified number of characters, and throws an exception if the end of the file is reached.
	This function is reset with every call to read().
	@param len The number of characters to peek.
	@return A string containing the characters peeked.
	@except IOException Thrown when a general I/O error occurs.
	@except ParseEOFException Thrown if the end of the file is reached unexpectedly.
	*/
	public String peekString(int len) throws IOException, ParseEOFException
	{
		final char[] charArray=new char[len];	//create a character array with enough room to hold these characters
		if(peek(charArray, 0, len)!=len)	//read the appropriate number of characters; if we didn't read enough
			throw new ParseEOFException("End of stream reached while peeking a character.", getLineIndex(), getCharIndex(), getName());	//show that we hit the end of the file G***Int
		return new String(charArray);	//create and return a string from the characters we peeked
	}

	/**Peeks the specified number of characters, but does not throw an exception
		if the end of the file is reached.
	This function is reset with every call to read().
	@param len The number of characters to peek.
	@return A string containing the characters peeked.
	@except IOException Thrown when a general I/O error occurs.
	*/
	public String peekStringEOF(int len) throws IOException
	{
//G***del System.out.println("Getting ready to peek "+len+" characters.");	//G***del
		final char[] charArray=new char[len];	//create a character array with enough room to hold these characters
		final int numCharsPeeked=peek(charArray, 0, len);	//try to read the appropriate number of characters, and find out how many were really read
//G***del System.out.println("Actually peeked "+numCharsPeeked+" characters.");	//G***del
		return new String(charArray, 0, numCharsPeeked);	//create and return a string from the characters peeked
	}

	/**Makes sure the next character to be read is one we expect, but that character is left to be read.
	This function is reset with every call to read().
	@param expectedChars A string with the list of allowed characters.
	@return The next character that will be read after the character retrieved in the last read() or peek().
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	*/
	public char peekExpectedChar(final String expectedChars) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{

//G***fix a little problem here that could show up: to throw the error, we read, but we don't know if peeking has been reset, so reading may not get the same character that was peeked

//G***del		resetPeek();	//reset our peeking so that we'll really be peeking the next character
		final char c=peekChar();	//see what the next character will be
		if(expectedChars.indexOf(c)==-1)	//if the next character isn't one we expect
			readExpectedChar(expectedChars);	//read the character, showing what we expect, which will throw an exception since it wasn't what we expected; this is easier than throwing an exception manually, because this will first update the read position and we don't care about performance since this is a fatal error, anyway
//G***del		else	//if the next character is expected, everything is OK
//G***del			resetPeek();	//reset peeking so that the next character peeked will reflect the next character to be read
		return c;	//return the character we peeked
	}



	/**Makes sure the next character to be read is one we expect, ignoring case,
		but that character is left to be read.
	This function is reset with every call to read().
	@param expectedChars A string with the list of allowed characters, ignoring case.
	@return The next character that will be read after the character retrieved in the last read() or peek().
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	*/
/*G***del; we probably don't need this, since we can just include both cases in the expectedChars string
	public char peekExpectedChar(final String expectedChars) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{

//G***fix a little problem here that could show up: to throw the error, we read, but we don't know if peeking has been reset, so reading may not get the same character that was peeked
		final char c=peekChar();	//see what the next character will be
		if(expectedChars.indexOf(c)==-1)	//if the next character isn't one we expect
			readExpectedChar(expectedChars);	//read the character, showing what we expect, which will throw an exception since it wasn't what we expected; this is easier than throwing an exception manually, because this will first update the read position and we don't care about performance since this is a fatal error, anyway
//G***del		else	//if the next character is expected, everything is OK
//G***del			resetPeek();	//reset peeking so that the next character peeked will reflect the next character to be read
		return c;	//return the character we peeked
	}
*/


	/**Checks to see if the string of characters waiting to be read matches any that are expected.
	If several strings match, the index of the first match will be returned.
	It is not required that peeking be reset before calling this function, but doing so will ensure
	that any error messages will correctly reflect the beginning search position. G***perhaps we could fix this
	Performance will be increased if strings are arranged in the array from shortest to longest. G***do this automatically, later
	To have this function not throw an exception of none of the strings are found,
	pass an empty string as the last argument in expectedStrings.
	@param expectedStrings An array of strings to expect.
	@return The index of the string which is waiting to be read.
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@see ParseReader#peekChar
	@see ParseReader#readExpectedStrings
	*/
	public int peekExpectedStrings(final String[] expectedStrings) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{
		final long beginLineIndex=getLineIndex(), beginCharIndex=getCharIndex()+1;	//make a note of where we start searching G***what if we've been peeking before getting here? compensate
//G***del		int compareIndex=0;	//we'll start comparing at the first position of every string
		String peekedChars="";	//this will hold the characters we peek
		int matchesRemaining;	//we'll use this to see how many strings we have left that still match up to and including compareIndex
		for(int i=0; i<expectedStrings.length; ++i)	//look at each string in our array
		{
//G***del System.out.println("peekExpectedStrings() string: "+i+" is "+expectedStrings[i]+", we have: "+peekedChars);	//G***del

			if(expectedStrings[i].length()>peekedChars.length())	//if the string we're comparing is longer than what we've peeked so far
			{
//G***del System.out.println("peekExpectedStrings() needs more data, but first check what we have.");	//G***del
				if(expectedStrings[i].substring(0, peekedChars.length()).equals(peekedChars))	//if what we've peeked so far matches the first part of this string
				{
//G***del System.out.println("peekExpectedStrings() needs more data, matches so far.");	//G***del
					peekedChars+=peekStringEOF(expectedStrings[i].length()-peekedChars.length());	//peek enough to make up for the difference so we can check it, but don't throw an exception if there aren't enough characters (a shorter string later may match)
				}
				else	//if what we have so far doesn't match this string
				{
//G***del System.out.println("peekExpectedStrings() doesn't match so far.");	//G***del
					continue;	//there's no use peeking and comparing the rest; this assumes that peeking is a more expensive operation than comparing)
				}
//G***maybe just read more characters one character at a time as we compare
			}
//G***del System.out.println("peekExpectedStrings() comparing strings.");	//G***del
			if(expectedStrings[i].equals(peekedChars.substring(0, Math.min(expectedStrings[i].length(), peekedChars.length()))))	//if this string matches what we've peeked (use a subset of what we've peeked in case we've peeked more than we've needed, something that would happen if the strings are not in ascending order of length, and we might have peeked less than needed, if we reached the end of the string)
			{
//G***del System.out.println("peekExpectedStrings() found string: "+expectedStrings[i]);	//G***del


				return i;	//show that this string matches what we're peeking
			}
		}
		throw new ParseUnexpectedDataException(expectedStrings, peekedChars, beginLineIndex, beginCharIndex, getName());	//show that we didn't get the string we were expecting G***we may want to have an XMLUnexpectedStringException or something
	}

	/**Reads a string of characters and makes sure they match a string in a given
	list, returning the index of the matched string.
	If several strings match, the index of the first match will be returned.
	Resets peeking.
	Performance will be increased if strings are arranged in the array from shortest to longest. G***do this automatically, later
	To have this function not throw an exception of none of the strings are found,
	pass an empty string as the last argument in expectedStrings.
	@param expectedStrings An array of strings to expect.
	@return The index of the string which is waiting to be read.
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@see ParseReader#peekExpectedStrings
	*/
	public int readExpectedStrings(final String[] expectedStrings) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{
		resetPeek();	//reset our peeking so that we'll really be peeking the next character
		final int matchedStringIndex=peekExpectedStrings(expectedStrings);	///see which string is waiting
		skip(expectedStrings[matchedStringIndex].length());	//skip the string we found to simulate reading it
		return matchedStringIndex;	//return the index of the string we found
	}

	/**Skips the specified number of characters, and throws an exception if the end of the file is reached.
	Resets peeking.
	@param n The number of characters to skip.
	@return The number of characters actually skipped.
	@except IOException Thrown when a general I/O error occurs.
	@except ParseEOFException Thrown if the end of the file is reached unexpectedly.
	*/
	public long skipChars(long n) throws IOException, ParseEOFException
	{
		if(skip(n)!=n)	//skip the requested number of characters; if we could not read as many as were requested
			throw new ParseEOFException("End of stream reached while reading a character.", getLineIndex(), getCharIndex(), getName());	//show that we hit the end of the file G***Int
		return n;	//if we make it here, we will have skipped all the characters
/*G***del
		for(int i=0; i<n; ++i)	//skip the correct amount of characters
			readChar();	//skip the next character
		return n;	//if we make it here, we will have skipped all the characters
*/
	}

	/**Skips every character that matches the skip characters, and returns the number of characters skipped.
	Throws an exception if the end of the file is unexpectedly reached.
	Resets peeking.
	@param skipChars The characters which should be skipped.
	@except IOException Thrown when an i/o error occurs.
	@except EOFException Thrown if the end of the file is reached unexpectedly.
	@return The number of characters skipped.
	*/
/*G***del
	public long skipChars(final String skipChars) throws IOException, ParseEOFException
	{
		resetPeek();	//reset our peeking so that we'll really be peeking the next character
		int numCharsSkipped=0;	//show that we haven't skipped any characters yet
		while(skipChars.indexOf(peekChar())!=-1)	//peek a character; if the character is one of our skipped characters
			++numCharsSkipped;	//show that we have one more character to skip
		skipChars(numCharsSkipped);	//now that we know how many to skip, actually skip the characters
		resetPeek();	//reset peeking so that the next character peeked will reflect the next character to be read
		return numCharsSkipped;	//return the number of characters we skipped
	}
*/

	/**Skips every character that matches the skip characters, and returns the number of characters skipped.
		Throws an exception if the end of the file is unexpectedly reached.
		Resets peeking.
	@param skipChars The characters which should be skipped.
	@except IOException Thrown when an i/o error occurs.
	@except EOFException Thrown if the end of the file is reached unexpectedly.
	@return The number of characters skipped.
	*/
	public long skipChars(final String skipChars) throws IOException, ParseEOFException
	{
//G***del System.out.println("Inside skipChars().");	//G***del
		final long numCharsSkipped=skipCharsEOF(skipChars);	//skip characters without throwing an exception when we run out of data
		if(isEOF())	//if we reached the end of the file
			throw new ParseEOFException("End of stream reached while skipping data.", getLineIndex(), getCharIndex(), getName());	//show that we hit the end of the file G***Int
		return numCharsSkipped;	//return the number of characters we skipped
	}

	/**Skips every character that matches the skip characters, and returns the number of characters skipped.
	No exception is thrown if the end of the file is reached.
	Resets peeking.
	@param skipChars The characters which should be skipped.
	@except IOException Thrown when an i/o error occurs.
	@return The number of characters skipped.
	*/
/*G***del when works
	public long skipCharsEOF(final String skipChars) throws IOException
	{
		resetPeek();	//reset our peeking so that we'll really be peeking the next character
		int numCharsSkipped=0;	//show that we haven't skipped any characters yet
		int i=peek();	//peek a character, not throwing an exception on the end of the file
		while(i!=-1 && skipChars.indexOf((char)i)!=-1)	//while we haven't reached the end of the file and we're still finding characters to skip
		{
			++numCharsSkipped;	//show that we have one more character to skip
			i=peek();	//peek another character, not throwing an exception on the end of the file
		}
		skip(numCharsSkipped);	//now that we know how many to skip, actually skip the characters; don't use skipChars(), which would throw an exception when the end of the file is reached (we know how many we peeked, so that's how many that should be skipped)
		resetPeek();	//reset peeking so that the next character peeked will reflect the next character to be read
		return numCharsSkipped;	//return the number of characters we skipped
	}
*/

	/**Skips every character that matches the skip characters, and returns the number of characters skipped.
		No exception is thrown if the end of the file is reached.
		Resets peeking.
	@param skipChars The characters which should be skipped.
	@except IOException Thrown when an i/o error occurs.
	@return The number of characters skipped.
	*/
	public long skipCharsEOF(final String skipChars) throws IOException
	{
//G***del System.out.println("Inside skipCharsEOF() skipping: "+skipChars);	//G***del
		resetPeek();	//reset our peeking so that we'll really be peeking the next character
		long numCharsSkipped=0;	//show that we haven't skipped any characters yet
		boolean foundNonDelimiter=false;	//show that we haven't found a non-matching character yet
		while(!isEOF())	//if we're not at the end of the file
		{
			final char[] buffer=getBuffer();	//get a reference to the buffer; the buffer address will always remain the same within this loop; future versions of fetchBuffer() may change the buffer though, so this variable shouldn't be relied upon after a call to fetchBuffer()
			int checkIndex=getReadIndex();	//start looking where we're supposed to start reading
			while(checkIndex<getFetchBufferIndex())	//look at each character until the end of the data in this buffer
			{
//G***del System.out.println("Looking at checkIndex: "+checkIndex+" finding character: "+buffer[checkIndex]);	//G***del
				if(skipChars.indexOf(buffer[checkIndex])==-1)	//if this character is not one we want to skip
				{
					foundNonDelimiter=true;	//show that we found a character that isn't a skip character
					break;	//stop checking
				}
				else	//if this is an accepted character
				{
					++checkIndex;	//look at the next character
					++numCharsSkipped;	//show that we just skipped another character
				}
			}
			setReadIndex(checkIndex);	//update our read position to wherever we wound up; this will also fetch another buffer if needed
			if(foundNonDelimiter)	//if we found a character that didn't match what we want to skip
				break;	//stop skipping
/*G***del
			else	//if all the characters match what we want to skip, the only way we got here was to have reached the next fetch index
				fetchBuffer();	//fetch another buffer (technically, the buffer would have already been fetched at setReadIndex() above, but this doesn't hurt performance much and it's much clearer code)
*/
		}
		resetPeek();	//reset peeking just because this function always does, even at the end of the file
		return numCharsSkipped;	//return the number of characters we skipped
	}

	/**Reads characters until a particular character is reached. The delimiter character reached will be the next character read.
	@param delimiterChar The character which indicates reading should stop.
	@except IOException Thrown when an i/o error occurs.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The characters up to but not including the delimiter character reached.
	*/
	public String readStringUntilChar(char delimiterChar) throws IOException, ParseEOFException
	{
		return readStringUntilChar(String.valueOf(delimiterChar));	//convert this character to a string and call the version of this function which accepts a string as a parameter
	}

	/**Reads characters until a particular character is reached
	or the end of the file is reached. If a delimiter character was reached, it will
	be the next character read. No exception is thrown if the end of the file is reached.
	@param delimiterChar The character which indicates reading should stop.
	@except IOException Thrown when an i/o error occurs.
	@return The characters up to but not including the delimiter character reached or the end of the file.
	*/
	public String readStringUntilCharEOF(char delimiterChar) throws IOException
	{
		return readStringUntilCharEOF(String.valueOf(delimiterChar));	//convert this character to a string and call the version of this function which accepts a string as a parameter
	}

	/**Reads characters until one of the characters in delimiterCharString is reached. The delimiter character reached will be the next character read.
	@param delimiterCharString A list of characters which indicate reading should stop.
	@except IOException Thrown when an i/o error occurs.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The characters up to but not including the delimiter character reached.
	*/
/*G***del when works
	public String readStringUntilChar(String delimiterCharString) throws IOException, ParseEOFException
	{
		String characterString="";	//this string will receive the characters we've read
			//keep reading characters until one of our delimiter characters is reached
		long PreviousLineIndex=getLineIndex();	//before doing anything, see which line we were on in case we want to unread a character; this only works because we won't unread more than one character
		long PreviousCharIndex=getCharIndex();	//see what character we were on
		char c=readChar();	//read a character
		while(delimiterCharString.indexOf(c)==-1)	//if we haven't found one of the delimiter characters, yet
		{
			characterString+=c;	//add this character to our character string
			PreviousLineIndex=getLineIndex();	//before reading the next character, update our last position in case we want to unread a character
			PreviousCharIndex=getCharIndex();	//see what character we were on
			c=readChar();	//read another character
		}
//G***del newio		getInReader().unread(c);	//put the character back so we can read it next time (there's no need to worry about the peek buffer, because our reads above will have cleared it)
		unskip(1);	//put the character back so we can read it next time (there's no need to worry about the peek buffer, because our reads above will have cleared it)
		setCurrentLineIndex(PreviousLineIndex);	//since we just unread a character, show that we're at our previoius position
		setCurrentCharIndex(PreviousCharIndex);	//
//G***del    unreadCharacter(c);	//put the delimiter character back
			//G***put an error here that indicates we didn't find one of the characters we were looking for
		return characterString;	//return the characters we read
	}
*/




	/**Reads characters until one of the characters in delimiterCharString is reached.
		The delimiter character reached will be the next character read.
		Resets peeking.
		Note that this function has a limitation of the largest integer for the number
		of characters returned.
	@param delimiterCharString A list of characters which indicate reading should stop.
	@except IOException Thrown when an i/o error occurs.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The characters up to but not including the delimiter character reached.
	*/
	public String readStringUntilChar(String delimiterCharString) throws IOException, ParseEOFException
	{
		final String characterString=readStringUntilCharEOF(delimiterCharString);	//read the characters until the delimiter without throwing an error when we run out of data
		if(isEOF())	//if we reached the end of the file
			throw new ParseEOFException("End of stream reached while reading data.", getLineIndex(), getCharIndex(), getName());	//show that we hit the end of the file G***Int
		return characterString;	//return the string we read
	}


	/**Reads characters until one of the characters in delimiterCharString is reached,
	or the end of the file is reached. If a delimiter character was reached, it will
	be the next character read. No exception is thrown if the end of the file is reached.
	@param delimiterCharString A list of characters which indicate reading should stop.
	@except IOException Thrown when an i/o error occurs.
	@return The characters up to but not including the delimiter character reached or the end of the file.
	*/
/*G***del when works
	public String readStringUntilCharEOF(String delimiterCharString) throws IOException
	{
		String characterString="";	//this string will receive the character's we've read
			//keep reading characters until one of our delimiter characters is reached
		long PreviousLineIndex=getLineIndex();	//before doing anything, see which line we were on in case we want to unread a character; this only works because we won't unread more than one character
		long PreviousCharIndex=getCharIndex();	//see what character we were on
		int i=read();	//read a character without throwing an exception for the end of the file
		while(i!=-1 && delimiterCharString.indexOf((char)i)==-1)	//if we haven't reached the end of the file and we haven't found one of the delimiter characters, yet
		{
			characterString+=(char)i;	//add this character to our character string
			PreviousLineIndex=getLineIndex();	//before reading the next character, update our last position in case we want to unread a character
			PreviousCharIndex=getCharIndex();	//see what character we were on
			i=read();	//read another character without throwing an exception for the end of the file

//G***del System.out.println(characterString);	//G***del

		}
		if(i!=-1)	//if we didn't reach the end of the file, but found one of the delimiter characters
		{
//G***del when works newio			getInReader().unread(i);	//put the character back so we can read it next time (there's no need to worry about the peek buffer, because our reads above will have cleared it)
			unskip(1);	//put the character back so we can read it next time (there's no need to worry about the peek buffer, because our reads above will have cleared it)
			setCurrentLineIndex(PreviousLineIndex);	//since we just unread a character, show that we're at our previoius position
			setCurrentCharIndex(PreviousCharIndex);	//
		}
			//G***put an error here that indicates we didn't find one of the characters we were looking for
		return characterString;	//return the characters we read
	}
*/



	/**Reads characters until one of the characters in delimiterCharString is reached,
		or the end of the file is reached. If a delimiter character was reached, it will
		be the next character read. No exception is thrown if the end of the file is reached.
		Resets peeking.
		Note that this function has a limitation of the largest integer for the number
		of characters returned.
	@param delimiterCharString A list of characters which indicate reading should stop.
	@except IOException Thrown when an i/o error occurs.
	@return The characters up to but not including the delimiter character reached or the end of the file.
	*/
	public String readStringUntilCharEOF(String delimiterCharString) throws IOException
	{
//G***del System.out.println("Inside readStringUntilCharEOF() with: "+delimiterCharString);	//G***del
		String characterString="";	//this string will receive the character's we've read
		boolean foundDelimiter=false;	//show that we haven't found the character, yet
		while(!isEOF())	//if we're not at the end of the file
		{
//G***del System.out.println("Not EOF");	//G***del
			final char[] buffer=getBuffer();	//get a reference to the buffer; the buffer address will always remain the same within this loop; future versions of fetchBuffer() may change the buffer though, so this variable shouldn't be relied upon after a call to fetchBuffer()
			int checkIndex=getReadIndex();	//start looking where we're supposed to start reading
			while(checkIndex<getFetchBufferIndex())	//look at each character until the end of the data in this buffer
//G***del when works			for(checkIndex=; checkIndex<getFetchBufferIndex(); ++checkIndex)	//look at each character until the end of the data in this buffer
			{
//G***del System.out.println("Checking character: "+buffer[checkIndex]);	//G***del
				if(delimiterCharString.indexOf(buffer[checkIndex])!=-1)	//if this character is one we're looking for
				{
					foundDelimiter=true;	//show that we found one of the delimiters
					break;	//stop checking
				}
				else	//if we haven't found a delimiter character
					++checkIndex;	//look at the next character
			}
			characterString+=new String(buffer, getReadIndex(), checkIndex-getReadIndex());	//create a string with the number of characters checked that were not the delimiter character (this could be zero if the first character was a delimiter) and add it to our characters read
//G***del System.out.println("Before updating read index EOF is: "+isEOF()+" lastBuffer is: "+isLastBuffer());	//G***del
			setReadIndex(checkIndex);	//update our read position to wherever we wound up; this will also fetch another buffer if needed
//G***del System.out.println("After updating read index EOF is: "+isEOF()+" lastBuffer is: "+isLastBuffer());	//G***del
			if(foundDelimiter)	//if we found the delimiter
				break;	//stop looking for one
/*G***del
			else	//if we haven't found the delimiter, the only way we got here was to have reached the next fetch index
				fetchBuffer();	//fetch another buffer (technically, the buffer would have already been fetched at setReadIndex() above, but this doesn't hurt performance much and it's much clearer code)
*/
		}
		resetPeek();	//reset peeking just because this function always does, even at the end of the file
		return characterString;	//return the characters we read
	}

	/**Gets the next character from the reader, and make sure it's the character we expected; otherwise an exception is thrown.
	@param expectedChar The character expected.
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The character read.
	*/
	public char readExpectedChar(final char expectedChar) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{
//G***del Debug.trace("Reading expected character.");

		char c=readChar();	//read a character
//G***del Debug.trace("Found character: "+String.valueOf(c));

//G***del System.out.println("Expecting "+expectedChar+", read "+c);	//G***del

		if(c!=expectedChar)	//if we don't get the character we expected
			throw new ParseUnexpectedDataException(expectedChar, c, getLineIndex(), getCharIndex(), getName());	//show that we didn't get the character we were expecting
		return c;	//return the character we read
	}

	/**Gets the next character from the reader, and make sure it's a character we expect; otherwise an exception is thrown.
	@param expectedChars A string with the list of allowed characters.
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The character read.
	*/
	public char readExpectedChar(final String expectedChars) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{
		char c=readChar();	//read a character
		if(expectedChars.indexOf(c)==-1)	//if this character doesn't match any we expect
			throw new ParseUnexpectedDataException(expectedChars, c, getLineIndex(), getCharIndex(), getName());	//show that we didn't get the character we were expecting
		return c;	//return the character we read
	}

	/**Gets the specified number of characters from the reader.
	@param len The number of characters to read.
	@except IOException Thrown when an i/o error occurs.
	@except ParseEOFException Thrown if the end of the file is reached unexpectedly.
	@return The characters read.
	*/
	public String readString(final int len) throws IOException, ParseEOFException
	{
		final char[] charArray=new char[len];	//create a character array with enough room to hold these characters
		if(read(charArray, 0, len)!=len)	//read the appropriate number of characters; if we didn't read enough
			throw new ParseEOFException("End of stream reached while reading a character.", getLineIndex(), getCharIndex(), getName());	//show that we hit the end of the file G***Int
		return new String(charArray);	//create and return a string from the characters we read
/*G***del
		String characterString="";
		while((len--)!=0)	//while they want more characters
			characterString+=readChar();	//read another character
		return characterString;	//return the characters we read
*/
	}

	/**Gets the a certain number of characters from the reader, and makes sure
		they are what we expected; otherwise an exception is thrown.
	@param expectedString The characters expected.
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The string read.
	*/
	public String readExpectedString(final String expectedString) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{
		final String foundString=readString(expectedString.length());  //read the string into memory
		if(!foundString.equals(expectedString))  //if the string we read does not match
			throw new ParseUnexpectedDataException(new String[]{expectedString}, foundString, getLineIndex(), getCharIndex(), getName());	//show that we didn't get the string we were expecting
/*G***del when works
		for(int i=0; i<expectedString.length(); ++i)	//look at each character in the expected string
			readExpectedChar(expectedString.charAt(i));	//read a character, expecting it to match the current character in the string
*/
		return foundString;	//return the string we read
	}

	/**Gets the a certain number of characters from the reader, and makes sure
		they are what we expected, ignoring case; otherwise an exception is thrown.
	@param expectedString The characters expected.
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The string read.
	*/
	public String readExpectedStringIgnoreCase(final String expectedString) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{
		final String foundString=readString(expectedString.length());  //read the string into memory
		if(!foundString.equalsIgnoreCase(expectedString))  //if the string we read does not match, even if we ignore case
			throw new ParseUnexpectedDataException(new String[]{expectedString}, foundString, getLineIndex(), getCharIndex(), getName());	//show that we didn't get the string we were expecting
/*G***del when works
		for(int i=0; i<expectedString.length(); ++i)	//look at each character in the expected string
			readExpectedChar(expectedString.charAt(i));	//read a character, expecting it to match the current character in the string
*/
		return foundString;	//return the string we read
	}
	
	/**Reads text from an input stream that ends in a certain delimiter.
		The ending delimiter will be discarded.
	@param endDelimiter The string to expect at the end of the characters.
	@except IOException Thrown when an i/o error occurs.
//G***del	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The characters before the delimiter.
	*/
	public String readDelimitedString(final String endDelimiter) throws IOException, /*G***del ParseUnexpectedDataException, */ParseEOFException
	{
		final StringBuffer characterData=new StringBuffer();	//this will hold the characters we find
		final int endDelimiterLength=endDelimiter.length();	//store the length of the ending delimiter
		final char endDelimiterLastChar=endDelimiter.charAt(endDelimiter.length()-1);	//get the last character in the ending delimiter
		while(true)	//we'll break out of this loop when we find a match of the end delimiter or an exception is thrown
		{
			characterData.append(readStringUntilChar(endDelimiterLastChar)).append(endDelimiterLastChar);	//read everything up to and including the last character in the delimiter
			skip(1);	//skip the ending delimiter's last character, since we already added it to the end of our string
			final int characterDataLength=characterData.length();	//find out how many characters we've read so far
				//if we've read enough to compare characters with the ending delimiter, and the last characters read match the ending delimiter
			if(characterDataLength>=endDelimiterLength && characterData.substring(characterDataLength-endDelimiterLength).equals(endDelimiter))	//G***this can be made more efficient
			{
				return characterData.substring(0, characterData.length()-endDelimiterLength);	//chop off the ending delimiter and return the characters we found which were between the delimiters
			}
		}
	}

	/**Reads text from an input stream that that has certain beginning and ending
		delimiters. The ending delimiter will be discarded.
	@param startDelimiter The string to expect at the first of the characters.
	@param endDelimiter The string to expect at the end of the characters.
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The characters between the delimiters.
	*/
	public String readDelimitedString(final String startDelimiter, final String endDelimiter) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{
//G***del		String characterData="";	//this will hold the characters we find
		readExpectedString(startDelimiter);	//make sure the characters start with the correct delimiters
		return readDelimitedString(endDelimiter);	//read the content up to the ending delimiter and throw away the end delimiter
		//G***somehow fit an error here for EOF (which happens now) which indicates that we were searching for the end delimiters (which we don't do now)
	}

	/**Reads text from an input stream that that has certain beginning and ending
		delimiter characters. The ending delimiter will be discarded.
	@param startDelimiter The character to expect at the first of the characters.
	@param endDelimiter The character to expect at the end of the characters.
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The characters between the delimiters.
	*/
	public String readDelimitedString(final char startDelimiter, final char endDelimiter) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{
		readExpectedChar(startDelimiter);	//read the first delimiter
		final String string=readStringUntilChar(endDelimiter);	//read until the end delimiter is reached
		readExpectedChar(endDelimiter);	//read the ending delimiter
		return string;	//return the string in-between
	}

	/**Reads text from an input stream that that has certain beginning and ending
		delimiter characters, and returns the string <em>including</em> the delimiters.
	@param startDelimiter The character to expect at the first of the characters.
	@param endDelimiter The character to expect at the end of the characters.
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The delimiters with the character between them.
	*/
	public String readDelimitedStringInclusive(final char startDelimiter, final char endDelimiter) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{
		return readExpectedChar(startDelimiter)+readStringUntilChar(endDelimiter)+readExpectedChar(endDelimiter);	//read the start and ending delimiters, and everything in-between		
	}

	/**Reads text from an input stream that that has certain beginning and ending
		delimiters, and returns the string <em>including</em> the delimiters.
	@param startDelimiter The string to expect at the first of the characters.
	@param endDelimiter The string to expect at the end of the characters.
	@except IOException Thrown when an i/o error occurs.
	@except ParseUnexpectedDataException Thrown when an unexpected character is found.
	@except ParseEOFException Thrown when the end of the input stream is reached unexpectedly.
	@return The delimiters with the character between them.
	*/
	public String readDelimitedStringInclusive(final String startDelimiter, final String endDelimiter) throws IOException, ParseUnexpectedDataException, ParseEOFException
	{
		//read the expected delimiters and the character data between them and return it all as one string
		return readExpectedString(startDelimiter)+readDelimitedString(startDelimiter, endDelimiter)+readExpectedString(endDelimiter);
	}
}
