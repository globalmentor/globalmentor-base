package com.garretwilson.io;

/**Class for unexpected characters encounted when parsing an input stream.
Used by com.garretwilson.io.ParseReader.
This implementation assumes that if a list of strings were expected, a string
	will have been found. That is, if <code>getExpectedCharacters()</code> returns
	a non-null value, <code>getCharacterFound()</code> will hold a valid value;
	otherwise, <code>getStringFound()</code> will hold the appropriate value.
@see ParseIOException
@see ParseReader
*/
public class ParseUnexpectedDataException extends ParseIOException
{

	/**The string of expected characters, if characters were expected, else <code>null</code>.*/
	private String ExpectedCharacters=null;

		/**@return The string of expected characters, if characters were expected, else <code>null</code>.*/
		public String getExpectedCharacters() {return ExpectedCharacters;}

		/**Sets the string of expected characters.
		@param expectedCharacters The string of expected characters.
		*/
		protected void setExpectedCharacters(final String expectedCharacters) {ExpectedCharacters=expectedCharacters;}

	/**The character found, if characters were expected.*/
	private char FoundCharacter=(char)0;

		/**@return The character found, if characters were expected.*/
		public char getFoundCharacter() {return FoundCharacter;}

		/**Sets the character found.
		@param characterFound The character found.
		*/
		protected void setFoundCharacter(final char foundCharacter) {FoundCharacter=foundCharacter;}

	/**The array of expected strings, if strings were expected, else <code>null</code>.*/
	private String[] ExpectedStrings=null;

		/**@return The array of expected strings, if strings were expected, else <code>null</code>.*/
		public String[] getExpectedStrings() {return ExpectedStrings;}

		/**Sets the array of expected strings.
		@param expectedStrings The array of expected strings.
		*/
		protected void setExpectedStrings(final String[] expectedStrings) {ExpectedStrings=expectedStrings;}

	/**The string found, if strings were expected.*/
	private String FoundString=null;

		/**@return The string found, if strings were expected.*/
		public String getFoundString() {return FoundString;}

		/**Sets the string found.
		@param stringFound The string found.
		*/
		protected void setFoundString(final String foundString) {FoundString=foundString;}

	/**Constructor for an unexpected character error.
	@param foundChar The character found at this location.
	@param lineIndex The index of the line in which the error occurred.
	@param charIndex The index of the character at which the error occurred on the current line.
	@param sourceName The name of the source of the data (perhaps a filename).
	*/
	public ParseUnexpectedDataException(final char foundChar, final long lineIndex, final long charIndex, final String sourceName)
	{
		super("Unexpected character: found "+
				convertDelimitersToMessage(String.valueOf(foundChar))+
				".",
				lineIndex, charIndex, sourceName);	//G***Int
		setFoundCharacter(foundChar);	//save the character found
	}

	/**Constructor for an unexpected character error, when one character was expected.
	@param expectedChar The character expected at this location.
	@param foundChar The character found at this location.
	@param lineIndex The index of the line in which the error occurred.
	@param charIndex The index of the character at which the error occurred on the current line.
	@param sourceName The name of the source of the data (perhaps a filename).
	*/
	public ParseUnexpectedDataException(final char expectedChar, final char foundChar, final long lineIndex, final long charIndex, final String sourceName)
	{
		super("Unexpected character: expected "+
				convertDelimitersToMessage(String.valueOf(expectedChar))+
				" found "+
				convertDelimitersToMessage(String.valueOf(foundChar))+
				".",
				lineIndex, charIndex, sourceName);	//G***Int
		setExpectedCharacters(String.valueOf(expectedChar));	//save the expected character
		setFoundCharacter(foundChar);	//save the character found
	}

	/**Constructor for an unexpected character error, when multiple characters were expected.
	@param expectedChars A string containing the characters expected at this location.
	@param foundChar The character found at this location.
	@param lineIndex The index of the line in which the error occurred.
	@param charIndex The index of the character at which the error occurred on the current line.
	@param sourceName The name of the source of the data (perhaps a filename).
	*/
	public ParseUnexpectedDataException(final String expectedChars, final char foundChar, final long lineIndex, final long charIndex, final String sourceName)
	{
		super("Unexpected character: expected one of "+
				convertDelimitersToMessage(expectedChars)+
				" found "+
				convertDelimitersToMessage(String.valueOf(foundChar))+
				".",
				lineIndex, charIndex, sourceName);	//G***Int
		setExpectedCharacters(expectedChars);	//save the expected characters
		setFoundCharacter(foundChar);	//save the character found
	}

	/**Constructor for an unexpected character error, when multiple strings were expected.
	@param expectedStrings An array containing the strings expected at this location.
	@param foundString The string found at this location.
	@param lineIndex The index of the line in which the error occurred.
	@param charIndex The index of the character at which the error occurred on the current line.
	@param sourceName The name of the source of the data (perhaps a filename).
	*/
	public ParseUnexpectedDataException(final String[] expectedStrings, final String foundString, final long lineIndex, final long charIndex, final String sourceName)
	{
		super("Unexpected character: expected one of "+
				convertStringsToMessage(expectedStrings)+
				" found "+
				convertStringsToMessage(new String[]{foundString})+
				".",
				lineIndex, charIndex, sourceName);	//G***Int
		setExpectedStrings(expectedStrings);	//save the expected strings
		setFoundString(foundString);	//save the string found
	}

	/**Returns a message with the expected data, either a list of characters or
		a list of strings.
	@return A message with the expected data.
	*/
	public String getExpectedMessage()
	{
		if(getExpectedCharacters()!=null)	//if we have expected characters
			return convertDelimitersToMessage(getExpectedCharacters());	//return a string of our expected characters
		else if(getExpectedStrings()!=null)	//if we have expected strings
			return convertStringsToMessage(getExpectedStrings());	//return a string of our expected strings
		else	//if we don't know what we were expecting
			return "";	//return a null string; this in theory should never happen
	}

	/**Returns a message with the data found, either a single character or a string.
	@return A message with the data found.
	*/
	public String getFoundMessage()
	{
		if(getExpectedCharacters()!=null)	//if we were expecting characters
			return convertDelimitersToMessage(String.valueOf(getFoundCharacter()));	//we will have found a character, so return it
		else if(getExpectedStrings()!=null)	//if we were expecting strings
			return convertStringsToMessage(new String[]{getFoundString()});	//we will have found a string, so return what we found
		else	//if we don't know what we were expecting
			return "";	//return a null string; this in theory should never happen
	}

}

