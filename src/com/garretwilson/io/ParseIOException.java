package com.garretwilson.io;

import java.io.IOException;

/**General exception class for all parsing errors.
Used by com.garretwilson.io.ParseReader.
@see IOException
@see ParseReader
*/
public class ParseIOException extends IOException
{
	/**The index of the line on which the error occurred.
	@see #charIndex
	*/
	private long lineIndex;

		/**@return The index of the line on which the error occurred.
		@see #getCharIndex
		*/
		public long getLineIndex() {return lineIndex;}

		/**Sets the index of the line on which the error occurred.
		@param lineIndex The new line index.
		@see #setCharIndex
		*/
		public void setLineIndex(final long lineIndex) {this.lineIndex=lineIndex;}

	/**The index of the character at which the error occurred on the current line.
	@see #lineIndex
	*/
	private long charIndex;

		/**@return The index of the character at which the error occurred on the current line.
		@see #getLineIndex
		*/
		public long getCharIndex()	{return charIndex;}

		/**Sets the index of the character at which the error occurred on the current line.
		@param charIndex The new character index.
		@see #setLineIndex
		*/
		public void setCharIndex(final long charIndex) {this.charIndex=charIndex;}

	/**The name of the source of this exception, such as a filename.*/
	private String sourceName="";

		/**@return The name of the source of this exception, such as a filename.*/
		public String getSourceName() {return sourceName;}

		/**Sets the name of the source of this exception, such as a filename.
		@param sourceName The new name of the sourceof the exception.
		*/
		public void setSourceName(final String sourceName) {this.sourceName=sourceName;}

	/**Default constructor for a generic parsing error.*/
	public ParseIOException()
	{
		super();
	}

	/**Constructor for a generic parsing error, along with a message.
	@param s The error message.
	*/
	public ParseIOException(String s)
	{
		super(s);
	}

	/**Constructor for a generic parsing error with error location specified.
	@param lineIndex The index of the line in which the error occurred.
	@param charIndex The index of the character at which the error occurred on the current line.
	@param sourceName The name of the source of the data (perhaps a filename).
	*/
	public ParseIOException(final long lineIndex, final long charIndex, final String sourceName)
	{
		super(sourceName+':'+lineIndex+':'+charIndex);
		setLineIndex(lineIndex);	//set the line index
		setCharIndex(charIndex);	//set the character index
		setSourceName(sourceName);	//set the source name
	}

	/**Constructor for a generic parsing error with error message and error location specified.
	@param s The error message.
	@param lineIndex The index of the line in which the error occurred.
	@param charIndex The index of the character at which the error occurred on the current line.
	@param sourceName The name of the source of the data (perhaps a filename).
	*/
	public ParseIOException(String s, final long lineIndex, final long charIndex, final String sourceName)
	{
		super(sourceName+':'+lineIndex+':'+charIndex+": "+s);
		setLineIndex(lineIndex);	//set the line index
		setCharIndex(charIndex);	//set the character index
		setSourceName(sourceName);	//set the source name
	}

	//G***make this print Unicode codes for characters which may not be displayable.
	/**Converts a list of delimiter characters to a string with the characters in a list, each in a single quote.
	Whitespace characters besides space are displayed in their escaped form.
	@param delimiterChars A string with the delimiter characters to be converted to a string.
	*/
	static public String convertDelimitersToMessage(final String delimiterChars)
	{
		String messageString="";	//this string will receive the message to return
		for(int i=0; i<delimiterChars.length(); ++i)	//look at each character in the string
		{
			messageString+='\'';	//add a single quote character
    	switch(delimiterChars.charAt(i))	//see which character this is
      {
				case '\t':	//tab
					messageString+="\\t";
					break;
				case '\r':	//CR
					messageString+="\\r";
					break;
				case '\n':	//LF
					messageString+="\\n";
					break;
				default:	//if we don't recognize the character
					messageString+=delimiterChars.charAt(i);	//add it normally
					break;
			}
      if(i<delimiterChars.length()-1)	//if this isn't the last character in the string
      	messageString+="', ";	//show that there will be another character
      else	//if this is the last character in the string
      	messageString+="'";	//add just a single quote
    }
    return messageString;	//return the message string we constructed
	}

	/**Converts an array of strings to a message with the strings separated by commas.
	@param stringArray An array of strings to be converted to a string.
	*/
	//G***convert the characters in these strings so that whitespace gets converted to characters
	static public String convertStringsToMessage(final String[] stringArray)
	{
		String messageString="";	//this string will receive the message to return
		for(int i=0; i<stringArray.length; ++i)	//look at each string in the array
		{
			messageString+="\""+stringArray[i];	//add a double quote character followed by this string
			if(i<stringArray.length-1)	//if this isn't the last string in the array
				messageString+="\", ";	//show that there will be another string
			else	//if this is the last string in the array
				messageString+='"';	//add just a double quote
		}
		return messageString;	//return the message string we constructed
	}

}

