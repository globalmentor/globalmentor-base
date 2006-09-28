package com.garretwilson.lang;

import java.io.*;
import java.util.List;
import static com.garretwilson.lang.CharacterUtilities.*;
import static com.garretwilson.lang.StringBuilderUtilities.*;
import com.garretwilson.text.*;
import com.garretwilson.util.Debug;

import static com.garretwilson.text.CharacterConstants.*;

/**Various text manipulating functions. These methods work on
	<code>String</code> objects, which are immutable heavyweight objects that must
	be recreated with every modification. Many of these methods should therefore
	be avoided in favor of their corresponding <code>StringBufferUtilities</code>
	methods, which operate on <code>StringBuffer</code> objects.
@see StringBufferUtilities
@author Garret Wilson
*/
public class StringUtilities 
{
	//TODO move most of the methods that reference CharSeqUtilities to that class

/*G***del
	public final static String WHITESPACE_STRING="\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037\040";
	public final static String PUNCTUATION_STRING=".,:;?!";	//punctuation characters
	public final static String WORD_DELIMITER_STRING=WHITESPACE_STRING+PUNCTUATION_STRING;	//characters which divide words
	public final static String WORD_WRAP_STRING=WORD_DELIMITER_STRING+"-/";	//characters which allow words to wrap
*/
//G***fix, maybe	public final static String LETTER_STRING="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
//G***del; using Character.isDigit()	public final static String NUMBER_STRING="0123456789";
//G***del; moved to CharacterConstants	public final static String EOL_STRING="\015\012";	//the characters which can mark the end of a line
//G***del	public final static String INVALID_FILENAME_CHARACTERS_STRING="*:\\/?\"><|";	//characters which are not allowed in filenames
	/**The non-breaking space character, Unicode 00A0.*/
//G***del	public final static char NON_BREAKING_SPACE='\u00A0'; //G***maybe put this in some Unicode class G***del; moved to com.garretwilson.text.CharacterConstants

	/**Creates an array of strings from the given string.
	@param string The string to include in the array, or <code>null</code> if the
		array should be empty.
	@return A non-<code>null</code> array containing the string, or empty if
		the string is <code>null</code>.
	*/
	public static String[] createArray(final String string)
	{
		return string!=null ? new String[]{string} : new String[]{};	//return an array containing the string, or an empty array if the string is null
	}
	
	/**Searches a string in reverse and returns the last index of any character
		in the specified string, starting from <code>fromIndex</code>, ignoring
		case.
	@param inString The string to be searched.
	@param charString The string of characters to check.
	@param fromIndex The index to search from
	@return The index of the last occurrence of one of the supplied characters, or -1 if none were found.
	*/
/*G***del if not needed
	static public int charLastIndexOfIgnoreCase(final String inString, final String charString, final int fromIndex)
	{
		return charLastIndexOf(inString.toUpperCase(), charString.toUpperCase(), fromIndex);  //search without regard to case
	}
*/

	/**Searches a string in reverse and returns the first index of any character in the specified string.
	@param inString The string to be searched.
	@param charString The string of characters to check.
	@return The index of the last occurrence of one of the supplied characters, or -1 if none were found.
	*/
/*G***fix
	static public int charLastIndexOf(final String inString, final String charString, final int fromIndex)
	{
		for(int i=inString.length()-1; i>=0; --i)	//look at each character in the string, starting at the end
		{
			if(charString.indexOf(inString.charAt(i))!=-1)	//if this character is in our character string
				return i;	//return the index we're at
		}
		return -1;	//if we make it to here, we didn't find any of the characters
	}
*/

	/**Concatenates the string representations of the objects
		in the array.
	@param objects The array of objects (such as strings) to be concatenated.
	@return A concatenation of string representations of all objects in the
		array.
	@see Object#toString
	@return The string containing the concatenated information.
	*/
	public static String append(final Object[] objects)
	{
		return StringBufferUtilities.append(new StringBuffer(), objects).toString();	//append the objects to a string buffer and return the string
	}
	
	/**Concatenates the string representations of the objects
		in the array, separated by the given separator character.
	@param objects The array of objects (such as strings) to be concatenated.
	@param separator The separator character to be inserted between the object
		strings. 
	@return A concatenation of string representations of all objects in the
		array, separted by the separator character.
	@see Object#toString
	@return The string containing the concatenated information.
	*/
	public static String concat(final Object[] objects, final char separator)
	{
		return StringBufferUtilities.append(new StringBuffer(), objects, separator).toString();	//append the objects to a string buffer and return the string
	}
		
	/**Concatenates the string representations of the objects
		in the array, separated by the given separator string.
	@param objects The array of objects (such as strings) to be concatenated.
	@param separator The separator string to be inserted between the object
		strings, or <code>null</code> (if no separator should be used. 
	@return A concatenation of string representations of all objects in the
		array, separted by the separator.
	@see Object#toString
	@return The string containing the concatenated information.
	*/
	public static String concat(final Object[] objects, final String separator)
	{
		return StringBufferUtilities.append(new StringBuffer(), objects, separator).toString();	//append the objects to a string buffer and return the string
	}
	
	/**Compares two strings to make sure that the strings are equal ignoring
		case, , or the strings are both set to <code>null</code>. If the first
		string is not <code>null</code>, it is compared to the second using the
		first strings's <code>equalIgnoreCase()</code> method.
		This is a convenience method to compare two strings using the
		<code>equalsIgnoreCase()</code> method when it's not known if one of the
		strings is <code>null</code>.
	@param string1 The first string to compare.
	@param string2 The second string to compare.
	@return <code>true</code> if the string are equal according to the first
		strings's <code>equalIgnoreCase()</code> method or if both strings are
		<code>null</code>.
	@see String#equalsIgnoreCase(java.lang.String)
	*/
	public final static boolean equalsIgnoreCase(final String string1, final String string2)
	{
			//if the first string isn't null, compare it to the second; otherwise, see if the second string is null as well
		return string1!=null ? string1.equalsIgnoreCase(string2) : string2==null;
	}	

	/**Extracts the string which comes after the given character. If the character
		does not exist in the string, the
	 */
/*G***del if not needed
	public static String extractAfter(final String string, final char c)
	{

	}
*/

	/**Creates an array of bytes from a string and stores the bytes of the string,
		in UTF-8, in the array postpended with a zero byte.
	@param string The string to store in bytes.
	@exception UnsupportedEncodingException Thrown if the given encoding is not
		supported.
	*/
/*G***fix
	public static byte[] getASCIIZBytes(final String string) throws UnsupportedEncodingException
	{
		return getASCIIZBytes(string, length, CharacterEncodingConstants.UTF_8);  //return the bytes, encoded using UTF-8
	}
*/

	/**Creates an array of bytes of the specified length and stores the bytes
		of the string, in UTF-8, in the array postpended with a zero byte. If the
		string is too long for the given length, it is truncated.
	@param string The string to store in bytes.
	@param length The length of bytes to return.
	@exception UnsupportedEncodingException Thrown if the given encoding is not
		supported.
	*/
	public static byte[] getASCIIZBytes(final String string, final int length) throws UnsupportedEncodingException
	{
		return getASCIIZBytes(string, length, CharacterEncodingConstants.UTF_8);  //return the bytes, encoded using UTF-8
	}

	/**Creates an array of bytes of the specified length and stores the bytes
		of the string, in the array postpended with a zero byte. If the string is
		too long for the given length, it is truncated.
	@param string The string to store in bytes.
	@param length The length of bytes to return.
	@param encoding The encoding to use in storing the string bytes.
	@exception UnsupportedEncodingException Thrown if the given encoding is not
		supported.
	*/
	public static byte[] getASCIIZBytes(String string, final int length, final String encoding) throws UnsupportedEncodingException
	{
		final byte[] stringBytes=string.getBytes(encoding); //get the bytes of the string
		final byte[] asciizBytes=new byte[length];  //create a byte array to return
		final int copyLength=Math.min(string.length(), length-1); //find out how many bytes to copy G***fix; this assumes UTF-8
		System.arraycopy(stringBytes, 0, asciizBytes, 0, copyLength); //copy the string bytes to the outgoing array
		asciizBytes[copyLength]=0;  //add a zero to the end of the string
		return asciizBytes; //return the byte array we constructed
	}

	/**Creates an input stream from which to read the given string.
	@param string The string for which an input stream should be created.
	@param encoding The encoding to use to turn the string into bytes.
	@return An input stream of the string bytes.
	@exception UnsupportedEncodingException Thrown if the given encoding is not
		supported.
	*/
	public static InputStream getInputStream(final String string, final String encoding) throws UnsupportedEncodingException
	{
		return new ByteArrayInputStream(string.getBytes(encoding)); //return an input stream to the bytes of the string, encoded using the given encoding
	}

	/**Returns the string or <code>null</code> if the string is the empty string ("").
	@param string The string to examine.
	@return <code>string</code> if the length of the string is greater than zero,
		otherwise <code>null</code>.
	*/
	public static String getNonEmptyString(final String string)
	{
		return string.length()>0 ? string : null;	//return the string if it has a positive length
	}

	/**Finds the index of a particular character, ignoring case.
	@param string The string to search.
	@param c The character to search for.
	@return The index of the character ignoring case, or -1 if the character could
		not be found.
	*/
	static public int indexOfIgnoreCase(final String string, final char c)
	{
		return indexOfIgnoreCase(string, c, 0); //attempt to search from the beginning
	}

	/**Finds the index of a particular character, ignoring case, from a given index.
	@param string The string to search.
	@param c The character to search for.
	@param fromIndex The index at which to begin searching
	@return The index of the character ignoring case, or -1 if the character could
		not be found.
	*/
	static public int indexOfIgnoreCase(final String string, final char c, final int fromIndex)
	{
		return string.toUpperCase().indexOf(Character.toUpperCase(c), fromIndex); //convert the string and character to uppercase and search
	}

	/**Finds the index of a particular substring, ignoring case.
	@param string The string to search.
	@param substring The string to search for.
	@return The index of the substring ignoring case, or -1 if the substring could
		not be found.
	*/
	static public int indexOfIgnoreCase(final String string, final String substring)
	{
		return indexOfIgnoreCase(string, substring, 0); //attempt to search from the beginning
	}

	/**Finds the index of a particular substring, ignoring case, from a given index.
	@param string The string to search.
	@param substring The string to search for.
	@param fromIndex The index at which to begin searching
	@return The index of the substring ignoring case, or -1 if the substring could
		not be found.
	*/
	static public int indexOfIgnoreCase(final String string, final String substring, final int fromIndex)
	{
		return string.toUpperCase().indexOf(substring.toUpperCase(), fromIndex); //convert the strings to uppercase and search
	}

	/**Inserts a string at a specified index.
	@param inString the String into which the information will be inserted.
	@param index The information will be inserted before the character that appears at this index.
	@param insertString The string to insert.
	@return A new string with the specified information inserted at the specified location.
	*/
	static public String insert(final String inString, final int index, final String insertString)
	{
		return inString.substring(0, index)+insertString+inString.substring(index);	//return a string with the specified string inserted at the specified location
	}

	/**Inserts a character at a specified index.
	@param inString the String into which the information will be inserted.
	@param index The information will be inserted before the character that appears at this index.
	@param insertChar The character to insert.
	@return A new string with the specified information inserted at the specified location.
	*/
	static public String insert(final String inString, final int index, final char insertChar)
	{
		return inString.substring(0, index)+insertChar+inString.substring(index);	//return a string with the specified character inserted at the specified location
	}

	/**Searches a string in reverse and returns the last index of a substring
		without case sensitivity, starting from <code>fromIndex</code>.
	@param string The string to be searched.
	@param substring The substring for which to search.
	@param fromIndex The index from which to search.
	@return The index of the last occurrence of the substring at or less than the
		given index, or -1 if none was found.
	*/
	static public int lastIndexOfIgnoreCase(final String string, final String substring, final int fromIndex)
	{
		return string.toUpperCase().lastIndexOf(substring.toUpperCase(), fromIndex);  //search without regard to case
	}

	/*Specifies whether or not a given character is a nunmber.
	@param ch Character to analyze.
	@return true if the character is a number.
	*/
/*G***del
	static public boolean isNumber(final char ch)
	{
		return NUMBER_STRING.indexOf(ch)!=-1;	//return true if we can find the character in the string of number characters
	}
*/

	/**Returns the first
	static public int firstDelimiterIndex(final String delimiters, final String s);

*/

/*G***fix
function FirstDelimiter(const Delimiters, S:String):Integer;	//returns the first character of S that is also in Delimiters; returns 0 if not found; modified from Borland's FindLastDelimiter function
begin
	Result:=NextDelimiter(Delimiters, S, 1);	//return the next delimiter, starting at position one
end;

function FirstNotDelimiter(const Delimiters, S:String):Integer;	//returns the first character of S that is not in Delimiters; returns 0 if not found
begin
	Result:=NextNotDelimiter(Delimiters, S, 1);	//return the next non-delimiter, starting at position one
end;

function NextDelimiter(const Delimiters, S:String; pos:Integer):Integer;	//returns the next character of S that is also in Delimiters; returns 0 if not found; modified from Borland's FindLastDelimiter function
begin
	Result:=pos;	//start looking at the first character
	while Result<=Length(S) do	//keep looking until we run out of characters
	begin
		if IsDelimiter(Delimiters, S, Result) then	//if this is one of the delimiters
			Exit;	//we're finished searching
		Inc(Result)	//look at the next character
	end;
	Result:=0;	//if we couldn't find a delimiter character, show that by returning zero
end;

function NextNotDelimiter(const Delimiters, S:String; pos:Integer):Integer;	//returns the next character of S, starting at pos, that is not in Delimiters; returns 0 if not found
begin
	Result:=pos;	//start looking at the first character
	while Result<=Length(S) do	//keep looking until we run out of characters
	begin
		if not IsDelimiter(Delimiters, S, Result) then	//if this is not one of the delimiters
			Exit;	//we're finished searching
		Inc(Result)	//look at the next character
	end;
	Result:=0;	//if we couldn't find a non-delimiter character, show that by returning zero
end;

function TrimLastDelimiters(const Delimiters, S: string):String;	//if there are delimiters is in string S, returns all characters up to, but not including, the last delimiter
var
	delimiterPos:Integer;
begin
	Result:=S;	//we'll start out with the entire string
	delimiterPos:=LastDelimiter(Delimiters, Result);	//find the last position of the delimiter
	if delimiterPos<>0 then	//if a delimiter is in the string
		Result:=Copy(Result, 1, delimiterPos-1);	//return the string up to but not including the last delimiter
end;

function TrimTrailingDelimiters(const Delimiters, S: string):String;	//if there are delimiters is in string S, and they are the last characters, returns all characters up to, but not including, the last delimiter
begin
	Result:=S;	//we'll start out with the entire string
	while (Length(S)>0) and IsDelimiter(Delimiters, Result, Length(S)) do	//if the last character in this string is a delimiter
		Result:=Copy(Result, 1, Length(S)-1);	//return the string up to but not including the last character
end;

function LinePos(S:String; lineNumber:Integer):Integer;	//returns the position of the given line in the string, or 0 if that line is not found
begin
	Result:=TokenPos(S, lineNumber, EOLString);	//a line is a token surrounded by EOL characters
end;

function LineEndPos(S:String; lineNumber:Integer):Integer;	//returns the ending position (one more than the last character) of the given line in the string, or 0 if that line is not found
begin
	Result:=TokenEndPos(S, lineNumber, EOLString);	//a line is a token surrounded by EOL characters
end;

function StringLine(S:String; lineNumber:Integer):String;	//returns the given line in the string, or '' if the line is not found
begin
	Result:=StringToken(S, lineNumber, EOLString);	//a line is a token surrounded by EOL character
end;

*/



	/**Determines whether the string starts with the given prefix, ignoring case.
	@param string The string to search.
	@param prefix The starting string to check for.
	@return <code>true</code> if the string starts with the given prefix, ignoring
		case.
	*/
	static public boolean startsWithIgnoreCase(final String string, final String prefix)
	{
		return string.toUpperCase().startsWith(prefix.toUpperCase()); //convert the strings to uppercase and check the prefix G***use a more efficient method that doesn't include creating new strings
	}

	/**Returns the index of the given numbered (one-based) token.
	@param inString The string to search.
	@param tokenNumber The number (one-based) of the token to find.
	@param delimiters The characters to use for delimiters.
	@return The index of the specified token number, or -1 if that token was not found.
	*/
	static public int tokenIndex(final String inString, int tokenNumber, final String delimiters)
	{
		int i=0;	//start at the beginning of the string
		while(true)
		{
			i=CharSequenceUtilities.notCharIndexOf(inString, delimiters, i);	//find the next token
			if(i==-1)	//if there is no other token
				break;		//exit, because there are no more tokens left
			tokenNumber--;	//show that we've found another token
			if(tokenNumber==0)	//if we've found all the tokens we needed to
				break;	//leave, because i now has the position of that token
			else	//if there are still more tokens to find
			{
				i=CharSequenceUtilities.charIndexOf(inString, delimiters, i);	//starting at our current position, find the next delimiter character
				if(i==-1)	//if there is no delimiter after this token (i.e. this is the last token)
					break;	//exit, because there are no more tokens left
			}
		}
		return i;	//return the index of the token, or -1 if this token doesn't exist
	}

	/**Returns the index right after the given numbered (one-based) token.
	@param inString The string to search.
	@param tokenNumber The number (one-based) of the token to find.
	@param delimiters The characters to use for delimiters.
	@return The index of one character past the last character of the specified token number, or -1 if that token was not found.
	*/
	static public int tokenEndIndex(final String inString, final int tokenNumber, final String delimiters)
	{
		int i=tokenIndex(inString, tokenNumber, delimiters);	//find the beginning of the specified token
		if(i!=-1)	//if we found the beginning of the specified token
		{
			i=CharSequenceUtilities.charIndexOf(inString, delimiters, i);	//find the character right after the token
			if(i==-1)	//if there are no more delimiters after this token
				i=inString.length();	//we know that this token goes to the end of the string
		}
		return i;	//return the index of one character past the token, or -1 if this token doesn't exist
	}


	/**Returns the specified numbered (one-based) token in the specified string, separated by delimiters.
	@param inString The string to search.
	@param tokenNumber The number (one-based) of the token to find.
	@param delimiters The characters to use for delimiters.
	@return The specified numbered (one-based) token in the specified string, separated by delimiters, or "" if that token was not found.
	*/
	static public String stringToken(final String inString, final int tokenNumber, final String delimiters)
	{
		String token="";	//assume we couldn't find the specified token
		int beginIndex=tokenIndex(inString, tokenNumber, delimiters);	//find the beginning of the specified token
		if(beginIndex!=-1)	//if we found the beginning of the specified token
		{
			int endIndex=tokenEndIndex(inString, tokenNumber, delimiters);	//find the end of the specified token
			token=inString.substring(beginIndex, endIndex);	//get the token
		}
		return token;	//return the token
	}

	/**Returns the index of the given numbered (one-based) word.
	@param inString The string to search.
	@param tokenNumber The number (one-based) of the token to find.
	@return The index of the specified word, or -1 if that word was not found.
	*/
	static public int wordIndex(final String inString, final int wordNumber)
	{
		return tokenIndex(inString, wordNumber, WORD_DELIMITER_CHARS);	//return the index of the word (a word is a token surrounded by word delimiters)
	}

	/**Returns the index right after the given numbered (one-based) word.
	@param inString The string to search.
	@param wordNumber The number (one-based) of the word to find.
	@return The index of one character past the last character of the specified word number, or -1 if that word was not found.
	*/
	static public int wordEndIndex(final String inString, final int wordNumber)
	{
		return tokenEndIndex(inString, wordNumber, WORD_DELIMITER_CHARS);	//return the ending index of the word (a word is a token surrounded by word delimiters)
	}

	/**Returns the specified numbered (one-based) word in the specified string.
	@param inString The string to search.
	@param wordNumber The number (one-based) of the word to find.
	@return The specified numbered (one-based) word in the specified string, or "" if that word was not found.
	*/
	static public String stringWord(final String inString, final int wordNumber)
	{
		return stringToken(inString, wordNumber, WORD_DELIMITER_CHARS);	//return the word (a word is a token surrounded by word delimiters)
	}

	/**Returns the given word in the string, or "" if that word is not present.
	@param inString The string with the word.
	@param wordIndex The index of the word to retrieve.
	@return The wordIndex word in inString, or "" if there are not enough words.
	*/
/*G***del
	static public String getWord(final String inString, final int wordIndex)
	{
				int wordBegin=getWordBeginning(outString, checkIndex);	//find the beginning of this word
				int originalWordEnd=getWordEnd(outString, checkIndex);	//find the end of this word
				int newWordEnd=originalWordEnd;	//we'll ignore ending punctuation marks



	}
*/


	/**Returns the beginning of the word at index. If the character at index is whitespace, the beginning of the previous word will be returned.
	@param inString The string with the word.
	@param index The index of the character in a word.
	@return The index of the beginning character of the word.
	*/
	static public int getWordBeginning(final String inString, final int index)	//G***del this function
	{
		int i;
		for(i=index; i>0 && !isWhitespace(inString.charAt(i-1)); --i);	//start at index and look back
		return i;	//return the index we found
	}

	/**Returns the end of the word at index. If the character at index is whitespace, the end of the next word will be returned.
	@param inString The string with the word.
	@param index The index of the character in a word.
	@return The index of the ending character of the word.*/
	static public int getWordEnd(final String inString, final int index)	//G***del this function
	{
		int i;
		for(i=index; i<inString.length()-1 && !isWhitespace(inString.charAt(i+1)); ++i);	//start at index and look forward
		return i;	//return the index we found
	}

	/**Replaces any XML-specific characters with their normal ASCII equivalents (e.g. replaces &quot; with '\"').
	@param inString A string which could contain XML character codes.
	@return A normal string.*/
/*G***del if not needed
	static public String fromXML(final String inString)
	{
		String outString=replace(inString, "&quot;", "\"");	//replace all occurences of &quot; with a quotes
		outString=replace(outString, "&apos;", "\'");	//replace all occurrences of &apos; with single quotes
		outString=replace(outString, "&gt;", ">");	//replace all occurrences of &gt; with a greater than sign
		outString=replace(outString, "&lt;", "<");	//replace all occurrences of &lt; with a less than sign
		outString=replace(outString, "#par;", "\n");	//replace all occurrences of #par; with a newline
		outString=replace(outString, "&amp;", "&");	//replace all occurrences of &amp; with an ampersand sign
		return outString;	//return our resulting string
	}
*/

	/**Manipulates a string (e.g. replaces quotes with \") so that it can be placed inside quotes in a JavaScript file.
	@param inString The string to be manipulated.
	@return A JavaScript-friendly string.*/
/*G***del if not needed
	static public String makeJavaScriptFriendly(final String inString)
	{
		String outString=replace(inString, '"', "\\\"");	//replace all quotes with \"
		outString=replace(outString, '\'', "\\'");	//replace all single quotes with \'
		outString=removeEveryChar(outString, "\r\n");	//remove every <CR> and <LF> from the string
		return outString;	//return our resulting string
	}
*/

	/**Turns a string into HTML. The following changes will be made:
	<UL>
	<LI>Hyperlink tags will be added, if hyperlinks are present.
	<LI>Paragraph tags will be added.
	</UL>
	@param inString The string that will be turned into HTML.
	@return A string with HTML paragraph tags.*/
/*G***del if not needed
	static public String makeHTML(final String inString)
	{
		String outString=inString.trim();	//this is the string we'll process; start out by trimming it
		outString=makeHTMLHyperlinks(outString);	//add hyperlinks
		outString=makeHTMLParagraphs(outString);	//add add paragraphs
		return outString;	//return the strign with the added HTML tags
	}
*/

	/**Parses the string and converts any <CR>'s or <LF>'s to HTML <BR> characters.
	@param inString The string that will be turned into HTML breaks.
	@return A string with HTML break tags.*/
/*G***del if not needed
	static public String makeHTMLBreaks(final String inString)
	{
		String outString=inString.trim();	//first, trim the leading and trailing whitespace (including CR/LF) from the string
		outString=removeEvery(outString, '\r');	//remove any <CR>'s that may be in the string
		outString=replace(outString, '\n', "<BR>");	//replace every occurrence of <LF> with a break tag
		return outString;	//return the processed string
	}
*/

	/**Parses the string and finds any hyperlinks, to which it then creates and inserts the appropriate HTML tags.
	@param inString The string in which to find hyperlinks.
	@return A string with HTML hyperlink tags embedded.*/
	static public String makeHTMLHyperlinks(final String inString)
	{
		String outString=inString;	//this is the string we'll process
		int fromIndex=0;	//we'll start looking for links at the beginning of the string
		while(fromIndex<outString.length())	//keep looking until we run out of characters
		{
			int checkIndex=CharSequenceUtilities.charIndexOf(outString, ".@", fromIndex);	//see if we can find any of the hyperlink characters
			if(checkIndex!=-1)	//if we found one of them
			{
				int wordBegin=getWordBeginning(outString, checkIndex);	//find the beginning of this word
				int originalWordEnd=getWordEnd(outString, checkIndex);	//find the end of this word
				int newWordEnd=originalWordEnd;	//we'll ignore ending punctuation marks
				while(newWordEnd>checkIndex)	//start looking for punctuation at the end of the word
				{
					if(isPunctuation(outString.charAt(newWordEnd)))	//if this is a punctuation mark
						newWordEnd--;	//back up a letter to specify the word end
					else	//if this is not a punctuation mark
						break;	//we now know that the word doesn't end in a punctuation mark
				}
				if(checkIndex!=wordBegin && checkIndex!=newWordEnd)	//if the hyperlink character is not at the beginning or ending character of the word
				{
					String HREF=outString.substring(wordBegin, newWordEnd+1);	//get the location to jump to
					if(HREF.indexOf("..")==-1)	//make sure there are not two periods in a row
					{
						final String protocolString=HREF.indexOf('@')!=-1 ? "mailto:" : "http://";	//if the location has the '@' character, it's an e-mail address; otherwise, it's an HTTP URL
						if(!((HREF.length()>3 && HREF.substring(0, 4).toLowerCase().equals("http")) || (HREF.length()>5 && HREF.substring(0, 6).toLowerCase().equals("mailto"))))	//if the hyperlink doesn't have a protocol at the beginning already G***fix for ftp
							HREF=protocolString+HREF;	//add the protocol to the beginning of the address for them (in our link, not in the normal text
						final String tagPrefix="<A HREF=\""+HREF+"\">";	//create the tag prefix
						final String tagPostfix="</A>";	//create the tag postfix
						outString=insert(outString, newWordEnd+1, tagPostfix);	//insert the tag postfix first (after the last letter of the word), so it won't mess up our word beginning index, yet
						outString=insert(outString, wordBegin, tagPrefix);	//insert the tag prefix
						fromIndex=originalWordEnd+tagPrefix.length()+tagPostfix.length()+1;	//we'll start looking for other hyperlinks one character after the original end of the word, compensating for the characters we added
						continue;	//go back and start checking from our new starting index
					}
				}
//G***del				else	//if this hyperlink character came at the beginning or ending of the word
					fromIndex=originalWordEnd+1;	//start looking again at the character following this word
//G***del					fromIndex=checkIndex+1;	//start looking again at the character following this hyperlink character
			}
			else	//if we didn't find any more hyperlink characters
				break;	//we've found all we could
		}
		return outString;	//return the resulting string
	}

	/**Parses the string and converts any <CR>'s or <LF>'s to HTML paragraphs.
	@param inString The string that will be turned into HTML paragraphs.
	@return A string with HTML paragraph tags.*/
/*G***del if not needed
	static public String makeHTMLParagraphs(final String inString)
	{
		String outString=inString.trim();	//first, trim the leading and trailing whitespace (including CR/LF) from the string
		outString="<P>"+outString+"</P>";	//make the string one big paragraph
		outString=removeEvery(outString, '\r');	//remove any <CR>'s that may be in the string
		outString=replace(outString, '\n', "</P><P>");	//replace every occurrence of <LF> with the end and beginning of a paragraph
		return outString;	//return the processed string
	}
*/

	/**Manipulates a string (e.g. replaces quotes with &quot;) so that it can be placed inside quotes in an XML file.
	@param inString The string to be manipulated.
	@return An XML-friendly string.*/
/*G***del if not needed
	static public String makeXMLFriendly(final String inString)
	{
		String outString=replace(inString, '&', "&amp;");	//replace all ampersand signs with &amp;
		outString=replace(outString, '"', "&quot;");	//replace all quotes with &quot;
		outString=replace(outString, '\'', "&apos;");	//replace all single quotes with &apos;
		outString=replace(outString, '>', "&gt;");	//replace all greater than signs with &gt;
		outString=replace(outString, '<', "&lt;");	//replace all less than signs with &lt;
		outString=removeEvery(outString, '\r');	//remove any <CR>'s that may be in the string
		outString=replace(outString, '\n', "#par;");	//replace all newlines with #par;
		return outString;	//return our resulting string
	}
*/

	/**Creates a string containing a range of characters.
	@param firstChar The character to start with.
	@param lastChar The last character to include, which should have a value
		higher than or equal to <code>firstChar</code>.
	@return A string containing a range of characters including the first and
		last characters provided.
	*/
	public static String createString(final char firstChar, final char lastChar)
	{
		final StringBuilder stringBuilder=new StringBuilder(lastChar-firstChar+1);	//create a string buffer with enough room to hold the characters
		for(char c=firstChar; c<=lastChar; stringBuilder.append(c++));	//append the entire range of characters to the string buffer
		return stringBuilder.toString();	//return the string we constructed
	}
	
	/**Creates a string with a given repetition of characters.
	@param ch The character to be in the string.
	@param count The number of repetitions of the character.
	@return A string with count repetitions of ch.*/
	public static String createString(final char ch, final int count)
	{
		return StringBuilderUtilities.append(new StringBuilder(), ch, count).toString();	//append the characters to a new string builder and return the string version of the result
	}

	/**Ensures that the given string is the correct length by adding or deleting characters to or from the front.
	@param inString The string to process.
	@param len The requested length.
	@param ch The character to be added to the string, if needed.
	@param pos The position at which to insert or delete characters, or -1 if the end should be used.
	@return A string with the correct length.*/
	public static String makeStringLength(final String inString, final int len, final char ch, int pos)	//TODO refactor this into a StringBuilderUtilities method
	{
		final int originalLength=inString.length();	//get the length of the original string
		if(originalLength==len)		//if the string is already the correct length
		{
			return inString;	//return the string untouched
		}
		else	//if the string isn't the correct length already
		{
			final StringBuilder stringBuilder=new StringBuilder(inString);	//create a new string builder with the contents of the existing string
			if(pos==-1 && pos>originalLength)	//if they want to insert/delete characters at the end of the string (or if they supplied an incorrect position) TODO don't be so lenient here---throw an exception, as this probably reflects an error somewhere else in the code
				pos=originalLength;	//find that position
			if(originalLength>len)	//if the string is too long
			{
				final int removeCount=originalLength-len;	//find out how many characters to remove
				if(pos>originalLength-removeCount)	//if our position is too close to the end to successfully remove all characters
					pos=originalLength-removeCount;	//place our position at just the right place to remove the characters
				stringBuilder.delete(pos, pos+removeCount);	//remove the characters
			}
			else	//if the string is too short
			{
				StringBuilderUtilities.insert(stringBuilder, pos, ch, len-originalLength);	//insert a string of the correct length at the correct position with the specified characters
			}
			return stringBuilder.toString();	//return a string representation of our string builder
		}
	}

	/**Parses the string and converts tabs, carriage returns, and linefeeds to HTML paragraphs.
	@param inString The string that will be made printable.
	@return A string with whitespace escaped.*/
/*G***del if not needed
	static public String makePrintable(final String inString)
	{
		String outString=inString;	//make a copy of the string
		outString=replace(outString, '\t', "\\t");	//replace the tab characters
		outString=replace(outString, '\r', "\\r");	//replace the carriage returns
		outString=replace(outString, '\n', "\\n");	//replace the linefeeds
		return outString;	//return the processed string
	}
*/

	/**Removes a character at the specified index.
	@param inString the String from which the information will be removed.
	@param index The index of the information to remove.
	@return A new string with the specified information removed from the specified location.*/
	static public String remove(final String inString, final int index)
	{
		return remove(inString, index, 1);	//remove one character from the specified location
	}

	/**Removes several characters at the specified index.
	@param inString the String from which the information will be removed.
	@param index The index of the information to remove.
	@param len The number of characters to remove.
	@return A new string with the specified information removed from the specified location.*/
	static public String remove(final String inString, final int index, final int len)	//G***now maybe just call replace() with "" for the replacement text
	{
		//G***what if index is zero? will this still work?
		return inString.substring(0, index)+inString.substring(index+len);	//return a string with the specified information removed
	}

	/**Removes all characters that come after and including the first occurrence
		of a character in the given delimiter string. If the character does not
		exist in the string, the original string will be returned.
	@param string The string to check.
	@param delimiters The characters that indicate removal should occur.
	@return The string with the first matching character and everything after it
		removed, or the original string if no characters were in the supplied set
		of delimiters.
	*/
	public static String removeAfterFirstChar(String string, final String delimiters)
	{
		for(int i=0; i<string.length(); ++i)  //look at each character in the string
		{
			if(delimiters.indexOf(string.charAt(i))>=0) //if this character is one of our delimiters
				return string.substring(0, i);  //return all the characters before this character
		}
		return string;  //return the original string if we couldn't find a match
	}

	/**Removes every occurrence of a specified character.
	@param inString the String from which the information will be removed.
	@param removeChar The character to remove from the string.
	@return A new string with the specified information removed.
	*/
/*G***del if not needed, else call the StringBuffer version
	public static String removeEvery(final String inString, final char removeChar)
	{
		String outString=inString;	//this is the string we'll process
		int i=0;	//start at the beginning of the string
		while(i<outString.length())	//keep going until we reach the end of the string
		{
			if(outString.charAt(i)==removeChar)	//if we have found a character to remove
				outString=remove(outString, i);	//remove this character from the string, which will mean that i is now at the next character
			else	//if we're not at a character to remove
				++i;	//go to the next character in the string
		}
		return outString;	//return the processed string
	}
*/

	/**Removes every occurrence of any of the given specified characters.
	@param inString the String from which the information will be removed.
	@param removeChars The characters to be removed from the string.
	@return A new string with the specified information removed.
	*/
/*G***del if not needed, else call the StringBuffer version
	public static String removeEveryChar(final String inString, final String removeChars)
	{
		String outString=inString;	//this is the string we'll process
		int i=0;	//start at the beginning of the string
		while(i<outString.length())	//keep going until we reach the end of the string
		{
			if(removeChars.indexOf(outString.charAt(i))!=-1)	//if this character is one of our remove characters
				outString=remove(outString, i);	//remove this character from the string, which will mean that i is now at the next character
			else	//if we're not at a character to remove
				++i;	//go to the next character in the string
		}
		return outString;	//return the processed string
	}
*/

	/**Removes all characters that come before the last occurrence of the given'
		character. If the character does not exist in the string, the original
		string will be returned.
	 */
	public static String removeBeforeLast(String string, final char c)
	{
		final int lastIndex=string.lastIndexOf(c); //get the last index of the character
		if(lastIndex>=0)  //if the character exists in the string
			string=string.substring(lastIndex+1); //throw away everything up to and including the character
		return string;  //return the string which may or may not have information removed
	}

	/**Removes the given substring, matched without case sensitivity, and
		everything following the string.
	@param string The string that may contain the substring.
	@param substring The string to match without case sensitivity.
	@return The string with the substring and everything following it removed,
		or the original string if no changes were made.
	*/
	public static String removeLengthIgnoreCase(final String string, final String substring)  //G***probably rename truncateIgnoreCase
	{
		final int index=indexOfIgnoreCase(string, substring);  //see if the substring appears in the text
			//if the substring is present, remove it and everything following
		return index>=0 ? string.substring(0, index) : string;
	}

	/**Converts a list of strings into an array of strings.
	@param list The list of strings.
	@return An array containing the strings in the list.
	*/
	public static String[] toStringArray(final List list)
	{
		return (String[])list.toArray(new String[list.size()]);	//create a string array of the correct size and put the contents of the list in the array
	}

	/**Trims the right side of the string beginning at the last occurrence of
		removeChar. If removeChar does not exist in the string, no information is removed.
	@param inString the String from which the information will be removed.
	@param removeChar The character to remove from the string.
	@return A new string with its end removed.
	*/
/*G***del
	static public String trimLastChar(final String inString, final char removeChar)
	{
		final int charIndex=charLastIndexOf(inString, String.valueOf(removeChar));	//get the last index of the specified character
		if(charIndex!=-1)	//if we found the character
			return inString.substring(0, charIndex);	//return the string with the ending removed
		else	//if we didn't find the character
			return inString;	//just return the string as we received it
	}
*/

	/**Trims the last side of the string beginning at the first occurrence of
		removeChar. If removeChar does not exist in the string, no information is
		removed.
	@param inString the String from which the information will be removed.
	@param removeChar The character to remove from the string.
	@return A new string with its beginning removed.
	*/
	static public String trimFirstChar(final String inString, final char removeChar)
	{
		return trimFirstChar(inString, removeChar, 1); //trim on the first occurrence of the character
	}

	/**Trims the left side of the string beginning at the specified occurrence of
		removeChar from the beginning. If removeChar does not exist in the string the
		required number of times, the string will be trimmed at the last occurrence.
		information is removed.
	@param inString the String from which the information will be removed.
	@param removeChar The character to remove from the string.
	@param occurrence The number of occurrences of the remove character before
		information should be removed.
	@return A new string with its end removed.
	*/
	static public String trimFirstChar(final String inString, final char removeChar, int occurrence)
	{
		int occurrenceIndex=-1;  //we'll start looking at the beginning
		for(int i=occurrenceIndex+1; i<inString.length(); ++i) //look at each character, starting at the end
		{
			if(inString.charAt(i)==removeChar)  //if this is the character to remove
			{
				occurrenceIndex=i;  //show where the last occurrence took place
				if((--occurrence)==0) //decrement occurrence; if we've used up all occurrences
				{
					break;  //stop searching for a place to trim
				}
			}
		}
		return inString.substring(occurrenceIndex+1);  //remove everything past but not including the last occurrence of the remove character	}
	}

	/**Trims the right side of the string beginning at the last occurrence of
		removeChar. If removeChar does not exist in the string, no information is removed.
	@param inString the String from which the information will be removed.
	@param removeChar The character to remove from the string.
	@return A new string with its end removed.
	*/
	static public String trimLastChar(final String inString, final char removeChar)
	{
		return trimLastChar(inString, removeChar, 1); //trim on the first occurrence of the character
	}

	/**Trims the right side of the string beginning at the specified occurrence of
		removeChar from the end. If removeChar does not exist in the string the
		required number of times, the string will be trimmed at the last occurrence.
		information is removed.
	@param inString the String from which the information will be removed.
	@param removeChar The character to remove from the string.
	@param occurrence The number of occurrences of the remove character before
		information should be removed.
	@return A new string with its end removed.
	*/
	static public String trimLastChar(final String inString, final char removeChar, int occurrence)
	{
		int occurrenceIndex=inString.length()+1;  //we'll start looking at the end
		for(int i=occurrenceIndex-1; i>=0; --i) //look at each character, starting at the end
		{
			if(inString.charAt(i)==removeChar)  //if this is the character to remove
			{
				occurrenceIndex=i;  //show where the last occurrence took place
				if((--occurrence)!=0) //decrement occurrence; if we've used up all occurrences
				{
					break;  //stop searching for a place to trim
				}
			}
		}
		return inString.substring(0, occurrenceIndex);  //remove everything up to but not including the last occurrence of the remove character
	}

	/**Replaces all occurrences of a character with a string.
	@param inString The string to manipulate.
	@param ch The character to replace.
	@param str The string to replace the character with.
	@return The resulting string.
	*/
/*G***del
	static public String replace(final String inString, final char ch, final String str)
	{
		String outString="";	//the string we'll return
		for(int i=0; i<inString.length(); ++i)	//look at each character in the string
		{
			final char currentChar=inString.charAt(i);	//get the character we're looking at
			if(currentChar==ch)	//if this character should be replaced
				outString+=str;	//add the appropriate replacement string
			else	//if the character shouldn't be replaced
				outString+=currentChar;	//add the normal character to the string
		}
		return outString;	//return the string we formed
	}
*/

	/**Removes several characters at the specified index and replaces them with the given string.
	@param inString the String from which the information will be removed.
	@param index The index of the information to remove.
	@param len The number of characters to remove.
	@param replaceString The string of characters to put in the place of the removed characters.
	@return A new string with the specified information removed from the specified location.*/
	static public String replace(final String inString, final int index, final int len, final String replaceString)
	{
		return inString.substring(0, index)+replaceString+inString.substring(index+len);	//return a string with the specified information removed
	}

	/**Replaces every occurrence of a specified character with a string.
	@param inString the String from in the information will be replaced.
	@param replaceChar The character to replace.
	@param withString The string that will replace replaceChar.
	@return A new string with the specified information replaced.*/
	static public String replace(final String inString, final char replaceChar, final String withString)  //G***this can be made more efficient with string buffers
	{
		String outString=inString;	//this is the string we'll process
		int i=0;	//start at the beginning of the string
		while(i<outString.length())	//keep going until we reach the end of the string
		{
			if(outString.charAt(i)==replaceChar)	//if we have found a character to replace
			{
				outString=remove(outString, i);	//remove this character from the string, which will mean that i is now at the next character
				outString=insert(outString, i, withString);	//insert the string at the same index, putting i at the beginning of the inserted string
				i+=withString.length();	//skip to the character just after the string we just inserted
			}
			else	//if we're not at a character to replace
				++i;	//go to the next character in the string
		}
		return outString;	//return the processed string
	}

	/**Replaces every occurrence of a specified string with another string.
	@param inString the String from in the information will be replaced.
	@param replaceString The string to replace.
	@param withString The string that will replace replaceString.
	@return A new string with the specified information replaced.*/
	public static String replace(final String inString, final String replaceString, final String withString)
	{
		final StringBuilder stringBuilder=new StringBuilder(inString);	//create a string builder from the string
		StringBuilderUtilities.replace(stringBuilder, replaceString, withString);	//replace the contents of the string builder
		return stringBuilder.toString();	//return the string in which we replaced the charactersz
	}

	/**Replaces any of several matching characters with a paricular character.
	@param inString The string in which characters should be replaced.
	@param matchChars The string containing characters to be matched; every
		character that matches one of these characters will be replaced with the
		replacement character.
	@param replacementChar The character to replace any matched character.
	@return A string with the appropriate characters replaced by the replacement
		character.
	*/
	public static String replace(final String inString, final String matchChars, final char replacementChar)
	{
		final StringBuffer outStringBuffer=new StringBuffer(inString);  //the output string will be identical in length to the input string, because we're replacing characters with characters
			//replace the characters in the string buffer; if there were actually any replacments made
		if(StringBufferUtilities.replace(outStringBuffer, matchChars, replacementChar)>0)
			return outStringBuffer.toString();  //return the new string
		else  //if no replacements were made
			return inString;  //just return the original string, which should be faster than converting the string buffer to a string
	}

	/**Replaces any of several matching characters with the corresponding string.
	@param inString The string in which characters should be replaced.
	@param matchChars The string containing characters to be matched. If the
		string contains duplicate characters, only the first occurrence will be
		used.
	@param replacementStrings The strings with which to replace any character
		matches, each positioned at the same index in the array as the corresponding
		match character is positioned in the <code>matchChars</code> string.
	@return A string with the appropriate characters replaced by the corresponding
		strings.
	*/
/*G***finish, maybe
	public static String replace(final String inString, final String matchChars, final String[] replacementStrings[])
	{


	}
*/

	/**Replaces each matching character with the corresponding replacement string.
	@param string The string in which the replacements will be made.
	@param matchChars An array of characters to be replaced.
	@param replacementStrings An array of strings to replace the characters appearing at the same indexes as those in <var>matchChars</var>.
	@return The string with replacements made, which may be the original string if no replacements were made.
	*/
	public static String replace(final String string, final char[] matchChars, final String[] replacementStrings)
	{
			//first see if this string contains one of the match characters
			//we assume that most strings will not contain a match character, so we won't have to do any replacements
			//letting the string do the per-character search is much faster than using String.charAt()
		for(final char matchChar:matchChars)	//for each character to match
		{
			if(string.indexOf(matchChar)>=0)	//if the string contains this match character
			{
				final StringBuffer stringBuffer=new StringBuffer(string); //create a new string buffer with the given text
				StringBufferUtilities.replace(stringBuffer, matchChars, replacementStrings);  //do the replacement on the buffer
				return stringBuffer.toString(); //convert the results to a string and return it
			}
		}
		return string;	//if there are no matching match characters in the string, there's nothing to replace, so just return the original string
	}

	/**Truncates the string, if needed, to ensure that the string is no longer
		than the provided length.
	@param string The string to truncate.
	@param maxLength The maximum length of the string; if the string is longer
		than the given length, it will be truncated.
	@return The string, if the string was shorter than or equal to the maximum
		length; otherwise, the first <code>maxLength</code> characters of the string.
	*/
	public static String truncate(final String string, final int maxLength)
	{
		return string.length()<maxLength ? string : string.substring(0, maxLength); //if the string is too long, use only the first maxLength characters
	}

	/**Removes everything after and including the first occurrence of one of the
		given characters.
	@param string The string that may contain one or more of the characters.
	@param delimiters The characters that will cause
	@return The string with the first occuring character and everything after it
	  removed, or the original string if no changes were made.
	*/
	public static String truncateChar(final String string, final String delimiters)
	{
		final int index=CharSequenceUtilities.charIndexOf(string, delimiters);  //find the first occurrence of one of the characters
			//if one of the characters is present, remove it and everything following
		return index>=0 ? string.substring(0, index) : string;
	}

	/**Collapses every run of any number of collapseChars to a single replaceChar.
	@param inString the String in which the information will be collapsed.
	@param collapseChars The characters to be removed from the string.
	@param replaceString The string which will replace the collapseChars.
	@return A new string with the specified information collapsed.*/
	static public String collapseEveryChar(final String inString, final String collapseChars, final String replaceString)
	{
		if(CharSequenceUtilities.charIndexOf(inString, collapseChars)>=0)  //first search the string to see if we would replace something; if so
		{
			final StringBuffer stringBuffer=new StringBuffer(inString); //create a new string buffer from the string
			StringBufferUtilities.collapse(stringBuffer, collapseChars, replaceString); //collapse the characters
			return stringBuffer.toString(); //convert the string buffer back to a string and return it
		}
		else  //if there are no characters to collapse
			return inString;  //return the original string
/*G***del
//G***shouldn't we trim the string, first?
//G***maybe use a string buffer for faster performance, if that can be done
		final int replaceLength=replaceString.length(); //find the length of the replacement string
		String outString=inString;	//this is the string we'll process
		int nextIndex=0;	//start at the beginning of the string
		while(nextIndex<outString.length())	//keep going until we reach the end of the string
		{
//G***del System.out.println(outString.length());	//G***del
			if(collapseChars.indexOf(outString.charAt(nextIndex))!=-1)	//if this character is one of our remove characters
			{
				int removeEnd=nextIndex+1;	//start looking at the next character to see how long this run is
				while(removeEnd<outString.length() && collapseChars.indexOf(outString.charAt(removeEnd))!=-1)	//while we still have more characters, and the characters we're finding are characters to remove
					++removeEnd;
				final int removeLength=removeEnd-nextIndex;	//find out how many characters to remove
//G***del; we want to replace, for example, '\n' with the replacement string				if(removeLength!=1)  //if there's more than one character
				{
//G***del System.out.println("Ready to remove stuff from: "+StringManipulator.replace(outString, '\n', "\\n"));	//G***del
					outString=replace(outString, nextIndex, removeLength, replaceString);	//replace our characters with the given string
				  nextIndex+=replaceLength; //move to the position after the replacement string
//G***del System.out.println("New out string: "+StringManipulator.replace(outString, '\n', "\\n"));	//G***del
				}
			}
			else  //if this is not a character to replace
				++nextIndex;	//look at the next character in the string
		}
		return outString;	//return the processed string
*/
	}

/*G***del when works
	static public String collapseEveryChar(final String inString, final String collapseChars, final char replaceChar)
	{
//G***shouldn't we trim the string, first?
		String outString=inString;	//this is the string we'll process
		int i=0;	//start at the beginning of the string
		while(i<outString.length())	//keep going until we reach the end of the string
		{
//G***del System.out.println(outString.length());	//G***del
			if(collapseChars.indexOf(outString.charAt(i))!=-1)	//if this character is one of our remove characters
			{
				int removeEnd=i+1;	//start looking at the next character to see how long this run is
				while(removeEnd<outString.length() && collapseChars.indexOf(outString.charAt(removeEnd))!=-1)	//while we still have more characters, and the characters we're finding are characters to remove
					++removeEnd;
				final int removeLength=removeEnd-i;	//find out how many characters to remove

				if(removeLength!=1 || outString.charAt(i)!=replaceChar)	//if there's more than one character, or there's only one character and it's a different one than we were given (if there's only one character to replace and it's the same as our replacement character, there would be no need to replace it)
				{
//G***del System.out.println("Ready to remove stuff from: "+StringManipulator.replace(outString, '\n', "\\n"));	//G***del
					outString=replace(outString, i, removeLength, String.valueOf(replaceChar));	//replace our character
//G***del System.out.println("New out string: "+StringManipulator.replace(outString, '\n', "\\n"));	//G***del
				}
			}
			++i;	//whatever we did, we'll want to look at the next character in the string, now
		}
		return outString;	//return the processed string
	}
*/


	/**Trims the specified delimiters from the beginning and end of the string.
	@param inString The string to be processed.
	@param delimiters The string containing delimiter characters.
	*/
	static public String trim(final String inString, final String delimiters) //G***call the StringBuffer version---or maybe not---this may be more efficient
	{
		int beginIndex, endIndex;
		final int length=inString.length(); //get the length of the original string
		for(beginIndex=0; beginIndex<length && delimiters.indexOf(inString.charAt(beginIndex))!=-1; ++beginIndex);	//find the first non-delimiter in the string
		for(endIndex=length; endIndex>beginIndex && delimiters.indexOf(inString.charAt(endIndex-1))!=-1; --endIndex);	//find the last non-delimiter in the string
		if(beginIndex>0 || endIndex<length) //if there is something to trim
			return inString.substring(beginIndex, endIndex);	//return the substring minus the beginning and ending delimiters
		else  //if there is nothing to trim
			return inString;  //return the original string
	}

	/**Trims whitespace, including the Unicode no-break space character 0x00A0,
		from the beginning and end of the string.
	@param inString The string to be processed.
	*/
	static public String trimWhitespaceNoBreak(final String inString)	//TODO update with our new Unicode 4.x constants
	{
		final int length=inString.length(); //get the string's length
		int beginIndex, endIndex;
		  //find the first non-whitespace character in the string
		for(beginIndex=0; beginIndex<length; ++beginIndex)  //look at each character
		{
		  final char c=inString.charAt(beginIndex); //get the character at this index
			if(!Character.isWhitespace(c) && c!=NO_BREAK_SPACE_CHAR) //if this is not whitespace or a non-breaking space
				break;  //stop looking for non-whitespace
		}
		  //find the last non-whitespace character in the string
		for(endIndex=length; endIndex>beginIndex; --endIndex)
		{
		  final char c=inString.charAt(endIndex-1); //get the character at the previous index
			if(!Character.isWhitespace(c) && c!=NO_BREAK_SPACE_CHAR) //if this is not whitespace or a non-breaking space
				break;  //stop looking for non-whitespace
		}
		if(beginIndex>0 || endIndex<length) //if there is something to trim
			return inString.substring(beginIndex, endIndex);	//return the substring minus the beginning and ending delimiters
		else  //if there is nothing to trim
			return inString;  //return the original string
	}

	/**Trims whitespace, including the Unicode no-break space character 0x00A0,
		from the beginning of the string.
	@param inString The string to be processed.
	*/
	static public String trimWhitespaceNoBreakBeginning(final String inString)
	{
		final int length=inString.length(); //get the string's length
		int beginIndex;
		  //find the first non-whitespace character in the string
		for(beginIndex=0; beginIndex<length; ++beginIndex)  //look at each character
		{
		  final char c=inString.charAt(beginIndex); //get the character at this index
			if(!Character.isWhitespace(c) && c!=NO_BREAK_SPACE_CHAR) //if this is not whitespace or a non-breaking space
				break;  //stop looking for non-whitespace
		}
		if(beginIndex>0) //if there is something to trim
			return inString.substring(beginIndex);	//return the substring minus the beginning
		else  //if there is nothing to trim
			return inString;  //return the original string
	}

	/**Trims whitespace, including the Unicode no-break space character 0x00A0,
		from the beginning of the string.
	@param inString The string to be processed.
	*/
	static public String trimWhitespaceNoBreakEnd(final String inString)
	{
		final int length=inString.length(); //get the string's length
		int endIndex;
		  //find the last non-whitespace character in the string
		for(endIndex=length; endIndex>0; --endIndex)
		{
		  final char c=inString.charAt(endIndex-1); //get the character at the previous index
			if(!Character.isWhitespace(c) && c!=NO_BREAK_SPACE_CHAR) //if this is not whitespace or a non-breaking space
				break;  //stop looking for non-whitespace
		}
		if(endIndex<length) //if there is something to trim
			return inString.substring(0, endIndex);	//return the substring minus the ending delimiters
		else  //if there is nothing to trim
			return inString;  //return the original string
	}

	/**If the input string begins with the specifid string, trims that string.
	@param inString The string to be processed.
	@param beginString The string to be trimmed, if it appears at the beginning of
		inString.
	@return The string, trimmed if needed.
	*/
	static public String trimBeginning(final String inString, final String beginString)
	{
		return inString.startsWith(beginString) ? //if the string begins with beginString
				inString.substring(beginString.length()) :  //trim the string
				inString; //if the string doesn't begin with beginString, return the string itself
	}

	/**If the input string ends with the specifid string, trims that string.
	@param inString The string to be processed.
	@param beginString The string to be trimmed, if it appears at the end of
		inString.
	@return The string, trimmed if needed.
	*/
	static public String trimEnd(final String inString, final String endString)
	{
		return inString.endsWith(endString) ? //if the string ends with beginString
				inString.substring(0, inString.length()-endString.length()) :  //trim the string
				inString; //if the string doesn't end with endString, return the string itself
	}

	/**Performs word-wrapping on the input string by strategically inserting
		ends-of-line, ensuring that each line is no longer than the specified length.
	@param inString The string to be wrapped.
	@param wrapLength The maximum length of any line.
	@return The string wrapped with the given characters inserted.
	*/
	static public String wrap(final String inString, final int wrapLength)
	{
		return wrap(inString, wrapLength, UNDEFINED_CHAR, '\n');  //wrap the string using newlines
/*G***del when works
//G***del Debug.trace("inside wrap() with string: "+inString+", length: "+wrapLength);	//G***del
		final StringBuffer outStringBuffer=new StringBuffer(inString.length());	//create a new string buffer that's at least as long as the input string
		int lineBeginIndex=0;	//this will keep track of the start of each line
		while(lineBeginIndex<inString.length())	//while we haven't reached the end of the input string
		{
//G***del Debug.trace("Loop iteration, lineBeginIndex: "+lineBeginIndex);	//G***del
			int lineEndIndex=lineBeginIndex+wrapLength;	//we'll assume that we can't find any character on which to break, which will mean we'll have to force a break at the maximum length of the line
			if(lineEndIndex>inString.length())	//if we went past the end of the string
				lineEndIndex=inString.length();	//there's no need to wrap -- the end of the string is shorting than our wrapping length
			else	//if there are characters that need wrapped
			{
				for(int i=lineEndIndex-1; i>=lineBeginIndex; --i)	//look at each character from the maximum end of the line to the beginning
				{
					if(isWordWrap(inString.charAt(i)))	//if we can wrap on this character
					{
						lineEndIndex=i+1;	//we'll wrap right after this character
						break;	//stop searching for more wrapping characters on this line
					}
				}
			}
			outStringBuffer.append(inString.substring(lineBeginIndex, lineEndIndex));	//add this line to our string buffer
			if(lineEndIndex<inString.length())	//if we're not at the end of the input string (i.e. don't add a newline return a the end of the string)
				outStringBuffer.append('\n');	//add an end-of-line character
			lineBeginIndex=lineEndIndex;	//start looking at the next line
		}
		return outStringBuffer.toString();	//return the string from the we constructed
*/
	}

	/**Performs word-wrapping on the input string by strategically inserting
		ends-of-line, ensuring that each line is no longer than the specified length.
	@param inString The string to be wrapped.
	@param wrapLength The maximum length of any line.
	@param padChar The character to use to pad each line, or
		<code>UNDEFINED_CHAR</code> if lines should not be padded.
	@param eolChar The string to insert at the end of each line, or
		<code>UNDEFINED_CHAR</code> if ends of lines should not be marked.
	@return The string wrapped with the given characters inserted.
	*/
	static public String wrap(final String inString, final int wrapLength, final char padChar, final char eolChar)
	{
//G***del Debug.trace("inside wrap() with string: "+inString+", length: "+wrapLength);	//G***del
		final StringBuffer outStringBuffer=new StringBuffer(inString.length());	//create a new string buffer that's at least as long as the input string
		int lineBeginIndex=0;	//this will keep track of the start of each line
		while(lineBeginIndex<inString.length())	//while we haven't reached the end of the input string
		{
//G***del Debug.trace("Loop iteration, lineBeginIndex: "+lineBeginIndex);	//G***del
			int lineEndIndex=lineBeginIndex+wrapLength;	//we'll assume that we can't find any character on which to break, which will mean we'll have to force a break at the maximum length of the line
			if(lineEndIndex>inString.length())	//if we went past the end of the string
				lineEndIndex=inString.length();	//there's no need to wrap -- the end of the string is shorting than our wrapping length
			else	//if there are characters that need wrapped
			{
				for(int i=lineEndIndex-1; i>=lineBeginIndex; --i)	//look at each character from the maximum end of the line to the beginning
				{
					if(isWordWrap(inString.charAt(i)))	//if we can wrap on this character
					{
						lineEndIndex=i+1;	//we'll wrap right after this character
						break;	//stop searching for more wrapping characters on this line
					}
				}
			}
			outStringBuffer.append(inString.substring(lineBeginIndex, lineEndIndex));	//add this line to our string buffer
			if(lineEndIndex<inString.length())	//if we're not at the end of the input string (i.e. don't add a newline return a the end of the string)
			{
				if(padChar!=UNDEFINED_CHAR) //if we have a pad character
				{
					for(int padCount=wrapLength-(lineEndIndex-lineBeginIndex); padCount>0; --padCount)  //add the correct number of pad characters
					{
						outStringBuffer.append(padChar);  //add the pad character
					}
				}
				if(eolChar!=UNDEFINED_CHAR) //if we have a character to mark the end of the line
					outStringBuffer.append(eolChar);	//add an end-of-line character
			}
/*G***del
			if(lineEndIndex<inString.length())	//if we're not at the end of the input string (i.e. don't add a newline return a the end of the string)
				outStringBuffer.append('\n');	//add an end-of-line character
*/
			lineBeginIndex=lineEndIndex;	//start looking at the next line
		}
		return outStringBuffer.toString();	//return the string from the we constructed
	}

}
