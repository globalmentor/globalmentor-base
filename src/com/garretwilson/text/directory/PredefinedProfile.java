package com.garretwilson.text.directory;

import java.io.*;
import java.util.*;
import com.garretwilson.io.*;
import com.garretwilson.util.LocaleText;

/**Profile for predefined types of a <code>text/directory</code> as defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class PredefinedProfile extends AbstractProfile implements ValueFactory, DirectoryConstants
{

	/**Default constructor.*/	
	public PredefinedProfile()
	{
			//register the predefined types in our map
		registerValueType(SOURCE_TYPE, URI_VALUE_TYPE);	//SOURCE: uri		
		registerValueType(NAME_TYPE, TEXT_VALUE_TYPE);	//NAME: text		
		registerValueType(PROFILE_TYPE, TEXT_VALUE_TYPE);	//PROFILE: text		
		registerValueType(BEGIN_TYPE, TEXT_VALUE_TYPE);	//BEGIN: text		
		registerValueType(END_TYPE, TEXT_VALUE_TYPE);	//END: text					
	}
	
	/**Processes the textual representation of a line's value and returns
		one or more object representing the value, as some value types
		support multiple values.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	<p>This method knows how to create predefined types, which,
		along with the objects returned, are as follows:</p>
	<ul>
		<li><code>URI_VALUE_TYPE</code> <code>URI</code></li>
		<li><code>TEXT_VALUE_TYPE</code> <code>LocaleText</code></li>
		<li><code>DATE_VALUE_TYPE</code> <code>Date</code></li>
		<li><code>TIME_VALUE_TYPE</code> <code>Date</code></li>
		<li><code>DATE_TIME_VALUE_TYPE</code> <code>Date</code></li>
		<li><code>INTEGER_VALUE_TYPE</code> <code>Integer</code></li>
		<li><code>BOOLEAN_VALUE_TYPE</code> <code>Boolean</code></li>
		<li><code>FLOAT_VALUE_TYPE</code> <code>Double</code></li>
	</ul>
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param valueType The type of value, or <code>null</code> if the type of value
		is unknown.
	@param reader The reader that contains the lines of the directory.
	@return An array of objects represent the value string, or <code>null</code>
		if the type of value cannot be determined by the given line information,
		in which case no information is removed from the reader.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	@see NameValuePair
	@see URI_VALUE_TYPE
	@see URI
	@see TEXT_VALUE_TYPE
	@see LocaleText
	@see DATE_VALUE_TYPE
	@see TIME_VALUE_TYPE
	@see DATE_TIME_VALUE_TYPE
	@see Date
	@see INTEGER_VALUE_TYPE
	@see Integer
	@see BOOLEAN_VALUE_TYPE
	@see Boolean
	@see FLOAT_VALUE_TYPE
	@see Double
	*/
	public Object[] createValues(final String profile, final String group, final String name, final List paramList, final String valueType, final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		if(TEXT_VALUE_TYPE.equalsIgnoreCase(valueType))	//if this is the "text" value type
		{
			return processTextValueList(reader, paramList);	//process the text value
		}
		return null;	//show that we can't create a value
	}
	
	/**Processes a text value.
	<p>The sequence "\n" or "\N" will be converted to a single newline character,
		'\n'.</p>
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return An array of locale text objects representing the values.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static LocaleText[] processTextValueList(final LineUnfoldParseReader reader, final List paramList) throws IOException, ParseIOException
	{
		final Locale locale=DirectoryUtilities.getLanguageParamValue(paramList);	//get the language, if any
		final List localeTextList=new ArrayList();	//create a new list to hold the locale text objects we find
		char delimiter;	//we'll store the last delimiter peeked		
		do
		{
			reader.resetPeek();	//reset peeking
			final String string=processTextValue(reader);	//read a string
//		G***del Debug.trace("read text string: ", string);	//G***del
			localeTextList.add(new LocaleText(string, locale));	//add the text to our list			
			delimiter=reader.peekChar();	//see what character is next
//		G***del Debug.trace("next delimiter: ", delimiter);	//G***del			
		}
		while(delimiter==VALUE_SEPARATOR_CHAR);	//keep getting strings while we are still running into value separators
		reader.resetPeek();	//reset peeking
		return LocaleText.toLocaleTextArray(localeTextList);	//convert the list of locale text objects to an array and return the array
	}

	/**The delimiters that can divide a text value: '\\' ',' and CR.*/
	protected final static String TEXT_VALUE_DELIMITER_CHARS=""+TEXT_ESCAPE_CHAR+VALUE_SEPARATOR_CHAR+CR; 

	/**Processes a single text value.
	<p>The sequence "\n" or "\N" will be converted to a single newline character,
		'\n'.</p>
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@return An array of strings representing the values.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static String processTextValue(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final StringBuffer stringBuffer=new StringBuffer();	//create a string buffer to hold whatever string we're processing
		char delimiter;	//we'll store the last delimiter peeked		
		do	
		{
//		G***del Debug.trace("string buffer so far: ", stringBuffer);	//G***del			
			stringBuffer.append(reader.readStringUntilChar(TEXT_VALUE_DELIMITER_CHARS));	//read all undelimited characters until we find a delimiter
			delimiter=reader.peekChar();	//see what the delimiter will be
			switch(delimiter)	//see which delimiter we found
			{
				case TEXT_ESCAPE_CHAR:	//if this is an escape character ('\\')
					{
						reader.skip(1);	//skip the delimiter
						final char escapedChar=reader.readChar();	//read the character after the escape character
						switch(escapedChar)	//see what character comes after this one
						{
							case TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR:	//"\n"
							case TEXT_LINE_BREAK_ESCAPED_UPPERCASE_CHAR:	//"\N"
								stringBuffer.append('\n');	//append a single newline character
								break;
							case '\\':
							case ',':
								stringBuffer.append(escapedChar);	//escaped backslashes and commas get appended normally
							default:	//if something else was escape, we don't recognize it
								throw new ParseUnexpectedDataException("\\,"+TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR+TEXT_LINE_BREAK_ESCAPED_UPPERCASE_CHAR, escapedChar, reader);	//show that we didn't expect this character here				
						}
					}
					break;
				case VALUE_SEPARATOR_CHAR:	//if this is the character separating multiple values (',')
				case CR:	//if we just read a carriage return
					break;	//don't do anything---we'll just collect our characters and leave
				default:	//if we read anything else (there shouldn't be anything else unless there is a logic error)					
					throw new ParseUnexpectedDataException(TEXT_VALUE_DELIMITER_CHARS, delimiter, reader);	//show that we didn't expect this character here
			}
		}
		while(delimiter!=VALUE_SEPARATOR_CHAR && delimiter!=CR);	//keep collecting parts of the string until we encounter a ',' or a CR
		//G***check the text value
		reader.resetPeek();	//reset peeking
//	G***del Debug.trace("returning string: ", stringBuffer);	//G***del			
		return stringBuffer.toString();	//return the string we've collected so far
	}


	/**Determines the value type of the given content line value.
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return The value type of the content line, or <code>null</code> if the
		value type cannot be determined.
	*/	
	public String getValueType(final String profile, final String group, final String name, final List paramList)
	{
		return getValueType(name);	//return whatever value type we have associated with this type name, if any
	}

	/**Creates a directory from the given content lines.
	Unrecognized or unusable content lines within the directory object will be
		saved as literal content lines so that their information will be preserved.
	This version creates a basic <code>Directory</code> object.
	@param contentLines The content lines that make up the directory.
	@return A directory object representing the directory, or <code>null</code>
		if this profile cannot create a directory from the given information.
	*/
	public Directory createDirectory(final ContentLine[] contentLines)
	{
		final Directory directory=new Directory();	//create a basic directory object
		for(int i=0; i<contentLines.length; ++i)	//look at each content line
		{
			final ContentLine contentLine=contentLines[i];	//get a reference to this content line
			final String typeName=contentLine.getTypeName();	//get this content line's type name
			if(NAME_TYPE.equalsIgnoreCase(typeName))	//if this is NAME
			{
				if(directory.getDisplayName()==null)	//if the directory does not yet have a display name
				{
					directory.setDisplayName((LocaleText)contentLine.getValue());	//set the directory display name
					continue;	//don't process this content line further
				}
			}
				//if we make it to here, we either don't recognize the content line
				//	or we can't proces it (e.g. a duplicate value we don't support)
			directory.getContentLineList().add(contentLine);	//add this unprocessed content line to the directory's list of content lines
		}
		return directory;	//return the directory we created
	}

}
