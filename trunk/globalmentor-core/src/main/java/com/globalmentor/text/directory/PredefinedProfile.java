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

package com.globalmentor.text.directory;

import java.io.*;
import java.net.*;
import java.util.*;

import org.urframework.*;

import static com.globalmentor.io.Charsets.*;
import static com.globalmentor.io.ReaderParser.*;
import static com.globalmentor.text.ABNF.*;
import static com.globalmentor.text.directory.Directory.*;

import com.globalmentor.io.*;
import com.globalmentor.iso.datetime.ISODate;
import com.globalmentor.iso.datetime.ISODateTime;
import com.globalmentor.iso.datetime.ISOTime;
import com.globalmentor.java.Characters;
import com.globalmentor.model.*;
import com.globalmentor.util.Base64;

/**
 * Profile for predefined types of a <code>text/directory</code> as defined in <a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
 * "A MIME Content-Type for Directory Information".
 * <p>
 * The predefined profile knows how to process the standard directory value types: {@value Directory#URI_VALUE_TYPE}, {@value Directory#TEXT_VALUE_TYPE},
 * {@value Directory#DATE_VALUE_TYPE}, {@value Directory#TIME_VALUE_TYPE}, {@value Directory#DATE_TIME_VALUE_TYPE}, {@value Directory#INTEGER_VALUE_TYPE},
 * {@value Directory#BOOLEAN_VALUE_TYPE}, and {@value Directory#FLOAT_VALUE_TYPE}.
 * </p>
 * @author Garret Wilson
 * @see Directory#URI_VALUE_TYPE
 * @see Directory#TEXT_VALUE_TYPE
 * @see Directory#DATE_VALUE_TYPE
 * @see Directory#TIME_VALUE_TYPE
 * @see Directory#DATE_TIME_VALUE_TYPE
 * @see Directory#INTEGER_VALUE_TYPE
 * @see Directory#BOOLEAN_VALUE_TYPE
 * @see Directory#FLOAT_VALUE_TYPE
 */
public class PredefinedProfile extends AbstractProfile implements ValueFactory, ValueSerializer
{

	/** Default constructor. */
	public PredefinedProfile()
	{
		//register the predefined types in our map
		registerValueType(SOURCE_TYPE, URI_VALUE_TYPE); //SOURCE: uri		
		registerValueType(NAME_TYPE, TEXT_VALUE_TYPE); //NAME: text		
		registerValueType(PROFILE_TYPE, TEXT_VALUE_TYPE); //PROFILE: text		
		registerValueType(BEGIN_TYPE, TEXT_VALUE_TYPE); //BEGIN: text		
		registerValueType(END_TYPE, TEXT_VALUE_TYPE); //END: text					
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This method knows how to create predefined types, which, along with the objects returned, are as follows:
	 * </p>
	 * <dl>
	 * <dt>{@value Directory#URI_VALUE_TYPE}</dt>
	 * <dd>{@link URI}</dd>
	 * <dt>{@value Directory#TEXT_VALUE_TYPE}</dt>
	 * <dd>{@link LocaledText}</dd>
	 * <dt>{@value Directory#DATE_VALUE_TYPE}</dt>
	 * <dd>{@link Date}</dd>
	 * <dt>{@value Directory#TIME_VALUE_TYPE}E</dt>
	 * <dd>{@link Date}</dd>
	 * <dt>{@value Directory#DATE_TIME_VALUE_TYPE}</dt>
	 * <dd>{@link Date}</dd>
	 * <dt>{@value Directory#INTEGER_VALUE_TYPE}</dt>
	 * <dd>{@link Integer}</dd>
	 * <dt>{@value Directory#BOOLEAN_VALUE_TYPE}</dt>
	 * <dd>{@link Boolean}</dd>
	 * <dt>{@value Directory#FLOAT_VALUE_TYPE}</dt>
	 * <dd>{@link Double}</dd>
	 * </dl>
	 * @see NameValuePair
	 * @see Directory#URI_VALUE_TYPE
	 * @see URI
	 * @see Directory#TEXT_VALUE_TYPE
	 * @see LocaledText
	 * @see Directory#DATE_VALUE_TYPE
	 * @see Directory#TIME_VALUE_TYPE
	 * @see Directory#DATE_TIME_VALUE_TYPE
	 * @see Date
	 * @see Directory#INTEGER_VALUE_TYPE
	 * @see Integer
	 * @see Directory#BOOLEAN_VALUE_TYPE
	 * @see Boolean
	 * @see Directory#FLOAT_VALUE_TYPE
	 * @see Double
	 */
	public Object[] createValues(final String profile, final String group, final String name, final List<NameValuePair<String, String>> paramList,
			final String valueType, final Reader reader) throws IOException, ParseIOException
	{
		if(TEXT_VALUE_TYPE.equalsIgnoreCase(valueType)) //text
		{
			return processTextValueList(reader, paramList); //process the text value
		}
		else if(URI_VALUE_TYPE.equalsIgnoreCase(valueType)) //uri
		{
			return new Object[] { processURIValue(reader) }; //process the URI value type			
		}
		else if(DATE_VALUE_TYPE.equalsIgnoreCase(valueType)) //date
		{
			return new Object[] { ISODate.valueOfLiberal(reach(reader, CR)) };
		}
		else if(TIME_VALUE_TYPE.equalsIgnoreCase(valueType)) //time
		{
			return new Object[] { ISOTime.valueOf(reach(reader, CR)) };
		}
		else if(DATE_TIME_VALUE_TYPE.equalsIgnoreCase(valueType)) //date-time
		{
			return new Object[] { ISODateTime.valueOfLiberal(reach(reader, CR)) };
		}
		return null; //show that we can't create a value
	}

	/**
	 * Processes a text value.
	 * <p>
	 * The sequence "\n" or "\N" will be converted to a single newline character, '\n'.
	 * </p>
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * @param reader The reader that contains the lines of the directory.
	 * @param paramList The list of parameters; a <code>null</code> value indicates that the name/value pair contained only a name.
	 * @return An array of locale text objects representing the values.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 */
	public static LocaledText[] processTextValueList(final Reader reader, final List<NameValuePair<String, String>> paramList) throws IOException,
			ParseIOException
	{
		final Locale locale = getLanguageParamValue(paramList); //get the language, if any
		final List<LocaledText> localeTextList = new ArrayList<LocaledText>(); //create a new list to hold the locale text objects we find
		char delimiter; //we'll store the last delimiter peeked		
		do
		{
			final String string = processTextValue(reader, paramList); //read a string
			//		TODO del Log.trace("read text string: ", string);	//TODO del
			localeTextList.add(new LocaledText(string, locale)); //add the text to our list			
			delimiter = peek(reader); //see what character is next
			//		TODO del Log.trace("next delimiter: ", delimiter);	//TODO del			
		}
		while(delimiter == VALUE_SEPARATOR_CHAR); //keep getting strings while we are still running into value separators
		return localeTextList.toArray(new LocaledText[localeTextList.size()]); //convert the list of locale text objects to an array and return the array
	}

	/** The delimiters that can divide a text value: '\\' ',' and CR. */
	protected final static Characters TEXT_VALUE_DELIMITER_CHARACTERS = new Characters(TEXT_ESCAPE_CHAR, VALUE_SEPARATOR_CHAR, CR);

	/**
	 * Processes a single text value.
	 * <p>
	 * The sequence "\n" or "\N" will be converted to a single newline character, '\n', and '\\' and ',' must be escaped with '\\'. This implementation also
	 * recognizes the non-standard escape sequence "\;" and converts it to ';'. Some producers (e.g. Nokia) may use this escape sequence because it appears in
	 * standard VCard structured types.
	 * </p>
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * @param reader The reader that contains the lines of the directory.
	 * @param paramList The list of parameters; a <code>null</code> value indicates that the name/value pair contained only a name.
	 * @return An array of strings representing the values.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 */
	public static String processTextValue(Reader reader, final List<NameValuePair<String, String>> paramList) throws IOException, ParseIOException
	{
		Characters delimiters = TEXT_VALUE_DELIMITER_CHARACTERS; //we'll start out assuming the normal delimiters
		//check for the non-standard base64 encoding used by producers such as Nokia
		final String encoding = getParamValue(paramList, ENCODING_PARAM_NAME); //see if an encoding is indicated
		if(B_ENCODING_TYPE.equalsIgnoreCase(encoding) || BASE64_ENCODING_TYPE.equalsIgnoreCase(encoding)) //if the text is encoded as binary (Nokia does this, for unknown reasons)
		{
			final String base64String = reach(reader, CR); //read the text TODO could some implementations have multiple encoded values, separated by commas?
			final byte[] bytes = Base64.decode(base64String); //decode the text into bytes
			final String encodedString = new String(bytes, UTF_8_CHARSET); //hope that the bytes represent UTF-8; using base64 for text is non-standard and undocumented
			reader = new StringReader(encodedString + CR); //replace the reader with the new, decoded content, adding a CR to mimic the original data
			delimiters = new Characters(CR); //because the text was Base64-encoded, it won't separate values using commas or escape characters---that was the purpose of the Base64 encoding
		}
		final StringBuilder stringBuilder = new StringBuilder(); //create a string builder to hold whatever string we're processing
		char delimiter; //we'll store the last delimiter peeked		
		do
		{
			//		TODO del Log.trace("string buffer so far: ", stringBuffer);	//TODO del			
			stringBuilder.append(reach(reader, delimiters)); //read all undelimited characters until we find a delimiter
			delimiter = peek(reader); //see what the delimiter will be
			switch(delimiter)
			//see which delimiter we found
			{
				case TEXT_ESCAPE_CHAR: //if this is an escape character ('\\')
				{
					reader.skip(1); //skip the delimiter
					final char escapedChar = readCharacter(reader); //read the character after the escape character
					switch(escapedChar)
					//see what character comes after this one
					{
						case TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR: //"\n"
						case TEXT_LINE_BREAK_ESCAPED_UPPERCASE_CHAR: //"\N"
							stringBuilder.append('\n'); //append a single newline character
							break;
						case TEXT_ESCAPE_CHAR:
						case VALUE_SEPARATOR_CHAR:
						case ';': //accept "\;" from VCard structured types, even though it is not mentioned in RFC 2425
							stringBuilder.append(escapedChar); //escaped backslashes and commas get appended normally
							break;
						default: //if something else was escaped, we don't recognize it
							throw new ParseUnexpectedDataException(new Characters('\\', ',', TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR, TEXT_LINE_BREAK_ESCAPED_UPPERCASE_CHAR),
									escapedChar, reader); //show that we didn't expect this character here				
					}
				}
					break;
				case VALUE_SEPARATOR_CHAR: //if this is the character separating multiple values (',')
				case CR: //if we just peeked a carriage return
					break; //don't do anything---we'll just collect our characters and leave
				default: //if we peeked anything else (there shouldn't be anything else unless there is a logic error)
					throw new AssertionError("The only possible values should have been " + TEXT_VALUE_DELIMITER_CHARACTERS + "; found " + Characters.getLabel(delimiter));
			}
		}
		while(delimiter != VALUE_SEPARATOR_CHAR && delimiter != CR); //keep collecting parts of the string until we encounter a ',' or a CR
		//TODO check the text value
		//	TODO del Log.trace("returning string: ", stringBuffer);	//TODO del			
		return stringBuilder.toString(); //return the string we've collected so far
	}

	/**
	 * Processes the value for the URI value types.
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * @param reader The reader that contains the lines of the directory.
	 * @return A URI object representing the value.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 */
	public static URI processURIValue(final Reader reader) throws IOException, ParseIOException
	{
		final String uriString = reach(reader, CR); //read the string representing the URI
		try
		{
			return new URI(uriString); //create a URI
		}
		catch(URISyntaxException uriSyntaxException) //if the URI was not syntactically correct
		{
			throw new ParseIOException(reader, uriSyntaxException);
		}
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This method knows how to serialized predefined types, which, along with the objects expected, are as follows:
	 * </p>
	 * <dl>
	 * <dt>{@value Directory#URI_VALUE_TYPE}</dt>
	 * <dd>{@link URI}</dd>
	 * <dt>{@value Directory#TEXT_VALUE_TYPE}</dt>
	 * <dd>{@link LocaledText}</dd>
	 * <dt>{@value Directory#DATE_VALUE_TYPE}</dt>
	 * <dd>{@link Date}</dd>
	 * <dt>{@value Directory#TIME_VALUE_TYPE}E</dt>
	 * <dd>{@link Date}</dd>
	 * <dt>{@value Directory#DATE_TIME_VALUE_TYPE}</dt>
	 * <dd>{@link Date}</dd>
	 * <dt>{@value Directory#INTEGER_VALUE_TYPE}</dt>
	 * <dd>{@link Integer}</dd>
	 * <dt>{@value Directory#BOOLEAN_VALUE_TYPE}</dt>
	 * <dd>{@link Boolean}</dd>
	 * <dt>{@value Directory#FLOAT_VALUE_TYPE}</dt>
	 * <dd>{@link Double}</dd>
	 * </dl>
	 */
	public boolean serializeValue(final String profile, final String group, final String name, final List<NameValuePair<String, String>> paramList,
			final Object value, final String valueType, final Writer writer) throws IOException
	{
		if(TEXT_VALUE_TYPE.equalsIgnoreCase(valueType)) //text
		{
			serializeTextValue(((LocaledText)value).getText(), writer); //serialize the text
			return true; //show that we serialized the value 
		}
		else if(URI_VALUE_TYPE.equalsIgnoreCase(valueType)) //uri
		{
			writer.write(((URI)value).toString()); //write the URI
			return true; //show that we serialized the value 
		}
		return false; //show that we can't serialize the value
	}

	/**
	 * Serializes a text value.
	 * <p>
	 * CR, LF, and CRLF will be be converted to "\\n"; and '\\' and ',' will be escaped with '\\'.
	 * </p>
	 * @param text The text value to serialize.
	 * @param writer The writer to which the directory information should be written.
	 * @exception IOException Thrown if there is an error writing to the directory.
	 */
	public void serializeTextValue(final String text, final Writer writer) throws IOException
	{
		writer.write(encodeTextValue(text)); //write the encoded string
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This version creates a basic <code>Directory</code> object.
	 * </p>
	 */
	public Directory createDirectory(final ContentLine[] contentLines)
	{
		final Directory directory = new Directory(); //create a basic directory object
		for(int i = 0; i < contentLines.length; ++i) //look at each content line
		{
			final ContentLine contentLine = contentLines[i]; //get a reference to this content line
			final String typeName = contentLine.getName(); //get this content line's type name
			if(NAME_TYPE.equalsIgnoreCase(typeName)) //if this is NAME
			{
				if(directory.getDisplayName() == null) //if the directory does not yet have a display name
				{
					directory.setDisplayName((LocaledText)contentLine.getValue()); //set the directory display name
					continue; //don't process this content line further
				}
			}
			//if we make it to here, we either don't recognize the content line
			//	or we can't process it (e.g. a duplicate value we don't support)
			directory.getContentLineList().add(contentLine); //add this unprocessed content line to the directory's list of content lines
		}
		return directory; //return the directory we created
	}

}
