package com.garretwilson.text.directory.vcard;

import java.io.*;
import java.util.*;
import java.net.MalformedURLException;
import java.net.URL;
//G***del if we don't need import java.text.MessageFormat;
import org.w3c.dom.DOMException;
import com.garretwilson.io.*;
import com.garretwilson.net.URLUtilities;
import com.garretwilson.text.CharacterEncoding;
import com.garretwilson.text.directory.*;
import com.garretwilson.text.xml.schema.*;
//G***del import com.garretwilson.util.StringManipulator;
import com.garretwilson.util.*;

/**Class that can create values for the "VCARD" profile of a
	<code>text/directory</code>as defined in
	 <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
TODO update comments: <p>The processor knows how to process
	the standard directory value types: <code>URI_VALUE_TYPE</code>,
	<code>TEXT_VALUE_TYPE</code>, <code>DATE_VALUE_TYPE</code>,
	<code>TIME_VALUE_TYPE</code>, <li><code>DATE_TIME_VALUE_TYPE</code>,
	<code>INTEGER_VALUE_TYPE</code>, <code>BOOLEAN_VALUE_TYPE</code>,
	and <code>FLOAT_VALUE_TYPE</code>.</p>
@author Garret Wilson
@see ValueFactory
*/
public class VCardProfile extends AbstractProfile implements DirectoryConstants, VCardConstants, ValueFactory
{
	
	/**Default constructor.*/
	public VCardProfile()
	{
					//identification types
		registerValueType(FN_TYPE, TEXT_VALUE_TYPE);	//FN: text
		registerValueType(N_TYPE, null);	//N: structured text
		registerValueType(NICKNAME_TYPE, TEXT_VALUE_TYPE);	//NICKNAME: text
		registerValueType(PHOTO_TYPE, BINARY_VALUE_TYPE);	//PHOTO: binary
		registerValueType(BDAY_TYPE, DATE_VALUE_TYPE);	//BDAY: date
					//delivery addressing types
		registerValueType(ADR_TYPE, null);	//BDAY: structured text
		registerValueType(LABEL_TYPE, TEXT_VALUE_TYPE);	//LABEL: text
					//telecommunications addressing types
		registerValueType(TEL_TYPE, PHONE_NUMBER_VALUE_TYPE);	//TEL: phone-number
		registerValueType(EMAIL_TYPE, TEXT_VALUE_TYPE);	//EMAIL: text
		registerValueType(MAILER_TYPE, TEXT_VALUE_TYPE);	//MAILER: text
					//geographical types
		registerValueType(TZ_TYPE, UTC_OFFSET_VALUE_TYPE);	//TZ: utc-offset
		registerValueType(GEO_TYPE, null);	//GEO: two floats		
					//organizational types
		registerValueType(TITLE_TYPE, TEXT_VALUE_TYPE);	//TITLE: text
		registerValueType(ROLE_TYPE, TEXT_VALUE_TYPE);	//ROLE: text
		registerValueType(LOGO_TYPE, BINARY_VALUE_TYPE);	//LOGO: binary
		registerValueType(AGENT_TYPE, VCARD_VALUE_TYPE);	//AGENT: vcard
		registerValueType(ORG_TYPE, null);	//ORG: structured text
					//explanatory types
		registerValueType(CATEGORIES_TYPE, TEXT_VALUE_TYPE);	//CATEGORIES: text
		registerValueType(NOTE_TYPE, TEXT_VALUE_TYPE);	//NOTE: text
		registerValueType(PRODID_TYPE, TEXT_VALUE_TYPE);	//PRODID: text
		registerValueType(REV_TYPE, DATE_TIME_VALUE_TYPE);	//REV: date-time
		registerValueType(SORT_STRING_TYPE, TEXT_VALUE_TYPE);	//SORT-STRING: text
		registerValueType(SOUND_TYPE, BINARY_VALUE_TYPE);	//SOUND: binary
		registerValueType(UID_TYPE, TEXT_VALUE_TYPE);	//UID: text
		registerValueType(URL_TYPE, TEXT_VALUE_TYPE);	//URL: uri
		registerValueType(VERSION_TYPE, TEXT_VALUE_TYPE);	//VERSION: text
					//security types
		registerValueType(CLASS_TYPE, TEXT_VALUE_TYPE);	//CLASS: text
		registerValueType(KEY_TYPE, BINARY_VALUE_TYPE);	//KEY: binary
	}

	/**Processes the textual representation of a line's value and returns
		one or more object representing the value, as some value types
		support multiple values.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	TODO update comments: <p>This method knows how to create predefined types, which,
		along with the objects returned, are as follows:</p>
	<ul>
		<li><code>URI_VALUE_TYPE</code> <code>URI</code></li>
		<li><code>TEXT_VALUE_TYPE</code> <code>String</code></li>
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
	*/
	public Object[] createValues(final String profile, final String group, final String name, final List paramList, final String valueType, final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
			//create the structured types
		if(N_TYPE.equalsIgnoreCase(name))	//N
		{
			final Name temp=processNValue(reader);
			Debug.trace("parsed name: ", temp);
			return new Object[]{temp};	//process the N value
//G***bring back			return new Object[]{processNValue(reader)};	//process the N value
		}
		return null;	//show that we can't create a value
	}
	
	/**Processes the value for the <code>N</code> type name.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@return An array of strings representing the values.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static Name processNValue(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final String[][] fields=processStructuredTextValue(reader);	//process the structured text fields
		final String[] familyNames=fields.length>0 ? fields[0] : new String[]{};	//get the family names, if present
		final String[] givenNames=fields.length>1 ? fields[1] : new String[]{};	//get the given names, if present
		final String[] additionalNames=fields.length>2 ? fields[2] : new String[]{};	//get the additional names, if present
		final String[] honorificPrefixes=fields.length>3 ? fields[3] : new String[]{};	//get the honorific prefixes, if present
		final String[] honorificSuffixes=fields.length>4 ? fields[4] : new String[]{};	//get the honorific suffixes, if present
		return new Name(familyNames, givenNames, additionalNames, honorificPrefixes, honorificSuffixes);	//create and return a vCard name object with the parsed information
	}

	/**Processes structured text into an array of string arrays.
	Structured text is series of fields separated by ';', each field of which
		can have multiple values separated by ','.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@return An array of string arrays, each string array representing the values
		of each field.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static String[][] processStructuredTextValue(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final List fieldList=new ArrayList();	//create a new list to hold the string arrays we find
		char delimiter;	//we'll store the last delimiter peeked		
		do
		{
			reader.resetPeek();	//reset peeking
			final String[] values=processStructuredTextFieldValue(reader);	//process this field
			fieldList.add(values);	//add the strings of the field to our list			
			delimiter=reader.peekChar();	//see what character is next
			if(delimiter!=CR)	//if we haven't reached the end of the value
				reader.skip(1);		//skip the delimiter
		}
		while(delimiter==STRUCTURED_TEXT_VALUE_DELIMITER);	//keep getting fields while we are still running into structured text value separators
		reader.resetPeek();	//reset peeking
		return (String[][])fieldList.toArray(new String[fieldList.size()][]);	//convert the list of string arrays to an array of string arrays and return the array
	}

	/**The delimiters that can divide a structured text value: ';' ',' and CR.*/
	protected final static String STRUCTURED_TEXT_VALUE_DELIMITER_CHARS=""+STRUCTURED_TEXT_VALUE_DELIMITER+VALUE_SEPARATOR_CHAR+CR; 

	/**Processes a single field of a structured text value.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@return An array of strings representing the values of the field.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static String[] processStructuredTextFieldValue(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final List valueList=new ArrayList();	//create a new list to hold the values we find
		char delimiter;	//we'll store the last delimiter peeked		
		do	
		{
			valueList.add(reader.readStringUntilChar(STRUCTURED_TEXT_VALUE_DELIMITER_CHARS));	//read all value characters until we find a delimiter, and add that value to the list
			delimiter=reader.peekChar();	//see what the delimiter will be
			switch(delimiter)	//see which delimiter we found
			{
				case VALUE_SEPARATOR_CHAR:	//if this is the character separating multiple values (',')
					reader.skip(1);	//skip the delimiter
					break;				
				case STRUCTURED_TEXT_VALUE_DELIMITER:	//if this is the character separating fields in the structured text value (';')
				case CR:	//if we just read a carriage return
					break;	//don't do anything---we'll just collect our characters and leave
				default:	//if we read anything else (there shouldn't be anything else unless there is a logic error)					
					throw new ParseUnexpectedDataException(STRUCTURED_TEXT_VALUE_DELIMITER_CHARS, delimiter, reader);	//show that we didn't expect this character here
			}
		}
		while(delimiter!=STRUCTURED_TEXT_VALUE_DELIMITER && delimiter!=CR);	//keep collecting parts of the string until we encounter a ';' or a CR
		reader.resetPeek();	//reset peeking
		return (String[])valueList.toArray(new String[valueList.size()]);	//convert the list of strings to an array of strings return the array
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

}
