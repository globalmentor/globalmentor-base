package com.garretwilson.text.directory;

import java.io.*;
import java.util.*;
import java.net.MalformedURLException;
import java.net.URL;
//G***del if we don't need import java.text.MessageFormat;
import org.w3c.dom.DOMException;
import com.garretwilson.io.*;
import com.garretwilson.net.URLUtilities;
import com.garretwilson.text.CharacterEncoding;
import com.garretwilson.text.xml.schema.*;
//G***del import com.garretwilson.util.StringManipulator;
import com.garretwilson.util.*;

/**Class that can process a directory of type <code>text/directory</code> as
	defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
<p>The processor knows how to process
	the standard directory value types: <code>URI_VALUE_TYPE</code>,
	<code>TEXT_VALUE_TYPE</code>, <code>DATE_VALUE_TYPE</code>,
	<code>TIME_VALUE_TYPE</code>, <li><code>DATE_TIME_VALUE_TYPE</code>,
	<code>INTEGER_VALUE_TYPE</code>, <code>BOOLEAN_VALUE_TYPE</code>,
	and <code>FLOAT_VALUE_TYPE</code>.</p>
@author Garret Wilson
@see ValueFactory
@see URI_VALUE_TYPE
@see TEXT_VALUE_TYPE
@see DATE_VALUE_TYPE
@see TIME_VALUE_TYPE
@see DATE_TIME_VALUE_TYPE
@see INTEGER_VALUE_TYPE
@see BOOLEAN_VALUE_TYPE
@see FLOAT_VALUE_TYPE
*/
public class DirectoryProcessor implements DirectoryConstants, ValueFactory
{
	
	/**A map of value factories keyed to the lowercase version of the value type.*/
	private final Map valueTypeValueFactoryMap=new HashMap();	

		/**Registers a value factory by value type.
		@param valueType The value type for which this value factory can create values.
		@param valueFactory The value factory to be registered with this value type.
		*/	
		public void registerValueFactoryByValueType(final String valueType, final ValueFactory valueFactory)
		{
			valueTypeValueFactoryMap.put(valueType.toLowerCase(), valueFactory);	//put the value factory in the map, keyed to the lowercase version of the type
		}
		
		/**Retrieves a value factory to create values for the given value type.
		@param valueType The value type for which a value factory should be returned.
		@return A value factory for this value type, or <code>null</code> if there
			is no value factory registered for this value type.
		*/ 
		protected ValueFactory getValueFactoryByValueType(final String valueType)
		{
			return (ValueFactory)valueTypeValueFactoryMap.get(valueType.toLowerCase());	//get the value factory keyed to the lowercase version of this value type
		}
		
	/**A map of value factories keyed to the lowercase version of the profile.*/
	private final Map profileValueFactoryMap=new HashMap();	

		/**Registers a value factory by profile.
		@param profile The profile for which this value factory can create values.
		@param valueFactory The value factory to be registered with this profile.
		*/	
		public void registerValueFactoryByProfile(final String profile, final ValueFactory valueFactory)
		{
			profileValueFactoryMap.put(profile.toLowerCase(), valueFactory);	//put the value factory in the map, keyed to the lowercase version of the profile
		}

		/**Retrieves a value factory to create values for the given value profile.
		@param profile The profile for which a value factory should be returned.
		@return A value factory for this profile, or <code>null</code> if there
			is no value factory registered for this profile.
		*/ 
		protected ValueFactory getValueFactoryByProfile(final String profile)
		{
			return (ValueFactory)profileValueFactoryMap.get(profile.toLowerCase());	//get the value factory keyed to the lowercase version of this profile
		}
		
	/**Default constructor.
	This class automatically registers itself with itself as a value factory for
		the standard value types.*/
	public DirectoryProcessor()
	{
		registerValueFactoryByValueType(URI_VALUE_TYPE, this);	//register ourselves as a value factory for the standard value types		
		registerValueFactoryByValueType(TEXT_VALUE_TYPE, this);		
		registerValueFactoryByValueType(DATE_VALUE_TYPE, this);		
		registerValueFactoryByValueType(TIME_VALUE_TYPE, this);		
		registerValueFactoryByValueType(DATE_TIME_VALUE_TYPE, this);		
		registerValueFactoryByValueType(INTEGER_VALUE_TYPE, this);		
		registerValueFactoryByValueType(BOOLEAN_VALUE_TYPE, this);		
		registerValueFactoryByValueType(FLOAT_VALUE_TYPE, this);		
	}

	/**The delimiter characters separating the main components of a content line
		with no group provided (';', ':', CR, and LF).
	*/
	protected final static String GROUPLESS_CONTENT_LINE_DELIMITER_CHARS=""+PARAM_SEPARATOR_CHAR+NAME_VALUE_SEPARATOR_CHAR+CR+LF;	

	/**The delimiter characters separating the main components of a content line
		('.', ';', ':', CR, and LF).
	*/
	protected final static String CONTENT_LINE_DELIMITER_CHARS=GROUP_NAME_SEPARATOR_CHAR+GROUPLESS_CONTENT_LINE_DELIMITER_CHARS;	

	/**Retrieves a line of content from a directory.
	@param reader The reader that contains the lines of the directory.
	@return A line of content from the directory
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public ContentLine processDirectoryLine(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		String group=null;	//we'll store the group here
		String name=null;	//we'll store the name here
		List paramList=null;	//we'll store parameters here, if we have any
		String token=reader.readStringUntilCharEOF(CONTENT_LINE_DELIMITER_CHARS);	//read the next line token
		char c=reader.readChar();	//get the delimiter character we encountered
		if(c==GROUP_NAME_SEPARATOR_CHAR)	//if we just read a group
		{
			//G***check the syntax of the group
			group=token;	//save the group we read
			token=reader.readStringUntilCharEOF(GROUPLESS_CONTENT_LINE_DELIMITER_CHARS);	//read the next line token after the group, which should be the name
			c=reader.readChar();	//get the delimiter character we encountered, and fall through to checking the name
		}
		switch(c)	//see which delimiter character we encountered
		{
			case PARAM_SEPARATOR_CHAR:	//if we just read a parameter separator
			case NAME_VALUE_SEPARATOR_CHAR:	//if we just read the character separates the name from the value
				//G***check the name
				name=token;	//this is the name
				if(c==PARAM_SEPARATOR_CHAR)	//if this was the character separating the name from parameters, read the parameters
				{
					paramList=processParameters(reader);	//process the parameters, consuming the ending delimiter
					reader.readExpectedChar(NAME_VALUE_SEPARATOR_CHAR);	//read the ':' that we expect to come after the parameters
				}
				else	//if there were no parameters
				{
					paramList=new ArrayList();	//create an empty list, since we didn't read any parameters
				}
				final String valueString=reader.readStringUntilChar(CR);	//everything before the carriage return will constitute the value
					//G***check the value
				final Object value=processValue(group, name, paramList, reader);	//process the value and get an object that represents the object
				//G***read the CRLF that's left
				//G***decide whether to require CRLF
				return null; //G***create and return the directory line


//G***this isn't right---we shouldn't even check for a CR or LF at this point
			case CR:	//if we just read a carriage return G***maybe later not even check for CR and LF here
/*G***don't allow blank lines until we find out differently
				reader.readExpectedChar(LF);	//there should always be an LF after a CR
				if(token.trim().length()>0)	//if there is content before the CRLF, but none of the other delimiters we expect, there's a syntax error in the line
					return new ParseIOException("")
				return null;	//G***decide what to do with an empty line
*/
			case LF:	//if we see an LF before a CR
			default:	//if we read anything else (there shouldn't be anything else unless there is a logic error)					
				throw new ParseUnexpectedDataException(""+PARAM_SEPARATOR_CHAR+NAME_VALUE_SEPARATOR_CHAR, c, reader);	//show that we didn't expect this character here
		}
	}

	/**When reading the parameter name, we expect either a parameter name/value
		separator ('=') or the line name/value separator (':'), indicating we've
		finished parameters.
	*/ 
//G***del	protected final static String PARAM_NAME_DELIMITER_CHARS=""+PARAM_NAME_VALUE_SEPARATOR_CHAR+NAME_VALUE_SEPARATOR_CHAR;

	/**After reading the parameter value, we expect either a parameter separator
		(';') the parameter value separator (',') indicating more values, or the
		line name/value separator (':'), indicating we've finished parameters.
	*/ 
	protected final static String PARAM_VALUE_DELIMITER_CHARS=""+PARAM_SEPARATOR_CHAR+PARAM_VALUE_SEPARATOR_CHAR+NAME_VALUE_SEPARATOR_CHAR;

	/**Retrieves parameters from a line of content from a directory.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@param The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>Object</code>.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	@see NameValuePair
	*/
	public List processParameters(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final List paramList=new ArrayList();	//create a list of parameters
		char nextCharacter;	//we'll store the last peeked delimiter here each time in the loop
		do	//read each parameter
		{
					//read the parameter name
			final String paramName=reader.readStringUntilChar(PARAM_NAME_VALUE_SEPARATOR_CHAR);	//get the parameter name, which is everything up to the ':' characters
			//G***check the param name for validity
			final List paramValueList=new ArrayList();	//create a list to hold the parameter values
			do	//read the parameter value(s)
			{
				reader.skip(1);	//skip the delimiter that got us here
				final String paramValue;	//we'll read the value and store it here
				switch(reader.peekChar())	//see what character is first in the value
				{
					case DQUOTE:	//if the string starts with a quote
						paramValue=reader.readDelimitedString(DQUOTE, DQUOTE);	//read the value within the quotes 
						break;
					default:	//if the string doesn't end with a quote
						paramValue=reader.readStringUntilChar(PARAM_VALUE_DELIMITER_CHARS);	//read everything until the end of this parameter
						break;
				}
				//G***check the parameter value, here
				paramList.add(new NameValuePair(paramName, paramValue));	//add this name/value pair to our list of parameters
				nextCharacter=reader.peekChar();	//see what delimiter will come next
			}
			while(nextCharacter==PARAM_VALUE_SEPARATOR_CHAR);	//keep getting parameter values while there are more parameter value separators
		}
		while(nextCharacter!=NAME_VALUE_SEPARATOR_CHAR);	//keep reading parameters until we get to the '=' that separates the name from the value
		reader.resetPeek();	//reset peeking, since we've been peeking
		return paramList;	//return the list of parameters we filled
	}

	/**Processes the textual representation of a line's value and returns
		one or more object representing the value, as some value types
		support multiple values.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	<p>When attempting to find a <code>ValueFactory</code> to process a given
		value, an attempt is made to locate a value factory based upon, in this
		order:</p>
	<ul>
		<li>value type; if there is no value type, or no value was created, then</li>
		<li>profile</li>
	</ul> 
	<p>If the value cannot be created using a <code>ViewFactory</code>, the
		value is converted to a single string.</p>
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>Object</code>.
	@param reader The reader that contains the lines of the directory.
	@return An array of objects represent the value string.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	@see NameValuePair
	*/
	protected Object[] processValue(final String group, final String name, final List paramList, final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		String valueType=null;	//start out not knowing what the value type will be
		final Iterator paramIterator=paramList.iterator();	//get an iterator to the parameters
		while(paramIterator.hasNext())	//while there are more parameters
		{
			final NameValuePair parameter=(NameValuePair)paramIterator.next();	//get the next parameter name/value pair
			if(VALUE_PARAM_NAME.equals(parameter.getName()))	//if this is the "value" parameter
			{
				valueType=(String)parameter.getValue();	//get the value type
				break;	//stop looking for a value type
			}
		}
		Object[] objects=null;	//start out by assuming we can't process the value
		final ValueFactory valueTypeValueFactory=getValueFactoryByValueType(valueType);	//see if we have a value factory registered with this value type
		if(valueTypeValueFactory!=null)	//if there is a value factory for this value type
		{
			objects=valueTypeValueFactory.createValue(group, name, paramList, valueType, reader);	//create objects for this value type
		}
		if(objects==null)	//if no objects were created
		{
/*G***fix looking up by profile			
			final ValueFactory profileValueFactory=getValueFactoryByProfile(profile);	//see if we have a value factory registered with this profile
			if(profileValueFactory!=null)	//if there is a value factory for this profile
			{
				objects=profileValueFactory.createValue(group, name, paramList, valueType, reader);	//create objects for this profile
			}
*/
		}
		if(objects==null)	//if no objects were created
		{
			final String valueString=reader.readStringUntilChar(CR);	//everything before the carriage return will constitute the value
			objects=new String[]{valueString};	//put the single value string in an array of strings and use that for the value objects
		}
		return objects;	//return the value objects we processed
	}

	/**Processes the textual representation of a line's value and returns
		one or more object representing the value, as some value types
		support multiple values.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	<p>This method knows how to create predefined types, which,
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
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>Object</code>.
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
	@see String
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
	public Object[] createValue(final String group, final String name, final List paramList, final String valueType, final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		if(TEXT_VALUE_TYPE.equalsIgnoreCase(valueType))	//if this is the "text" value type
		{
			return processTextValueList(reader);	//process the text value
		}
		return null;	//show that we can't create a value
	}

	/**Processes a text value.
	<p>The sequence "\n" or "\N" will be converted to a single newline character,
		'\n'.</p>
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@return An array of strings representing the values.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	protected String[] processTextValueList(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final List stringList=new ArrayList();	//create a new list to hold the strings we find
		char delimiter;	//we'll store the last delimiter peeked		
		do
		{
			reader.resetPeek();	//reset peeking
			final String string=processTextValue(reader);	//read a string
			delimiter=reader.peekChar();	//see what character is next
		}
		while(delimiter==VALUE_SEPARATOR_CHAR);	//keep getting strings while we are still running into value separators
		reader.resetPeek();	//reset peeking
		return (String[])stringList.toArray(new String[stringList.size()]);	//convert the list of strings to an array of strings and return the array
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
	protected String processTextValue(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final StringBuffer stringBuffer=new StringBuffer();	//create a string buffer to hold whatever string we're processing
		char delimiter;	//we'll store the last delimiter peeked		
		do	
		{		
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
		return stringBuffer.toString();	//return the string we've collected so far
	}

}
