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
<p>This processor makes the following decisions for ambiguities in the
	specification:</p>
<ul>
	<li>Every content line, including the last, is required to end in CRLF.</li>
	<li>Lines containing only whitespace are ignored.</li>
</ul>
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
	
	/**A map of value type strings keyed to supported type name strings.*/
	private final Map typeNameValueTypeMap=new HashMap();

		/**Registers a value type keyed to the lowercase version of a type name.
		@param typeName The type name for which a value type should be retrieved.
		@param valueType The value type to associate with this type name.
		*/
		protected void registerValueType(final String typeName, final String valueType)
		{
			typeNameValueTypeMap.put(typeName.toLowerCase(), valueType);	//put the value type in the map, keyed to the lowercase version of the type name		
		}

		/**Returns a value type keyed to the lowercase version of a type name.
		@param typeName The type name for which a value type should be associated.
		@return The value type associated with this type name, or
			<code>null</code> if no value type has been registered with the type name.
		*/
		protected String getValueType(final String typeName)
		{
			return (String)typeNameValueTypeMap.get(typeName.toLowerCase());	//get whatever value type we have associated with this type name, if any
		}

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

	/**The profile last encountered in a "profile:" type content line.*/
	private String defaultProfile=null;

	/**Whether the default profile was the last profile encountered.*/
	private boolean useDefaultProfile=false;
	
	/**The stack of profiles encountered in a "begin:"/"end:" blocks;
		created before processing and released afterwards.
	*/
	private LinkedList profileStack=null;
		
		/**Sets the profile to be used for subsequent content lines.
		If in the middle of a profile "begin:"/"end:" block, the profile of that
			block will be suspended until the block ends or another block begins.
		@param profile The new profile of the directory.
		*/
		protected void setProfile(final String profile)
		{
			defaultProfile=profile;	//save the profile
			useDefaultProfile=true;	//show that we should use the default profile
		}
		
		/**@return The current profile, either the last set profile, the profile of
			the current "begin:"/"end:" block, or <code>null</code> if there is no
			profile, in that order.
		@return String
		*/
		protected String getProfile()
		{
			if(useDefaultProfile && defaultProfile!=null)	//if we should use the default profile and there is a profile set
			{
				return defaultProfile;	//return the last set profile
			}
			else if(profileStack.size()>0)	//if we're in a profile "begin:"/"end:" block
			{
				return (String)profileStack.getLast();	//return the profile of the current block
			}
			else	//if no profile is set, and we're not in a profile "begin:"/"end:" block
			{
				return defaultProfile;	//if there's no profile "begin:"/"end:" block, we'll have to use the default profile, even if it is null
			}
		}
		
		/**Pushes the given profile on the stack, and removes the set profile, if any.
		Suspends the currently set profile, if any.
		@param profile The profile of the new "begin:"/"end:" block block. 
		*/
		protected void pushProfile(final String profile)
		{
			profileStack.addLast(profile);	//push the profile onto the stack
			useDefaultProfile=false;	//suspend use of the default profile
		}
		
		/**Removes the profile from the top of the stack.
		Suspends the currently set profile, if any.
		@return The profile from the top of the stack.
		@exception NoSuchElementException Thrown if there are no more profiles on
			the stack.
		*/
		protected String popProfile()
		{
			useDefaultProfile=false;	//suspend use of the default profile
			return (String)profileStack.removeLast();	//pop the profile from the stack
		}

	/**Default constructor.
	This class automatically registers itself with itself as a value factory for
		the standard value types.*/
	public DirectoryProcessor()
	{
			//register the predefined types in our map
		registerValueType(SOURCE_TYPE, URI_VALUE_TYPE);	//SOURCE: uri		
		registerValueType(NAME_TYPE, TEXT_VALUE_TYPE);	//NAME: text		
		registerValueType(PROFILE_TYPE, TEXT_VALUE_TYPE);	//PROFILE: text		
		registerValueType(BEGIN_TYPE, TEXT_VALUE_TYPE);	//BEGIN: text		
		registerValueType(END_TYPE, TEXT_VALUE_TYPE);	//END: text		
			//register ourselves as a value factory for the standard value types
		registerValueFactoryByValueType(URI_VALUE_TYPE, this);			
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

	/**Retrieves content lines from a directory of type <code>text/directory</code>.
	@param reader The reader that contains the lines of the directory.
	@param sourceObject The source of the data (e.g. a <code>String</code>,
		<code>File</code>, <code>URL</code>, or <code>URI</code>).
	@return An object representing the directory.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public Directory processDirectory(final Reader reader, final Object sourceObject) throws IOException, ParseIOException
	{
		return processDirectory(new LineUnfoldParseReader(reader, sourceObject));	//create a new line unfold parse reader and use that to process the directory
	}

	/**Retrieves content lines from a directory of type <code>text/directory</code>.
	@param reader The reader that contains the lines of the directory.
	@return An object representing the directory.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public Directory processDirectory(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		profileStack=new LinkedList();	//create a new profile stack
		defaultProfile=null;	//show that there is no default profile
		useDefaultProfile=false;	//don't use the default profile
		final Directory directory=new Directory();	//create a new directory
		while(!reader.isEOF())	//while we haven't reached the end of the file
		{		
			final ContentLine[] contentLines=processContentLine(reader);	//process one or more lines of contents, all of which should have the same type
			if(contentLines!=null)	//if there were one or more content lines
			{
				for(int i=0; i<contentLines.length; ++i)	//look at each line of content
				{
					final ContentLine contentLine=contentLines[i];	//get a reference to this content line
//G***del Debug.trace("just processed content line: ", contentLine);	//G***del
					final String typeName=contentLine.getTypeName();	//get the type
					if(END_TYPE.equalsIgnoreCase(typeName))	//if this is END
					{
						if(directory.getName()==null)	//if the directory does not yet have a name
						{
							directory.setName((String)contentLine.getValue());	//get the directory name
						}
					}
					else if(PROFILE_TYPE.equalsIgnoreCase(typeName))	//if this is PROFILE
					{
						final String profile=(String)contentLine.getValue();	//get the profile
						contentLine.setProfile(profile);	//a profile type should have the same profile as the one it sets
						setProfile(profile);	//set the profile to the new profile
					}
					else if(BEGIN_TYPE.equalsIgnoreCase(typeName))	//if this is BEGIN:xxx
					{
						final String profile=(String)contentLine.getValue();	//get the profile
						contentLine.setProfile(profile);	//a beginning profile type should have the same profile as the one it sets
						pushProfile(profile);	//push the new profile
					}
					else if(END_TYPE.equalsIgnoreCase(typeName))	//if this is END:xxx
					{
						final String profile=(String)contentLine.getValue();	//get the profile
						contentLine.setProfile(profile);	//an ending profile type should have the same profile to which it refers
						try
						{
							final String oldProfile=popProfile();	//pop the profile from the stack
							//G***make sure the old profile is what we expect
						}
						catch(NoSuchElementException noSuchElementException)	//if there are no more profiles on the stack
						{
							throw new ParseIOException("Profile \""+profile+"\" END without BEGIN.", reader);	//throw an error indicating that there was no beginning to the profile
						}
					}
					directory.getContentLineList().add(contentLine);	//add this content line to the directory
				}
			}
		}		
		profileStack=null;	//release the profile stack
		defaultProfile=null;	//show that there is no default profile
		useDefaultProfile=false;	//don't use the default profile
		return directory;	//return the directory we processed				
	}

	/**Retrieves one or more content lines from a directory, all of which will
		have the same type name
	If the parsed content line has multiple values, a new identical content line
		will be created for to contain each value, differing only in the value.
	@param reader The reader that contains the lines of the directory.
	@return A one or more content lines from the directory, or <code>null</code>
		if the line contained only whitespace.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public ContentLine[] processContentLine(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		String profile=getProfile();	//get the current profile, if there is one
		String group=null;	//we'll store the group here
		String name=null;	//we'll store the name here
		List paramList=null;	//we'll store parameters here, if we have any
		String token=reader.readStringUntilCharEOF(CONTENT_LINE_DELIMITER_CHARS);	//read the next line token; don't throw an exception if the end of the file is reached, because this could be an empty line
		if(reader.isEOF())	//if we reached the end of the file
		{
			if(token.trim().length()>0)	//if there is non-whitespace content before the end of the line, but none of the other delimiters we expect, there's a syntax error in the line
			{
				throw new ParseEOFException(reader);	//show that we didn't expect to run out of data here
			}
			else	//if this was an empty line
			{
				return null;	//there's no content on this line			
			}
		}
		char c=reader.readChar();	//get the delimiter character we encountered
		if(c==GROUP_NAME_SEPARATOR_CHAR)	//if we just read a group
		{
			//G***check the syntax of the group
			group=token;	//save the group we read
//		G***del Debug.trace("found group: ", group);
			token=reader.readStringUntilChar(GROUPLESS_CONTENT_LINE_DELIMITER_CHARS);	//read the next line token after the group, which should be the name
			c=reader.readChar();	//get the delimiter character we encountered, and fall through to checking the name
		}
		switch(c)	//see which delimiter character we encountered
		{
			case PARAM_SEPARATOR_CHAR:	//if we just read a parameter separator
			case NAME_VALUE_SEPARATOR_CHAR:	//if we just read the character separates the name from the value
				//G***check the name
				name=token;	//this is the name
//		G***del Debug.trace("found name: ", name);
				if(c==PARAM_SEPARATOR_CHAR)	//if this was the character separating the name from parameters, read the parameters
				{
					paramList=processParameters(reader);	//process the parameters
					reader.readExpectedChar(NAME_VALUE_SEPARATOR_CHAR);	//read the ':' that we expect to come after the parameters
				}
				else	//if there were no parameters
				{
					paramList=new ArrayList();	//create an empty list, since we didn't read any parameters
				}
//		G***del Debug.trace("ready to process value");
				final Object[] values=processValue(profile, group, name, paramList, reader);	//process the value and get an object that represents the object
				reader.readExpectedString(CRLF);	//there should always be a CRLF after the value
				final ContentLine[] contentLines=new ContentLine[values.length];	//create an array of content lines that we'll fill with new content lines
				for(int i=0; i<values.length; ++i)	//look at each value
				{
					contentLines[i]=new ContentLine(profile, group, name, new ArrayList(paramList), values[i]);	//create a content line with this value, making a copy of the parameter list
				}
				return contentLines; //return the array of content lines we created and filled
			case CR:	//if we just read a carriage return
				if(token.trim().length()>0)	//if there is content before the CRLF, but none of the other delimiters we expect, there's a syntax error in the line
				{
					throw new ParseUnexpectedDataException(""+PARAM_SEPARATOR_CHAR+NAME_VALUE_SEPARATOR_CHAR, c, reader);	//show that we didn't expect this character here
				}
				reader.readExpectedChar(LF);	//there should always be an LF after a CR
				return null;	//G***decide what to do with an empty line
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
		value of type <code>String</code>.
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
//		G***del Debug.trace("found param name: ", paramName);
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
//			G***del Debug.trace("found param value: ", paramValue);
				//G***check the parameter value, here
				paramList.add(new NameValuePair(paramName, paramValue));	//add this name/value pair to our list of parameters
				nextCharacter=reader.peekChar();	//see what delimiter will come next
			}
			while(nextCharacter==PARAM_VALUE_SEPARATOR_CHAR);	//keep getting parameter values while there are more parameter value separators
			if(nextCharacter==PARAM_SEPARATOR_CHAR)	//if the next character is the character that separates multiple parameters
			{
				reader.skip(1);	//skip the parameter separator
			}
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
		value, an attempt is made to locate a value factory based in this order:</p>
	<ol>
		<li>If no explicit value type is given and a profile is known, the
			<code>ValueFactory</code> registered for that profile, if any, is asked
			for the type.</li>
		<li>If no explicit value type is still not known, the directory processor
			attempts to locate a predefined value type for the predefined type name.</li>
		<li>If the value type is known, the <code>ValueFactory</code> registered for
			the type, if any, is asked to create the value object.</li>
		<li>If no value object was created and a profile is known, the
			<code>ValueFactory</code> registered with the profile, if any, is asked
			to create the value object.</li>
		<li>If no value object was created, a string is returned containing the
			literal contents of the value.</li> 
	</ol> 
	<p>If the value cannot be created using a <code>ViewFactory</code>, the
		value is converted to a single string.</p>
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param reader The reader that contains the lines of the directory.
	@return An array of objects represent the value string.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	@see NameValuePair
	*/
	protected Object[] processValue(final String profile, final String group, final String name, final List paramList, final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		Object[] objects=null;	//start out by assuming we can't process the value
		String valueType=DirectoryUtilities.getParamValue(paramList, VALUE_PARAM_NAME);	//get the value type parameter value
		if(valueType==null)	//if the value type wasn't explicitly given
		{
			if(profile!=null)	//if we know the profile
			{
				final ValueFactory profileValueFactory=getValueFactoryByProfile(profile);	//see if we have a value factory registered with this profile
				if(profileValueFactory!=null)	//if there is a value factory for this profile
				{
					valueType=profileValueFactory.getValueType(profile, group, name, paramList);	//ask this profile's value factory for the value type
				}
			}
			if(valueType==null)	//if we still don't know the type
			{
				valueType=getValueType(profile, group, name, paramList);	//ask ourselves for the value type
			}
		}
		if(valueType!=null)	//if we know the value type
		{
			final ValueFactory valueTypeValueFactory=getValueFactoryByValueType(valueType);	//see if we have a value factory registered with this value type
			if(valueTypeValueFactory!=null)	//if there is a value factory for this value type
			{
				objects=valueTypeValueFactory.createValues(profile, group, name, paramList, valueType, reader);	//create objects for this value type
			}
		}
		if(objects==null && profile!=null)	//if no objects were created, to use use a value factory based upon the profile, if we have a profile
		{
			final ValueFactory profileValueFactory=getValueFactoryByProfile(profile);	//see if we have a value factory registered with this profile
			if(profileValueFactory!=null)	//if there is a value factory for this profile
			{
				objects=profileValueFactory.createValues(profile, group, name, paramList, valueType, reader);	//create objects for this profile
			}
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
	public Object[] createValues(final String profile, final String group, final String name, final List paramList, final String valueType, final LineUnfoldParseReader reader) throws IOException, ParseIOException
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
//		G***del Debug.trace("read text string: ", string);	//G***del
			stringList.add(string);	//add the string to our list			
			delimiter=reader.peekChar();	//see what character is next
//		G***del Debug.trace("next delimiter: ", delimiter);	//G***del			
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

}
