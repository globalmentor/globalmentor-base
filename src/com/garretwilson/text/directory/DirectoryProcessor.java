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
public class DirectoryProcessor implements DirectoryConstants
{
	
	/**The profile for the predefined types.*/
	private final PredefinedProfile predefinedProfile=new PredefinedProfile();		

		/**@return The profile for the predefined types.*/
		protected PredefinedProfile getPredefinedProfile() {return predefinedProfile;}
		
	/**A map of profiles keyed to the lowercase version of the profile name.*/
	private final Map profileMap=new HashMap();	

		/**Registers a profile.
		@param profileName The name of the profile.
		@param profile The profile to be registered with this profilename.
		*/	
		public void registerProfile(final String profileName, final Profile profile)
		{
			profileMap.put(profileName.toLowerCase(), profile);	//put the profile in the map, keyed to the lowercase version of the profile name
		}

		/**Retrieves a profile for the given profile name.
		@param profileName The name of the profile to return, or <code>null</code>
			if the predefined profile should be returned.
		@return A profile for this profile name, or <code>null</code> if there
			is no profile registered for this profile name.
		@see #getPredefinedProfile
		*/ 
		protected Profile getProfile(final String profileName)
		{
			return profileName!=null ? (Profile)profileMap.get(profileName.toLowerCase()) : getPredefinedProfile();	//get the profile keyed to the lowercase version of the profile name, or return the predefined profile if null was passed
		}
		
	/**A map of value factories keyed to the lowercase version of the value type.*/
	private final Map valueFactoryMap=new HashMap();	

		/**Registers a value factory by value type.
		@param valueType The value type for which this value factory can create values.
		@param valueFactory The value factory to be registered with this value type.
		*/	
		public void registerValueFactory(final String valueType, final ValueFactory valueFactory)
		{
			valueFactoryMap.put(valueType.toLowerCase(), valueFactory);	//put the value factory in the map, keyed to the lowercase version of the type
		}
		
		/**Retrieves a value factory to create values for the given value type.
		@param valueType The value type for which a value factory should be returned.
		@return A value factory for this value type, or <code>null</code> if there
			is no value factory registered for this value type.
		*/ 
		protected ValueFactory getValueFactory(final String valueType)
		{
			return (ValueFactory)valueFactoryMap.get(valueType.toLowerCase());	//get the value factory keyed to the lowercase version of this value type
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
	This class automatically registers a predefined profile for the <code>null</code>
		profile name, and registers that profile as a value factory for standard
		value types.
	*/
	public DirectoryProcessor()
	{
			//register the predefined profile as a value factory for the standard value types
		registerValueFactory(URI_VALUE_TYPE, getPredefinedProfile());			
		registerValueFactory(TEXT_VALUE_TYPE, getPredefinedProfile());		
		registerValueFactory(DATE_VALUE_TYPE, getPredefinedProfile());		
		registerValueFactory(TIME_VALUE_TYPE, getPredefinedProfile());		
		registerValueFactory(DATE_TIME_VALUE_TYPE, getPredefinedProfile());		
		registerValueFactory(INTEGER_VALUE_TYPE, getPredefinedProfile());		
		registerValueFactory(BOOLEAN_VALUE_TYPE, getPredefinedProfile());		
		registerValueFactory(FLOAT_VALUE_TYPE, getPredefinedProfile());		
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
Debug.trace("found group: ", group);
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
		<li>If no explicit value type is given and a profile name is known, the
			<code>Profile</code> registered for that profile, if any, is asked
			for the type.</li>
		<li>If no explicit value type is still not known, the predefined profile
			is asked for the predefined type name.</li>
		<li>If a profile name is known and the
			<code>Profile</code> registered with the profile, if any, implements
			<code>ValueFactory</code>, it is asked to create the value object.</li>
		<li>If no value object was created, if the value type is known,
			the <code>ValueFactory</code> registered for the type, if any, is asked
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
	protected Object[] processValue(final String profileName, final String group, final String name, final List paramList, final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		Object[] objects=null;	//start out by assuming we can't process the value
		final Profile profile=getProfile(profileName);	//see if we have a profile registered with this profile name
		String valueType=DirectoryUtilities.getParamValue(paramList, VALUE_PARAM_NAME);	//get the value type parameter value
		if(valueType==null)	//if the value type wasn't explicitly given
		{
			if(profile!=null)	//if there is a profile for this profile name
			{
				valueType=profile.getValueType(profileName, group, name, paramList);	//ask this profile's value factory for the value type
			}
			if(valueType==null && profile!=getPredefinedProfile())	//if we still don't know the type, and we didn't already check the predefined profile 
			{
				valueType=getPredefinedProfile().getValueType(profileName, group, name, paramList);	//ask the predefined profile for the value type
			}
		}
		if(profile instanceof ValueFactory)	//if our profile is a value factory, use the profile as a  value factory
		{
			objects=((ValueFactory)profile).createValues(profileName, group, name, paramList, valueType, reader);	//create objects for this profile
		}
		if(objects==null && valueType!=null)	//if no objects were created, but we know the value type
		{
			final ValueFactory valueFactory=getValueFactory(valueType);	//see if we have a value factory registered with this value type
			if(valueFactory!=null)	//if there is a value factory for this value type
			{
				objects=valueFactory.createValues(profileName, group, name, paramList, valueType, reader);	//create objects for this value type
			}
		}
		if(objects==null)	//if no objects were created
		{
			final String valueString=reader.readStringUntilChar(CR);	//everything before the carriage return will constitute the value
			objects=new String[]{valueString};	//put the single value string in an array of strings and use that for the value objects
		}
		return objects;	//return the value objects we processed
	}

}