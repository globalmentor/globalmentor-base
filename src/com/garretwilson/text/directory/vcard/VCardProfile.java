package com.garretwilson.text.directory.vcard;

import java.io.*;
import java.util.*;
import com.garretwilson.io.*;
import com.garretwilson.text.directory.*;
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

	/**Creates a vCard object from the vCard profile directory entries in the
		given directory. Unrecognized or unusable content lines within the vCard
		profile will be saved as literal content lines so that their information
		will be preserved.
	@param directory The <code>text/directory</code> containing a vCard profile.
	@return A vCard object, or <code>null</code> if no vCard profile was included
		in the given directory.
	*/ 
	public static VCard createVCard(final Directory directory)
	{
		final VCard vcard;	//we'll create a vCard and store it here
		final ContentLine[] contentLines=directory.getContentLinesByProfile(VCARD_PROFILE_NAME);	//get all vCard content lines
		if(contentLines.length>0)	//if there are content lines in the vCard profile
		{
			vcard=new VCard();	//create a new default vCard
			for(int i=0; i<contentLines.length; ++i)	//look at each content line
			{
				final ContentLine contentLine=contentLines[i];	//get a reference to this content line
				final String typeName=contentLine.getTypeName();	//get this content line's type name
				if(BEGIN_TYPE.equalsIgnoreCase(typeName))	//BEGIN
				{
					//ignore begin
				}
				else if(END_TYPE.equalsIgnoreCase(typeName))	//END
				{
					//ignore end
				}
						//identification types
				else if(FN_TYPE.equalsIgnoreCase(typeName))	//FN
				{
					if(vcard.getFormattedName()==null)	//if there is not yet a formatted name
					{
						vcard.setFormattedName(contentLine.getValue().toString());	//get the formatted name
						continue;	//don't process this content line further
					}
				}
				else if(NAME_TYPE.equalsIgnoreCase(typeName))	//NAME
				{
					if(vcard.getName()==null)	//if there is not yet a name
					{
						vcard.setName((Name)contentLine.getValue());	//get the name
						continue;	//don't process this content line further
					}
				}
				else if(NICKNAME_TYPE.equalsIgnoreCase(typeName))	//NICKNAME
				{
					vcard.getNicknameList().add(contentLine.getValue().toString());	//add this nickname to our list
					continue;	//don't process this content line further
				}
						//delivery addressing types
				else if(ADR_TYPE.equalsIgnoreCase(typeName))	//ADR
				{
					vcard.getAddressList().add((Address)contentLine.getValue());	//add this address to our list
					continue;	//don't process this content line further
				}
				else if(LABEL_TYPE.equalsIgnoreCase(typeName))	//LABEL
				{
//TODO make sure the NameAddressPanel stores label information so that it won't be lost
					vcard.getLabelList().add(contentLine.getValue().toString());	//add this label to our list
					continue;	//don't process this content line further
				}
						//telecommunications addressing types
				else if(TEL_TYPE.equalsIgnoreCase(typeName))	//TEL
				{
					vcard.getTelephoneList().add((Telephone)contentLine.getValue());	//add this telephone to our list
					continue;	//don't process this content line further
				}
				else if(EMAIL_TYPE.equalsIgnoreCase(typeName))	//EMAIL
				{
					vcard.getEmailList().add((Email)contentLine.getValue());	//add this email to our list
					continue;	//don't process this content line further
				}
						//organizational type
				else if(ORG_TYPE.equalsIgnoreCase(typeName))	//ORG
				{
					final String[] org=(String[])contentLine.getValue();	//get the organization information
					if(org.length>0 && vcard.getOrganizationName()==null)	//if there is an organization name, and we haven't yet stored an organization name
					{
						vcard.setOrganizationName(org[0]);	//set the organization name from the first oganizational component
						if(org.length>0)	//if there are units specified
						{
							final String[] units=new String[org.length-1];	//create a string array to contain all the units (ignore the first item, the organization name)
							System.arraycopy(org, 1, units, 0, units.length);	//copy the units from the organizational array to the units array
							vcard.setOrganizationUnits(units);	//set the vCard units 
						}
						continue;	//don't process this content line further
					}
				}
				else if(TITLE_TYPE.equalsIgnoreCase(typeName))	//TITLE
				{
					if(vcard.getTitle()==null)	//if there is not yet a title
					{
						vcard.setTitle(contentLine.getValue().toString());	//get the title
						continue;	//don't process this content line further
					}
				}
				else if(ROLE_TYPE.equalsIgnoreCase(typeName))	//ROLE
				{
					if(vcard.getRole()==null)	//if there is not yet a role
					{
						vcard.setRole(contentLine.getValue().toString());	//get the role
						continue;	//don't process this content line further
					}
				}
						//explanatory types
				else if(CATEGORIES_TYPE.equalsIgnoreCase(typeName))	//CATEGORIES
				{
					vcard.addCategories((String[])contentLine.getValue());	//add these categories to our list
					continue;	//don't process this content line further
				}
				else if(NOTE_TYPE.equalsIgnoreCase(typeName))	//NOTE
				{
					if(vcard.getNote()==null)	//if there is not yet a note
					{
						vcard.setNote(contentLine.getValue().toString());	//get the note
						continue;	//don't process this content line further
					}
				}
					//if we make it to here, we either don't recognize the content line
					//	or we can't proces it (e.g. a duplicate value we don't support)
//TODO store the unrecognized content lines
			}
		}
		else	//if there are no vCard content lines
		{
			vcard=null;	//there can be no vCard
		}
		return vcard;	//return the vCard we created or null if there were no vCard profile content lines 
	}
}
