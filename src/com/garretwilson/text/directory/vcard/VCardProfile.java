package com.garretwilson.text.directory.vcard;

import java.io.*;
import java.lang.ref.*;
import java.util.*;
import com.garretwilson.io.*;
import com.garretwilson.itu.*;
import com.garretwilson.text.directory.*;
import com.garretwilson.util.*;

/**Class that can create values for the "VCARD" profile of a
	<code>text/directory</code>as defined in
	 <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
<p>The processor knows how to process the vCard types:
	<code>BINARY_VALUE_TYPE</code>, <code>VCARD_VALUE_TYPE</code>,
	<code>PHONE_NUMBER_VALUE_TYPE</code>, and <code>UTC_OFFSET_VALUE_TYPE</code>.</p>
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
		registerValueType(ADR_TYPE, null);	//ADR: structured text
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
	<p>This method knows how to create predefined types, which,
		along with the objects returned, are as follows:</p>
	<ul>
		<li><code>FN_TYPE</code> <code>String</code></li>
		<li><code>N_TYPE</code> <code>Name</code></li>
		<li><code>NICKNAME_TYPE</code> <code>String</code></li>
		<li><code>PHOTO_TYPE</code></li>
		<li><code>BDAY_TYPE</code></li>
		<li><code>ADR_TYPE</code> <code>Address</code></li>
		<li><code>LABEL_TYPE</code> <code>String</code></li>
		<li><code>TEL_TYPE</code> <code>TelephoneNumber</code></li>
		<li><code>EMAIL_TYPE</code> <code>String</code></li>
		<li><code>MAILER_TYPE</code></li>
		<li><code>TZ_TYPE</code></li>
		<li><code>GEO_TYPE</code></li>
		<li><code>TITLE_TYPE</code> <code>String</code></li>
		<li><code>ROLE_TYPE</code> <code>String</code></li>
		<li><code>LOGO_TYPE</code></li>
		<li><code>AGENT_TYPE</code></li>
		<li><code>ORG_TYPE</code> <code>String[]</code></li>
		<li><code>CATEGORIES_TYPE</code> <code>String</code></li>
		<li><code>NOTE_TYPE</code> <code>String</code></li>
		<li><code>PRODID_TYPE</code></li>
		<li><code>REV_TYPE</code></li>
		<li><code>SORT_STRING_TYPE</code></li>
		<li><code>SOUND_TYPE</code></li>
		<li><code>UID_TYPE</code></li>
		<li><code>URL_TYPE=</code></li>
		<li><code>VERSION_TYPE</code></li>
		<li><code>CLASS_TYPE</code></li>
		<li><code>KEY_TYPE</code></li>	
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
			//see if we recognize the value type
		if(PHONE_NUMBER_VALUE_TYPE.equalsIgnoreCase(valueType))	//phone-number
		{
			return new Object[]{processPhoneNumberValue(reader)};	//process the phone number value type			
		}		
			//see if we recognize the type name		
				//identification types
		if(N_TYPE.equalsIgnoreCase(name))	//N
		{
			return new Object[]{processNValue(reader)};	//process the N value
		}
				//delivery addressing types
		else if(ADR_TYPE.equalsIgnoreCase(name))	//ADR
		{
			return new Object[]{processADRValue(reader, paramList)};	//process the ADR value
		}
			//telecommunications addressing types
/*G***del when works			
		else if(TEL_TYPE.equalsIgnoreCase(name))	//TEL
		{
			return new Object[]{processTELValue(reader, paramList)};	//process the TEL value
		}
*/
			//organizational types
		else if(ORG_TYPE.equalsIgnoreCase(name))	//ORG
		{
			return new Object[]{processORGValue(reader)};	//process the ORG value
		}
		return null;	//show that we can't create a value
	}
	
	/**Processes the value for the <code>N</code> type name.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@return An object representing the vCard structured name.
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

	/**The reference to a map of <code>Integer</code>s representing address
		types, keyed to lowercase versions of address type names. This map can
		be reclaimed by the JVM if it is not being used.
	@see Address 
	*/
	private static SoftReference addressTypeIntegerMapReference=null;
	
	/**Determines the integer address type value to represent the given address
		type name. Comparison is made without regard to case.
	@param addressTypeName The name of the address type.
	@return The delivery address type, one of the
		<code>Address.XXX_ADDRESS_TYPE</code> constants, or
		<code>Address.NO_ADDRESS_TYPE</code> if the address type name was not
		recognized.
	@see Address
	*/
	public static int getAddressType(final String addressTypeName)
	{
			//get the map, if it has been created and hasn't been reclaimed
		Map addressTypeIntegerMap=addressTypeIntegerMapReference!=null ? (Map)addressTypeIntegerMapReference.get() : null;
		if(addressTypeIntegerMap==null)	//if we no longer have a map, create one and initialize it with lowercase address type values
		{
			addressTypeIntegerMap=new HashMap();	//create a new map
			addressTypeIntegerMap.put(ADR_DOM_PARAM_VALUE.toLowerCase(), new Integer(Address.DOMESTIC_ADDRESS_TYPE));
			addressTypeIntegerMap.put(ADR_INTL_PARAM_VALUE.toLowerCase(), new Integer(Address.INTERNATIONAL_ADDRESS_TYPE));
			addressTypeIntegerMap.put(ADR_POSTAL_PARAM_VALUE.toLowerCase(), new Integer(Address.POSTAL_ADDRESS_TYPE));
			addressTypeIntegerMap.put(ADR_PARCEL_PARAM_VALUE.toLowerCase(), new Integer(Address.PARCEL_ADDRESS_TYPE));
			addressTypeIntegerMap.put(ADR_HOME_PARAM_VALUE.toLowerCase(), new Integer(Address.HOME_ADDRESS_TYPE));
			addressTypeIntegerMap.put(ADR_WORK_PARAM_VALUE.toLowerCase(), new Integer(Address.WORK_ADDRESS_TYPE));
			addressTypeIntegerMap.put(ADR_PREF_PARAM_VALUE.toLowerCase(), new Integer(Address.PREFERRED_ADDRESS_TYPE));
			addressTypeIntegerMapReference=new SoftReference(addressTypeIntegerMap);	//store the map in a soft reference, so it can be reclaimed if needed			
		}
		final Integer addressTypeInteger=(Integer)addressTypeIntegerMap.get(addressTypeName.toLowerCase());	//get the integer representing this address type name
		return addressTypeInteger!=null ? addressTypeInteger.intValue() : Address.NO_ADDRESS_TYPE;	//return the address type we found, or NO_ADDRESS_TYPE if we didn't find an address type
	}

	/**Processes the value for the <code>ADR</code> type name.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return An address object representing the value.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static Address processADRValue(final LineUnfoldParseReader reader, final List paramList) throws IOException, ParseIOException
	{
		int addressType=Address.NO_ADDRESS_TYPE;	//start out not knowing any address type
		final String[] types=DirectoryUtilities.getParamValues(paramList, TYPE_PARAM_NAME);	//get the address types specified
		for(int i=types.length-1; i>=0; --i)	//look at each address type
		{
			addressType|=getAddressType(types[i]);	//get this address type and combine it with the ones we've found already
		}
		final String[][] fields=processStructuredTextValue(reader);	//process the structured text fields
		final String postOfficeBox=fields.length>0 && fields[0].length>0? fields[0][0] : null;	//get the post office box, if present
		final String[] extendedAddresses=fields.length>1 ? fields[1] : new String[]{};	//get the extended addresses, if present
		final String[] streetAddresses=fields.length>2 ? fields[2] : new String[]{};	//get the street addresses, if present
		final String locality=fields.length>3 && fields[3].length>0 ? fields[3][0] : null;	//get the locality, if present
		final String region=fields.length>4 && fields[4].length>0 ? fields[4][0] : null;	//get the region, if present
		final String postalCode=fields.length>5 && fields[5].length>0 ? fields[5][0] : null;	//get the postal code, if present
		final String countryName=fields.length>6 && fields[6].length>0 ? fields[6][0] : null;	//get the country name, if present
		return new Address(postOfficeBox, extendedAddresses, streetAddresses, locality, region, postalCode, countryName, addressType);	//create and return a vCard address with the parsed information
	}

	/**The reference to a map of <code>Integer</code>s representing telephone
		types, keyed to lowercase versions of telephone type names. This map can
		be reclaimed by the JVM if it is not being used.
	@see Address 
	*/
	private static SoftReference telephoneTypeIntegerMapReference=null;
	
	/**Determines the integer telephone type value to represent the given
		telephone type name. Comparison is made without regard to case.
	@param telephoneTypeName The name of the telephone type.
	@return The telephone type, one of the
		<code>Telephone.XXX_TELEPHONE_TYPE</code> constants, or
		<code>Telephone.NO_TELEPHONE_TYPE</code> if the telephone type name was not
		recognized.
	@see Telephone
	*/
	public static int getTelephoneType(final String telephoneTypeName)
	{
			//get the map, if it has been created and hasn't been reclaimed
		Map telephoneTypeIntegerMap=telephoneTypeIntegerMapReference!=null ? (Map)telephoneTypeIntegerMapReference.get() : null;
		if(telephoneTypeIntegerMap==null)	//if we no longer have a map, create one and initialize it with lowercase telephone type values
		{
			telephoneTypeIntegerMap=new HashMap();	//create a new map
			telephoneTypeIntegerMap.put(TEL_HOME_PARAM_VALUE.toLowerCase(), new Integer(Telephone.HOME_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_MSG_PARAM_VALUE.toLowerCase(), new Integer(Telephone.MESSAGE_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_WORK_PARAM_VALUE.toLowerCase(), new Integer(Telephone.WORK_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_PREF_PARAM_VALUE.toLowerCase(), new Integer(Telephone.PREFERRED_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_VOICE_PARAM_VALUE.toLowerCase(), new Integer(Telephone.VOICE_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_FAX_PARAM_VALUE.toLowerCase(), new Integer(Telephone.FAX_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_CELL_PARAM_VALUE.toLowerCase(), new Integer(Telephone.CELL_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_VIDEO_PARAM_VALUE.toLowerCase(), new Integer(Telephone.VIDEO_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_PAGER_PARAM_VALUE.toLowerCase(), new Integer(Telephone.PAGER_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_BBS_PARAM_VALUE.toLowerCase(), new Integer(Telephone.BBS_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_MODEM_PARAM_VALUE.toLowerCase(), new Integer(Telephone.MODEM_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_CAR_PARAM_VALUE.toLowerCase(), new Integer(Telephone.CAR_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_ISDN_PARAM_VALUE.toLowerCase(), new Integer(Telephone.ISDN_TELEPHONE_TYPE));
			telephoneTypeIntegerMap.put(TEL_PCS_PARAM_VALUE.toLowerCase(), new Integer(Telephone.PCS_TELEPHONE_TYPE));
			telephoneTypeIntegerMapReference=new SoftReference(telephoneTypeIntegerMap);	//store the map in a soft reference, so it can be reclaimed if needed			
		}
		final Integer telephoneTypeInteger=(Integer)telephoneTypeIntegerMap.get(telephoneTypeName.toLowerCase());	//get the integer representing this telephone type name
		return telephoneTypeInteger!=null ? telephoneTypeInteger.intValue() : Telephone.NO_TELEPHONE_TYPE;	//return the telephone type we found, or NO_TELEPHONE_TYPE if we didn't find a telephone type
	}

	/**Processes the value for the <code>TEL</code> type name.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@return A telephone object representing the value.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static TelephoneNumber processPhoneNumberValue(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final String telephoneNumberString=reader.readStringUntilChar(CR);	//read the string representing the telephone number
		try
		{
			return new TelephoneNumber(telephoneNumberString);	//convert the string to a telephone number
		}
		catch(TelephoneNumberSyntaxException telephoneNumberSyntaxException)	//if the telephone number was not syntactically correct
		{
			final ParseIOException parseIOException=new ParseIOException(telephoneNumberSyntaxException.getMessage(), reader);	//create an I/O parse exception from the telephone number syntax exception
			parseIOException.initCause(telephoneNumberSyntaxException);	//show what caused this exception
			throw parseIOException;	//throw the I/O parse exceptoin
		}
	}

	/**The reference to a map of <code>Integer</code>s representing email
		types, keyed to lowercase versions of email type names. This map can
		be reclaimed by the JVM if it is not being used.
	@see Address 
	*/
	private static SoftReference emailTypeIntegerMapReference=null;
	
	/**Determines the integer email type value to represent the given
		email type name. Comparison is made without regard to case.
	@param emailTypeName The name of the email type.
	@return The email type, one of the
		<code>Email.XXX_EMAIL_TYPE</code> constants, or
		<code>Email.NO_EMAIL_TYPE</code> if the email type name was not
		recognized.
	@see Email
	*/
	public static int getEmailType(final String emailTypeName)
	{
			//get the map, if it has been created and hasn't been reclaimed
		Map emailTypeIntegerMap=emailTypeIntegerMapReference!=null ? (Map)emailTypeIntegerMapReference.get() : null;
		if(emailTypeIntegerMap==null)	//if we no longer have a map, create one and initialize it with lowercase email type values
		{
			emailTypeIntegerMap=new HashMap();	//create a new map
			emailTypeIntegerMap.put(EMAIL_INTERNET_PARAM_VALUE.toLowerCase(), new Integer(Email.INTERNET_EMAIL_TYPE));
			emailTypeIntegerMap.put(EMAIL_X400_PARAM_VALUE.toLowerCase(), new Integer(Email.X400_EMAIL_TYPE));
			emailTypeIntegerMap.put(EMAIL_PREF_PARAM_VALUE.toLowerCase(), new Integer(Email.PREFERRED_EMAIL_TYPE));
			emailTypeIntegerMapReference=new SoftReference(emailTypeIntegerMap);	//store the map in a soft reference, so it can be reclaimed if needed			
		}
		final Integer emailTypeInteger=(Integer)emailTypeIntegerMap.get(emailTypeName.toLowerCase());	//get the integer representing this email type name
		return emailTypeInteger!=null ? emailTypeInteger.intValue() : Email.NO_EMAIL_TYPE;	//return the email type we found, or NO_EMAIL_TYPE if we didn't find an email type
	}

	/**Processes the value for the <code>EMAIL</code> type name.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return An email object representing the value.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
/*G***del when works	
	public static Email processEMAILValue(final LineUnfoldParseReader reader, final List paramList) throws IOException, ParseIOException
	{
		int emailType=Email.NO_EMAIL_TYPE;	//start out not knowing any email type
		final String[] types=DirectoryUtilities.getParamValues(paramList, TYPE_PARAM_NAME);	//get the email types specified
		for(int i=types.length-1; i>=0; --i)	//look at each email type
		{
			emailType|=getEmailType(types[i]);	//get this email type and combine it with the ones we've found already
		}
		final String emailAddress=reader.readStringUntilChar(CR);	//read the string representing the email address
		return new Email(emailAddress, emailType);	//create and return an email from the address and type we parsed
	}
*/

	/**Processes the value for the <code>ORG</code> type name.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@return An array of strings representing the organizational name and units.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static String[] processORGValue(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final List orgList=new ArrayList();	//create a list into which we will place the organizational name and units, as we'll ignore any structured components that are empty 
		final String[][] fields=processStructuredTextValue(reader);	//process the structured text fields
		for(int i=0; i<fields.length; ++i)	//look at each field
		{
			for(int j=0; j<fields.length; ++j)	//look at each field value (this isn't in the specification, but it won't hurt to add these values within each field to keep from losing them, even if they shouldn't be there)
			{
				orgList.add(fields[i][j]);	//add this field value as either the organization name or an organizational unit
			}
		}
		return (String[])orgList.toArray(new String[orgList.size()]);	//return an array version of the organization component list
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
							//TODO fix to get the locale; comment
						final LocaleText formattedName=new LocaleText(contentLine.getValue().toString());
						vcard.setFormattedName(formattedName);	//get the formatted name
						continue;	//don't process this content line further
					}
				}
				else if(N_TYPE.equalsIgnoreCase(typeName))	//N
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
					int telephoneType=Telephone.NO_TELEPHONE_TYPE;	//start out not knowing any telephone type
					final String[] types=DirectoryUtilities.getParamValues(contentLine.getParamList(), TYPE_PARAM_NAME);	//get the telephone types specified
					for(int typeIndex=types.length-1; typeIndex>=0; --typeIndex)	//look at each telephone type
					{
						telephoneType|=getTelephoneType(types[typeIndex]);	//get this telephone type and combine it with the ones we've found already
					}
					final TelephoneNumber telephoneNumber=((TelephoneNumber)contentLine.getValue());	//get the telephone number
					try
					{
						final Telephone telephone=new Telephone(telephoneNumber, telephoneType);	//create a vCard telephone from the telephone number and telephone type
						vcard.getTelephoneList().add(telephone);	//add this telephone to our list
						continue;	//don't process this content line further
					}
					catch(TelephoneNumberSyntaxException telephoneNumberSyntaxException)	//if the telephone number was not syntactically correct (this should never happen here, because it has already been parsed and handed to us), ignore the error and just store the content line
					{
					}
				}
				else if(EMAIL_TYPE.equalsIgnoreCase(typeName))	//EMAIL
				{
//G***del when works					vcard.getEmailList().add((Email)contentLine.getValue());	//add this email to our list
					int emailType=Email.NO_EMAIL_TYPE;	//start out not knowing any email type
					final String[] types=DirectoryUtilities.getParamValues(contentLine.getParamList(), TYPE_PARAM_NAME);	//get the email types specified
					for(int typeIndex=types.length-1; typeIndex>=0; --typeIndex)	//look at each email type
					{
						emailType|=getEmailType(types[typeIndex]);	//get this email type and combine it with the ones we've found already
					}
					final Email email=new Email(contentLine.getValue().toString(), emailType);	//create an email from the address and type we parsed
					vcard.getEmailList().add(email);	//add this email to our list
					continue;	//don't process this content line further
				}
						//organizational type
				else if(ORG_TYPE.equalsIgnoreCase(typeName))	//ORG
				{
					final String[] org=(String[])contentLine.getValue();	//get the organization information
					if(org.length>0 && vcard.getOrganizationName()==null)	//if there is an organization name, and we haven't yet stored an organization name
					{
						vcard.setOrganizationName(org[0]);	//set the organization name from the first oganizational component
						if(org.length>1)	//if there are units specified
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
					vcard.getCategoryList().add(contentLine.getValue().toString());	//add this category to our list
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
				vcard.getContentLineList().add(contentLine);	//add this unprocessed content line to the vCard's list of content lines
			}
		}
		else	//if there are no vCard content lines
		{
			vcard=null;	//there can be no vCard
		}
		return vcard;	//return the vCard we created or null if there were no vCard profile content lines 
	}
}
