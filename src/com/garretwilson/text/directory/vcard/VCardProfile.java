package com.garretwilson.text.directory.vcard;

import java.io.*;
import java.lang.ref.*;
import java.net.*;
import java.util.*;
import com.garretwilson.io.*;
import com.garretwilson.itu.*;
import com.garretwilson.lang.*;
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
*/
public class VCardProfile extends AbstractProfile implements DirectoryConstants, VCardConstants, ValueFactory, ValueSerializer
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
		registerValueType(URL_TYPE, URI_VALUE_TYPE);	//URL: uri
		registerValueType(VERSION_TYPE, TEXT_VALUE_TYPE);	//VERSION: text
					//security types
		registerValueType(CLASS_TYPE, TEXT_VALUE_TYPE);	//CLASS: text
		registerValueType(KEY_TYPE, BINARY_VALUE_TYPE);	//KEY: binary
	}

	/**Processes the textual representation of a line's value and returns
		one or more object representing the value, as some value types
		support multiple values.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	<p>This method knows how to create vCard types, which,
		along with the objects returned, are as follows:</p>
	<ul>
		<li><code>FN_TYPE</code> <code>LocaleText</code></li>
		<li><code>N_TYPE</code> <code>Name</code></li>
		<li><code>NICKNAME_TYPE</code> <code>String</code></li>
		<li><code>PHOTO_TYPE</code></li>
		<li><code>BDAY_TYPE</code></li>
		<li><code>ADR_TYPE</code> <code>Address</code></li>
		<li><code>LABEL_TYPE</code> <code>Label</code></li>
		<li><code>TEL_TYPE</code> <code>Telephone</code></li>
		<li><code>EMAIL_TYPE</code> <code>LocaleText</code></li>
		<li><code>MAILER_TYPE</code></li>
		<li><code>TZ_TYPE</code></li>
		<li><code>GEO_TYPE</code></li>
		<li><code>TITLE_TYPE</code> <code>LocaleText</code></li>
		<li><code>ROLE_TYPE</code> <code>LocaleText</code></li>
		<li><code>LOGO_TYPE</code></li>
		<li><code>AGENT_TYPE</code></li>
		<li><code>ORG_TYPE</code> <code>LocaleText[]</code></li>
		<li><code>CATEGORIES_TYPE</code> <code>LocaleText</code></li>
		<li><code>NOTE_TYPE</code> <code>LocaleText</code></li>
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
			return new Object[]{processPhoneNumberValue(reader, paramList)};	//process the phone number value type			
		}		
			//see if we recognize the type name		
				//identification types
		if(N_TYPE.equalsIgnoreCase(name))	//N
		{
			return new Object[]{processNValue(reader, paramList)};	//process the N value
		}
				//delivery addressing types
		else if(ADR_TYPE.equalsIgnoreCase(name))	//ADR
		{
			return new Object[]{processADRValue(reader, paramList)};	//process the ADR value
		}
		else if(LABEL_TYPE.equalsIgnoreCase(name))	//LABEL
		{
			final LocaleText[] localeTexts=PredefinedProfile.processTextValueList(reader, paramList);	//process the text values
			int addressType;	//we'll determine the address type
			final String[] types=DirectoryUtilities.getParamValues(paramList, TYPE_PARAM_NAME);	//get the address types specified
			if(types.length>0)	//if there are types given
			{
				addressType=Address.NO_ADDRESS_TYPE;	//start out not knowing any address type
				for(int i=types.length-1; i>=0; --i)	//look at each address type
				{
					addressType|=getAddressType(types[i]);	//get this address type and combine it with the ones we've found already
				}
			}
			else	//if there are no types given
			{
				addressType=Address.DEFAULT_ADDRESS_TYPE;	//use the default address type
			}
			final Label[] labels=new Label[localeTexts.length];	//create a new array of labels
			for(int i=localeTexts.length-1; i>=0; --i)	//look at each locale text object
			{
				labels[i]=new Label(localeTexts[i], addressType);	//create a label from the locale text
			}
			return labels;	//return the labels we constructed from the locale test information
		}
			//organizational types
		else if(ORG_TYPE.equalsIgnoreCase(name))	//ORG
		{
			return new Object[]{processORGValue(reader, paramList)};	//process the ORG value
		}
		return null;	//show that we can't create a value
	}
	
	/**Processes the value for the <code>N</code> type name.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return An object representing the vCard structured name.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static Name processNValue(final LineUnfoldParseReader reader, final List paramList) throws IOException, ParseIOException
	{
		final Locale locale=DirectoryUtilities.getLanguageParamValue(paramList);	//get the language, if there is one
		final String[][] fields=processStructuredTextValue(reader);	//process the structured text fields
		final String[] familyNames=fields.length>0 ? fields[0] : new String[]{};	//get the family names, if present
		final String[] givenNames=fields.length>1 ? fields[1] : new String[]{};	//get the given names, if present
		final String[] additionalNames=fields.length>2 ? fields[2] : new String[]{};	//get the additional names, if present
		final String[] honorificPrefixes=fields.length>3 ? fields[3] : new String[]{};	//get the honorific prefixes, if present
		final String[] honorificSuffixes=fields.length>4 ? fields[4] : new String[]{};	//get the honorific suffixes, if present
		return new Name(familyNames, givenNames, additionalNames, honorificPrefixes, honorificSuffixes, locale);	//create and return a vCard name object with the parsed information
	}

	/**The reference to a map of <code>Integer</code>s representing address
		types, keyed to lowercase versions of address type names. This map can
		be reclaimed by the JVM if it is not being used.
	@see Address 
	*/
	private static SoftReference addressTypeIntegerMapReference=null;

	/**@return The map of <code>Integer</code>s representing address
		types, keyed to lowercase versions of address type names, or a new map if
		the old one has been reclaimed by the JVM.
	*/
	protected static Map getAddressTypeIntegerMap()
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
		return addressTypeIntegerMap;	//return the map
	}
	
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
		final Map addressTypeIntegerMap=getAddressTypeIntegerMap();	//get the map of integers keyed to address types
		final Integer addressTypeInteger=(Integer)addressTypeIntegerMap.get(addressTypeName.toLowerCase());	//get the integer representing this address type name
		return addressTypeInteger!=null ? addressTypeInteger.intValue() : Address.NO_ADDRESS_TYPE;	//return the address type we found, or NO_ADDRESS_TYPE if we didn't find an address type
	}
	
	/**Determines the address type names to represent the given address type.
	@param addressType The delivery address types, one or more of the
		<code>Address.XXX_ADDRESS_TYPE</code> constants ORed together.
	@return The names of the of the given address type.
	@see Address
	*/
	public static String[] getAddressTypeNames(final int addressType)
	{
		final List addressTypeNameList=new ArrayList();	//create an array of address type names
		final Iterator addressTypeEntryIterator=getAddressTypeIntegerMap().entrySet().iterator();	//create an iterator to the address type entries
		while(addressTypeEntryIterator.hasNext())	//while there are more address type entries
		{
			final Map.Entry addressTypeEntry=(Map.Entry)addressTypeEntryIterator.next();	//get the next address type entry
			final int addressTypeIntValue=((Integer)addressTypeEntry.getValue()).intValue();	//get the value of this address type
			if((addressType & addressTypeIntValue)==addressTypeIntValue)	//if our address type includes this value
			{
				addressTypeNameList.add(addressTypeEntry.getKey());	//add this address type name to our list 
			}
		}
		return StringUtilities.toStringArray(addressTypeNameList);	//return our list of address type names as an array
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
		final Locale locale=DirectoryUtilities.getLanguageParamValue(paramList);	//get the language, if there is one
		int addressType;	//we'll determine the address type
		final String[] types=DirectoryUtilities.getParamValues(paramList, TYPE_PARAM_NAME);	//get the address types specified
		if(types.length>0)	//if there are types given
		{
			addressType=Address.NO_ADDRESS_TYPE;	//start out not knowing any address type
			for(int i=types.length-1; i>=0; --i)	//look at each address type
			{
				addressType|=getAddressType(types[i]);	//get this address type and combine it with the ones we've found already
			}
		}
		else	//if there are no types given
		{
			addressType=Address.DEFAULT_ADDRESS_TYPE;	//use the default address type
		}
		final String[][] fields=processStructuredTextValue(reader);	//process the structured text fields
		final String postOfficeBox=fields.length>0 && fields[0].length>0? fields[0][0] : null;	//get the post office box, if present
		final String[] extendedAddresses=fields.length>1 ? fields[1] : new String[]{};	//get the extended addresses, if present
		final String[] streetAddresses=fields.length>2 ? fields[2] : new String[]{};	//get the street addresses, if present
		final String locality=fields.length>3 && fields[3].length>0 ? fields[3][0] : null;	//get the locality, if present
		final String region=fields.length>4 && fields[4].length>0 ? fields[4][0] : null;	//get the region, if present
		final String postalCode=fields.length>5 && fields[5].length>0 ? fields[5][0] : null;	//get the postal code, if present
		final String countryName=fields.length>6 && fields[6].length>0 ? fields[6][0] : null;	//get the country name, if present
		return new Address(postOfficeBox, extendedAddresses, streetAddresses, locality, region, postalCode, countryName, addressType, locale);	//create and return a vCard address with the parsed information
	}

	/**The reference to a map of <code>Integer</code>s representing telephone
		types, keyed to lowercase versions of telephone type names. This map can
		be reclaimed by the JVM if it is not being used.
	@see Address 
	*/
	private static SoftReference telephoneTypeIntegerMapReference=null;

	/**@return The map of <code>Integer</code>s representing telephone
		types, keyed to lowercase versions of telephone type names, or a new map if
		the old one has been reclaimed by the JVM.
	*/
	protected static Map getTelephoneTypeIntegerMap()
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
		return telephoneTypeIntegerMap;	//return the map
	}
	
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
		final Map telephoneTypeIntegerMap=getTelephoneTypeIntegerMap();	//get the map of integers keyed to telephone types
		final Integer telephoneTypeInteger=(Integer)telephoneTypeIntegerMap.get(telephoneTypeName.toLowerCase());	//get the integer representing this telephone type name
		return telephoneTypeInteger!=null ? telephoneTypeInteger.intValue() : Telephone.NO_TELEPHONE_TYPE;	//return the telephone type we found, or NO_TELEPHONE_TYPE if we didn't find a telephone type
	}

	/**Determines the telephone type names to represent the given telpehone type.
	@param telephoneType The telephone types, one or more of the
		<code>Telephone.XXX_TELEPHONE_TYPE</code> constants ORed together.
	@return The names of the of the given telephone type.
	@see Telephone
	*/
	public static String[] getTelephoneTypeNames(final int telephoneType)
	{
		final List telephoneTypeNameList=new ArrayList();	//create an array of telephone type names
		final Iterator telephoneTypeEntryIterator=getTelephoneTypeIntegerMap().entrySet().iterator();	//create an iterator to the telephone type entries
		while(telephoneTypeEntryIterator.hasNext())	//while there are more telephone type entries
		{
			final Map.Entry telephoneTypeEntry=(Map.Entry)telephoneTypeEntryIterator.next();	//get the next telephone type entry
			final int telephoneTypeIntValue=((Integer)telephoneTypeEntry.getValue()).intValue();	//get the value of this telephone type
			if((telephoneType & telephoneTypeIntValue)==telephoneTypeIntValue)	//if our telephone type includes this value
			{
				telephoneTypeNameList.add(telephoneTypeEntry.getKey());	//add this telephone type name to our list 
			}
		}
		return StringUtilities.toStringArray(telephoneTypeNameList);	//return our list of telephone type names as an array
	}

	/**Processes the value for the <code>TEL</code> type name.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param reader The reader that contains the lines of the directory.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return A telephone object representing the value.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static Telephone processPhoneNumberValue(final LineUnfoldParseReader reader, final List paramList) throws IOException, ParseIOException
	{
		final Locale locale=DirectoryUtilities.getLanguageParamValue(paramList);	//get the language, if there is one
		final String telephoneNumberString=reader.readStringUntilChar(CR);	//read the string representing the telephone number
		int telephoneType;	//we'll determine the telephone type
		final String[] types=DirectoryUtilities.getParamValues(paramList, TYPE_PARAM_NAME);	//get the telephone types specified
		if(types.length>0)	//if there are types given
		{
			telephoneType=Telephone.NO_TELEPHONE_TYPE;	//start out not knowing any telephone type
			for(int typeIndex=types.length-1; typeIndex>=0; --typeIndex)	//look at each telephone type
			{
				telephoneType|=getTelephoneType(types[typeIndex]);	//get this telephone type and combine it with the ones we've found already
			}
		}
		else	//if there are no types given
		{
			telephoneType=Telephone.DEFAULT_TELEPHONE_TYPE;	//use the default telephone type
		}
		try
		{
			return new Telephone(telephoneNumberString, telephoneType);	//create a telephone from the telephone number and telephone type
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

	/**@return The map of <code>Integer</code>s representing email
		types, keyed to lowercase versions of email type names, or a new map if
		the old one has been reclaimed by the JVM.
	*/
	protected static Map getEmailTypeIntegerMap()
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
		return emailTypeIntegerMap;	//return the map
	}
	
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
		final Map emailTypeIntegerMap=getEmailTypeIntegerMap();	//get the map of integers keyed to email types
		final Integer emailTypeInteger=(Integer)emailTypeIntegerMap.get(emailTypeName.toLowerCase());	//get the integer representing this email type name
		return emailTypeInteger!=null ? emailTypeInteger.intValue() : Email.NO_EMAIL_TYPE;	//return the email type we found, or NO_EMAIL_TYPE if we didn't find an email type
	}

	/**Determines the email type names to represent the given email type.
	@param addressType The email types, one or more of the
		<code>Email.XXX_EMAIL_TYPE</code> constants ORed together.
	@return The names of the of the given email type.
	@see Email
	*/
	public static String[] getEmailTypeNames(final int emailType)
	{
		final List emailTypeNameList=new ArrayList();	//create an array of email type names
		final Iterator emailTypeEntryIterator=getEmailTypeIntegerMap().entrySet().iterator();	//create an iterator to the email type entries
		while(emailTypeEntryIterator.hasNext())	//while there are more email type entries
		{
			final Map.Entry emailTypeEntry=(Map.Entry)emailTypeEntryIterator.next();	//get the next email type entry
			final int emailTypeIntValue=((Integer)emailTypeEntry.getValue()).intValue();	//get the value of this email type
			if((emailType & emailTypeIntValue)==emailTypeIntValue)	//if our email type includes this value
			{
				emailTypeNameList.add(emailTypeEntry.getKey());	//add this email type name to our list 
			}
		}
		return StringUtilities.toStringArray(emailTypeNameList);	//return our list of email type names as an array
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
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return An array of locale text objects representing the organizational name
		and units.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static LocaleText[] processORGValue(final LineUnfoldParseReader reader, final List paramList) throws IOException, ParseIOException
	{
		final Locale locale=DirectoryUtilities.getLanguageParamValue(paramList);	//get the language, if there is one
		final List orgList=new ArrayList();	//create a list into which we will place the organizational name and units, as we'll ignore any structured components that are empty 
		final String[][] fields=processStructuredTextValue(reader);	//process the structured text fields
		for(int i=0; i<fields.length; ++i)	//look at each field
		{
			for(int j=0; j<fields[i].length; ++j)	//look at each field value (this isn't in the specification, but it won't hurt to add these values within each field to keep from losing them, even if they shouldn't be there)
			{
				orgList.add(new LocaleText(fields[i][j], locale));	//add this field value as either the organization name or an organizational unit
			}
		}
		return LocaleText.toLocaleTextArray(orgList);	//return an array version of the organization component list
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

	/**The delimiters that can divide a structured text value: '\\', ';' ',' and CR.*/
	protected final static String STRUCTURED_TEXT_VALUE_DELIMITER_CHARS=""+TEXT_ESCAPE_CHAR+STRUCTURED_TEXT_VALUE_DELIMITER+VALUE_SEPARATOR_CHAR+CR; 

	/**Processes a single field of a structured text value.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	<p>The sequence "\n" or "\N" will be converted to a single newline character,
		'\n', and '\\', ';', and ',' must be escaped with '\\'.</p>
	@param reader The reader that contains the lines of the directory.
	@return An array of strings representing the values of the field.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static String[] processStructuredTextFieldValue(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final List valueList=new ArrayList();	//create a new list to hold the values we find
		final StringBuffer stringBuffer=new StringBuffer();	//create a string buffer to hold whatever string we're processing
		char delimiter;	//we'll store the last delimiter peeked		
		do	
		{
			stringBuffer.append(reader.readStringUntilChar(STRUCTURED_TEXT_VALUE_DELIMITER_CHARS));	//read all value characters until we find a delimiter, and add the value so far to the string buffer
//G***del when works			valueList.add(reader.readStringUntilChar(STRUCTURED_TEXT_VALUE_DELIMITER_CHARS));	//read all value characters until we find a delimiter, and add that value to the list
			delimiter=reader.peekChar();	//see what the delimiter will be
			switch(delimiter)	//see which delimiter we found
			{
				case VALUE_SEPARATOR_CHAR:	//if this is the character separating multiple values (',')
					valueList.add(stringBuffer.toString());	//add the value we collected
					stringBuffer.delete(0, stringBuffer.length());	//clear the string buffer
					reader.skip(1);	//skip the delimiter
					break;				
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
							case ';':
							case ',':
								stringBuffer.append(escapedChar);	//escaped backslashes and commas get appended normally
								break;
							default:	//if something else was escaped, we don't recognize it
								throw new ParseUnexpectedDataException("\\;,"+TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR+TEXT_LINE_BREAK_ESCAPED_UPPERCASE_CHAR, escapedChar, reader);	//show that we didn't expect this character here				
						}
					}
					break;
				case STRUCTURED_TEXT_VALUE_DELIMITER:	//if this is the character separating fields in the structured text value (';')
				case CR:	//if we just read a carriage return
					break;	//don't do anything---we'll just collect our characters and leave
				default:	//if we read anything else (there shouldn't be anything else unless there is a logic error)					
					throw new ParseUnexpectedDataException(STRUCTURED_TEXT_VALUE_DELIMITER_CHARS, delimiter, reader);	//show that we didn't expect this character here
			}
		}
		while(delimiter!=STRUCTURED_TEXT_VALUE_DELIMITER && delimiter!=CR);	//keep collecting parts of the string until we encounter a ';' or a CR
		valueList.add(stringBuffer.toString());	//add the value we collected
		reader.resetPeek();	//reset peeking
		return (String[])valueList.toArray(new String[valueList.size()]);	//convert the list of strings to an array of strings return the array
	}

	/**Serializes a line's value.
	<p>Only the value will be serialized, not any previous or subsequent
		parts of the line or delimiters.</p>
	<p>This method knows how to serialize vCard types, which,
		along with the objects returned, are as follows:</p>
	<ul>
		<li><code>FN_TYPE</code> <code>LocaleText</code></li>
		<li><code>N_TYPE</code> <code>Name</code></li>
		<li><code>NICKNAME_TYPE</code> <code>String</code></li>
		<li><code>PHOTO_TYPE</code></li>
		<li><code>BDAY_TYPE</code></li>
		<li><code>ADR_TYPE</code> <code>Address</code></li>
		<li><code>LABEL_TYPE</code> <code>Label</code></li>
		<li><code>TEL_TYPE</code> <code>Telephone</code></li>
		<li><code>EMAIL_TYPE</code> <code>LocaleText</code></li>
		<li><code>MAILER_TYPE</code></li>
		<li><code>TZ_TYPE</code></li>
		<li><code>GEO_TYPE</code></li>
		<li><code>TITLE_TYPE</code> <code>LocaleText</code></li>
		<li><code>ROLE_TYPE</code> <code>LocaleText</code></li>
		<li><code>LOGO_TYPE</code></li>
		<li><code>AGENT_TYPE</code></li>
		<li><code>ORG_TYPE</code> <code>LocaleText[]</code></li>
		<li><code>CATEGORIES_TYPE</code> <code>LocaleText</code></li>
		<li><code>NOTE_TYPE</code> <code>LocaleText</code></li>
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
	@param value The value to serialize.
	@param valueType The type of value, or <code>null</code> if the type of value
		is unknown.
	@param writer The writer to which the directory information should be written.
	@return <code>true</code> if the operation was successful, else
		<code>false</code> if this class does not support writing the given value.
	@exception IOException Thrown if there is an error writing to the directory.
	@see NameValuePair
	*/	
	public boolean serializeValue(final String profile, final String group, final String name, final List paramList, final Object value, final String valueType, final Writer writer) throws IOException
	{
			//see if we recognize the value type
		if(PHONE_NUMBER_VALUE_TYPE.equalsIgnoreCase(valueType))	//phone-number
		{
			writer.write(((Telephone)value).toCanonicalString());	//write the canonical version of the phone number
			return true;	//show that we serialized the value 
		}		
			//see if we recognize the type name		
				//identification types
		if(N_TYPE.equalsIgnoreCase(name))	//N
		{
			serializeNValue((Name)value, writer);	//serialize the value
			return true;	//show that we serialized the value 
		}
				//delivery addressing types
		else if(ADR_TYPE.equalsIgnoreCase(name))	//ADR
		{
			serializeADRValue((Address)value, writer);	//serialize the value
			return true;	//show that we serialized the value 
		}
			//organizational types
		else if(ORG_TYPE.equalsIgnoreCase(name))	//ORG
		{
			serializeORGValue((LocaleText[])value, writer);	//serialize the value
			return true;	//show that we serialized the value 
		}
		return false;	//show that we can't serialize the value
	}

	/**Serializes the value for the <code>N</code> type name.
	<p>Only the value will be serialized, not any previous or subsequent
		parts of the line or delimiters.</p>
	@param name An object representing the vCard structured name to serialize.
	@param writer The writer to which the directory information should be written.
	@exception IOException Thrown if there is an error reading the directory.
	*/
	public static void serializeNValue(final Name name, final Writer writer) throws IOException
	{
			//place the field arrays into an array
		final String[][] n=new String[][]{name.getFamilyNames(), name.getGivenNames(), name.getAdditionalNames(), name.getHonorificPrefixes(), name.getHonorificSuffixes()};
		serializeStructuredTextValue(n, writer);	//serialize the value
	}

	/**Serializes the value for the <code>ADR</code> type name.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	<p>Only the value will be serialized, not any previous or subsequent
		parts of the line or delimiters.</p>
	@param address An address object representing the value to serialize.
	@param name An object representing the vCard structured name to serialize.
	@param writer The writer to which the directory information should be written.
	@exception IOException Thrown if there is an error reading the directory.
	*/
	public static void serializeADRValue(final Address address, final Writer writer) throws IOException
	{
			//place the field arrays into an array
		final String[][] adr=new String[][]
				{
					new String[]{address.getPostOfficeBox()!=null ? address.getPostOfficeBox() : ""},
					address.getExtendedAddresses(),
					address.getStreetAddresses(),
					new String[]{address.getLocality()!=null ? address.getLocality() : ""},
					new String[]{address.getRegion()!=null ? address.getRegion() : ""},
					new String[]{address.getPostalCode()!=null ? address.getPostalCode() : ""},
					new String[]{address.getCountryName()!=null ? address.getCountryName() : ""}
				};
		serializeStructuredTextValue(adr, writer);	//serialize the value
	}

	/**Serializes the value for the <code>ORG</code> type name.
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param org An array of locale text objects representing the organizational
		name and units.
	@param writer The writer to which the directory information should be written.
	@exception IOException Thrown if there is an error reading the directory.
	*/
	public static void serializeORGValue(final LocaleText[] org, final Writer writer) throws IOException
	{
		final String[][] orgFields=new String[org.length][];	//create an array of string arrays
		for(int i=org.length-1; i>=0; --i)	//look at each of the org components
		{
			orgFields[i]=new String[]{org[i].getText()};	//store the text of this org component in an array representing this field
		}
		serializeStructuredTextValue(orgFields, writer);	//serialize the value
	}

	/**Serializes structured text.
	Structured text is series of fields separated by ';', each field of which
		can have multiple values separated by ','.
	<p>Only the value will be serialized, not any previous or subsequent
		parts of the line or delimiters.</p>
	@param structuredText An array of string arrays, each string array representing the values
		of each field.
	@param writer The writer to which the directory information should be written.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public static void serializeStructuredTextValue(final String[][] structuredText, final Writer writer) throws IOException
	{
		for(int fieldIndex=0; fieldIndex<structuredText.length; ++fieldIndex)	//look at each structured text field
		{
			final String[] fieldValues=structuredText[fieldIndex];	//get the values for this field
			for(int fieldValueIndex=0; fieldValueIndex<fieldValues.length; ++fieldValueIndex)	//look at each field value
			{
				serializeStructuredTextFieldValue(fieldValues[fieldValueIndex], writer);	//write this structured text field value
				if(fieldValueIndex<fieldValues.length-1)	//if this is not the last field value
					writer.write(VALUE_SEPARATOR_CHAR);	//write the value separator ','
			}
			if(fieldIndex<structuredText.length-1)	//if this is not the last field
				writer.write(STRUCTURED_TEXT_VALUE_DELIMITER);	//write the field separator ';'
		}
	}

	/**The characters that must be escaped in structured text: '\n', '\\', and ','.*/
	protected final static char[] STRUCTURED_TEXT_MATCH_CHARS=new char[]{'\n', '\\', ';', ','};

	/**The strings to replace the characters to be escaped in structured text.*/
	protected final static String[] STRUCTURED_TEXT_REPLACEMENT_STRINGS=new String[]{"\n", "\\", "\\;", "\\,"};

	/**Serializes a structured text field value.
	<p>The newline character '\n' will be be converted to "\n", and 
		and '\\', ';', and ',' will be escaped with '\\'.</p>
	@param text The structured text value to serialize.
	@param writer The writer to which the directory information should be written.
	@exception IOException Thrown if there is an error writing to the directory.
	*/	
	public static void serializeStructuredTextFieldValue(final String text, final Writer writer) throws IOException
	{
			//replace characters with their escaped versions and write the resulting string
		writer.write(StringUtilities.replace(text, STRUCTURED_TEXT_MATCH_CHARS, STRUCTURED_TEXT_REPLACEMENT_STRINGS));
	}

	/**Creates a directory from the given content lines.
	Unrecognized or unusable content lines within the directory object will be
		saved as literal content lines so that their information will be preserved.
	This version creates a <code>VCard</code> object.
	@param contentLines The content lines that make up the directory.
	@return A directory object representing the directory, or <code>null</code>
		if this profile cannot create a directory from the given information.
	@see VCard
	*/
	public Directory createDirectory(final ContentLine[] contentLines)
	{
		final VCard vcard=new VCard();	//we'll store the vCard information here
		for(int i=0; i<contentLines.length; ++i)	//look at each content line
		{
			final ContentLine contentLine=contentLines[i];	//get a reference to this content line
			final String typeName=contentLine.getTypeName();	//get this content line's type name
			if(BEGIN_TYPE.equalsIgnoreCase(typeName))	//BEGIN
			{
				continue;	//ignore begin; don't process this content line further G***maybe only ignore these if they are the vCard profile
			}
			else if(END_TYPE.equalsIgnoreCase(typeName))	//END
			{
				continue;	//ignore end; don't process this content line further
			}
			else if(NAME_TYPE.equalsIgnoreCase(typeName))	//if this is NAME
			{
				if(vcard.getDisplayName()==null)	//if the vCard does not yet have a display name
				{
					vcard.setDisplayName((LocaleText)contentLine.getValue());	//set the vCard display name
					continue;	//don't process this content line further
				}
			}
					//identification types
			else if(FN_TYPE.equalsIgnoreCase(typeName))	//FN
			{
				if(vcard.getFormattedName()==null)	//if there is not yet a formatted name
				{							
					vcard.setFormattedName((LocaleText)contentLine.getValue());	//get the formatted name
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
				vcard.getNicknameList().add((LocaleText)contentLine.getValue());	//add this nickname to our list
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
				vcard.getLabelList().add((Label)contentLine.getValue());	//add this label to our list
				continue;	//don't process this content line further
			}
					//telecommunications addressing types
			else if(TEL_TYPE.equalsIgnoreCase(typeName))	//TEL
			{
				vcard.getTelephoneList().add((Telephone)contentLine.getValue());	//add this telephone to our list
				continue;	//don't process this content line further
			}
			else if(EMAIL_TYPE.equalsIgnoreCase(typeName))	//EMAIL
			{	//G***decide if we want to add the email object when this line is processed
//G***del when works					vcard.getEmailList().add((Email)contentLine.getValue());	//add this email to our list
				int emailType;	//we'll determine the email type
				final String[] types=DirectoryUtilities.getParamValues(contentLine.getParamList(), TYPE_PARAM_NAME);	//get the email types specified
				if(types.length>0)	//if there are types given
				{
					emailType=Email.NO_EMAIL_TYPE;	//start out not knowing any email type
					for(int typeIndex=types.length-1; typeIndex>=0; --typeIndex)	//look at each email type
					{
						emailType|=getEmailType(types[typeIndex]);	//get this email type and combine it with the ones we've found already
					}
				}
				else	//if there are no types given
				{
					emailType=Email.DEFAULT_EMAIL_TYPE;	//use the default email type
				}
				final Email email=new Email(((LocaleText)contentLine.getValue()).getText(), emailType);	//create an email from the address and type we parsed
				vcard.getEmailList().add(email);	//add this email to our list
				continue;	//don't process this content line further
			}
					//organizational type
			else if(ORG_TYPE.equalsIgnoreCase(typeName))	//ORG
			{
				final LocaleText[] org=(LocaleText[])contentLine.getValue();	//get the organization information
				if(org.length>0 && vcard.getOrganizationName()==null)	//if there is an organization name, and we haven't yet stored an organization name
				{
					vcard.setOrganizationName(org[0]);	//set the organization name from the first oganizational component
					if(org.length>1)	//if there are units specified
					{
						final LocaleText[] units=new LocaleText[org.length-1];	//create a string array to contain all the units (ignore the first item, the organization name)
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
//TODO add support for multiple titles with multiple languages
					vcard.setTitle((LocaleText)contentLine.getValue());	//set the title
					continue;	//don't process this content line further
				}
			}
			else if(ROLE_TYPE.equalsIgnoreCase(typeName))	//ROLE
			{
				if(vcard.getRole()==null)	//if there is not yet a role
				{
					vcard.setRole((LocaleText)contentLine.getValue());	//set the role
					continue;	//don't process this content line further
				}
			}
					//explanatory types
			else if(CATEGORIES_TYPE.equalsIgnoreCase(typeName))	//CATEGORIES
			{
				vcard.getCategoryList().add((LocaleText)contentLine.getValue());	//add this category to our list
				continue;	//don't process this content line further
			}
			else if(NOTE_TYPE.equalsIgnoreCase(typeName))	//NOTE
			{
				if(vcard.getNote()==null)	//if there is not yet a note
				{
					vcard.setNote((LocaleText)contentLine.getValue());	//set the note
					continue;	//don't process this content line further
				}
			}
/*TODO fix when we allow this to be edited
			else if(SORT_STRING_TYPE.equalsIgnoreCase(typeName))	//SORT-STRING
			{
				if(vcard.getSortString()==null)	//if there is not yet a sorting string
				{
					vcard.setSortString((LocaleText)contentLine.getValue());	//set the sorting string
					continue;	//don't process this content line further
				}
			}
*/
			else if(URL_TYPE.equalsIgnoreCase(typeName))	//URL
			{
				if(vcard.getURL()==null)	//if there is not yet a URL
				{
					vcard.setURL((URI)contentLine.getValue());	//set the URL
					continue;	//don't process this content line further
				}
			}
			else if(VERSION_TYPE.equalsIgnoreCase(typeName))	//VERSION
			{
				vcard.setVersion(((LocaleText)contentLine.getValue()).getText());	//set the version
				continue;	//don't process this content line further
			}
				//if we make it to here, we either don't recognize the content line
				//	or we can't proces it (e.g. a duplicate value we don't support)
			vcard.getContentLineList().add(contentLine);	//add this unprocessed content line to the vCard's list of content lines
		}
		return vcard;	//return the vCard we created 
	}

	/**Creates a list of content lines from the given vCard.
	<p>This implementation ignores the version of the given vCard and adds a
		content line with the version used here: "3.0".</p>
	@param vcard The vCard object to be converted to content lines.
	@return A directory object representing the directory, or <code>null</code>
		if this profile cannot create a directory from the given information.
	@return The content lines that represent the vCard information.
	@see VCardConstants#VCARD_VERSION_VALUE
	*/
	public static ContentLine[] createContentLines(final VCard vcard)	//TODO make sure displayName and formattedName are included
	{
//G***del		final List contentLineList=new ArrayList(vcard.getContentLineList());	//create a content line list initially containing all the unrecognized content lines of the vCard
		final List contentLineList=new ArrayList();	//create a content line list to fill
		contentLineList.add(new ContentLine(BEGIN_TYPE, new LocaleText(VCARD_PROFILE_NAME)));	//BEGIN:VCARD
				//predefined directory types
		if(vcard.getDisplayName()!=null)	//NAME
		{
			contentLineList.add(new ContentLine(NAME_TYPE, vcard.getDisplayName()));	//add the display name
//G***del			contentLineList.add(0, new ContentLine(NAME_TYPE, vcard.getDisplayName()));	//add the display name at the front of the list
		}
				//identification types
		if(vcard.getFormattedName()!=null)	//FN
		{
			contentLineList.add(DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, FN_TYPE, vcard.getFormattedName()));	//FN
		}
		if(vcard.getName()!=null)	//N
		{
			contentLineList.add(DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, N_TYPE, vcard.getName(), vcard.getName().getLocale()));	//N
		}
		final Iterator nicknameIterator=vcard.getNicknameList().iterator();	//get an iterator to the nicknames
		while(nicknameIterator.hasNext())	//while there are more nicknames
		{
			contentLineList.add(DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, NICKNAME_TYPE, (LocaleText)nicknameIterator.next()));	//NICKNAME			
		}
				//delivery addressing types
		final Iterator adrIterator=vcard.getAddressList().iterator();	//get an iterator to the addresses
		while(adrIterator.hasNext())	//while there are more addresses
		{
			final Address address=(Address)adrIterator.next();	//get the next address
			final ContentLine contentLine=DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, ADR_TYPE, address, address.getLocale());	//ADR
			final String[] addressTypeNames=getAddressTypeNames(address.getAddressType());	//get the address type names
			for(int i=0; i<addressTypeNames.length; ++i)	//look at each address type
			{
				DirectoryUtilities.addParam(contentLine.getParamList(), TYPE_PARAM_NAME, addressTypeNames[i]);	//add this address type parameter
			}
			contentLineList.add(contentLine);	//add the content line
		}
		final Iterator labelIterator=vcard.getLabelList().iterator();	//get an iterator to the labels
		while(labelIterator.hasNext())	//while there are more labels
		{
			final Label label=(Label)labelIterator.next();	//get the next label
			final ContentLine contentLine=DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, LABEL_TYPE, label);	//LABEL
			final String[] addressTypeNames=getAddressTypeNames(label.getAddressType());	//get the label type names
			for(int i=0; i<addressTypeNames.length; ++i)	//look at each address type
			{
				DirectoryUtilities.addParam(contentLine.getParamList(), TYPE_PARAM_NAME, addressTypeNames[i]);	//add this address type parameter
			}
			contentLineList.add(contentLine);	//add the content line
		}
				//telecommunications addressing types
		final Iterator telIterator=vcard.getTelephoneList().iterator();	//get an iterator to the telephones
		while(telIterator.hasNext())	//while there are more telephones
		{
			final Telephone telephone=(Telephone)telIterator.next();	//get the next telephone
			final ContentLine contentLine=new ContentLine(VCARD_PROFILE_NAME, null, TEL_TYPE, telephone);	//TEL
			final String[] telephoneTypeNames=getTelephoneTypeNames(telephone.getTelephoneType());	//get the telephone type names
			for(int i=0; i<telephoneTypeNames.length; ++i)	//look at each telephone type
			{
				DirectoryUtilities.addParam(contentLine.getParamList(), TYPE_PARAM_NAME, telephoneTypeNames[i]);	//add this telephone type parameter
			}
			contentLineList.add(contentLine);	//add the content line
		}
		final Iterator emailIterator=vcard.getEmailList().iterator();	//get an iterator to the emails
		while(emailIterator.hasNext())	//while there are more emails
		{
			final Email email=(Email)emailIterator.next();	//get the next email
			final ContentLine contentLine=DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, EMAIL_TYPE, new LocaleText(email.getAddress(), email.getLocale()));	//EMAIL G***maybe fix to store the email object, if that's what we decide to store there when reading the value
			final String[] emailTypeNames=getEmailTypeNames(email.getEmailType());	//get the email type names
			for(int i=0; i<emailTypeNames.length; ++i)	//look at each email type
			{
				DirectoryUtilities.addParam(contentLine.getParamList(), TYPE_PARAM_NAME, emailTypeNames[i]);	//add this email type parameter
			}
			contentLineList.add(contentLine);	//add the content line
		}
				//organizational type
		final LocaleText[] org;	//we'll create an array for the org or, if there is no org name, just use the units array
		final LocaleText[] units=vcard.getOrganizationUnits();	//get the organizational units
		if(vcard.getOrganizationName()!=null)	//if a name is supplied
		{
			org=new LocaleText[units.length+1];	//create a new array with room for the name and the units
			org[0]=vcard.getOrganizationName();	//store the organization name as the first element
			System.arraycopy(units, 0, org, 1, units.length);	//copy the units into the org array after the organization name
		}
		else	//if there is no name supplied
		{
			org=units;	//just use the units, since there is no organization name
		}
		if(org.length>0)	//ORG
		{
			final Locale orgLocale=org[0].getLocale();	//get the locale of the first organization component
			contentLineList.add(DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, ORG_TYPE, org, orgLocale));	//ORG
		}
		if(vcard.getTitle()!=null)	//TITLE
		{
			contentLineList.add(DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, TITLE_TYPE, vcard.getTitle()));	//TITLE
		}
		if(vcard.getRole()!=null)	//ROLE
		{
			contentLineList.add(DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, ROLE_TYPE, vcard.getRole()));	//ROLE
		}
				//explanatory types
		final Iterator categoryIterator=vcard.getCategoryList().iterator();	//get an iterator to the categories
		while(categoryIterator.hasNext())	//while there are more categories
		{
			final LocaleText category=(LocaleText)categoryIterator.next();	//get the next category
			contentLineList.add(DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, CATEGORIES_TYPE, category));	//CATEGORIES
		}
		if(vcard.getNote()!=null)	//NOTE
		{
			contentLineList.add(DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, NOTE_TYPE, vcard.getNote()));	//NOTE
		}
/*TODO fix when we allow this to be edited
		if(vcard.getSortString()!=null)	//SORT-STRING
		{
			contentLineList.add(DirectoryUtilities.createContentLine(VCARD_PROFILE_NAME, null, SORT_STRING_TYPE, vcard.getNote()));	//SORT-STRING
		}
*/
		if(vcard.getURL()!=null)	//URL
		{
			contentLineList.add(new ContentLine(VCARD_PROFILE_NAME, null, URL_TYPE, vcard.getURL()));	//URL
		}
			//ignore the given vCard version, and always create "version:3.0"
		contentLineList.add(new ContentLine(VERSION_TYPE, new LocaleText(VCARD_VERSION_VALUE)));	//VERSION
		contentLineList.addAll(vcard.getContentLineList());	//add all of our unrecognized content lines
		contentLineList.add(new ContentLine(END_TYPE, new LocaleText(VCARD_PROFILE_NAME)));	//END:VCARD
		return (ContentLine[])contentLineList.toArray(new ContentLine[contentLineList.size()]);	//return the content lines we produced	
	}
}
