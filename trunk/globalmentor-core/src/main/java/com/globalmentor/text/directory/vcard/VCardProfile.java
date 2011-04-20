/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text.directory.vcard;

import java.io.*;
import java.net.*;
import java.util.*;

import static com.globalmentor.io.ReaderParser.*;
import static com.globalmentor.text.ABNF.*;
import static com.globalmentor.text.directory.Directory.*;
import static com.globalmentor.text.directory.vcard.VCard.*;
import static java.util.Arrays.asList;

import com.globalmentor.io.*;
import com.globalmentor.java.*;
import com.globalmentor.model.LocaledText;
import com.globalmentor.model.NameValuePair;
import com.globalmentor.text.ArgumentSyntaxException;
import com.globalmentor.text.directory.*;
import com.globalmentor.urf.AbstractURFDateTime;

/**
 * Class that can create values for the "VCARD" profile of a <code>text/directory</code>as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC
 * 2426</a>, "vCard MIME Directory Profile".
 * <p>
 * The processor knows how to process the vCard types: <code>BINARY_VALUE_TYPE</code>, <code>VCARD_VALUE_TYPE</code>, <code>PHONE_NUMBER_VALUE_TYPE</code>, and
 * <code>UTC_OFFSET_VALUE_TYPE</code>.
 * </p>
 * @author Garret Wilson
 */
public class VCardProfile extends AbstractProfile implements ValueFactory, ValueSerializer
{

	/** Default constructor. */
	public VCardProfile()
	{
		//identification types
		registerValueType(FN_TYPE, TEXT_VALUE_TYPE); //FN: text
		registerValueType(N_TYPE, null); //N: structured text
		registerValueType(NICKNAME_TYPE, TEXT_VALUE_TYPE); //NICKNAME: text
		registerValueType(PHOTO_TYPE, BINARY_VALUE_TYPE); //PHOTO: binary
		registerValueType(BDAY_TYPE, null); //BDAY: date, optionally date-time
		//delivery addressing types
		registerValueType(ADR_TYPE, null); //ADR: structured text
		registerValueType(LABEL_TYPE, TEXT_VALUE_TYPE); //LABEL: text
		//telecommunications addressing types
		registerValueType(TEL_TYPE, PHONE_NUMBER_VALUE_TYPE); //TEL: phone-number
		registerValueType(EMAIL_TYPE, TEXT_VALUE_TYPE); //EMAIL: text
		registerValueType(MAILER_TYPE, TEXT_VALUE_TYPE); //MAILER: text
		//geographical types
		registerValueType(TZ_TYPE, UTC_OFFSET_VALUE_TYPE); //TZ: utc-offset
		registerValueType(GEO_TYPE, null); //GEO: two floats		
		//organizational types
		registerValueType(TITLE_TYPE, TEXT_VALUE_TYPE); //TITLE: text
		registerValueType(ROLE_TYPE, TEXT_VALUE_TYPE); //ROLE: text
		registerValueType(LOGO_TYPE, BINARY_VALUE_TYPE); //LOGO: binary
		registerValueType(AGENT_TYPE, VCARD_VALUE_TYPE); //AGENT: vcard
		registerValueType(ORG_TYPE, null); //ORG: structured text
		//explanatory types
		registerValueType(CATEGORIES_TYPE, TEXT_VALUE_TYPE); //CATEGORIES: text
		registerValueType(NOTE_TYPE, TEXT_VALUE_TYPE); //NOTE: text
		registerValueType(PRODID_TYPE, TEXT_VALUE_TYPE); //PRODID: text
		registerValueType(REV_TYPE, DATE_TIME_VALUE_TYPE); //REV: date-time
		registerValueType(SORT_STRING_TYPE, Directory.TEXT_VALUE_TYPE); //SORT-STRING: text
		registerValueType(SOUND_TYPE, BINARY_VALUE_TYPE); //SOUND: binary
		registerValueType(UID_TYPE, TEXT_VALUE_TYPE); //UID: text
		registerValueType(URL_TYPE, URI_VALUE_TYPE); //URL: uri
		registerValueType(VERSION_TYPE, TEXT_VALUE_TYPE); //VERSION: text
		//security types
		registerValueType(CLASS_TYPE, TEXT_VALUE_TYPE); //CLASS: text
		registerValueType(KEY_TYPE, BINARY_VALUE_TYPE); //KEY: binary
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This method knows how to create vCard types, which, along with the objects returned, are as follows:
	 * </p>
	 * <ul>
	 * <li><code>FN_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>N_TYPE</code> <code>Name</code></li>
	 * <li><code>NICKNAME_TYPE</code> <code>String</code></li>
	 * <li><code>PHOTO_TYPE</code></li>
	 * <li><code>BDAY_TYPE</code></li>
	 * <li><code>ADR_TYPE</code> <code>Address</code></li>
	 * <li><code>LABEL_TYPE</code> <code>Label</code></li>
	 * <li><code>TEL_TYPE</code> <code>Telephone</code></li>
	 * <li><code>EMAIL_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>MAILER_TYPE</code></li>
	 * <li><code>TZ_TYPE</code></li>
	 * <li><code>GEO_TYPE</code></li>
	 * <li><code>TITLE_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>ROLE_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>LOGO_TYPE</code></li>
	 * <li><code>AGENT_TYPE</code></li>
	 * <li><code>ORG_TYPE</code> <code>LocaleText[]</code></li>
	 * <li><code>CATEGORIES_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>NOTE_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>PRODID_TYPE</code></li>
	 * <li><code>REV_TYPE</code></li>
	 * <li><code>SORT_STRING_TYPE</code></li>
	 * <li><code>SOUND_TYPE</code></li>
	 * <li><code>UID_TYPE</code></li>
	 * <li><code>URL_TYPE</code></li>
	 * <li><code>VERSION_TYPE</code></li>
	 * <li><code>CLASS_TYPE</code></li>
	 * <li><code>KEY_TYPE</code></li>
	 * </ul>
	 */
	public Object[] createValues(final String profile, final String group, final String name, final List<NameValuePair<String, String>> paramList,
			final String valueType, final Reader reader) throws IOException, ParseIOException
	{
		//see if we recognize the value type
		if(PHONE_NUMBER_VALUE_TYPE.equalsIgnoreCase(valueType)) //phone-number
		{
			return new Object[] { processPhoneNumberValue(reader, paramList) }; //process the phone number value type			
		}
		//see if we recognize the type name		
		//identification types
		if(N_TYPE.equalsIgnoreCase(name)) //N
		{
			return new Object[] { processNValue(reader, paramList) }; //process the N value
		}
		else if(BDAY_TYPE.equalsIgnoreCase(name)) //BDAY
		{
			return new Object[] { AbstractURFDateTime.valueOfLiberal(reach(reader, CR)) }; //a birthday should normally be a date, but sometimes it could be a date-time as well
		}
		//delivery addressing types
		else if(ADR_TYPE.equalsIgnoreCase(name)) //ADR
		{
			return new Object[] { processADRValue(reader, paramList) }; //process the ADR value
		}
		else if(LABEL_TYPE.equalsIgnoreCase(name)) //LABEL
		{
			final LocaledText[] localeTexts = PredefinedProfile.processTextValueList(reader, paramList); //process the text values

			final Set<Address.Type> addressTypes = EnumSet.noneOf(Address.Type.class); //we'll determine the address types
			List<String> typeStrings = getParamValues(paramList, TYPE_PARAM_NAME); //get the address types specified
			if(typeStrings.isEmpty()) //if no types were given, see if bare parameter names were given, in case some producers provide types as bare names instead of in the form TYPE=XXX
			{
				typeStrings = getParamNamesByValue(paramList, null);
			}
			for(final String typeString : typeStrings)
			{
				try
				{
					addressTypes.add(Address.Type.valueOf(typeString.toUpperCase()));
				}
				catch(final IllegalArgumentException illegalArgumentException)
				{
					throw new ParseIOException("Unrecognized address type: " + typeString, illegalArgumentException);
				}
			}
			final Label[] labels = new Label[localeTexts.length]; //create a new array of labels
			for(int i = localeTexts.length - 1; i >= 0; --i) //look at each locale text object
			{
				labels[i] = new Label(localeTexts[i], addressTypes); //create a label from the locale text
			}
			return labels; //return the labels we constructed from the locale test information
		}
		//organizational types
		else if(ORG_TYPE.equalsIgnoreCase(name)) //ORG
		{
			return new Object[] { processORGValue(reader, paramList) }; //process the ORG value
		}
		return null; //show that we can't create a value
	}

	/**
	 * Processes the value for the <code>N</code> type name.
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * @param reader The reader that contains the lines of the directory.
	 * @param paramList The list of parameters; a <code>null</code> value indicates that the name/value pair contained only a name.
	 * @return An object representing the vCard structured name.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 */
	public static Name processNValue(final Reader reader, final List<NameValuePair<String, String>> paramList) throws IOException, ParseIOException
	{
		final Locale locale = getLanguageParamValue(paramList); //get the language, if there is one
		final String[][] fields = processStructuredTextValue(reader); //process the structured text fields
		final String[] familyNames = fields.length > 0 ? fields[0] : new String[] {}; //get the family names, if present
		final String[] givenNames = fields.length > 1 ? fields[1] : new String[] {}; //get the given names, if present
		final String[] additionalNames = fields.length > 2 ? fields[2] : new String[] {}; //get the additional names, if present
		final String[] honorificPrefixes = fields.length > 3 ? fields[3] : new String[] {}; //get the honorific prefixes, if present
		final String[] honorificSuffixes = fields.length > 4 ? fields[4] : new String[] {}; //get the honorific suffixes, if present
		return new Name(familyNames, givenNames, additionalNames, honorificPrefixes, honorificSuffixes, locale); //create and return a vCard name object with the parsed information
	}

	/**
	 * Processes the value for the <code>ADR</code> type name.
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * @param reader The reader that contains the lines of the directory.
	 * @param paramList The list of parameters; a <code>null</code> value indicates that the name/value pair contained only a name.
	 * @return An address object representing the value.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 */
	public static Address processADRValue(final Reader reader, final List<NameValuePair<String, String>> paramList) throws IOException, ParseIOException
	{
		final Locale locale = getLanguageParamValue(paramList); //get the language, if there is one
		final Set<Address.Type> addressTypes = EnumSet.noneOf(Address.Type.class); //we'll determine the address types
		List<String> typeStrings = getParamValues(paramList, TYPE_PARAM_NAME); //get the address types specified
		if(typeStrings.isEmpty()) //if no types were given, see if bare parameter names were given, in case some producers provide types as bare names instead of in the form TYPE=XXX
		{
			typeStrings = getParamNamesByValue(paramList, null);
		}
		for(final String typeString : typeStrings)
		{
			try
			{
				addressTypes.add(Address.Type.valueOf(typeString.toUpperCase()));
			}
			catch(final IllegalArgumentException illegalArgumentException)
			{
				throw new ParseIOException("Unrecognized address type: " + typeString, illegalArgumentException);
			}
		}
		final String[][] fields = processStructuredTextValue(reader); //process the structured text fields
		final String postOfficeBox = fields.length > 0 && fields[0].length > 0 ? fields[0][0] : null; //get the post office box, if present
		final String[] extendedAddresses = fields.length > 1 ? fields[1] : new String[] {}; //get the extended addresses, if present
		final String[] streetAddresses = fields.length > 2 ? fields[2] : new String[] {}; //get the street addresses, if present
		final String locality = fields.length > 3 && fields[3].length > 0 ? fields[3][0] : null; //get the locality, if present
		final String region = fields.length > 4 && fields[4].length > 0 ? fields[4][0] : null; //get the region, if present
		final String postalCode = fields.length > 5 && fields[5].length > 0 ? fields[5][0] : null; //get the postal code, if present
		final String countryName = fields.length > 6 && fields[6].length > 0 ? fields[6][0] : null; //get the country name, if present
		return new Address(postOfficeBox, asList(extendedAddresses), asList(streetAddresses), locality, region, postalCode, countryName, addressTypes, locale); //create and return a vCard address with the parsed information
	}

	/**
	 * Processes the value for the <code>TEL</code> type name.
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * @param reader The reader that contains the lines of the directory.
	 * @param paramList The list of parameters; a <code>null</code> value indicates that the name/value pair contained only a name.
	 * @return A telephone object representing the value.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 */
	public static Telephone processPhoneNumberValue(final Reader reader, final List<NameValuePair<String, String>> paramList) throws IOException,
			ParseIOException
	{
		final Locale locale = getLanguageParamValue(paramList); //get the language, if there is one
		final String telephoneNumberString = reach(reader, CR); //read the string representing the telephone number
		final Set<Telephone.Type> telephoneTypes = EnumSet.noneOf(Telephone.Type.class); //we'll determine the telephone types
		List<String> typeStrings = getParamValues(paramList, TYPE_PARAM_NAME); //get the telephone types specified
		//some VCard producers, such as Nokia, provide types as bare parameter names instead of in the form TYPE=XXX
		if(typeStrings.isEmpty()) //if no telephone types were given, see if bare parameter names were given
		{
			typeStrings = getParamNamesByValue(paramList, null);
		}
		for(final String typeString : typeStrings)
		{
			try
			{
				telephoneTypes.add(Telephone.Type.valueOf(typeString.toUpperCase()));
			}
			catch(final IllegalArgumentException illegalArgumentException)
			{
				throw new ParseIOException("Unrecognized telephone type: " + typeString, illegalArgumentException);
			}
		}
		try
		{
			return new Telephone(telephoneNumberString, telephoneTypes); //create a telephone from the telephone number and telephone type
		}
		catch(final ArgumentSyntaxException syntaxException) //if the telephone number was not syntactically correct
		{
			throw new ParseIOException(reader, syntaxException); //create an I/O parse exception from the telephone number syntax exception
		}
	}

	/**
	 * Processes the value for the <code>ORG</code> type name.
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * @param reader The reader that contains the lines of the directory.
	 * @param paramList The list of parameters; a <code>null</code> value indicates that the name/value pair contained only a name.
	 * @return An array of locale text objects representing the organizational name and units.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 */
	public static LocaledText[] processORGValue(final Reader reader, final List<NameValuePair<String, String>> paramList) throws IOException, ParseIOException
	{
		final Locale locale = getLanguageParamValue(paramList); //get the language, if there is one
		final List<LocaledText> orgList = new ArrayList<LocaledText>(); //create a list into which we will place the organizational name and units, as we'll ignore any structured components that are empty 
		final String[][] fields = processStructuredTextValue(reader); //process the structured text fields
		for(int i = 0; i < fields.length; ++i) //look at each field
		{
			for(int j = 0; j < fields[i].length; ++j) //look at each field value (this isn't in the specification, but it won't hurt to add these values within each field to keep from losing them, even if they shouldn't be there)
			{
				orgList.add(new LocaledText(fields[i][j], locale)); //add this field value as either the organization name or an organizational unit
			}
		}
		return orgList.toArray(new LocaledText[orgList.size()]); //return an array version of the organization component list
	}

	/**
	 * Processes structured text into an array of string arrays. Structured text is series of fields separated by ';', each field of which can have multiple
	 * values separated by ','.
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * @param reader The reader that contains the lines of the directory.
	 * @return An array of string arrays, each string array representing the values of each field.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 */
	public static String[][] processStructuredTextValue(final Reader reader) throws IOException, ParseIOException
	{
		final List<String[]> fieldList = new ArrayList<String[]>(); //create a new list to hold the string arrays we find
		char delimiter; //we'll store the last delimiter peeked		
		do
		{
			final String[] values = processStructuredTextFieldValue(reader); //process this field
			fieldList.add(values); //add the strings of the field to our list			
			delimiter = peek(reader); //see what character is next
			if(delimiter != CR) //if we haven't reached the end of the value
				reader.skip(1); //skip the delimiter
		}
		while(delimiter == STRUCTURED_TEXT_VALUE_DELIMITER); //keep getting fields while we are still running into structured text value separators
		return fieldList.toArray(new String[fieldList.size()][]); //convert the list of string arrays to an array of string arrays and return the array
	}

	/** The delimiters that can divide a structured text value: '\\', ';' ',' and CR. */
	protected final static Characters STRUCTURED_TEXT_VALUE_DELIMITER_CHARACTERS = new Characters(TEXT_ESCAPE_CHAR, STRUCTURED_TEXT_VALUE_DELIMITER,
			VALUE_SEPARATOR_CHAR, CR);

	/**
	 * Processes a single field of a structured text value.
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * <p>
	 * The sequence "\n" or "\N" will be converted to a single newline character, '\n', and '\\', ';', and ',' must be escaped with '\\'.
	 * </p>
	 * @param reader The reader that contains the lines of the directory.
	 * @return An array of strings representing the values of the field.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 */
	public static String[] processStructuredTextFieldValue(final Reader reader) throws IOException, ParseIOException
	{
		final List<String> valueList = new ArrayList<String>(); //create a new list to hold the values we find
		final StringBuilder stringBuilder = new StringBuilder(); //create a string builder to hold whatever string we're processing
		char delimiter; //we'll store the last delimiter peeked		
		do
		{
			stringBuilder.append(reach(reader, STRUCTURED_TEXT_VALUE_DELIMITER_CHARACTERS)); //read all value characters until we find a delimiter, and add the value so far to the string buffer
			//TODO del when works			valueList.add(reader.readStringUntilChar(STRUCTURED_TEXT_VALUE_DELIMITER_CHARS));	//read all value characters until we find a delimiter, and add that value to the list
			delimiter = peek(reader); //see what the delimiter will be
			switch(delimiter)
			//see which delimiter we found
			{
				case VALUE_SEPARATOR_CHAR: //if this is the character separating multiple values (',')
					valueList.add(stringBuilder.toString()); //add the value we collected
					stringBuilder.delete(0, stringBuilder.length()); //clear the string buffer
					reader.skip(1); //skip the delimiter
					break;
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
						case '\\':
						case ';':
						case ',':
							stringBuilder.append(escapedChar); //escaped backslashes and commas get appended normally
							break;
						default: //if something else was escaped, we don't recognize it
							throw new ParseUnexpectedDataException(new Characters('\\', ';', ',', TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR,
									TEXT_LINE_BREAK_ESCAPED_UPPERCASE_CHAR), escapedChar, reader); //show that we didn't expect this character here				
					}
				}
					break;
				case STRUCTURED_TEXT_VALUE_DELIMITER: //if this is the character separating fields in the structured text value (';')
				case CR: //if we just peeked a carriage return
					break; //don't do anything---we'll just collect our characters and leave
				default: //if we peeked anything else (there shouldn't be anything else unless there is a logic error)					
					throw new AssertionError("The only possible values should have been " + STRUCTURED_TEXT_VALUE_DELIMITER_CHARACTERS + "; found "
							+ Characters.getLabel(delimiter));
			}
		}
		while(delimiter != STRUCTURED_TEXT_VALUE_DELIMITER && delimiter != CR); //keep collecting parts of the string until we encounter a ';' or a CR
		valueList.add(stringBuilder.toString()); //add the value we collected
		return valueList.toArray(new String[valueList.size()]); //convert the list of strings to an array of strings return the array
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This method knows how to serialize vCard types, which, along with the objects returned, are as follows:
	 * </p>
	 * <ul>
	 * <li><code>FN_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>N_TYPE</code> <code>Name</code></li>
	 * <li><code>NICKNAME_TYPE</code> <code>String</code></li>
	 * <li><code>PHOTO_TYPE</code></li>
	 * <li><code>BDAY_TYPE</code></li>
	 * <li><code>ADR_TYPE</code> <code>Address</code></li>
	 * <li><code>LABEL_TYPE</code> <code>Label</code></li>
	 * <li><code>TEL_TYPE</code> <code>Telephone</code></li>
	 * <li><code>EMAIL_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>MAILER_TYPE</code></li>
	 * <li><code>TZ_TYPE</code></li>
	 * <li><code>GEO_TYPE</code></li>
	 * <li><code>TITLE_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>ROLE_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>LOGO_TYPE</code></li>
	 * <li><code>AGENT_TYPE</code></li>
	 * <li><code>ORG_TYPE</code> <code>LocaleText[]</code></li>
	 * <li><code>CATEGORIES_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>NOTE_TYPE</code> <code>LocaleText</code></li>
	 * <li><code>PRODID_TYPE</code></li>
	 * <li><code>REV_TYPE</code></li>
	 * <li><code>SORT_STRING_TYPE</code></li>
	 * <li><code>SOUND_TYPE</code></li>
	 * <li><code>UID_TYPE</code></li>
	 * <li><code>URL_TYPE</code></li>
	 * <li><code>VERSION_TYPE</code></li>
	 * <li><code>CLASS_TYPE</code></li>
	 * <li><code>KEY_TYPE</code></li>
	 * </ul>
	 */
	public boolean serializeValue(final String profile, final String group, final String name, final List<NameValuePair<String, String>> paramList,
			final Object value, final String valueType, final Writer writer) throws IOException
	{
		//see if we recognize the value type
		if(PHONE_NUMBER_VALUE_TYPE.equalsIgnoreCase(valueType)) //phone-number
		{
			writer.write(((Telephone)value).getCanonicalString()); //write the canonical version of the phone number
			return true; //show that we serialized the value 
		}
		//see if we recognize the type name		
		//identification types
		if(N_TYPE.equalsIgnoreCase(name)) //N
		{
			serializeNValue((Name)value, writer); //serialize the value
			return true; //show that we serialized the value 
		}
		//delivery addressing types
		else if(ADR_TYPE.equalsIgnoreCase(name)) //ADR
		{
			serializeADRValue((Address)value, writer); //serialize the value
			return true; //show that we serialized the value 
		}
		//organizational types
		else if(ORG_TYPE.equalsIgnoreCase(name)) //ORG
		{
			serializeORGValue((LocaledText[])value, writer); //serialize the value
			return true; //show that we serialized the value 
		}
		return false; //show that we can't serialize the value
	}

	/**
	 * Serializes the value for the <code>N</code> type name.
	 * <p>
	 * Only the value will be serialized, not any previous or subsequent parts of the line or delimiters.
	 * </p>
	 * @param name An object representing the vCard structured name to serialize.
	 * @param writer The writer to which the directory information should be written.
	 * @exception IOException Thrown if there is an error reading the directory.
	 */
	public static void serializeNValue(final Name name, final Writer writer) throws IOException
	{
		//place the field arrays into an array
		final String[][] n = new String[][] { name.getFamilyNames(), name.getGivenNames(), name.getAdditionalNames(), name.getHonorificPrefixes(),
				name.getHonorificSuffixes() };
		serializeStructuredTextValue(n, writer); //serialize the value
	}

	/**
	 * Serializes the value for the <code>ADR</code> type name.
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * <p>
	 * Only the value will be serialized, not any previous or subsequent parts of the line or delimiters.
	 * </p>
	 * @param address An address object representing the value to serialize.
	 * @param name An object representing the vCard structured name to serialize.
	 * @param writer The writer to which the directory information should be written.
	 * @exception IOException Thrown if there is an error reading the directory.
	 */
	public static void serializeADRValue(final Address address, final Writer writer) throws IOException
	{
		//place the field arrays into an array
		final String[][] adr = new String[][] { new String[] { address.getPostOfficeBox() != null ? address.getPostOfficeBox() : "" },
				address.getExtendedAddresses().toArray(new String[address.getExtendedAddresses().size()]),
				address.getStreetAddresses().toArray(new String[address.getStreetAddresses().size()]),
				new String[] { address.getLocality() != null ? address.getLocality() : "" }, new String[] { address.getRegion() != null ? address.getRegion() : "" },
				new String[] { address.getPostalCode() != null ? address.getPostalCode() : "" },
				new String[] { address.getCountryName() != null ? address.getCountryName() : "" } };
		serializeStructuredTextValue(adr, writer); //serialize the value
	}

	/**
	 * Serializes the value for the <code>ORG</code> type name.
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * @param org An array of locale text objects representing the organizational name and units.
	 * @param writer The writer to which the directory information should be written.
	 * @exception IOException Thrown if there is an error reading the directory.
	 */
	public static void serializeORGValue(final LocaledText[] org, final Writer writer) throws IOException
	{
		final String[][] orgFields = new String[org.length][]; //create an array of string arrays
		for(int i = org.length - 1; i >= 0; --i) //look at each of the org components
		{
			orgFields[i] = new String[] { org[i].getText() }; //store the text of this org component in an array representing this field
		}
		serializeStructuredTextValue(orgFields, writer); //serialize the value
	}

	/**
	 * Serializes structured text. Structured text is series of fields separated by ';', each field of which can have multiple values separated by ','.
	 * <p>
	 * Only the value will be serialized, not any previous or subsequent parts of the line or delimiters.
	 * </p>
	 * @param structuredText An array of string arrays, each string array representing the values of each field.
	 * @param writer The writer to which the directory information should be written.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 */
	public static void serializeStructuredTextValue(final String[][] structuredText, final Writer writer) throws IOException
	{
		for(int fieldIndex = 0; fieldIndex < structuredText.length; ++fieldIndex) //look at each structured text field
		{
			final String[] fieldValues = structuredText[fieldIndex]; //get the values for this field
			for(int fieldValueIndex = 0; fieldValueIndex < fieldValues.length; ++fieldValueIndex) //look at each field value
			{
				serializeStructuredTextFieldValue(fieldValues[fieldValueIndex], writer); //write this structured text field value
				if(fieldValueIndex < fieldValues.length - 1) //if this is not the last field value
					writer.write(VALUE_SEPARATOR_CHAR); //write the value separator ','
			}
			if(fieldIndex < structuredText.length - 1) //if this is not the last field
				writer.write(STRUCTURED_TEXT_VALUE_DELIMITER); //write the field separator ';'
		}
	}

	/** The characters that must be escaped in structured text: '\n', '\\', and ','. */
	private final static char[] STRUCTURED_TEXT_MATCH_CHARS = new char[] { '\n', '\\', ';', ',' };

	/** The strings to replace the characters to be escaped in structured text. */
	private final static String[] STRUCTURED_TEXT_REPLACEMENT_STRINGS = new String[] { "\n", "\\", "\\;", "\\," };

	/**
	 * Serializes a structured text field value.
	 * <p>
	 * The newline character '\n' will be be converted to "\n", and and '\\', ';', and ',' will be escaped with '\\'.
	 * </p>
	 * @param text The structured text value to serialize.
	 * @param writer The writer to which the directory information should be written.
	 * @exception IOException Thrown if there is an error writing to the directory.
	 */
	public static void serializeStructuredTextFieldValue(final String text, final Writer writer) throws IOException
	{
		//replace characters with their escaped versions and write the resulting string
		writer.write(Strings.replace(text, STRUCTURED_TEXT_MATCH_CHARS, STRUCTURED_TEXT_REPLACEMENT_STRINGS));
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This version creates a VCard.
	 * </p>
	 */
	public VCard createDirectory(final ContentLine[] contentLines)
	{
		final VCard vcard = new VCard(); //we'll store the vCard information here
		for(int i = 0; i < contentLines.length; ++i) //look at each content line
		{
			final ContentLine contentLine = contentLines[i]; //get a reference to this content line
			final String typeName = contentLine.getName(); //get this content line's type name
			if(BEGIN_TYPE.equalsIgnoreCase(typeName)) //BEGIN
			{
				continue; //ignore begin; don't process this content line further TODO maybe only ignore these if they are the vCard profile
			}
			else if(END_TYPE.equalsIgnoreCase(typeName)) //END
			{
				continue; //ignore end; don't process this content line further
			}
			else if(NAME_TYPE.equalsIgnoreCase(typeName)) //if this is NAME
			{
				if(vcard.getDisplayName() == null) //if the vCard does not yet have a display name
				{
					vcard.setDisplayName((LocaledText)contentLine.getValue()); //set the vCard display name
					continue; //don't process this content line further
				}
			}
			//identification types
			else if(FN_TYPE.equalsIgnoreCase(typeName)) //FN
			{
				if(vcard.getFormattedName() == null) //if there is not yet a formatted name
				{
					vcard.setFormattedName((LocaledText)contentLine.getValue()); //get the formatted name
					continue; //don't process this content line further
				}
			}
			else if(N_TYPE.equalsIgnoreCase(typeName)) //N
			{
				if(vcard.getName() == null) //if there is not yet a name
				{
					vcard.setName((Name)contentLine.getValue()); //get the name
					continue; //don't process this content line further
				}
			}
			else if(NICKNAME_TYPE.equalsIgnoreCase(typeName)) //NICKNAME
			{
				vcard.getNicknames().add((LocaledText)contentLine.getValue()); //add this nickname to our list
				continue; //don't process this content line further
			}
			else if(BDAY_TYPE.equalsIgnoreCase(typeName)) //BDAY
			{
				vcard.setBirthday((AbstractURFDateTime)contentLine.getValue()); //set the birthday
				continue; //don't process this content line further
			}
			//delivery addressing types
			else if(ADR_TYPE.equalsIgnoreCase(typeName)) //ADR
			{
				vcard.getAddresses().add((Address)contentLine.getValue()); //add this address to our list
				continue; //don't process this content line further
			}
			else if(LABEL_TYPE.equalsIgnoreCase(typeName)) //LABEL
			{
				vcard.getLabels().add((Label)contentLine.getValue()); //add this label to our list
				continue; //don't process this content line further
			}
			//telecommunications addressing types
			else if(TEL_TYPE.equalsIgnoreCase(typeName)) //TEL
			{
				vcard.getTelephones().add((Telephone)contentLine.getValue()); //add this telephone to our list
				continue; //don't process this content line further
			}
			else if(EMAIL_TYPE.equalsIgnoreCase(typeName)) //EMAIL
			{
				final Set<Email.Type> emailTypes = EnumSet.noneOf(Email.Type.class); //we'll determine the email types
				List<String> typeStrings = getParamValues(contentLine.getParamList(), TYPE_PARAM_NAME); //get the email types specified
				if(typeStrings.isEmpty()) //if no types were given, see if bare parameter names were given, in case some producers provide types as bare names instead of in the form TYPE=XXX
				{
					typeStrings = getParamNamesByValue(contentLine.getParamList(), null);
				}
				for(final String typeString : typeStrings)
				{
					try
					{
						emailTypes.add(Email.Type.valueOf(typeString.toUpperCase()));
					}
					catch(final IllegalArgumentException illegalArgumentException)
					{
						throw new IllegalArgumentException("Unrecognized email type: " + typeString, illegalArgumentException);
					}
				}
				final Email email = new Email(((LocaledText)contentLine.getValue()).getText(), emailTypes); //create an email from the address and types we parsed
				vcard.getEmails().add(email); //add this email to our list
				continue; //don't process this content line further
			}
			//organizational type
			else if(ORG_TYPE.equalsIgnoreCase(typeName)) //ORG
			{
				@SuppressWarnings("unchecked")
				final List<LocaledText> org = (List<LocaledText>)contentLine.getValue(); //get the organization information
				if(!org.isEmpty() && vcard.getOrganizationName() == null) //if there is an organization name, and we haven't yet stored an organization name
				{
					vcard.setOrganizationName(org.get(0)); //set the organization name from the first organizational component
					if(org.size() > 1) //if there are units specified
					{
						final List<LocaledText> units = new ArrayList<LocaledText>(org); //create a list to contain all the units
						units.remove(0); //remove the first item, the organization name
						vcard.setOrganizationUnits(units.toArray(new LocaledText[units.size()])); //set the vCard units 
					}
					continue; //don't process this content line further
				}
			}
			else if(TITLE_TYPE.equalsIgnoreCase(typeName)) //TITLE
			{
				if(vcard.getTitle() == null) //if there is not yet a title
				{
					//TODO add support for multiple titles with multiple languages
					vcard.setTitle((LocaledText)contentLine.getValue()); //set the title
					continue; //don't process this content line further
				}
			}
			else if(ROLE_TYPE.equalsIgnoreCase(typeName)) //ROLE
			{
				if(vcard.getRole() == null) //if there is not yet a role
				{
					vcard.setRole((LocaledText)contentLine.getValue()); //set the role
					continue; //don't process this content line further
				}
			}
			//explanatory types
			else if(CATEGORIES_TYPE.equalsIgnoreCase(typeName)) //CATEGORIES
			{
				vcard.getCategories().add((LocaledText)contentLine.getValue()); //add this category to our list
				continue; //don't process this content line further
			}
			else if(NOTE_TYPE.equalsIgnoreCase(typeName)) //NOTE
			{
				vcard.getNotes().add((LocaledText)contentLine.getValue()); //add this note to our list
				continue; //don't process this content line further
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
			else if(URL_TYPE.equalsIgnoreCase(typeName)) //URL
			{
				if(vcard.getURL() == null) //if there is not yet a URL
				{
					vcard.setURL((URI)contentLine.getValue()); //set the URL
					continue; //don't process this content line further
				}
			}
			else if(VERSION_TYPE.equalsIgnoreCase(typeName)) //VERSION
			{
				vcard.setVersion(((LocaledText)contentLine.getValue()).getText()); //set the version
				continue; //don't process this content line further
			}
			//if we make it to here, we either don't recognize the content line
			//	or we can't proces it (e.g. a duplicate value we don't support)
			vcard.getContentLineList().add(contentLine); //add this unprocessed content line to the vCard's list of content lines
		}
		return vcard; //return the vCard we created 
	}

	/**
	 * Creates a list of content lines from the given vCard.
	 * <p>
	 * This implementation ignores the version of the given vCard and adds a content line with the version used here: "3.0".
	 * </p>
	 * @param vcard The vCard object to be converted to content lines.
	 * @return The content lines that represent the vCard information.
	 * @see VCard#VCARD_VERSION_VALUE
	 */
	public static ContentLine[] createContentLines(final VCard vcard) //TODO make sure displayName and formattedName are included
	{
		//TODO del		final List contentLineList=new ArrayList(vcard.getContentLineList());	//create a content line list initially containing all the unrecognized content lines of the vCard
		final List<ContentLine> contentLineList = new ArrayList<ContentLine>(); //create a content line list to fill
		contentLineList.add(new ContentLine(BEGIN_TYPE, new LocaledText(VCARD_PROFILE_NAME))); //BEGIN:VCARD
		//ignore the given vCard version, and always create "version:3.0"
		contentLineList.add(new ContentLine(VERSION_TYPE, new LocaledText(VCARD_VERSION_VALUE))); //VERSION
		//predefined directory types
		if(vcard.getDisplayName() != null) //NAME
		{
			contentLineList.add(new ContentLine(NAME_TYPE, vcard.getDisplayName())); //add the display name
			//TODO del			contentLineList.add(0, new ContentLine(NAME_TYPE, vcard.getDisplayName()));	//add the display name at the front of the list
		}
		//identification types
		if(vcard.getFormattedName() != null) //FN
		{
			contentLineList.add(createContentLine(VCARD_PROFILE_NAME, null, FN_TYPE, vcard.getFormattedName())); //FN
		}
		if(vcard.getName() != null) //N
		{
			contentLineList.add(createContentLine(VCARD_PROFILE_NAME, null, N_TYPE, vcard.getName(), vcard.getName().getLocale())); //N
		}
		for(final LocaledText nickname : vcard.getNicknames()) //for each nickname
		{
			contentLineList.add(createContentLine(VCARD_PROFILE_NAME, null, NICKNAME_TYPE, nickname)); //NICKNAME			
		}
		if(vcard.getBirthday() != null) //BDAY
		{
			contentLineList.add(createContentLine(VCARD_PROFILE_NAME, null, BDAY_TYPE, vcard.getBirthday())); //BDAY
		}
		//delivery addressing types
		for(final Address address : vcard.getAddresses()) //for each address
		{
			final ContentLine contentLine = createContentLine(VCARD_PROFILE_NAME, null, ADR_TYPE, address, address.getLocale()); //ADR
			for(final Address.Type addressType : address.getTypes()) //for each address type
			{
				addParam(contentLine.getParamList(), TYPE_PARAM_NAME, addressType.toString()); //add this address type parameter
			}
			contentLineList.add(contentLine); //add the content line
		}
		for(final Label label : vcard.getLabels()) //for each label
		{
			final ContentLine contentLine = createContentLine(VCARD_PROFILE_NAME, null, LABEL_TYPE, label); //LABEL
			for(final Address.Type addressType : label.getAddressTypes()) //for each address type
			{
				addParam(contentLine.getParamList(), TYPE_PARAM_NAME, addressType.toString()); //add this address type parameter
			}
			contentLineList.add(contentLine); //add the content line
		}
		//telecommunications addressing types
		for(final Telephone telephone : vcard.getTelephones()) //for each telephone
		{
			final ContentLine contentLine = new ContentLine(VCARD_PROFILE_NAME, null, TEL_TYPE, telephone); //TEL
			for(final Telephone.Type telephoneType : telephone.getTypes()) //for each telephone type
			{
				addParam(contentLine.getParamList(), TYPE_PARAM_NAME, telephoneType.toString()); //add this telephone type parameter
			}
			contentLineList.add(contentLine); //add the content line
		}
		for(final Email email : vcard.getEmails()) //for each email
		{
			final ContentLine contentLine = createContentLine(VCARD_PROFILE_NAME, null, EMAIL_TYPE, new LocaledText(email.getAddress(), email.getLocale())); //EMAIL TODO maybe fix to store the email object, if that's what we decide to store there when reading the value
			for(final Email.Type emailType : email.getTypes()) //for each email type
			{
				addParam(contentLine.getParamList(), TYPE_PARAM_NAME, emailType.toString()); //add this email type parameter
			}
			contentLineList.add(contentLine); //add the content line
		}
		//organizational type
		final List<LocaledText> org; //we'll create an array for the org or, if there is no org name, just use the units array
		final List<LocaledText> units = vcard.getOrganizationUnits(); //get the organizational units
		if(vcard.getOrganizationName() != null) //if a name is supplied
		{
			org = new ArrayList<LocaledText>(units.size() + 1); //create a new list with room for the name and the units
			org.add(vcard.getOrganizationName()); //store the organization name as the first element
			org.addAll(units); //copy the units into the list array after the organization name
		}
		else
		//if there is no name supplied
		{
			org = units; //just use the units, since there is no organization name
		}
		if(!org.isEmpty()) //ORG
		{
			final Locale orgLocale = org.get(0).getLocale(); //get the locale of the first organization component
			contentLineList.add(createContentLine(VCARD_PROFILE_NAME, null, ORG_TYPE, org, orgLocale)); //ORG
		}
		if(vcard.getTitle() != null) //TITLE
		{
			contentLineList.add(createContentLine(VCARD_PROFILE_NAME, null, TITLE_TYPE, vcard.getTitle())); //TITLE
		}
		if(vcard.getRole() != null) //ROLE
		{
			contentLineList.add(createContentLine(VCARD_PROFILE_NAME, null, ROLE_TYPE, vcard.getRole())); //ROLE
		}
		//explanatory types
		for(final LocaledText category : vcard.getCategories()) //for each category
		{
			contentLineList.add(createContentLine(VCARD_PROFILE_NAME, null, CATEGORIES_TYPE, category)); //CATEGORIES
		}
		for(final LocaledText note : vcard.getNotes()) //for each note
		{
			contentLineList.add(createContentLine(VCARD_PROFILE_NAME, null, NOTE_TYPE, note)); //NOTE
		}
		/*TODO fix when we allow this to be edited
				if(vcard.getSortString()!=null)	//SORT-STRING
				{
					contentLineList.add(createContentLine(VCARD_PROFILE_NAME, null, SORT_STRING_TYPE, vcard.getNote()));	//SORT-STRING
				}
		*/
		if(vcard.getURL() != null) //URL
		{
			contentLineList.add(new ContentLine(VCARD_PROFILE_NAME, null, VCard.URL_TYPE, vcard.getURL())); //URL
		}
		contentLineList.addAll(vcard.getContentLineList()); //add all of our unrecognized content lines
		contentLineList.add(new ContentLine(END_TYPE, new LocaledText(VCARD_PROFILE_NAME))); //END:VCARD
		return (ContentLine[])contentLineList.toArray(new ContentLine[contentLineList.size()]); //return the content lines we produced	
	}
}
