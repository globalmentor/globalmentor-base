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
/*G***fix		
		if(TEXT_VALUE_TYPE.equalsIgnoreCase(valueType))	//if this is the "text" value type
		{
			return processTextValueList(reader);	//process the text value
		}
*/
		return null;	//show that we can't create a value
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
