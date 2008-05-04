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

package com.globalmentor.text.directory;

import java.util.*;

import static com.globalmentor.text.ABNF.*;

import com.globalmentor.java.StringBuilders;
import com.globalmentor.util.*;

/**Utilities for working with directories of type <code>text/directory</code> as
	defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class DirectoryUtilities
{
	/**Creates a directory content line from locale text.
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param localeText The value of the information.
	*/
	public static ContentLine createContentLine(final String profile, final String group, final String name, final LocaledText localeText)
	{
		return createContentLine(profile, group, name, localeText, localeText.getLocale());	//create and return a content line from the locale text and the locale
	}

	/**Creates a directory content line with the given language parameter.
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param value The value of the information.
	@param locale The value to give to the language parameter, or
		<code>null</code> if no language should be specified. 
	@see #setLanguageParamValue
	*/
	public static ContentLine createContentLine(final String profile, final String group, final String name, final Object value, final Locale locale)
	{
		final ContentLine contentLine=new ContentLine(profile, group, name, value);	//create a content line with the value
		if(locale!=null)	//if a locale was specified
			setLanguageParamValue(contentLine.getParamList(), locale);	//set the language of the content line
		return contentLine;	//return the content line we created
	}

	/**Creates a <code>LocaleText</code> by combining the language param, if
		present, with the string value of the given object.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param value The content line value.
	@return An object representing the text and locale of the value
	*/
	public static LocaledText createLocaleTextValue(final List<NameValuePair<String, String>> paramList, final Object value)
	{
		final Locale locale=getLanguageParamValue(paramList);	//get the locale
		return new LocaledText(value.toString(), locale);	//create and return the locale text
	}

	/**The characters that must be escaped in text: CR, LF, '\\', and ','. (Note that CRLF runs should first be replaced with a single LF to prevent duplicate linefeeds.*/
	protected final static char[] TEXT_MATCH_CHARS=new char[]{LF, Directory.TEXT_ESCAPE_CHAR, Directory.VALUE_SEPARATOR_CHAR, CR};

	/**The strings to replace the characters to be escaped in text.*/
	protected final static String[] TEXT_REPLACEMENT_STRINGS=new String[]{Directory.TEXT_ESCAPE_STRING+Directory.TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR, Directory.TEXT_ESCAPE_STRING+Directory.TEXT_ESCAPE_CHAR, Directory.TEXT_ESCAPE_STRING+Directory.VALUE_SEPARATOR_CHAR, Directory.TEXT_ESCAPE_STRING+Directory.TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR};

	/**Encodes a text value.
	<p>CR, LF, and CRLF will be be converted to "\\n"; and '\\' and ',' will be escaped with '\\'.</p>
	@param text The text value to encode.
	@return The encoded text value.
	*/	
	public static String encodeTextValue(final String text)
	{
		final StringBuilder stringBuilder=new StringBuilder(text);	//create a string buffer to use for escaping values
		StringBuilders.replace(stringBuilder, CRLF, "\n");	//replace every occurrence of CRLF with "\n" (there may still be lone CRs or LFs); this will get replaced with "\\n" in the next step
		StringBuilders.replace(stringBuilder, TEXT_MATCH_CHARS, TEXT_REPLACEMENT_STRINGS);	//replace characters with their escaped versions
		return stringBuilder.toString();	//return the resulting string
	}

	/**Decodes a text value.
	<p>"\\n" will be converted to CRLF; "\\," will be converted to ',', and "\\\\" will be converted to '\'.</p>
	@param text The text value to decode.
	@return The decoded text value.
	*/	
	public static String decodeTextValue(final String text)
	{
		final StringBuilder stringBuilder=new StringBuilder(text);	//create a string buffer to use for escaping values
		StringBuilders.replace(stringBuilder, Directory.TEXT_ESCAPE_STRING+Directory.TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR, CRLF);	//replace an escaped linefeed with CRLF
		StringBuilders.replace(stringBuilder, Directory.TEXT_ESCAPE_STRING+Directory.TEXT_ESCAPE_CHAR, String.valueOf(Directory.TEXT_ESCAPE_CHAR));	//replace an escaped backslash with '\\'
		StringBuilders.replace(stringBuilder, Directory.TEXT_ESCAPE_STRING+Directory.VALUE_SEPARATOR_CHAR, String.valueOf(Directory.VALUE_SEPARATOR_CHAR));	//replace an escaped comma with ','
		return stringBuilder.toString();	//return the resulting string
	}

	/**Retrieves the value of the first language parameter as a <code>Locale</code>.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return A locale representing the given language, or <code>null</code> if
		no language is indicated.
	*/
	public static Locale getLanguageParamValue(final List<NameValuePair<String, String>> paramList)
	{
		final String languageValue=getParamValue(paramList, Directory.LANGUAGE_PARAM_NAME);	//get the first language parameter
		return languageValue!=null && languageValue.trim().length()>0	//if there is a language and it isn't just whitespace 
			? Locales.createLocale(languageValue.trim())	//create a locale from the language value
			: null;	//show that there was no language
	}
	
	/**Retrieves the first value of a parameter with the given name.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	@return The value of the first matching parameter, or <code>null</code> if
		there is no matching parameter. 
	*/
	public static String getParamValue(final List<NameValuePair<String, String>> paramList, final String paramName)
	{
		for(final NameValuePair<String, String> parameter:paramList)	//for each parameter
		{
			if(paramName.equals(parameter.getName()))	//if this is the correct parameter
			{
				return parameter.getValue();	//return the parameter value
			}
		}
		return null;	//show that we could not find a matching parameter
	}

	/**Retrieves the values of all parameters with the given name.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	@return The values of all matching parameters. 
	*/
	public static String[] getParamValues(final List<NameValuePair<String, String>> paramList, final String paramName)
	{
		final List<String> paramValueList=new ArrayList<String>(paramList.size());	//create a list to hold parameter values, knowing we won't need room for more parameters than the we were given
		for(final NameValuePair<String, String> parameter:paramList)	//for each parameter
		{
			if(paramName.equals(parameter.getName()))	//if this is the correct parameter
			{
				paramValueList.add(parameter.getValue());	//add the parameter value
			}
		}
		return paramValueList.toArray(new String[paramValueList.size()]);	//return an array version of the list of values
	}

	/**An iterator to stop through all parameters with a given name.*/
/*G***del; this is too complicated, and wouldn't save us from creating an object 
	private static ParamIterator implements Iterator
	{

		
		
	}
*/

	/**Removes all parameters with the given name.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	*/
	public static void removeParams(final List<NameValuePair<String, String>> paramList, final String paramName)
	{
		final Iterator<NameValuePair<String, String>> paramIterator=paramList.iterator();	//get an iterator to the parameters
		while(paramIterator.hasNext())	//while there are more parameters
		{
			final NameValuePair<String, String> parameter=paramIterator.next();	//get the next parameter name/value pair
			if(paramName.equals(parameter.getName()))	//if this is the correct parameter
			{
				paramIterator.remove();	//remove this parameter
			}
		}
	}

	/**Sets the language parameter to the value of a <code>Locale</code>.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param locale The value to give to the language parameter. 
	*/
	public static void setLanguageParamValue(final List<NameValuePair<String, String>> paramList, final Locale locale)
	{
		setParamValue(paramList, Directory.LANGUAGE_PARAM_NAME, Locales.getLanguageTag(locale));	//store the language tag representation of the locale as the value of the language parameter 
	}

	/**Removes all parameters with the given name and adds a new
		parameter with the given value.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	@param paramValue The value to give to the added parameter. 
	@see #removeParams
	@see #addParam
	*/
	public static void setParamValue(final List<NameValuePair<String, String>> paramList, final String paramName, final String paramValue)
	{
		removeParams(paramList, paramName);	//remove all parameters with the given name
		addParam(paramList, paramName, paramValue);	//add the param name and value
	}

	/**Adds a new	parameter with the given value.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	@param paramValue The value to give to the added parameter. 
	*/
	public static void addParam(final List<NameValuePair<String, String>> paramList, final String paramName, final String paramValue)
	{
		paramList.add(new NameValuePair<String, String>(paramName, paramValue));	//create a name value pair with the given name and value
	}

}
