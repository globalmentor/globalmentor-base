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

import static com.globalmentor.text.ABNF.*;

import java.util.*;

import com.globalmentor.java.Characters;
import com.globalmentor.java.StringBuilders;
import com.globalmentor.model.*;
import com.globalmentor.net.ContentType;

/**
 * A directory of type <code>text/directory</code> as defined in <a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
 * "A MIME Content-Type for Directory Information".
 * @author Garret Wilson
 */
public class Directory
{

	/** The content type for directories: <code>text/directory</code>. */
	public final static ContentType CONTENT_TYPE = ContentType.getInstance(ContentType.TEXT_PRIMARY_TYPE, "directory");

	/**
	 * Whitespace as defined by RFC 2425: "space, ASCII decimal 32, or horizontal tab, ASCII decimal 9".
	 */
	public final static Characters WHITESPACE_CHARACTERS = new Characters((char)32, (char)9);

	/** The recommended character length greater than which a line should be folded. */
	public final static int LONG_LINE_LENGTH = 75;

	/** The character separating the group from the name in a content line. */
	public final static char GROUP_NAME_SEPARATOR_CHAR = '.';

	/** The character separating parameters in a content line. */
	public final static char PARAM_SEPARATOR_CHAR = ';';

	/** The character separating a name from a value in a content line. */
	public final static char NAME_VALUE_SEPARATOR_CHAR = ':';

	/** The character separating a parameter name from a parameter value in a content line. */
	public final static char PARAM_NAME_VALUE_SEPARATOR_CHAR = '=';

	/** The character separating multiple parameter values in a content line. */
	public final static char PARAM_VALUE_SEPARATOR_CHAR = ',';

	/** The character used to separate multiple values. */
	public final static char VALUE_SEPARATOR_CHAR = ',';

	//predefined types

	/**
	 * The type to identify the source of directory information contained in the content type.
	 * @see #URI_VALUE_TYPE
	 */
	public final static String SOURCE_TYPE = "SOURCE";

	/**
	 * The type to identify the displayable name of the directory entity to which information in the content type pertains.
	 * @see #TEXT_VALUE_TYPE
	 */
	public final static String NAME_TYPE = "NAME";

	/**
	 * The type to identify the type of directory entity to which information in the content type pertains.
	 * @see #TEXT_VALUE_TYPE
	 */
	public final static String PROFILE_TYPE = "PROFILE";

	/**
	 * The type to denote the beginning of a syntactic entity within a text/directory content-type.
	 * @see #TEXT_VALUE_TYPE
	 */
	public final static String BEGIN_TYPE = "BEGIN";

	/**
	 * The type to denote the end of a syntactic entity within a text/directory content-type.
	 * @see #TEXT_VALUE_TYPE
	 */
	public final static String END_TYPE = "END";

	//predefined parameters and predefined value types

	/** The encoding predefined type. */
	public final static String ENCODING_PARAM_NAME = "encoding";

	/** The binary encoding type from RFC 2047. */
	public final static String B_ENCODING_TYPE = "b"; //TODO check to see how this relates to RFC 2047, and if this constant should be defined elsewhere	

	/** The value type predefined type. */
	public final static String VALUE_PARAM_NAME = "value";

	/** The generic URI value type from section 5 of RFC 1738. */
	public final static String URI_VALUE_TYPE = "uri"; //TODO check to see how this relates to RFC 1738, and why the test/directory specification says "genericurl"

	/** The text value type. */
	public final static String TEXT_VALUE_TYPE = "text";

	/** The character used to escape characters in text values ('\\'). */
	public final static char TEXT_ESCAPE_CHAR = 92;

	/** The string of a single character used to escape characters in text values ("\\"). */
	public final static String TEXT_ESCAPE_STRING = String.valueOf(TEXT_ESCAPE_CHAR);

	/** The lowercase version of an escaped line break in text ('n'). */
	public final static char TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR = 110;

	/** The uppercase version of an escaped line break in text ('N'). */
	public final static char TEXT_LINE_BREAK_ESCAPED_UPPERCASE_CHAR = 78;

	/** The date value type. */
	public final static String DATE_VALUE_TYPE = "date";

	/** The time value type. */
	public final static String TIME_VALUE_TYPE = "time";

	/** The date/time value type. */
	public final static String DATE_TIME_VALUE_TYPE = "date-time";

	/** The integer value type. */
	public final static String INTEGER_VALUE_TYPE = "integer";

	/** The boolean value type. */
	public final static String BOOLEAN_VALUE_TYPE = "boolean";

	/** The float value type. */
	public final static String FLOAT_VALUE_TYPE = "float";

	/** The language parameter type, as defined by RFC 1766. */
	public final static String LANGUAGE_PARAM_NAME = "language";

	/** The context parameter type. */
	public final static String CONTEXT_PARAM_NAME = "context";

	//content lines

	/**
	 * Creates a directory content line from locale text.
	 * @param profile The profile of this content line, or <code>null</code> if there is no profile.
	 * @param group The group specification, or <code>null</code> if there is no group.
	 * @param name The name of the information.
	 * @param localeText The value of the information.
	 */
	public static ContentLine createContentLine(final String profile, final String group, final String name, final LocaledText localeText)
	{
		return createContentLine(profile, group, name, localeText, localeText.getLocale()); //create and return a content line from the locale text and the locale
	}

	/**
	 * Creates a directory content line with the given language parameter.
	 * @param profile The profile of this content line, or <code>null</code> if there is no profile.
	 * @param group The group specification, or <code>null</code> if there is no group.
	 * @param name The name of the information.
	 * @param value The value of the information.
	 * @param locale The value to give to the language parameter, or <code>null</code> if no language should be specified.
	 * @see #setLanguageParamValue
	 */
	public static ContentLine createContentLine(final String profile, final String group, final String name, final Object value, final Locale locale)
	{
		final ContentLine contentLine = new ContentLine(profile, group, name, value); //create a content line with the value
		if(locale != null) //if a locale was specified
			setLanguageParamValue(contentLine.getParamList(), locale); //set the language of the content line
		return contentLine; //return the content line we created
	}

	/**
	 * Creates a <code>LocaleText</code> by combining the language param, if present, with the string value of the given object.
	 * @param paramList The list of parameters, each item of which is a <code>NameValuePair</code> with a name of type <code>String</code> and a value of type
	 *          <code>String</code>.
	 * @param value The content line value.
	 * @return An object representing the text and locale of the value
	 */
	public static LocaledText createLocaleTextValue(final List<NameValuePair<String, String>> paramList, final Object value)
	{
		final Locale locale = getLanguageParamValue(paramList); //get the locale
		return new LocaledText(value.toString(), locale); //create and return the locale text
	}
	
	//parameters

	/**
	 * Retrieves the first value of a parameter with the given name.
	 * @param paramList The list of parameters.
	 * @param paramName The name of the parameter, which will be matched against available parameters in a case insensitive way.
	 * @return The value of the first matching parameter, or <code>null</code> if there is no matching parameter.
	 */
	public static String getParamValue(final List<NameValuePair<String, String>> paramList, final String paramName)
	{
		for(final NameValuePair<String, String> parameter : paramList) //for each parameter
		{
			if(paramName.equals(parameter.getName())) //if this is the correct parameter
			{
				return parameter.getValue(); //return the parameter value
			}
		}
		return null; //show that we could not find a matching parameter
	}

	/**
	 * Retrieves the values of all parameters with the given name.
	 * @param paramList The list of parameters.
	 * @param paramName The name of the parameter, which will be matched against available parameters in a case insensitive way.
	 * @return The values of all matching parameters.
	 */
	public static String[] getParamValues(final List<NameValuePair<String, String>> paramList, final String paramName)
	{
		final List<String> paramValueList = new ArrayList<String>(paramList.size()); //create a list to hold parameter values, knowing we won't need room for more parameters than the we were given
		for(final NameValuePair<String, String> parameter : paramList) //for each parameter
		{
			if(paramName.equals(parameter.getName())) //if this is the correct parameter
			{
				paramValueList.add(parameter.getValue()); //add the parameter value
			}
		}
		return paramValueList.toArray(new String[paramValueList.size()]); //return an array version of the list of values
	}

	/**
	 * Removes all parameters with the given name.
	 * @param paramList The list of parameters.
	 * @param paramName The name of the parameter, which will be matched against available parameters in a case insensitive way.
	 */
	public static void removeParams(final List<NameValuePair<String, String>> paramList, final String paramName)
	{
		final Iterator<NameValuePair<String, String>> paramIterator = paramList.iterator(); //get an iterator to the parameters
		while(paramIterator.hasNext()) //while there are more parameters
		{
			final NameValuePair<String, String> parameter = paramIterator.next(); //get the next parameter name/value pair
			if(paramName.equals(parameter.getName())) //if this is the correct parameter
			{
				paramIterator.remove(); //remove this parameter
			}
		}
	}

	/**
	 * Removes all parameters with the given name and adds a new parameter with the given value.
	 * @param paramList The list of parameters.
	 * @param paramName The name of the parameter, which will be matched against available parameters in a case insensitive way.
	 * @param paramValue The value to give to the added parameter.
	 * @see #removeParams(List, String)
	 * @see #addParam(List, String, String)
	 */
	public static void setParamValue(final List<NameValuePair<String, String>> paramList, final String paramName, final String paramValue)
	{
		removeParams(paramList, paramName); //remove all parameters with the given name
		addParam(paramList, paramName, paramValue); //add the param name and value
	}

	/**
	 * Adds a new parameter with the given value.
	 * @param paramList The list of parameters.
	 * @param paramName The name of the parameter, which will be matched against available parameters in a case insensitive way.
	 * @param paramValue The value to give to the added parameter.
	 */
	public static void addParam(final List<NameValuePair<String, String>> paramList, final String paramName, final String paramValue)
	{
		paramList.add(new NameValuePair<String, String>(paramName, paramValue)); //create a name value pair with the given name and value
	}

	//language parameter

	/**
	 * Retrieves the value of the first language parameter as a <code>Locale</code>.
	 * @param paramList The list of parameters.
	 * @return A locale representing the given language, or <code>null</code> if no language is indicated.
	 */
	public static Locale getLanguageParamValue(final List<NameValuePair<String, String>> paramList)
	{
		final String languageValue = getParamValue(paramList, LANGUAGE_PARAM_NAME); //get the first language parameter
		return languageValue != null && languageValue.trim().length() > 0 //if there is a language and it isn't just whitespace 
		? Locales.createLocale(languageValue.trim()) //create a locale from the language value
				: null; //show that there was no language
	}

	/**
	 * Sets the language parameter to the value of a {@link Locale}.
	 * @param paramList The list of parameters.
	 * @param locale The value to give to the language parameter.
	 */
	public static void setLanguageParamValue(final List<NameValuePair<String, String>> paramList, final Locale locale)
	{
		setParamValue(paramList, LANGUAGE_PARAM_NAME, Locales.getLanguageTag(locale)); //store the language tag representation of the locale as the value of the language parameter 
	}

	//text
	
	/**
	 * The characters that must be escaped in text: CR, LF, '\\', and ','. (Note that CRLF runs should first be replaced with a single LF to prevent duplicate
	 * linefeeds.
	 */
	protected final static char[] TEXT_MATCH_CHARS = new char[] { LF, TEXT_ESCAPE_CHAR, VALUE_SEPARATOR_CHAR, CR };

	/** The strings to replace the characters to be escaped in text. */
	protected final static String[] TEXT_REPLACEMENT_STRINGS = new String[] { TEXT_ESCAPE_STRING + TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR,
			TEXT_ESCAPE_STRING + TEXT_ESCAPE_CHAR, TEXT_ESCAPE_STRING + VALUE_SEPARATOR_CHAR,
			TEXT_ESCAPE_STRING + TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR };

	/**
	 * Encodes a text value.
	 * <p>
	 * CR, LF, and CRLF will be be converted to "\\n"; and '\\' and ',' will be escaped with '\\'.
	 * </p>
	 * @param text The text value to encode.
	 * @return The encoded text value.
	 */
	public static String encodeTextValue(final String text)
	{
		final StringBuilder stringBuilder = new StringBuilder(text); //create a string buffer to use for escaping values
		StringBuilders.replace(stringBuilder, CRLF, "\n"); //replace every occurrence of CRLF with "\n" (there may still be lone CRs or LFs); this will get replaced with "\\n" in the next step
		StringBuilders.replace(stringBuilder, TEXT_MATCH_CHARS, TEXT_REPLACEMENT_STRINGS); //replace characters with their escaped versions
		return stringBuilder.toString(); //return the resulting string
	}

	/**
	 * Decodes a text value.
	 * <p>
	 * "\\n" will be converted to CRLF; "\\," will be converted to ',', and "\\\\" will be converted to '\'.
	 * </p>
	 * @param text The text value to decode.
	 * @return The decoded text value.
	 */
	public static String decodeTextValue(final String text)
	{
		final StringBuilder stringBuilder = new StringBuilder(text); //create a string buffer to use for escaping values
		StringBuilders.replace(stringBuilder, TEXT_ESCAPE_STRING + TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR, CRLF); //replace an escaped linefeed with CRLF
		StringBuilders.replace(stringBuilder, TEXT_ESCAPE_STRING + TEXT_ESCAPE_CHAR, String.valueOf(TEXT_ESCAPE_CHAR)); //replace an escaped backslash with '\\'
		StringBuilders.replace(stringBuilder, TEXT_ESCAPE_STRING + VALUE_SEPARATOR_CHAR, String.valueOf(VALUE_SEPARATOR_CHAR)); //replace an escaped comma with ','
		return stringBuilder.toString(); //return the resulting string
	}
	

	/** The display name of the directory. */
	private LocaledText displayName = null;

	/** @return The display name of the directory. */
	public LocaledText getDisplayName()
	{
		return displayName;
	}

	/**
	 * Sets the display name of the directory.
	 * @param displayName The new display name of the directory.
	 */
	public void setDisplayName(final LocaledText displayName)
	{
		this.displayName = displayName;
	}

	/** The list of content lines that represent unrecognized and/or unprocessed information. */
	private final List<ContentLine> contentLineList = new ArrayList<ContentLine>();

	/** @return The list of content lines that represent unrecognized and/or unprocessed information. */
	public List<ContentLine> getContentLineList()
	{
		return contentLineList;
	}

	/** Default constructor. */
	public Directory()
	{
	}

}
