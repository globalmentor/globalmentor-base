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

package com.globalmentor.model;

import java.util.*;
import static java.util.Collections.*;
import java.util.regex.Pattern;

import static com.globalmentor.io.Files.*;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.text.RegularExpressions.*;

/**Utilities for manipulating Java locales.
@author Garret Wilson
@see <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>
*/
public class Locales
{

	/**The character used to separate components in a locale: '_'.*/
	public final static char LOCALE_SEPARATOR='_';

	/**The character '-' used to separate components in language tags as
		defined in <a href="http://www.ietf.org/rfc/rfc14646.txt">RFC 4646</a>,
		"Tags for the Identifying".
	@see <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>
	*/
	public final static char LANGUAGE_TAG_SEPARATOR='-';

	/**The pattern that matches one of three language tag delimiters:
	<ul>
		<li>The underscore character ('_') as represented by {@link Locale}.</li>
		<li>The hyphen characters ('-') as defined in <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>, "Tags for the Identifying Languages".</li>
	</ul>
	@see <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>
	*/
	private final static Pattern LANGUAGE_TAG_DELIMITER_PATTERN=Pattern.compile(createCharacterClass(LOCALE_SEPARATOR, LANGUAGE_TAG_SEPARATOR));
	
	/**Constructs a locale object from a locale string with a language, an
		optional country code, and an optional variant. These components can be
		separated by underscore characters ('_') as represented by
		{@link Locale}, or by hyphen characters ('-')
		as defined in <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>,
		"Tags for Identifying Languages".
	@param localeString The string containing the language, optional country, and optional variant.
	@return A local corresponding to the given local string.
	@exception IllegalArgumentException if the given locale string has more than three components.
	@see <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>
	*/
	public static Locale createLocale(final String localeString)
	{
		String language=""; //the language which we may find
		String country="";  //the country which we may find
		String variant="";  //the variant which we may find
		final String[] tags=LANGUAGE_TAG_DELIMITER_PATTERN.split(localeString);	//split the string into its components
		final int tagCount=tags.length;	//see how many tags there are
		if(tagCount>0) //if there is another part of the locale
		{
			language=tags[0]; //get the language
			if(tagCount>1) //if there is another part of the locale
			{
				country=tags[1]; //get the country
				if(tagCount>2) //if there is another part of the locale
				{
					variant=tags[2]; //get the variant
					if(tagCount>3)	//if there are more tags
					{
						throw new IllegalArgumentException("Locale does not support more than three language tag components.");
					}
				}
			}
		}
		return new Locale(language, country, variant);  //create a locale with the parts we found
	}

	/**Retrieves a locale based upon a given display name in the current local.
	  That is, if the default locale is <code>en_US</code>, the display language
	  "French" will return the local for <code>fr</code>.
	@param displayLanguage The name of a language in the current locale.
	@return The local that matches the given language, or <code>null</code> if
		no locale could be found with the given display language.
	*/
	public static Locale createDisplayLanguageLocale(final String displayLanguage)
	{
		final Locale[] availableLocales=Locale.getAvailableLocales(); //get a list of all available locales
		for(int i=availableLocales.length-1; i>=0; --i) //look at each of the locales
		{
			final Locale locale=availableLocales[i];  //get a reference to this locale
		  if(locale.getDisplayLanguage().equalsIgnoreCase(displayLanguage)) //if the display language of this locale matches the language given
			{
				return createLocale(locale.getLanguage());  //create a locale for just that language
			}
		}
		return null;  //show that we couldn't find a matching language
	}

	/**Retrieves a sorted list of display countries for all available locales.
		The display countries will use the default locale for the localized name.
	@return An array of display country names for installed locales.
	*/
	public static String[] getAvailableDisplayCountries()
	{
		return getAvailableDisplayCountries(Locale.getDefault());	//get available display countries in the default locale
	}
	
	/**Retrieves a sorted list of display countries for all available locales.
		The display countries will use the given locale for the localized name.
	@param inLocale The locale for which the country names should be localized.
	@return An array of display country names for installed locales.
	*/
	public static String[] getAvailableDisplayCountries(final Locale inLocale)
	{
		final Set<String> displayCountrySet=new HashSet<String>();	//create a set to ensure no duplicate country names, as multiple locales could have the same country name
		final Locale[] availableLocales=Locale.getAvailableLocales(); //get a list of all available locales
		for(int i=availableLocales.length-1; i>=0; --i) //look at each of the locales TODO improve; this currently takes a very long time
		{
			final Locale locale=availableLocales[i];  //get a reference to this locale
			displayCountrySet.add(locale.getDisplayCountry(inLocale));	//get the display country for this locale in the given locale, and add it to our list of display countries
		}
		final List<String> sortedDisplayCountryList=new ArrayList<String>(displayCountrySet);	//create a list from the display country set
		sort(sortedDisplayCountryList);	//sort the list of countries
		return (String[])sortedDisplayCountryList.toArray(new String[sortedDisplayCountryList.size()]);	//return a string array from the sorted list
	}

	/**Determines the string to represent a language identifier according as
		defined in <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>,
		"Tags for the Identification of Languages".
	@exception NullPointerException if the given locale is <code>null</code>.
	@see <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>
	*/
	public static String getLanguageTag(final Locale locale)
	{
		return locale.toString().replace(Locales.LOCALE_SEPARATOR, Locales.LANGUAGE_TAG_SEPARATOR);  //replace the locale separator with the RFC 4646 separator
	}

	/**Determines the locale-sensitive path of the given base path based upon a depth index.
	Based upon the provided locale and depth, a candidate resource path is generated as follows:
	<dl>
		<dt>depth 3</dt> <dd> <var>basePath</var> + "_" + <var>language</var> + "_" + <var>country</var> + "_" + <var>variant</var> + "." + <var>extension</var></dd>
		<dt>depth 2</dt> <dd> <var>basePath</var> + "_" + <var>language</var> + "_" + <var>country</var> + "." + <var>extension</var> </dd>
		<dt>depth 1</dt> <dd> <var>basePath</var> + "_" + <var>language</var> + "." + <var>extension</var> </dd>
		<dt>depth 0</dt> <dd> <var>basePath</var> + "." + <var>extension</var> </dd>
	</ol>
	Any extension of the base path will be preserved.
	If the resource does not have sufficient components for the given depth, <code>null</code> will be returned.
	@param basePath The base path for which a candidate path should be generated.
	@param locale The locale to use in generating the candidate path.
	@param depth The depth at which the candidate path should be generated.
	@return The candidate path for provided base path at the given locale and depth, or <code>null</code> if the given locale does not have enough information to generate a candidate path at the given depth.
	@exception NullPointerException if the given base path and/or locale is <code>null</code>.
	@exception IllegalArgumentException if the given depth is not within the range (<var>depth</var>&gt;=0 and <var>depth</var>&lt;=3).
	*/
	public static String getLocaleCandidatePath(final String basePath, final Locale locale, final int depth)
	{
		checkInstance(basePath, "Base path cannot be null.");
		checkInstance(locale, "Locale cannot be null.");
		if(depth<0)	//if the depth is too low to be valid
		{
			throw new IllegalArgumentException("Depth "+depth+" is less than 0.");
		}
		else	//if the depth is at least 0
		{
			if(depth==0)	//if depth 0 was requested
			{
				return basePath;	//return basePath
			}
			else	//if the depth is at least 1
			{			
				final String language=locale.getLanguage();	//get the language
				if(language.length()>0)	//if this locale has a language
				{
					if(depth==1)	//if depth 1 was requested
					{
						return appendFilename(basePath, Locales.LOCALE_SEPARATOR+language);	//return basePath_language.ext
					}
					else	//if the depth is at least 2
					{
						final String country=locale.getCountry();	//get the country
						if(country.length()>0)	//if this locale has a country
						{
							if(depth==2)	//if depth 2 was requested
							{
								return appendFilename(basePath, Locales.LOCALE_SEPARATOR+language+Locales.LOCALE_SEPARATOR+country);	//return basePath_language_country.ext
							}
							else	//if the depth is at least 3
							{
								final String variant=locale.getVariant();	//get the variant
								if(variant.length()>0)	//if this locale has a variant
								{
									if(depth==3)	//if depth 3 was requested
									{
										return appendFilename(basePath, Locales.LOCALE_SEPARATOR+language+Locales.LOCALE_SEPARATOR+country+Locales.LOCALE_SEPARATOR+variant);	//return basePath_language_country_variant.ext
									}
									else	//if something higher than depth 3 was requested
									{
										throw new IllegalArgumentException("Depth "+depth+" is higher than 3.");
									}
								}
							}
						}
					}
				}
			}
		}
		return null;	//indicate that the required locale component for the given depth was not present
	}

}