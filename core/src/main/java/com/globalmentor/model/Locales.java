/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
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

import javax.annotation.*;

import com.globalmentor.io.Filenames;

import static java.util.Objects.*;
import static com.globalmentor.text.RegularExpressions.*;

/**
 * Utilities for manipulating Java {@link Locale}.
 * @apiNote Further utilities for working with <cite>RFC 5646</cite> language tags themselves are available in {@link LanguageTags}.
 * @author Garret Wilson
 * @see LanguageTags
 */
public final class Locales {

	/** The character used to separate components in a locale: '_'. */
	public static final char LOCALE_SEPARATOR = '_';

	/**
	 * The character '-' used to separate components in language tags as defined in <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>, "Tags for the
	 * Identifying".
	 * @see <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>
	 * @deprecated to be removed in favor of {@link LanguageTags#SUBTAG_SEPARATOR}.
	 */
	@Deprecated
	public static final char LANGUAGE_TAG_SEPARATOR = '-';

	/**
	 * The pattern that matches one of three language tag delimiters:
	 * <ul>
	 * <li>The underscore character ('_') as represented by {@link Locale}.</li>
	 * <li>The hyphen characters ('-') as defined in <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>, "Tags for the Identifying Languages".</li>
	 * </ul>
	 * @see <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>
	 * @deprecated to be removed with {@link #createLocale(String)}.
	 */
	@Deprecated
	private static final Pattern LANGUAGE_TAG_DELIMITER_PATTERN = Pattern.compile(characterClassOf(LOCALE_SEPARATOR, LANGUAGE_TAG_SEPARATOR));

	/**
	 * Constructs a locale object from a locale string with a language, an optional country code, and an optional variant. These components can be separated by
	 * underscore characters ('_') as represented by {@link Locale}, or by hyphen characters ('-') as defined in <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC
	 * 4646</a>, "Tags for Identifying Languages".
	 * @param localeString The string containing the language, optional country, and optional variant.
	 * @return A local corresponding to the given local string.
	 * @throws IllegalArgumentException if the given locale string has more than three components.
	 * @see <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>
	 * @deprecated to be removed in favor of {@link Locale#forLanguageTag(String)} when the input string is a language tag.
	 */
	@Deprecated
	public static Locale createLocale(final String localeString) {
		String language = ""; //the language which we may find
		String country = ""; //the country which we may find
		String variant = ""; //the variant which we may find
		final String[] tags = LANGUAGE_TAG_DELIMITER_PATTERN.split(localeString); //split the string into its components
		final int tagCount = tags.length; //see how many tags there are
		if(tagCount > 0) { //if there is another part of the locale
			language = tags[0]; //get the language
			if(tagCount > 1) { //if there is another part of the locale
				country = tags[1]; //get the country
				if(tagCount > 2) { //if there is another part of the locale
					variant = tags[2]; //get the variant
					if(tagCount > 3) { //if there are more tags
						throw new IllegalArgumentException("Locale does not support more than three language tag components.");
					}
				}
			}
		}
		return new Locale(language, country, variant); //create a locale with the parts we found
	}

	/**
	 * Finds the language code of a locale.
	 * @apiNote This is a utility method that calls {@link Locale#getLanguage()}, returning {@link Optional#empty()} rather than the empty string to indicate no
	 *          defined value.
	 * @implNote This method returns the new forms for the obsolete ISO 639 codes.
	 * @param locale The locale from which to find the value.
	 * @return The language code, which will not be present if none is defined.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public static Optional<String> findLanguage(@Nonnull final Locale locale) {
		return Optional.of(locale.getLanguage()).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds the script for a locale, which is an ISO 15924 4-letter script code. The first letter is uppercase and the rest are lowercase; for example
	 * <code>Latn</code> and <code>Cyrl</code>.
	 * @apiNote This is a utility method that calls {@link Locale#getScript()}, returning {@link Optional#empty()} rather than the empty string to indicate no
	 *          defined value.
	 * @param locale The locale from which to find the value.
	 * @return The script code, which will not be present if none is defined.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public static Optional<String> findScript(@Nonnull final Locale locale) {
		return Optional.of(locale.getScript()).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds the country/region code for a locale, which is either an uppercase ISO 3166 2-letter code or a UN M.49 3-digit code.
	 * @apiNote This is a utility method that calls {@link Locale#getCountry()}, returning {@link Optional#empty()} rather than the empty string to indicate no
	 *          defined value.
	 * @param locale The locale from which to find the value.
	 * @return The country/region code, which will not be present if none is defined.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public static Optional<String> findCountry(@Nonnull final Locale locale) {
		return Optional.of(locale.getCountry()).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds the variant code for a locale.
	 * @apiNote This is a utility method that calls {@link Locale#getVariant()}, returning {@link Optional#empty()} rather than the empty string to indicate no
	 *          defined value.
	 * @param locale The locale from which to find the value.
	 * @return The variant code, which will not be present if none is defined.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public static Optional<String> findVariant(@Nonnull final Locale locale) {
		return Optional.of(locale.getVariant()).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds the three-letter abbreviation of a locale's language. If the language matches an ISO 639-1 two-letter code, the corresponding ISO 639-2/T
	 * three-letter lowercase code is returned.
	 * @apiNote This is a utility method that calls {@link Locale#getISO3Language()}, returning {@link Optional#empty()} rather than the empty string to indicate
	 *          no defined value.
	 * @param locale The locale from which to find the value.
	 * @return A three-letter abbreviation of a locale's language.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 * @throws MissingResourceException if a three-letter language abbreviation is not available for the locale.
	 */
	public static Optional<String> findISO3Language(@Nonnull final Locale locale) throws MissingResourceException {
		return Optional.of(locale.getISO3Language()).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds a name for the locale's language that is appropriate for display to the user.
	 * @apiNote This is a utility method that calls {@link Locale#getDisplayLanguage()}, returning {@link Optional#empty()} rather than the empty string to
	 *          indicate no defined value.
	 * @param locale The locale from which to find the value.
	 * @return The name of the display language.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public final static Optional<String> findDisplayLanguage(@Nonnull final Locale locale) {
		return Optional.of(locale.getDisplayLanguage()).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds a name for the locale's language that is appropriate for display to the user.
	 * @apiNote This is a utility method that calls {@link Locale#getDisplayLanguage(Locale)}, returning {@link Optional#empty()} rather than the empty string to
	 *          indicate no defined value.
	 * @param locale The locale from which to find the value.
	 * @param inLocale The locale for which to retrieve the display language.
	 * @return The name of the display language appropriate to the given locale.
	 * @throws NullPointerException either of the given locales is <code>null</code>.
	 */
	public static Optional<String> findDisplayLanguage(@Nonnull final Locale locale, @Nonnull final Locale inLocale) {
		return Optional.of(locale.getDisplayLanguage(inLocale)).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds a name for the locale's script that is appropriate for display to the user.
	 * @apiNote This is a utility method that calls {@link Locale#getDisplayScript()}, returning {@link Optional#empty()} rather than the empty string to indicate
	 *          no defined value.
	 * @param locale The locale from which to find the value.
	 * @return The display name of the script code for the current default {@link Locale.Category#DISPLAY DISPLAY} locale.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public static Optional<String> findDisplayScript(@Nonnull final Locale locale) {
		return Optional.of(locale.getDisplayScript()).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds a name for the locale's script that is appropriate for display to the user.
	 * @apiNote This is a utility method that calls {@link Locale#getDisplayScript(Locale)}, returning {@link Optional#empty()} rather than the empty string to
	 *          indicate no defined value.
	 * @param locale The locale from which to find the value.
	 * @param inLocale The locale for which to retrieve the display script.
	 * @return The display name of the script code for the current default {@link Locale.Category#DISPLAY DISPLAY} locale.
	 * @throws NullPointerException if either of the given locales is <code>null</code>.
	 */
	public static Optional<String> findDisplayScript(@Nonnull final Locale locale, @Nonnull final Locale inLocale) {
		return Optional.of(locale.getDisplayScript(inLocale)).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds a name for the locale's country that is appropriate for display to the user.
	 * @apiNote This is a utility method that calls {@link Locale#getDisplayCountry()}, returning {@link Optional#empty()} rather than the empty string to
	 *          indicate no defined value.
	 * @param locale The locale from which to find the value.
	 * @return The name of the country appropriate to the locale.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public static Optional<String> findDisplayCountry(@Nonnull final Locale locale) {
		return Optional.of(locale.getDisplayCountry()).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds a name for the locale's country that is appropriate for display to the user.
	 * @apiNote This is a utility method that calls {@link Locale#getDisplayCountry(Locale)}, returning {@link Optional#empty()} rather than the empty string to
	 *          indicate no defined value.
	 * @param locale The locale from which to find the value.
	 * @param inLocale The locale for which to retrieve the display country.
	 * @return The name of the country appropriate to the given locale.
	 * @throws NullPointerException if either of the given locales is <code>null</code>.
	 */
	public static Optional<String> findDisplayCountry(@Nonnull final Locale locale, @Nonnull final Locale inLocale) {
		return Optional.of(locale.getDisplayCountry(inLocale)).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds a name for the locale's variant code that is appropriate for display to the user.
	 * @apiNote This is a utility method that calls {@link Locale#getDisplayVariant()}, returning {@link Optional#empty()} rather than the empty string to
	 *          indicate no defined value.
	 * @param locale The locale from which to find the value.
	 * @return The name of the display variant code appropriate to the locale.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public static Optional<String> findDisplayVariant(@Nonnull final Locale locale) {
		return Optional.of(locale.getDisplayVariant()).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds a name for the locale's variant code that is appropriate for display to the user.
	 * @apiNote This is a utility method that calls {@link Locale#getDisplayVariant(Locale)}, returning {@link Optional#empty()} rather than the empty string to
	 *          indicate no defined value.
	 * @param locale The locale from which to find the value.
	 * @param inLocale The locale for which to retrieve the display variant code.
	 * @return The name of the display variant code appropriate to the given locale.
	 * @throws NullPointerException if either of the given locales is <code>null</code>.
	 */
	public static Optional<String> findDisplayVariant(@Nonnull final Locale locale, @Nonnull final Locale inLocale) {
		return Optional.of(locale.getDisplayVariant(inLocale)).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds a name for the locale that is appropriate for display to the user.
	 * @apiNote This is a utility method that calls {@link Locale#getDisplayName()}, returning {@link Optional#empty()} rather than the empty string to indicate
	 *          no defined value.
	 * @param locale The locale from which to find the value.
	 * @return The name of the locale appropriate to display.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public static Optional<String> findDisplayName(@Nonnull final Locale locale) {
		return Optional.of(locale.getDisplayName()).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds a name for the locale that is appropriate for display to the user.
	 * @apiNote This is a utility method that calls {@link Locale#getDisplayName(Locale)}, returning {@link Optional#empty()} rather than the empty string to
	 *          indicate no defined value.
	 * @param locale The locale from which to find the value.
	 * @param inLocale The locale for which to retrieve the display name.
	 * @return The name of the locale appropriate to display.
	 * @throws NullPointerException if either of the given locales is <code>null</code>.
	 */
	public static Optional<String> findDisplayName(@Nonnull final Locale locale, @Nonnull final Locale inLocale) {
		return Optional.of(locale.getDisplayName(inLocale)).filter(__ -> !__.isEmpty()); //TODO switch to `filter(not(String::isEmpty))` in Java 11
	}

	/**
	 * Finds the Unicode locale type associated with the specified Unicode locale key for a locale. Returns the empty string for keys that are defined with no
	 * type. Returns null if the key is not defined. Keys are case-insensitive. The key must be two alphanumeric characters ([0-9a-zA-Z]).
	 * @apiNote This is a utility method that calls {@link Locale#getUnicodeLocaleType(String)}, returning {@link Optional#empty()} rather than the empty string
	 *          to indicate no defined value.
	 * @param locale The locale from which to find the value.
	 * @param key The Unicode locale key.
	 * @return The Unicode locale type associated with the key, which may be the empty string for keys that are defined with no type; or empty if the locale does
	 *         not define the key.
	 * @throws NullPointerException if the given locale and/or key is <code>null</code>.
	 * @throws IllegalArgumentException if the key is not well-formed.
	 */
	public static Optional<String> findUnicodeLocaleType(@Nonnull final Locale locale, @Nonnull final String key) {
		return Optional.ofNullable(locale.getUnicodeLocaleType(key));
	}

	/**
	 * Retrieves a locale based upon a given display name in the current local. That is, if the default locale is <code>en_US</code>, the display language
	 * "French" will return the local for <code>fr</code>.
	 * @param displayLanguage The name of a language in the current locale.
	 * @return The local that matches the given language, or <code>null</code> if no locale could be found with the given display language.
	 */
	public static Locale createDisplayLanguageLocale(final String displayLanguage) {
		final Locale[] availableLocales = Locale.getAvailableLocales(); //get a list of all available locales
		for(int i = availableLocales.length - 1; i >= 0; --i) { //look at each of the locales
			final Locale locale = availableLocales[i]; //get a reference to this locale
			if(locale.getDisplayLanguage().equalsIgnoreCase(displayLanguage)) { //if the display language of this locale matches the language given
				return new Locale.Builder().setLanguage(locale.getLanguage()).build(); //create a locale for just that language
			}
		}
		return null; //show that we couldn't find a matching language
	}

	/**
	 * Retrieves a sorted list of display countries for all available locales. The display countries will use the default locale for the localized name.
	 * @return An array of display country names for installed locales.
	 */
	public static String[] getAvailableDisplayCountries() {
		return getAvailableDisplayCountries(Locale.getDefault()); //get available display countries in the default locale
	}

	/**
	 * Retrieves a sorted list of display countries for all available locales. The display countries will use the given locale for the localized name.
	 * @param inLocale The locale for which the country names should be localized.
	 * @return An array of display country names for installed locales.
	 */
	public static String[] getAvailableDisplayCountries(final Locale inLocale) {
		final Set<String> displayCountrySet = new HashSet<String>(); //create a set to ensure no duplicate country names, as multiple locales could have the same country name
		final Locale[] availableLocales = Locale.getAvailableLocales(); //get a list of all available locales
		for(int i = availableLocales.length - 1; i >= 0; --i) { //look at each of the locales TODO improve; this currently takes a very long time
			final Locale locale = availableLocales[i]; //get a reference to this locale
			displayCountrySet.add(locale.getDisplayCountry(inLocale)); //get the display country for this locale in the given locale, and add it to our list of display countries
		}
		final List<String> sortedDisplayCountryList = new ArrayList<String>(displayCountrySet); //create a list from the display country set
		sort(sortedDisplayCountryList); //sort the list of countries
		return sortedDisplayCountryList.toArray(new String[sortedDisplayCountryList.size()]); //return a string array from the sorted list
	}

	/**
	 * Determines the string to represent a language identifier according as defined in <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>, "Tags for the
	 * Identification of Languages".
	 * @param locale The language identifier.
	 * @return The string representation of the language identifier.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 * @see <a href="http://www.ietf.org/rfc/rfc4646.txt">RFC 4646</a>
	 * @deprecated to be removed in favor of {@link Locale#toLanguageTag()}.
	 */
	@Deprecated
	public static String getLanguageTag(final Locale locale) {
		return locale.toString().replace(Locales.LOCALE_SEPARATOR, Locales.LANGUAGE_TAG_SEPARATOR); //replace the locale separator with the RFC 4646 separator
	}

	/**
	 * Determines the locale-sensitive path of the given base path based upon a depth index. Based upon the provided locale and depth, a candidate resource path
	 * is generated as follows:
	 * <dl>
	 * <dt>depth 3</dt>
	 * <dd><var>basePath</var> + "_" + <var>language</var> + "_" + <var>country</var> + "_" + <var>variant</var> + "." + <var>extension</var></dd>
	 * <dt>depth 2</dt>
	 * <dd><var>basePath</var> + "_" + <var>language</var> + "_" + <var>country</var> + "." + <var>extension</var></dd>
	 * <dt>depth 1</dt>
	 * <dd><var>basePath</var> + "_" + <var>language</var> + "." + <var>extension</var></dd>
	 * <dt>depth 0</dt>
	 * <dd><var>basePath</var> + "." + <var>extension</var></dd>
	 * </dl>
	 * Any extension of the base path will be preserved. If the resource does not have sufficient components for the given depth, <code>null</code> will be
	 * returned.
	 * @param basePath The base path for which a candidate path should be generated.
	 * @param locale The locale to use in generating the candidate path.
	 * @param depth The depth at which the candidate path should be generated.
	 * @return The candidate path for provided base path at the given locale and depth, or <code>null</code> if the given locale does not have enough information
	 *         to generate a candidate path at the given depth.
	 * @throws NullPointerException if the given base path and/or locale is <code>null</code>.
	 * @throws IllegalArgumentException if the given depth is not within the range (<var>depth</var>&gt;=0 and <var>depth</var>&lt;=3).
	 * @deprecated TODO move to Rincl
	 */
	@Deprecated
	public static String getLocaleCandidatePath(final String basePath, final Locale locale, final int depth) { //TODO update to use Path
		requireNonNull(basePath, "Base path cannot be null.");
		requireNonNull(locale, "Locale cannot be null.");
		if(depth < 0) { //if the depth is too low to be valid
			throw new IllegalArgumentException("Depth " + depth + " is less than 0.");
		} else { //if the depth is at least 0
			if(depth == 0) { //if depth 0 was requested
				return basePath; //return basePath
			} else { //if the depth is at least 1			
				final String language = locale.getLanguage(); //get the language
				if(language.length() > 0) { //if this locale has a language
					if(depth == 1) { //if depth 1 was requested
						return Filenames.appendBase(basePath, Locales.LOCALE_SEPARATOR + language); //return basePath_language.ext
					} else { //if the depth is at least 2
						final String country = locale.getCountry(); //get the country
						if(country.length() > 0) { //if this locale has a country
							if(depth == 2) { //if depth 2 was requested
								return Filenames.appendBase(basePath, Locales.LOCALE_SEPARATOR + language + Locales.LOCALE_SEPARATOR + country); //return basePath_language_country.ext
							} else { //if the depth is at least 3
								final String variant = locale.getVariant(); //get the variant
								if(variant.length() > 0) { //if this locale has a variant
									if(depth == 3) { //if depth 3 was requested
										return Filenames.appendBase(basePath,
												Locales.LOCALE_SEPARATOR + language + Locales.LOCALE_SEPARATOR + country + Locales.LOCALE_SEPARATOR + variant); //return basePath_language_country_variant.ext
									} else { //if something higher than depth 3 was requested
										throw new IllegalArgumentException("Depth " + depth + " is higher than 3.");
									}
								}
							}
						}
					}
				}
			}
		}
		return null; //indicate that the required locale component for the given depth was not present
	}

}
