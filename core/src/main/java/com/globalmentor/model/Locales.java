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

import javax.annotation.*;

import static java.util.function.Predicate.*;

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
	 * Finds the language code of a locale.
	 * @apiNote This is a utility method that calls {@link Locale#getLanguage()}, returning {@link Optional#empty()} rather than the empty string to indicate no
	 *          defined value.
	 * @implNote This method returns the new forms for the obsolete ISO 639 codes.
	 * @param locale The locale from which to find the value.
	 * @return The language code, which will not be present if none is defined.
	 * @throws NullPointerException if the given locale is <code>null</code>.
	 */
	public static Optional<String> findLanguage(@Nonnull final Locale locale) {
		return Optional.of(locale.getLanguage()).filter(not(String::isEmpty));
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
		return Optional.of(locale.getScript()).filter(not(String::isEmpty));
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
		return Optional.of(locale.getCountry()).filter(not(String::isEmpty));
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
		return Optional.of(locale.getVariant()).filter(not(String::isEmpty));
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
		return Optional.of(locale.getISO3Language()).filter(not(String::isEmpty));
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
		return Optional.of(locale.getDisplayLanguage()).filter(not(String::isEmpty));
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
		return Optional.of(locale.getDisplayLanguage(inLocale)).filter(not(String::isEmpty));
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
		return Optional.of(locale.getDisplayScript()).filter(not(String::isEmpty));
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
		return Optional.of(locale.getDisplayScript(inLocale)).filter(not(String::isEmpty));
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
		return Optional.of(locale.getDisplayCountry()).filter(not(String::isEmpty));
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
		return Optional.of(locale.getDisplayCountry(inLocale)).filter(not(String::isEmpty));
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
		return Optional.of(locale.getDisplayVariant()).filter(not(String::isEmpty));
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
		return Optional.of(locale.getDisplayVariant(inLocale)).filter(not(String::isEmpty));
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
		return Optional.of(locale.getDisplayName()).filter(not(String::isEmpty));
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
		return Optional.of(locale.getDisplayName(inLocale)).filter(not(String::isEmpty));
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

}
