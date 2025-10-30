/*
 * Copyright © 2023 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.Conditions.*;
import static java.lang.String.format;
import static java.nio.charset.StandardCharsets.*;

import java.util.*;
import java.util.regex.*;

import org.jspecify.annotations.*;

import com.globalmentor.text.ASCII;

/**
 * Constants and utilities for working with language tags as per <cite>RFC 5646</cite>.
 * @author Garret Wilson
 * @see <a href="https://www.rfc-editor.org/rfc/rfc5646.html">RFC 5646: Tags for Identifying Languages</a>
 * @see Locale
 * @see Locales
 */
public final class LanguageTags {

	private LanguageTags() {
	}

	/** The character used to separate subtags. */
	public static final char SUBTAG_SEPARATOR = '-';

	static final String LANGTAG_PATTERN_GROUP_EXTLANG = "extlang";
	private static final String EXTLANG = format("(?<%s>\\p{Alpha}{3}(?:%s\\p{Alpha}{3}){0,2})", LANGTAG_PATTERN_GROUP_EXTLANG, SUBTAG_SEPARATOR);
	static final String LANGTAG_PATTERN_GROUP_LANGUAGE_CODE = "languagecode";
	static final String LANGTAG_PATTERN_GROUP_LANGUAGE = "language";
	private static final String LANGUAGE = format("(?<%s>(?<%s>\\p{Alpha}{2,3})(?:%s%s)?|\\p{Alpha}{4}|\\p{Alpha}{5,8})", LANGTAG_PATTERN_GROUP_LANGUAGE,
			LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, SUBTAG_SEPARATOR, EXTLANG);
	static final String LANGTAG_PATTERN_GROUP_SCRIPT = "script";
	private static final String SCRIPT = format("(?<%s>\\p{Alpha}{4})", LANGTAG_PATTERN_GROUP_SCRIPT);
	static final String LANGTAG_PATTERN_GROUP_REGION = "region";
	private static final String REGION = format("(?<%s>\\p{Alpha}{2}|\\p{Digit}{3})", LANGTAG_PATTERN_GROUP_REGION);
	static final String LANGTAG_PATTERN_GROUP_VARIANT = "variant"; //this group will only report the last matched variant after a match of the langtag
	private static final String VARIANT = format("(?<%s>\\p{Alnum}{5,8}|(?:\\p{Digit}\\p{Alnum}{3}))", LANGTAG_PATTERN_GROUP_VARIANT);
	static final String LANGTAG_PATTERN_GROUP_SINGLETON = "singleton";
	private static final String SINGLETON = format("(?<%s>\\p{Digit}|[A-W]|[Y-Z]|[a-w]|[y-z])", LANGTAG_PATTERN_GROUP_SINGLETON);
	static final String LANGTAG_PATTERN_GROUP_EXTENSION = "extension"; //this group will only report the last matched extension after a match of the langtag
	private static final String EXTENSION = format("(?<%s>%s(?:%s\\p{Alnum}{2,8}){1,})", LANGTAG_PATTERN_GROUP_EXTENSION, SINGLETON, SUBTAG_SEPARATOR);
	static final String LANGTAG_PATTERN_GROUP_PRIVATEUSE = "privateuse";
	private static final String PRIVATEUSE = format("(?<%s>[xX](?:%s\\p{Alnum}{1,8}){1,})", LANGTAG_PATTERN_GROUP_PRIVATEUSE, SUBTAG_SEPARATOR);

	static final String LANGTAG_PATTERN_GROUP_VARIANTS = "variants"; //group may be empty; never null
	static final String LANGTAG_PATTERN_GROUP_EXTENSIONS = "extensions"; //group may be empty; never null

	/**
	 * A pattern for a language tag matching the <code>langtag</code> production.
	 * @apiNote This pattern and the patterns which comprise it match without regard to ASCII case, as per <code>RFC 5646</code>.
	 */
	public static final Pattern LANGTAG_PATTERN = Pattern.compile(format("%s(?:%s%s)?(?:%s%s)?(?<%s>(?:%s%s)*)(?<%s>(?:%s%s)*)(?:%s%s)?", LANGUAGE,
			SUBTAG_SEPARATOR, SCRIPT, SUBTAG_SEPARATOR, REGION, LANGTAG_PATTERN_GROUP_VARIANTS, SUBTAG_SEPARATOR, VARIANT, LANGTAG_PATTERN_GROUP_EXTENSIONS,
			SUBTAG_SEPARATOR, EXTENSION, SUBTAG_SEPARATOR, PRIVATEUSE));

	/** A pattern for a language tag that is solely a private use tag. */
	public static final Pattern PRIVATEUSE_PATTERN = Pattern.compile(PRIVATEUSE);

	/**
	 * Extracts all extensions from the extension group.
	 * @implNote This implementation does virtually no validation of the group string. It is assumed that the group has already been validated by virtue of
	 *           matching the regular expression.
	 * @implNote This implementation does no normalization of the group string. It is assumed that the group has already been normalized.
	 * @param group The entire matched extension group, in the form <code>-a-myext-b-another-one-c-last</code>.
	 * @return A list of extensions, such as <code>a-myext</code>, <code>b-another-one</code>, <code>c-last</code>.
	 * @see #LANGTAG_PATTERN_GROUP_EXTENSIONS
	 */
	public static List<String> parseExtensions(@NonNull final String group) {
		final List<String> extensions = new ArrayList<>();
		final int groupLength = group.length();
		final int maxLastExtensionDividerIndex = groupLength - 5; //(inclusive) the smallest remaining extension possible at the end is `x-xx`, so ignore delimiters after that
		int beginIndex = 1; //skip the initial delimiter introducing all the extensions, separating them from the langtag
		while(beginIndex < groupLength) { //do the check up-front just to prevent an exception if a short string was passed
			int endIndex = beginIndex;
			do {
				endIndex = group.indexOf(SUBTAG_SEPARATOR, endIndex + 1);
				if(endIndex == -1 || endIndex > maxLastExtensionDividerIndex) { //if we find no more separators, or there could be no more distinct extensions
					endIndex = groupLength; //consume the rest of the group for this extension
					break;
				}
			} while(group.charAt(endIndex + 2) != SUBTAG_SEPARATOR); //we've found the end of this extension if a singleton (e.g. `a-…`) follows
			final String extension = group.substring(beginIndex, endIndex);
			assert extension.charAt(0) != SUBTAG_SEPARATOR;
			assert extension.charAt(1) == SUBTAG_SEPARATOR;
			extensions.add(extension);
			beginIndex = endIndex + 1;
		}
		return extensions;
	}

	private static enum NormalizeMode {
		LOWERCASE, UPPERCASE, TITLECASE
	}

	/**
	 * Normalizes language tag text to the recommended form, reflecting the common conventions for the various ISO standards which make up <cite>RFC 5646</cite>.
	 * This normalization applies to all language tag, including grandfathered and private-use tag strings. Although the given text must contain only characters
	 * from the US-ASCII charset, this method does not otherwise validate the format of the language tag.
	 * @apiNote This method is useful for normalizing language tag data without creating an instance of {@link LanguageTags}. Retrieving a {@link LanguageTags}
	 *          instance however includes normalization, in which case calling this method would be redundant.
	 * @param languageTagText The language tag text to be normalized.
	 * @return A character sequence that represents the normalized form of the given language tag text.
	 * @throws IllegalArgumentException if one of the given input characters is outside the range of the US-ASCII charset.
	 * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646#section-2.1.1">RFC 5646 § 2.1.1. Formatting of Language Tags</a>
	 */
	public static CharSequence normalize(@NonNull final CharSequence languageTagText) {
		final int length = languageTagText.length();
		byte[] normalizedLanguageTagTextIfNeeded = null; //lazily created only if needed; bytes support US-ASCII charset
		boolean isAfterSingleton = false;
		for(int start = 0, end = 1; end <= length; end++) {
			if(end == length || languageTagText.charAt(end) == SUBTAG_SEPARATOR) { //normalize a subtag at a time
				final int subtagLength = end - start;
				final NormalizeMode normalizeMode;
				if(subtagLength == 2 && start > 0 && !isAfterSingleton) {
					normalizeMode = NormalizeMode.UPPERCASE; //e.g. `en-CA-x-ca`
				} else if(subtagLength == 4 && start > 0 && !isAfterSingleton) {
					normalizeMode = NormalizeMode.TITLECASE; //e.g. `az-Latn-x-latn`
				} else {
					normalizeMode = NormalizeMode.LOWERCASE; //e.g. `en`
				}
				for(int i = start; i < end; i++) { //normalize this subtag
					final char c = languageTagText.charAt(i);
					checkArgument(c <= ASCII.MAX_VALUE, "Language tag text character `%s` is outside the US-ASCII range.", c);
					final char normalizedChar;
					switch(normalizeMode) {
						case LOWERCASE:
							normalizedChar = ASCII.toLowerCase(c);
							break;
						case UPPERCASE:
							normalizedChar = ASCII.toUpperCase(c);
							break;
						case TITLECASE:
							normalizedChar = i == start ? ASCII.toUpperCase(c) : ASCII.toLowerCase(c);
							break;
						default:
							throw new AssertionError(format("Unrecognized language tag subtag normalize mode `%s`.", normalizeMode));
					}
					if(normalizedChar != c && normalizedLanguageTagTextIfNeeded == null) { //if something changed, see if we need to start copying
						normalizedLanguageTagTextIfNeeded = new byte[length];
						for(int j = 0; j < i; j++) { //initialize with all character up to this point
							normalizedLanguageTagTextIfNeeded[j] = (byte)languageTagText.charAt(j); //we've verified all characters as ASCII to this point 
						}
					}
					if(normalizedLanguageTagTextIfNeeded != null) { //once we start copying, we must always copy until the end
						normalizedLanguageTagTextIfNeeded[i] = (byte)normalizedChar;
					}
				}
				if(normalizedLanguageTagTextIfNeeded != null && end < length) { //once we are copying, we must copy the delimiters as well
					assert languageTagText.charAt(end) == SUBTAG_SEPARATOR;
					normalizedLanguageTagTextIfNeeded[end] = SUBTAG_SEPARATOR;
				}
				start = end + 1; //keep track of the start of the next subtag
				isAfterSingleton = (subtagLength == 1); //keep track of whether the previous subtag was a singleton
			}
		}
		return normalizedLanguageTagTextIfNeeded != null ? new String(normalizedLanguageTagTextIfNeeded, US_ASCII) : languageTagText;
	}

}
