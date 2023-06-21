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
import static java.util.stream.Collectors.*;

import java.util.*;
import java.util.regex.*;
import java.util.stream.*;

import javax.annotation.*;

import com.globalmentor.text.ASCII;

/**
 * Representation of a language tag as per <cite>RFC 5646</cite>.
 * @author Garret Wilson
 * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646">RFC 5646: Tags for Identifying Languages</a>
 */
public final class LanguageTag {

	/** The types of language tags based upon their identifier format. Most language tags are expected to be {@link #NORMAL}. */
	public static enum Type {
		/** Normal language tag. */
		NORMAL,
		/** Private use tag. */
		PRIVATE_USE,
		/** Grandfathered tag registered under <cite>RFC 3066</cite>. */
		GRANDFATHERED
	}

	/**
	 * Tag strings considered grandfathered under <cite>RFC 5646</cite>, stored in normalized form.
	 * @see #normalize(CharSequence)
	 */
	public static final Set<String> GRANDFATHERED_TAG_STRINGS;

	static {
		final Stream<String> regularGrandfatheredTagStrings = Stream.of("art-lojban", "cel-gaulish", "no-bok", "no-nyn", "zh-guoyu", "zh-hakka", "zh-min",
				"zh-min-nan", "zh-xiang");
		final Stream<String> irregularGrandfatheredTagStrings = Stream.of("en-GB-oed", "i-ami", "i-bnn", "i-default", "i-enochian", "i-hak", "i-klingon", "i-lux",
				"i-mingo", "i-navajo", "i-pwn", "i-tao", "i-tay", "i-tsu", "sgn-BE-FR", "sgn-BE-NL", "sgn-CH-DE");
		GRANDFATHERED_TAG_STRINGS = Stream.concat(irregularGrandfatheredTagStrings, regularGrandfatheredTagStrings)
				.collect(collectingAndThen(toSet(), Collections::unmodifiableSet));
	}

	/** The character used to separate subtags. */
	private static final char SUBTAG_SEPARATOR = '-';

	static final String LANGTAG_PATTERN_GROUP_EXTLANG = "extlang";
	private static final String EXTLANG = format("(?<%s>\\p{Alpha}{3}(?:-\\p{Alpha}{3}){0,2})", LANGTAG_PATTERN_GROUP_EXTLANG);
	static final String LANGTAG_PATTERN_GROUP_LANGUAGE = "language";
	private static final String LANGUAGE = format("(?<%s>\\p{Alpha}{2,3}%s?|\\p{Alpha}{4}|\\p{Alpha}{5,8})", LANGTAG_PATTERN_GROUP_LANGUAGE, EXTLANG);
	static final String LANGTAG_PATTERN_GROUP_SCRIPT = "script";
	private static final String SCRIPT = format("(?<%s>\\p{Alpha}{4})", LANGTAG_PATTERN_GROUP_SCRIPT);
	static final String LANGTAG_PATTERN_GROUP_REGION = "region";
	private static final String REGION = format("(?<%s>\\p{Alpha}{2}|\\p{Digit}{3})", LANGTAG_PATTERN_GROUP_REGION);
	static final String LANGTAG_PATTERN_GROUP_VARIANT = "variant";
	private static final String VARIANT = format("(?<%s>\\p{Alnum}{5,8}|(?:\\p{Digit}\\p{Alnum}{3}))", LANGTAG_PATTERN_GROUP_VARIANT);
	static final String LANGTAG_PATTERN_GROUP_SINGLETON = "singleton";
	private static final String SINGLETON = format("(?<%s>\\p{Digit}|[A-W]|[Y-Z]|[a-w]|[y-z])", LANGTAG_PATTERN_GROUP_SINGLETON);
	static final String LANGTAG_PATTERN_GROUP_EXTENSION = "extension";
	private static final String EXTENSION = format("(?<%s>%s(?:-\\p{Alnum}{2,8}){1,})", LANGTAG_PATTERN_GROUP_EXTENSION, SINGLETON);
	static final String LANGTAG_PATTERN_GROUP_PRIVATEUSE = "privateuse";
	private static final String PRIVATEUSE = format("(?<%s>[xX](?:-\\p{Alnum}{1,8}){1,})", LANGTAG_PATTERN_GROUP_PRIVATEUSE);

	/** A pattern for a language tag matching the <code>langtag</code> production. */
	public static final Pattern LANGTAG_PATTERN = Pattern.compile(format("%s(?:%s%s)?(?:%s%s)?(?:%s%s)*(?:%s%s)*(?:%s%s)?", LANGUAGE, SUBTAG_SEPARATOR, SCRIPT,
			SUBTAG_SEPARATOR, REGION, SUBTAG_SEPARATOR, VARIANT, SUBTAG_SEPARATOR, EXTENSION, SUBTAG_SEPARATOR, PRIVATEUSE));

	/** A pattern for a language tag that is solely a private use tag. */
	public static final Pattern PRIVATEUSE_PATTERN = Pattern.compile(PRIVATEUSE);

	/** The conventional string form of the entire language tag. */
	private final String tagString;

	private final Type type;

	/** @return The type of language tag. */
	public Type getType() {
		return type;
	}

	private final String language;

	/**
	 * Returns the indicated language in conventional form. Normal tags will have a language, but private use and grandfathered tags may not.
	 * @return The language if any.
	 */
	public Optional<String> findLanguage() {
		return Optional.ofNullable(language);
	}

	private final String script;

	/**
	 * Returns the indicated script in conventional form.
	 * @return The script if any.
	 */
	public Optional<String> findScript() {
		return Optional.ofNullable(script);
	}

	private final String region;

	/**
	 * Returns the indicated region in conventional form.
	 * @return The region if any.
	 */
	public Optional<String> findRegion() {
		return Optional.ofNullable(region);
	}

	/**
	 * Constructor.
	 * @param text The text form of the entire language tag; will be normalized.
	 * @throws IllegalArgumentException if the given text is not a valid representation of a language tag.
	 * @see #normalize(CharSequence)
	 */
	LanguageTag(@Nonnull final CharSequence text) {
		tagString = normalize(text).toString();
		if(GRANDFATHERED_TAG_STRINGS.contains(tagString)) { //grandfathered tag strings are stored in normalized form
			type = Type.GRANDFATHERED;
			//even the regular grandfathered forms, which match the `langtag` production, may have different component semantics for some of the matching groups 
			language = null;
			script = null;
			region = null;
			return;
		}
		final Matcher matcher = LANGTAG_PATTERN.matcher(text);
		if(matcher.matches()) {
			type = Type.NORMAL;
			language = matcher.group(LANGTAG_PATTERN_GROUP_LANGUAGE);
			script = matcher.group(LANGTAG_PATTERN_GROUP_SCRIPT);
			region = matcher.group(LANGTAG_PATTERN_GROUP_REGION);
		} else if(PRIVATEUSE_PATTERN.matcher(text).matches()) {
			type = Type.PRIVATE_USE;
			language = null;
			script = null;
			region = null;
		} else {
			throw new IllegalArgumentException("Invalid language tag: " + text);
		}
	}

	/**
	 * Returns a language tag from the text of the tag.
	 * @param text The text of a language tag.
	 * @return A new or existing language tag representing the given text.
	 * @throws IllegalArgumentException if the given text is not a valid representation of a language tag.
	 */
	public static LanguageTag of(@Nonnull final CharSequence text) {
		return new LanguageTag(text);
	}

	private static enum NormalizeMode {
		LOWERCASE, UPPERCASE, TITLECASE
	}

	/**
	 * Normalizes language tag text to the recommended form, reflecting the common conventions for the various ISO standards which make up <cite>RFC 5646</cite>.
	 * This normalization applies to all language tag, including grandfathered and private-use tag strings. Although the given text must contain only characters
	 * from the US-ASCII charset, this method does not otherwise validate the format of the language tag.
	 * @apiNote This method is useful for normalizing language tag data without creating an instance of {@link LanguageTag}. Retrieving a {@link LanguageTag}
	 *          instance however includes normalization, in which case calling this method would be redundant.
	 * @param languageTagText The language tag text to be normalized.
	 * @return A character sequence that represents the normalized form of the given language tag text.
	 * @throws IllegalArgumentException if one of the given input characters is outside the range of the US-ASCII charset.
	 * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646#section-2.1.1">RFC 5646 § 2.1.1. Formatting of Language Tags</a>
	 */
	public static CharSequence normalize(@Nonnull final CharSequence languageTagText) {
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

	@Override
	public int hashCode() {
		return ASCII.hashCodeIgnoreCase(tagString);
	}

	@Override
	public boolean equals(final Object object) {
		if(this == object) {
			return true;
		}
		if(!(object instanceof LanguageTag)) {
			return false;
		}
		return ASCII.equalsIgnoreCase(tagString, ((LanguageTag)object).tagString);
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version returns the language tag text.
	 */
	@Override
	public String toString() {
		return tagString;
	}

}
