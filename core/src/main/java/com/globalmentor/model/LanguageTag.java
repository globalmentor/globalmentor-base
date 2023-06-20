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

import static java.lang.String.format;
import static java.util.function.Function.*;
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

	/** Tag strings considered grandfathered under <cite>RFC 5646</cite>. */
	public static final Set<String> GRANDFATHERED_STRINGS;

	/** The conventional forms of grandfathered language tags, mapped to their lowercase string version. */
	private static final Map<String, String> GRANDFATHERED_CONVENTIONAL_TAG_STRINGS_BY_LOWERCASE_STRING;

	static {
		final Stream<String> irregularGrandfatheredTagStrings = Stream.of("en-GB-oed", "i-ami", "i-bnn", "i-default", "i-enochian", "i-hak", "i-klingon", "i-lux",
				"i-mingo", "i-navajo", "i-pwn", "i-tao", "i-tay", "i-tsu", "sgn-BE-FR", "sgn-BE-NL", "sgn-CH-DE");
		final Stream<String> regularGrandfatheredTagStrings = Stream.of("art-lojban", "cel-gaulish", "no-bok", "no-nyn", "zh-guoyu", "zh-hakka", "zh-min",
				"zh-min-nan", "zh-xiang");
		GRANDFATHERED_STRINGS = Stream.concat(irregularGrandfatheredTagStrings, regularGrandfatheredTagStrings)
				.collect(collectingAndThen(toSet(), Collections::unmodifiableSet));
		GRANDFATHERED_CONVENTIONAL_TAG_STRINGS_BY_LOWERCASE_STRING = GRANDFATHERED_STRINGS.stream().collect(toMap(ASCII::toLowerCaseString, identity()));
	}

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
	private static final String PRIVATEUSE = format("(?<%s>x(?:-\\p{Alnum}{1,8}){1,})", LANGTAG_PATTERN_GROUP_PRIVATEUSE);

	/** A pattern for a language tag matching the <code>langtag</code> production. */
	public static final Pattern LANGTAG_PATTERN = Pattern
			.compile(format("%s(?:-%s)?(?:-%s)?(?:-%s)*(?:-%s)*(?:-%s)?", LANGUAGE, SCRIPT, REGION, VARIANT, EXTENSION, PRIVATEUSE));

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
	 * @param text The text form of the entire language tag.
	 * @throws IllegalArgumentException if the given text is not a valid representation of a language tag.
	 */
	LanguageTag(@Nonnull final CharSequence text) {
		final String conventionalGrandfatheredTagString = GRANDFATHERED_CONVENTIONAL_TAG_STRINGS_BY_LOWERCASE_STRING.get(ASCII.toLowerCaseString(text));
		if(conventionalGrandfatheredTagString != null) {
			type = Type.GRANDFATHERED;
			tagString = conventionalGrandfatheredTagString;
			//even the regular grandfathered forms, which match the `langtag` production, have different component semantics for some of the matching groups 
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
		this.tagString = text.toString();
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
