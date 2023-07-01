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

import static com.globalmentor.collections.Sets.*;
import static com.globalmentor.java.Conditions.*;
import static java.lang.Math.*;
import static java.lang.String.format;
import static java.nio.charset.StandardCharsets.*;
import static java.util.Collections.*;
import static java.util.stream.Collectors.*;

import java.util.*;
import java.util.concurrent.*;
import java.util.regex.*;
import java.util.stream.*;

import javax.annotation.*;

import com.globalmentor.java.Characters;
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
	public static final char SUBTAG_SEPARATOR = '-';

	/** The characters version of the subtag delimiter for efficiently splitting subtags. */
	private static final Characters SUBTAG_SEPARATOR_CHARACTERS = Characters.of(SUBTAG_SEPARATOR);

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
	 * @implNote This pattern and the patterns which comprise it match without regard to ASCII case, even though in this class it is used against a string that
	 *           has already been normalized.
	 */
	public static final Pattern LANGTAG_PATTERN = Pattern.compile(format("%s(?:%s%s)?(?:%s%s)?(?<%s>(?:%s%s)*)(?<%s>(?:%s%s)*)(?:%s%s)?", LANGUAGE,
			SUBTAG_SEPARATOR, SCRIPT, SUBTAG_SEPARATOR, REGION, LANGTAG_PATTERN_GROUP_VARIANTS, SUBTAG_SEPARATOR, VARIANT, LANGTAG_PATTERN_GROUP_EXTENSIONS,
			SUBTAG_SEPARATOR, EXTENSION, SUBTAG_SEPARATOR, PRIVATEUSE));

	/** A pattern for a language tag that is solely a private use tag. */
	public static final Pattern PRIVATEUSE_PATTERN = Pattern.compile(PRIVATEUSE);

	/** The conventional string form of the entire language tag. */
	private final String tagString;

	private final Type type;

	/** @return The type of language tag. */
	public Type getType() {
		return type;
	}

	private final String primaryLanguage;

	/**
	 * Returns the primary language, which is the first subtag of a language, in conventional form. Normal tags will have a primary language, but private use and
	 * grandfathered tags may not.
	 * @apiNote The primary language is usually a two or three-letter ISO 639 code, and it never contains the following extended language subtag(s). For example
	 *          for the language tag <code>zh-cmn-Hans-CN</code>, {@link #findLanguage()} would return <code>zh-cmn</code>, while {@link #findPrimaryLanguage()}
	 *          would return <code>zh</code>. Thus {@link #findPrimaryLanguage()} is usually more useful in comparing two language tags for a common base
	 *          language.
	 * @return The primary language if any.
	 * @see #findLanguage()
	 */
	public Optional<String> findPrimaryLanguage() {
		return Optional.ofNullable(primaryLanguage);
	}

	private final String language;

	/**
	 * Returns the full indicated language in conventional form. Normal tags will have a language, but private use and grandfathered tags may not.
	 * @apiNote Most often {@link #findPrimaryLanguage()} is more appropriate for comparing two language tags for a common base language in some cases. The full
	 *          language may include one or more extended language subtags. For example for the language tag <code>zh-cmn-Hans-CN</code>, {@link #findLanguage()}
	 *          would return <code>zh-cmn</code>, while {@link #findPrimaryLanguage()} would return <code>zh</code>.
	 * @return The language if any.
	 * @see #findPrimaryLanguage()
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

	private final Set<String> variants;

	/**
	 * Returns the variants in conventional, lowercase form. Most language tags do not have variants.
	 * @return The set of variants, which will be empty if there are no variants.
	 */
	public Set<String> getVariants() {
		return variants;
	}

	private final Set<String> extensions;

	/**
	 * Returns the extensions in conventional, lowercase form. Most language tags do not have extensions.
	 * @return The set of extensions, which will be empty if there are no extensions.
	 */
	public Set<String> getExtensions() {
		return extensions;
	}

	private final String privateUse;

	/**
	 * Returns the private use in conventional, lowercase form. Most language tags do not have a private-use section.
	 * @apiNote The values returned is from the private-use section of {@link Type#NORMAL} language tag; these are distinct from a language tag that is itself a
	 *          {@link Type#PRIVATE_USE} language tag.
	 * @return The private use if any.
	 */
	public Optional<String> findPrivateUse() {
		return Optional.ofNullable(privateUse);
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
			primaryLanguage = null;
			script = null;
			region = null;
			variants = emptySet();
			extensions = emptySet();
			privateUse = null;
			return;
		}
		final Matcher matcher = LANGTAG_PATTERN.matcher(tagString); //match on the normalized tag string so we won't have to normalize the individual components
		if(matcher.matches()) {
			type = Type.NORMAL;
			language = matcher.group(LANGTAG_PATTERN_GROUP_LANGUAGE);
			final String extlang = matcher.group(LANGTAG_PATTERN_GROUP_EXTLANG);
			//if the language has extended subtags, remove them and use the language code for the primary language
			primaryLanguage = extlang != null ? matcher.group(LANGTAG_PATTERN_GROUP_LANGUAGE_CODE) : language;
			script = matcher.group(LANGTAG_PATTERN_GROUP_SCRIPT);
			region = matcher.group(LANGTAG_PATTERN_GROUP_REGION);
			variants = matcher.group(LANGTAG_PATTERN_GROUP_VARIANT) != null //if a variant matched (which will be the last one)
					? immutableSetOf(SUBTAG_SEPARATOR_CHARACTERS.split(matcher.group(LANGTAG_PATTERN_GROUP_VARIANTS))) //split out the variants in the entire variants section
					: emptySet();
			extensions = matcher.group(LANGTAG_PATTERN_GROUP_EXTENSION) != null //if an extension matched (which will be the last one)
					? immutableSetOf(parseExtensions(matcher.group(LANGTAG_PATTERN_GROUP_EXTENSIONS))) //parse out the extensions in the entire extensions section
					: emptySet();
			//check for duplicate extensions
			final BitSet extensionDuplicateDetector = new BitSet('z' - '0'); //an extension singleton only allows the effective range '0'-'z' 
			for(final String extension : extensions) {
				assert !extension.isEmpty();
				final char singleton = extension.charAt(0);
				final int singletonBitIndex = singleton - '0';
				checkArgument(!extensionDuplicateDetector.get(singletonBitIndex), "Duplicate extension for `%s`.", singleton);
				extensionDuplicateDetector.set(singletonBitIndex);
			}
			privateUse = matcher.group(LANGTAG_PATTERN_GROUP_PRIVATEUSE);
		} else if(PRIVATEUSE_PATTERN.matcher(text).matches()) {
			type = Type.PRIVATE_USE;
			language = null;
			primaryLanguage = null;
			script = null;
			region = null;
			variants = emptySet();
			extensions = emptySet();
			privateUse = null;
		} else {
			throw new IllegalArgumentException("Invalid language tag: " + text);
		}
	}

	/**
	 * Extracts all extensions from the extension group.
	 * @implNote This implementation does virtually no validation of the group string. It is assumed that the group has already been validated by virtue of
	 *           matching the regular expression.
	 * @implNote This implementation does no normalization of the group string. It is assumed that the group has already been normalized.
	 * @param group The entire matched extension group, in the form <code>-a-myext-b-another-one-c-last</code>.
	 * @return A list of extensions, such as <code>a-myext</code>, <code>b-another-one</code>, <code>c-last</code>.
	 */
	static List<String> parseExtensions(@Nonnull final String group) {
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

	/**
	 * The cache of commonly-used language tags, retrieved by ASCII case-insensitive hash code modulus.
	 * @implSpec Each position in the primary cache represents the modulus of the language tag hash code. Other language tags with the same effective hash codes
	 *           will be placed in the secondary cache, searched linearly, until the secondary cache is full, at which point the value will overwrite the value in
	 *           the main cache to provide caching of the most recently used instance.
	 * @implNote This algorithm has benign race conditions. When storing in the main cache one thread may lose a value; however the next time it is checked it
	 *           will simply be created and placed in the secondary cache if space is available, or overwrite the first value in the primary cache. Moreover the
	 *           array elements are not volatile, which increases the chances of race conditions on the first caching for a particular bucket. This algorithm is
	 *           assumed to have a smaller memory overhead than a {@link ConcurrentHashMap} (even when sparsely filled); perform more efficiently for a small
	 *           number of cached items; and provide most-recently-used lookup for hash collisions, which would not occur in a plain hash map. The only downside
	 *           would be when two language tags with equivalent hashes are used multiple times alternating. Even in this case, a limited-size cache that is full
	 *           would perform at least as bad.
	 */
	private static final LanguageTag[] PRIMARY_CACHE = new LanguageTag[16];

	/** The secondary cache of commonly-used language tags, stored with hashes, and retrieved by linear traversal. */
	private static final List<Map.Entry<Integer, LanguageTag>> SECONDARY_CACHE = new CopyOnWriteArrayList<>();

	/**
	 * Returns a language tag from the text of the tag.
	 * @param text The textual representation of a language tag.
	 * @return A new or existing language tag representing the given text.
	 * @throws IllegalArgumentException if the given text is not a valid representation of a language tag.
	 */
	public static LanguageTag parse(@Nonnull final CharSequence text) {
		final int cacheSize = PRIMARY_CACHE.length;
		final int hash = ASCII.hashCodeIgnoreCase(text);
		final int cacheIndex = abs(hash) % cacheSize;
		//check the primary cache
		final LanguageTag primaryCachedLanguageTag = PRIMARY_CACHE[cacheIndex];
		if(primaryCachedLanguageTag == null) {
			final LanguageTag languageTag = new LanguageTag(text); //cache miss in primary cache; create new language tag
			PRIMARY_CACHE[cacheIndex] = languageTag; //cache the language tag; a race condition might lose a value that suddenly appeared, but it would be cached the next time if space were available
			return languageTag;
		}
		if(ASCII.equalsIgnoreCase(primaryCachedLanguageTag.toString(), text)) { //this doesn't consider non-canonical forms equivalent
			return primaryCachedLanguageTag;
		}
		//check the secondary cache
		for(final Map.Entry<Integer, LanguageTag> secondaryCacheEntry : SECONDARY_CACHE) {
			if(secondaryCacheEntry.getKey() == hash) {
				final LanguageTag secondaryCachedLanguageTag = secondaryCacheEntry.getValue();
				if(ASCII.equalsIgnoreCase(secondaryCachedLanguageTag.toString(), text)) { //this doesn't consider non-canonical forms equivalent
					return secondaryCachedLanguageTag;
				}
			}
		}
		//cache miss in both caches; create new language tag and attempt to cache it 
		final LanguageTag languageTag = new LanguageTag(text);
		final boolean secondaryCacheWasFull;
		if(SECONDARY_CACHE.size() < cacheSize) { //assume the secondary cache should not be larger than the primary cache
			synchronized(SECONDARY_CACHE) { //even though the secondary cache is write-safe, synchronize to prevent a race condition that would allow unbounded growing, however unlikely
				if(SECONDARY_CACHE.size() < cacheSize) { //double-check under synchronization
					secondaryCacheWasFull = false;
					SECONDARY_CACHE.add(new AbstractMap.SimpleImmutableEntry<Integer, LanguageTag>(hash, languageTag));
				} else {
					secondaryCacheWasFull = true;
				}
			}
		} else {
			secondaryCacheWasFull = true;
		}
		if(secondaryCacheWasFull) { //if both caches are full
			PRIMARY_CACHE[cacheIndex] = languageTag; //replace the value in the primary cache to provide most-recently-used lookup			
		}
		return languageTag;
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
	static CharSequence normalize(@Nonnull final CharSequence languageTagText) {
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
