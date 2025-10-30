/*
 * Copyright © 2019 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.lex;

import static com.globalmentor.java.Conditions.*;
import static java.util.Collections.*;

import java.beans.Introspector;
import java.util.*;
import java.util.function.*;

import org.jspecify.annotations.*;

/**
 * A compound tokenization implementation that relies on variation from non-uppercase to uppercase to delimit tokens.
 * @apiNote This tokenization supports both <code>dromedaryCase</code> and <code>PascalCase</code> variations. That is, this tokenization is agnostic to whether
 *          the first segment is capitalized, and thus supports round-trip split+join.
 * @apiNote This class normally need not be instantiated. Instead use the constant singleton instance {@link CompoundTokenization#CAMEL_CASE}.
 * @apiNote This method provides transformation methods {@link #transformSegmentToCamelCase(int, CharSequence)},
 *          {@link #transformCamelCaseSegmentToDromedaryCase(int, CharSequence)}, and {@link #transformCamelCaseSegmentToPascalCase(int, CharSequence)} as
 *          utility functions for deriving new compound tokenizations if needed. However normally they should not be used directly; instead
 *          {@link CompoundTokenization#CAMEL_CASE}, {@link CompoundTokenization#DROMEDARY_CASE}, and {@link CompoundTokenization#PASCAL_CASE}, respectively
 *          should be used.
 * @implSpec This implementation does not recognize delimiters whose code points lie outside the BMP (i.e. that depend on surrogate pairs).
 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
 * @author Garret Wilson
 */
public class CamelCase extends AbstractCompoundTokenization {

	/**
	 * Name constructor using {@link #transformSegmentToCamelCase(int, CharSequence)} as the transformation.
	 * @param name The name to use for the compound tokenization.
	 */
	CamelCase(@NonNull final String name) {
		this(name, CamelCase::transformSegmentToCamelCase);
	}

	/**
	 * Name and segment transformation constructor.
	 * @param name The name to use for the compound tokenization.
	 * @param segmentTransformation The function to be applied to each segment before joining with {@link #join(Iterable)}. The first function parameter is the
	 *          index of the segment being joined. The second function parameter is the non-empty segment being joined.
	 */
	protected CamelCase(@NonNull final String name, final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> segmentTransformation) {
		super(name, segmentTransformation);
	}

	@Override
	public List<String> split(final CharSequence token) {
		ArrayList<String> segments = null; //we'll only create this if we have to
		int segmentIndex = 0;
		final int length = token.length();
		checkArgument(length > 0, "Token cannot be empty.");
		int start = 0;
		while(start < length) {
			boolean wasUppercase = Character.isUpperCase(token.charAt(start));
			for(int end = start + 1; end <= length; end++) { //go clear to the end (one past the last character)
				final boolean isEnd = end == length;
				final boolean isUppercase = !isEnd && Character.isUpperCase(token.charAt(end));
				if((isUppercase && !wasUppercase) || isEnd) { //if we switched from non-uppercase to uppercase (or reached the end)
					final String segment = transformSplitSegment(segmentIndex, token.subSequence(start, end)).toString();
					if(segments == null) { //if there are no segments yet
						if(isEnd) { //only one segment
							return singletonList(segment);
						}
						segments = new ArrayList<>();
					}
					segments.add(segment);
					segmentIndex++;
					start = end;
					break;
				}
				wasUppercase = isUppercase;
			}
		}
		return segments != null ? segments : emptyList();
	}

	/**
	 * Determines the segment to use after splitting. The first segment is unchanged, as the capitalization of the first segment is irrelevant to the compound
	 * tokenization.
	 * @implSpec The default implementation changes the first character to lowercase if the first character is uppercase but not followed by another uppercase
	 *           character.
	 * @param segmentIndex The index of the segment being split.
	 * @param segment The non-empty segment being split.
	 * @return The segment after splitting.
	 * @throws NullPointerException if the segment is <code>null</code>.
	 * @throws IllegalArgumentException if the segment is the empty string.
	 * @see Introspector#decapitalize(String)
	 */
	protected CharSequence transformSplitSegment(final int segmentIndex, @NonNull final CharSequence segment) {
		checkArgument(segment.length() != 0, "Compound token segment cannot be empty.");
		if(segmentIndex != 0) {
			final char firstChar = segment.charAt(0);
			//if the first character is uppercase, only decapitalize if it is not followed by another capital letter
			if(Character.isUpperCase(firstChar) && (segment.length() == 1 || !Character.isUpperCase(segment.charAt(1)))) {
				final StringBuilder stringBuilder = new StringBuilder(segment);
				stringBuilder.setCharAt(0, Character.toLowerCase(firstChar));
				return stringBuilder;
			}
		}
		return segment;
	}

	/**
	 * Converts a segment being joined to <code>camelCase</code>.
	 * @implSpec Changes the first character to uppercase, except that the first segment is unchanged, as the capitalization of the first segment is irrelevant to
	 *           the compound tokenization.
	 * @param segmentIndex The index of the segment being joined.
	 * @param segment The non-empty segment being joined.
	 * @return The segment to be joined.
	 * @throws NullPointerException if the segment is <code>null</code>.
	 * @throws IllegalArgumentException if the segment is the empty string.
	 */
	public static CharSequence transformSegmentToCamelCase(final int segmentIndex, @NonNull final CharSequence segment) {
		if(segmentIndex == 0) {
			return segment;
		}
		checkArgument(segment.length() != 0, "Compound token segment cannot be empty.");
		final char firstChar = segment.charAt(0);
		if(Character.isUpperCase(firstChar)) { //if the first character is already in uppercase
			return segment; //no changes need to be made
		}
		final StringBuilder stringBuilder = new StringBuilder(segment);
		stringBuilder.setCharAt(0, Character.toUpperCase(firstChar));
		return stringBuilder;
	}

	/**
	 * Converts a segment from <code>camelCase</code> to <code>dromedaryCase</code> (lower <code>camelCase</code>) by decapitalizing the initial letter.
	 * @apiNote The resulting token will not necessarily be in <code>dromedaryCase</code> (that is, {@link #isDromedaryCase(CharSequence)} may not return
	 *          <code>true</code> for the resulting token), e.g. if the first character is a symbol.
	 * @apiNote This is a one-way conversion; it will not be possible to return to the previous tokenization unless the previous capitalization was known.
	 * @param segmentIndex The index of the segment being joined.
	 * @param segment The non-empty segment being joined, already in <code>camelCase</code>.
	 * @return The segment to be joined, using the <code>dromedaryCase</code> tokenization.
	 * @throws NullPointerException if the segment is <code>null</code>.
	 * @throws IllegalArgumentException if the segment is the empty string.
	 */
	public static CharSequence transformCamelCaseSegmentToDromedaryCase(final int segmentIndex, @NonNull final CharSequence segment) {
		if(segmentIndex > 0) { //`camelCase` to `dromedaryCase` only affects the first segment
			return segment;
		}
		checkArgument(segment.length() != 0, "Compound token segment cannot be empty.");
		final char firstChar = segment.charAt(0);
		if(Character.isLowerCase(firstChar)) { //if the first character is already in lowercase
			return segment; //no changes need to be made
		}
		final StringBuilder stringBuilder = new StringBuilder(segment);
		stringBuilder.setCharAt(0, Character.toLowerCase(firstChar));
		return stringBuilder;
	}

	/**
	 * Converts a token from <code>camelCase</code> to <code>PascalCase</code> (upper <code>camelCase</code>) by capitalizing the initial letter.
	 * @apiNote The resulting token will not necessarily be in <code>PascalCase</code> (that is, {@link #isPascalCase(CharSequence)} may not return
	 *          <code>true</code> for the resulting token), e.g. if the first character is a symbol.
	 * @apiNote This is a one-way conversion; it will not be possible to return to the previous tokenization unless the previous capitalization was known.
	 * @param segmentIndex The index of the segment being joined.
	 * @param segment The non-empty segment being joined, already in <code>camelCase</code>.
	 * @return The segment to be joined, using the <code>PascalCase</code> tokenization.
	 * @throws NullPointerException if the segment is <code>null</code>.
	 * @throws IllegalArgumentException if the segment is the empty string.
	 */
	public static CharSequence transformCamelCaseSegmentToPascalCase(final int segmentIndex, @NonNull final CharSequence segment) {
		if(segmentIndex > 0) { //`camelCase` to `PascalCase` only affects the first segment
			return segment;
		}
		checkArgument(segment.length() != 0, "Compound token segment cannot be empty.");
		final char firstChar = segment.charAt(0);
		if(Character.isUpperCase(firstChar)) {
			return segment;
		}
		final StringBuilder stringBuilder = new StringBuilder(segment);
		stringBuilder.setCharAt(0, Character.toUpperCase(firstChar));
		return stringBuilder.toString();
	}

	@Override
	public CamelCase namedWithAddedSegmentStringTransformation(@NonNull final String name,
			@NonNull final Function<? super String, ? extends CharSequence> segmentTransformation) {
		return (CamelCase)super.namedWithAddedSegmentStringTransformation(name, segmentTransformation);
	}

	@Override
	public CamelCase namedWithAddedSegmentTransformation(@NonNull final String name,
			@NonNull final Function<? super CharSequence, ? extends CharSequence> segmentTransformation) {
		return (CamelCase)super.namedWithAddedSegmentTransformation(name, segmentTransformation);
	}

	@Override
	public CamelCase namedWithAddedSegmentTransformation(@NonNull final String name,
			@NonNull final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> segmentTransformation) {
		return new CamelCase(name, addSegmentTransformation(segmentTransformation));
	}

	/**
	 * Indicates whether a token is in <code>dromedaryCase</code> (lower <code>camelCase</code>); this, whether its first letter is in lowercase.
	 * @apiNote It is possible for a token in <code>camelCase</code> to be neither in <code>dromedaryCase</code> nor in <code>PascalCase</code>, e.g. if the first
	 *          character is a symbol.
	 * @param token The compound token in <code>camelCase</code>.
	 * @return <code>true</code> if the first character is in lowercase.
	 * @throws IllegalArgumentException if the token is empty.
	 */
	public boolean isDromedaryCase(@NonNull final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		return Character.isLowerCase(token.charAt(0));
	}

	/**
	 * Indicates whether a token is in <code>PascalCase</code> (upper <code>camelCase</code>); this, whether its first letter is in uppercase.
	 * @apiNote It is possible for a token in <code>camelCase</code> to be neither in <code>dromedaryCase</code> nor in <code>PascalCase</code>, e.g. if the first
	 *          character is a symbol.
	 * @param token The compound token in <code>camelCase</code>.
	 * @return <code>true</code> if the first character is in uppercase.
	 * @throws IllegalArgumentException if the token is empty.
	 */
	public boolean isPascalCase(@NonNull final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		return Character.isUpperCase(token.charAt(0));
	}

}
