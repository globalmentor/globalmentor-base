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

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Conditions.*;

import java.util.*;
import java.util.function.*;

import org.jspecify.annotations.*;

import com.globalmentor.java.Characters;

/**
 * A compound tokenization implementation that relies on a delimiter character between segments.
 * @author Garret Wilson
 */
public class CharacterDelimitedCompoundTokenization extends AbstractCompoundTokenization {

	private final Characters delimiterCharacters;

	private final char delimiter;

	/**
	 * Returns the delimiter used by this tokenization.
	 * @return The delimiter used by this tokenization.
	 */
	public char getDelimiter() {
		return delimiter;
	}

	/**
	 * Name, delimiter, segment transformation constructor.
	 * @param name The name to use for the compound tokenization.
	 * @param delimiter The delimiter for splitting and joining a segment token.
	 * @param segmentTransformation The function to be applied to each segment before joining with {@link #join(Iterable)}. The first function parameter is the
	 *          index of the segment being joined. The second function parameter is the non-empty segment being joined.
	 */
	protected CharacterDelimitedCompoundTokenization(@NonNull final String name, final char delimiter,
			final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> segmentTransformation) {
		super(name, segmentTransformation);
		delimiterCharacters = Characters.of(delimiter);
		this.delimiter = delimiter;
	}

	@Override
	public CharacterDelimitedCompoundTokenization namedWithAddedSegmentStringTransformation(@NonNull final String name,
			@NonNull final Function<? super String, ? extends CharSequence> segmentTransformation) {
		return (CharacterDelimitedCompoundTokenization)super.namedWithAddedSegmentStringTransformation(name, segmentTransformation);
	}

	@Override
	public CharacterDelimitedCompoundTokenization namedWithAddedSegmentTransformation(@NonNull final String name,
			@NonNull final Function<? super CharSequence, ? extends CharSequence> segmentTransformation) {
		return (CharacterDelimitedCompoundTokenization)super.namedWithAddedSegmentTransformation(name, segmentTransformation);
	}

	@Override
	public CharacterDelimitedCompoundTokenization namedWithAddedSegmentTransformation(@NonNull final String name,
			@NonNull final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> segmentTransformation) {
		return new CharacterDelimitedCompoundTokenization(name, getDelimiter(), addSegmentTransformation(segmentTransformation));
	}

	@Override
	public List<String> split(final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		return delimiterCharacters.split(token);
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version validates that the segment does not contain the delimiter.
	 * @throws IllegalArgumentException if one of the segments already contains the tokenization delimiter.
	 * @see #getDelimiter()
	 */
	@Override
	protected void validateSegment(final int segmentIndex, final CharSequence segment) {
		super.validateSegment(segmentIndex, segment);
		checkArgument(!contains(segment, delimiter), "Segment `%s` cannot contain the delimiter `%s`.", segment, getDelimiter());
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation appends the delimiter returned by {@link #getDelimiter()}.
	 * @see #getDelimiter()
	 */
	@Override
	protected StringBuilder appendJoinDelimiter(@NonNull StringBuilder stringBuilder, final int delimiterIndex) {
		return super.appendJoinDelimiter(stringBuilder, delimiterIndex).append(getDelimiter()); //the call to super is just for completeness; it does nothing
	}

}
