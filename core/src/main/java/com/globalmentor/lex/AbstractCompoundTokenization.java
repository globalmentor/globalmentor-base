/*
 * Copyright © 2019-2023 GlobalMentor, Inc. <https://www.globalmentor.com/>
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
import static java.util.Objects.*;

import java.util.*;
import java.util.function.BiFunction;

import org.jspecify.annotations.*;

/**
 * A base compound tokenization implementation.
 * @implNote This implementation provides a generalized algorithm in {@link #join(Iterable)} for joining segments, which can be customized by subclasses by
 *           overriding {@link #appendJoinDelimiter(StringBuilder, int)} and passing a transformation function to the constructor to transform each segment
 *           before being joined.
 * @author Garret Wilson
 */
public abstract class AbstractCompoundTokenization implements CompoundTokenization {

	/**
	 * Returns a segment transformation function that performs no changes on a segment before joining.
	 * @return a segment transformation function that performs no changes on a segment before joining.
	 */
	protected static BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> noSegmentTransformation() {
		return (i, segment) -> segment;
	}

	private final String name;

	@Override
	public String getName() {
		return name;
	}

	/** The function applied to each segment when joining via {@link #join(Iterable)}. */
	private final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> segmentTransformation;

	/**
	 * Segment transformation constructor.
	 * @param name The name to use for the compound tokenization.
	 * @param segmentTransformation The function to be applied to each segment before joining with {@link #join(Iterable)}. The first function parameter is the
	 *          index of the segment being joined. The second function parameter is the non-empty segment being joined.
	 */
	protected AbstractCompoundTokenization(@NonNull final String name,
			final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> segmentTransformation) {
		this.name = requireNonNull(name);
		this.segmentTransformation = requireNonNull(segmentTransformation);
	}

	/**
	 * Returns a composed segment transformation function that will first perform the existing segment transformation of this compound tokenization, and then
	 * apply the given function to that result.
	 * @apiNote This method is meant to facilitate the implementation of {@link #namedWithAddedSegmentTransformation(String, BiFunction)}.
	 * @param segmentTransformation The function to apply during joining after the transformation function of this compound tokenization is applied. The first
	 *          function parameter is the index of the segment being joined. The second function parameter is the non-empty segment being joined.
	 * @return A composed function to be applied to segments before they are joined, that first applies this compound tokenization's transformation and then
	 *         applies the given function with the result.
	 * @throws NullPointerException if the given function is <code>null</code>.
	 * @see #join(Iterable)
	 */
	protected BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> addSegmentTransformation(
			@NonNull final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> segmentTransformation) {
		requireNonNull(segmentTransformation);
		return (segmentIndex, segment) -> segmentTransformation.apply(segmentIndex, this.segmentTransformation.apply(segmentIndex, segment));
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation invokes the internal segment transformation to transform each segment as needed, and calls
	 *           {@link #appendJoinDelimiter(StringBuilder, int)} between segments to add any delimiter if needed.
	 * @implNote This implementation performs joining manually rather than calling {@link String#join(CharSequence, Iterable)} for efficiency and to check each
	 *           segment.
	 * @throws IllegalArgumentException if one of the segments is empty.
	 */
	@Override
	public String join(final Iterable<? extends CharSequence> segments) {
		final Iterator<? extends CharSequence> segmentIterator = segments.iterator();
		boolean hasNext = segmentIterator.hasNext();
		checkArgument(hasNext, "Cannot create compound tokenization with no segments to join.");
		int segmentIndex = -1;
		final StringBuilder stringBuilder = new StringBuilder();
		do { //we know there is at least one segment
			segmentIndex++;
			final CharSequence segment = segmentIterator.next();
			validateSegment(segmentIndex, segment);
			stringBuilder.append(segmentTransformation.apply(segmentIndex, segment));
			hasNext = segmentIterator.hasNext();
			if(hasNext) {
				appendJoinDelimiter(stringBuilder, segmentIndex);
			}
		} while(hasNext);
		return stringBuilder.toString();
	}

	/**
	 * Validates the segment to use before joining.
	 * @apiNote Subclasses should always first call the super class version.
	 * @implSpec The default implementation validates that the segment is not empty.
	 * @param segmentIndex The index of the segment being joined.
	 * @param segment The non-empty segment being joined.
	 * @throws NullPointerException if the segment is <code>null</code>.
	 * @throws IllegalArgumentException if the segment is the empty string.
	 */
	protected void validateSegment(final int segmentIndex, @NonNull final CharSequence segment) {
		checkArgument(segment.length() != 0, "Compound token segment cannot be empty.");
	}

	/**
	 * Appends any necessary delimiter to the given string builder when joining.
	 * @apiNote Subclasses should first call the super class version.
	 * @implSpec The default implementation does nothing.
	 * @param stringBuilder The string builder representing the segments being joined.
	 * @param delimiterIndex The index of the delimiter, which will be equal to the index of the segment just added.
	 * @return The given string builder.
	 */
	protected StringBuilder appendJoinDelimiter(@NonNull StringBuilder stringBuilder, final int delimiterIndex) {
		return stringBuilder;
	}

}
