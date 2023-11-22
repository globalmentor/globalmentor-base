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

package com.globalmentor.collections.comparators;

import static com.globalmentor.java.Conditions.*;

import java.util.*;

import javax.annotation.*;

/**
 * The order in which sorting should be performed.
 * @author Garret Wilson
 * @see Comparator
 */
public enum SortOrder {

	/** Indicates that items should be sorted in increasing order. */
	ASCENDING('+'),

	/** Indicates that items should be sorted in decreasing order. */
	DESCENDING('-');

	private char sign;

	/**
	 * Returns the sign indicating the order: <code>+</code> for {@link #ASCENDING} or <code>-</code> for {@link #DESCENDING}.
	 * @return The sign indicating the order.
	 */
	public char getSign() {
		return sign;
	}

	/**
	 * Constructor.
	 * @param sign The sign of <code>+</code> for {@link #ASCENDING} or <code>-</code> for {@link #DESCENDING} order.
	 */
	private SortOrder(final char sign) {
		this.sign = sign;
	}

	/**
	 * Attempts to determine a sort order from its sign.
	 * @param sign The sign to check; <code>+</code> for {@link #ASCENDING} or <code>-</code> for {@link #DESCENDING} order.
	 * @return The corresponding sort order, which will not be present if the sign was not recognized.
	 */
	public static Optional<SortOrder> findFromSign(final char sign) {
		if(sign == ASCENDING.getSign()) {
			return Optional.of(ASCENDING);
		} else if(sign == DESCENDING.getSign()) {
			return Optional.of(DESCENDING);
		} else {
			return Optional.empty();
		}
	}

	/**
	 * Determines a sort order from its sign.
	 * @param sign The sign to check; <code>+</code> for {@link #ASCENDING} or <code>-</code> for {@link #DESCENDING} order.
	 * @return The corresponding sort order.
	 * @throws IllegalArgumentException if the given sign is not recognized.
	 */
	public static SortOrder fromSign(final char sign) {
		return findFromSign(sign).orElseThrow(() -> new IllegalArgumentException(String.format("Unrecognized sort order sign `%s`", sign)));
	}

	/**
	 * Parses a sort order from its sign as text.
	 * @param text The text to parse.
	 * @return The parsed sort order.
	 * @throws IllegalArgumentException if the given sign text is not recognized.
	 * @see #fromSign(char)
	 */
	public static SortOrder parse(@Nonnull final CharSequence text) {
		checkArgument(text.length() == 1, "Invalid sort order `%`; must be a single character.", text);
		return fromSign(text.charAt(0));
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns the string form of {@link #getSign()}.
	 */
	@Override
	public String toString() {
		return String.valueOf(getSign());
	}

	/**
	 * Applies the sort order to a comparator by reversing the comparator's order if needed.
	 * @apiNote If this sort order is {@link #ASCENDING}, there will be no change in the comparator's order.
	 * @param <T> The type of objects that may be compared by the comparator.
	 * @param comparator The comparator to which a sort order should be applied; used as the ascending order.
	 * @return The comparator or its reverse order, based on this sort order.
	 * @see Comparator#reversed()
	 */
	public <T> Comparator<T> applyTo(@Nonnull final Comparator<T> comparator) {
		switch(this) {
			case ASCENDING:
				return comparator;
			case DESCENDING:
				return comparator.reversed();
			default:
				throw new AssertionError(String.format("Unrecognized sort order `%s`.", this));
		}
	}

}
