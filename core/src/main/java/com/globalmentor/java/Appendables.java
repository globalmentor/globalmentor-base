/*
 * Copyright © 2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.java;

import static java.util.Objects.*;

import java.io.IOException;
import java.util.Iterator;

import javax.annotation.*;

/**
 * Utilities for working with {@link Appendable} objects.
 * 
 * @author Garret Wilson
 * 
 * @see Appendable
 */
public class Appendables {

	private Appendables() {
	}

	/**
	 * Appends a given repetition of characters to an appendable.
	 * @param <A> The type of appendable.
	 * @param appendable The appendable to which the characters should be appended.
	 * @param character The character to append.
	 * @param count The number of repetitions of the character.
	 * @return The appendable with the appended repetitions of the character.
	 * @throws NullPointerException if the given appendable is <code>null</code>.
	 * @throws IOException if there is an error appending to the appendable.
	 */
	public static <A extends Appendable> A append(@Nonnull final A appendable, final char character, int count) throws IOException {
		for(; count > 0; --count) {
			appendable.append(character);
		}
		return appendable;
	}

	/**
	 * Appends character sequences by joining them using the given delimiter character. As with {@link Appendable#append(CharSequence)}, if any element is
	 * <code>null</code>, the four letters <code>"null"</code> will be appended.
	 * @apiNote This method is similar to {@link String#join(CharSequence, CharSequence...)}, but is expected to be more efficient as it does not require the
	 *          unnecessary creation of an intermediate string.
	 * @param <A> The type of appendable.
	 * @param appendable The appendable to append to.
	 * @param delimiter The delimiter that separates each element.
	 * @param elements The elements to join together.
	 * @return The given appendable with the joined content appended.
	 * @throws NullPointerException if the given appendable and/or the elements is <code>null</code>.
	 * @throws IOException if there is an error appending to the appendable.
	 * @see String#join(CharSequence, CharSequence...)
	 */
	public static <A extends Appendable> A appendJoined(@Nonnull final A appendable, final char delimiter, @Nonnull CharSequence... elements) throws IOException {
		final int count = elements.length;
		if(count > 0) { //append the first element separately to avoid checking for the last element each time
			appendable.append(elements[0]);
		}
		for(int i = 1; i < count; i++) { //prefix the rest of the elements with the delimiter
			appendable.append(delimiter).append(elements[i]);
		}
		return appendable;
	}

	/**
	 * Appends character sequences by joining them using the given delimiter character. As with {@link Appendable#append(CharSequence)}, if any element is
	 * <code>null</code>, the four letters <code>"null"</code> will be appended.
	 * @apiNote This method is similar to {@link String#join(CharSequence, CharSequence...)}, but is expected to be more efficient as it does not require the
	 *          unnecessary creation of an intermediate string.
	 * @param <A> The type of appendable.
	 * @param appendable The appendable to append to.
	 * @param delimiter The delimiter that separates each element.
	 * @param elements The elements to join together.
	 * @return The given appendable with the joined content appended.
	 * @throws NullPointerException if the given appendable and/or the elements is <code>null</code>.
	 * @throws IOException if there is an error appending to the appendable.
	 * @see String#join(CharSequence, CharSequence...)
	 */
	public static <A extends Appendable> A appendJoined(@Nonnull final A appendable, final char delimiter, @Nonnull Iterable<? extends CharSequence> elements)
			throws IOException {
		requireNonNull(delimiter);
		final Iterator<? extends CharSequence> iterator = elements.iterator();
		if(iterator.hasNext()) { //append the first element separately to avoid checking for the last element each time
			appendable.append(iterator.next());
		}
		while(iterator.hasNext()) {
			appendable.append(delimiter).append(iterator.next());
		}
		return appendable;
	}

	/**
	 * Appends character sequences by joining them using the given delimiter character sequence. As with {@link Appendable#append(CharSequence)}, if any element
	 * is <code>null</code>, the four letters <code>"null"</code> will be appended.
	 * @apiNote This method is similar to {@link String#join(CharSequence, CharSequence...)}, but is expected to be more efficient as it does not require the
	 *          unnecessary creation of an intermediate string.
	 * @param <A> The type of appendable.
	 * @param appendable The appendable to append to.
	 * @param delimiter The delimiter that separates each element.
	 * @param elements The elements to join together.
	 * @return The given appendable with the joined content appended.
	 * @throws NullPointerException if the given appendable, delimiter, and/or the elements is <code>null</code>.
	 * @throws IOException if there is an error appending to the appendable.
	 * @see String#join(CharSequence, CharSequence...)
	 */
	public static <A extends Appendable> A appendJoined(@Nonnull final A appendable, @Nullable final CharSequence delimiter, @Nonnull CharSequence... elements)
			throws IOException {
		requireNonNull(delimiter);
		final int count = elements.length;
		if(count > 0) { //append the first element separately to avoid checking for the last element each time
			appendable.append(elements[0]);
		}
		for(int i = 1; i < count; i++) { //prefix the rest of the elements with the delimiter
			appendable.append(delimiter).append(elements[i]);
		}
		return appendable;
	}

	/**
	 * Appends character sequences by joining them using the given delimiter character sequence. As with {@link Appendable#append(CharSequence)}, if any element
	 * is <code>null</code>, the four letters <code>"null"</code> will be appended.
	 * @apiNote This method is similar to {@link String#join(CharSequence, CharSequence...)}, but is expected to be more efficient as it does not require the
	 *          unnecessary creation of an intermediate string.
	 * @param <A> The type of appendable.
	 * @param appendable The appendable to append to.
	 * @param delimiter The delimiter that separates each element.
	 * @param elements The elements to join together.
	 * @return The given appendable with the joined content appended.
	 * @throws NullPointerException if the given appendable, delimiter, and/or the elements is <code>null</code>.
	 * @throws IOException if there is an error appending to the appendable.
	 * @see String#join(CharSequence, CharSequence...)
	 */
	public static <A extends Appendable> A appendJoined(@Nonnull final A appendable, @Nullable final CharSequence delimiter,
			@Nonnull Iterable<? extends CharSequence> elements) throws IOException {
		requireNonNull(delimiter);
		final Iterator<? extends CharSequence> iterator = elements.iterator();
		if(iterator.hasNext()) { //append the first element separately to avoid checking for the last element each time
			appendable.append(iterator.next());
		}
		while(iterator.hasNext()) {
			appendable.append(delimiter).append(iterator.next());
		}
		return appendable;
	}

}
