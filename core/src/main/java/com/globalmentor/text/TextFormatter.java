/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.text;

import java.io.IOException;
import java.util.*;

import static com.globalmentor.java.Characters.*;
import static com.globalmentor.java.Conditions.*;

import com.globalmentor.java.*;
import com.globalmentor.java.Objects;

/**
 * Utilities for formatting text.
 * @author Garret Wilson
 */
public final class TextFormatter {

	private TextFormatter() {
	}

	/**
	 * Appends the string representations of the given items separated by a {@value Characters#COMMA_CHAR}. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <A> The type of the appendable.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 * @see Characters#COMMA_CHAR
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final Iterable<T> items) throws IOException {
		return formatList(appendable, COMMA_CHAR, items);
	}

	/**
	 * Appends the string representations of the given items separated by a {@value Characters#COMMA_CHAR}. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 * @see Characters#COMMA_CHAR
	 */
	public static <T> String formatList(final Iterable<T> items) {
		try {
			return formatList(new StringBuilder(), items).toString();
		} catch(final IOException ioException) {
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator character. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <A> The type of the appendable.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final char separator, final Iterable<T> items) throws IOException {
		final Iterator<T> iterator = items.iterator();
		boolean hasNext = iterator.hasNext();
		while(hasNext) {
			final T object = iterator.next();
			appendable.append(Objects.toString(object)); //append the item
			hasNext = iterator.hasNext();
			if(separator != UNDEFINED_CHAR && hasNext) { //if we have a separator and there is another item
				appendable.append(separator); //append the separator
			}
		}
		return appendable; //return the appendable, now containing the new information
	}

	/**
	 * Appends the string representations of the given items separated by a separator character. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 */
	public static <T> String formatList(final char separator, final Iterable<T> items) {
		try {
			return formatList(new StringBuilder(), separator, items).toString();
		} catch(final IOException ioException) {
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator string. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <A> The type of the appendable.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param separator The separator to be inserted between the item strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final String separator, final Iterable<T> items) throws IOException {
		final Iterator<T> iterator = items.iterator();
		boolean hasNext = iterator.hasNext();
		while(hasNext) {
			final T object = iterator.next();
			appendable.append(Objects.toString(object)); //append the item
			hasNext = iterator.hasNext();
			if(separator != null && hasNext) { //if we have a separator and there is another item
				appendable.append(separator); //append the separator
			}
		}
		return appendable; //return the appendable, now containing the new information
	}

	/**
	 * Appends the string representations of the given items separated by a separator string. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param separator The separator to be inserted between the item strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 */
	public static <T> String formatList(final String separator, final Iterable<T> items) {
		try {
			return formatList(new StringBuilder(), separator, items).toString();
		} catch(final IOException ioException) {
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator character. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <A> The type of the appendable.
	 * @param appendable The formatting destination.
	 * @param separator The separator character to be inserted between the object strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable> A formatList(final A appendable, final char separator, final Object... items) throws IOException {
		return formatList(appendable, items, separator, null);
	}

	/**
	 * Appends the string representations of the given items separated by a {@value Characters#COMMA_CHAR}. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <A> The type of the appendable.
	 * @param appendable The formatting destination.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 * @see Characters#COMMA_CHAR
	 */
	public static <A extends Appendable> A formatList(final A appendable, final Object... items) throws IOException {
		return formatList(appendable, COMMA_CHAR, items);
	}

	/**
	 * Appends the string representations of the given items separated by a {@value Characters#COMMA_CHAR}. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 * @see Characters#COMMA_CHAR
	 */
	/*TODO fix; causes ambiguity
	public static String formatList(final Object... items)
	{
		try
		{
			return formatList(new StringBuilder(), items).toString();
		}
		catch(final IOException ioException)
		{
			throw unexpected(ioException);
		}
	}*/

	/**
	 * Appends the string representations of the given items separated by a separator character. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param separator The separator character to be inserted between the object strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 */
	public static String formatList(final char separator, final Object... items) {
		try {
			return formatList(new StringBuilder(), separator, items).toString();
		} catch(final IOException ioException) {
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator character. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <A> The type of the appendable.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param items The items to be formatted.
	 * @param separator The separator character to be inserted between the object strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param ignoreItem The item to ignore, or <code>null</code> if no items should be ignored.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final T[] items, final char separator, final T ignoreItem) throws IOException {
		int appendedItemCount = 0;
		for(final T item : items) { //for each item
			if(ignoreItem != null && item == ignoreItem) { //ignore certain items if requested 
				continue;
			}
			if(appendedItemCount > 0 && separator != UNDEFINED_CHAR) { //if we have already appended items and we have a separator
				appendable.append(separator); //append the separator
			}
			appendable.append(Objects.toString(item)); //append the item
			appendedItemCount++; //show that we appended another item
		}
		return appendable; //return the appendable we used
	}

	/**
	 * Appends the string representations of the given items separated by a separator character. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param items The items to be formatted.
	 * @param separator The separator character to be inserted between the object strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param ignoreItem The item to ignore, or <code>null</code> if no items should be ignored.
	 * @return The string containing the new information.
	 * @see Object#toString()
	 */
	public static <T> String formatList(final T[] items, final char separator, final T ignoreItem) {
		try {
			return formatList(new StringBuilder(), items, separator, ignoreItem).toString();
		} catch(final IOException ioException) {
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator string. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <A> The type of the appendable.
	 * @param appendable The formatting destination.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable> A formatList(final A appendable, final String separator, final Object... items) throws IOException {
		return formatList(appendable, items, separator, null);
	}

	/**
	 * Appends the string representations of the given items separated by a separator string. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 */
	public static String formatList(final String separator, final Object... items) {
		try {
			return formatList(new StringBuilder(), separator, items).toString();
		} catch(final IOException ioException) {
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator string. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <A> The type of the appendable.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param items The items to be formatted.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if there should be no separator.
	 * @param ignoreItem The item to ignore, or <code>null</code> if no items should be ignored.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final T[] items, final String separator, final T ignoreItem) throws IOException {
		int appendedItemCount = 0;
		for(final T item : items) { //for each item
			if(ignoreItem != null && item == ignoreItem) { //ignore certain items if requested 
				continue;
			}
			if(appendedItemCount > 0 && separator != null) { //if we have already appended items and we have a separator
				appendable.append(separator); //append the separator
			}
			appendable.append(Objects.toString(item)); //append the item
			appendedItemCount++; //show that we appended another item
		}
		return appendable; //return the appendable we used
	}

	/**
	 * Formats a single name-value attribute using {@value Characters#EQUALS_SIGN_CHAR} for assignment and {@value Characters#QUOTATION_MARK_CHAR} for value
	 * quotation.
	 * @param <A> The type of the appendable.
	 * @param appendable The formatting destination.
	 * @param attribute The attribute to format.
	 * @return The appendable used for formatting.
	 * @throws IOException if there is an error writing to the appendable.
	 */
	public static <A extends Appendable> A formatAttribute(final A appendable, final Map.Entry<?, ?> attribute) throws IOException {
		return formatAttribute(appendable, attribute, QUOTATION_MARK_CHAR);
	}

	/**
	 * Formats a single name-value attribute using {@value Characters#EQUALS_SIGN_CHAR} for assignment.
	 * @param <A> The type of the appendable.
	 * @param appendable The formatting destination.
	 * @param attribute The attribute to format.
	 * @param quote The quote character to use for the value, or {@link Characters#UNDEFINED_CHAR} if the value should not be quoted.
	 * @return The appendable used for formatting.
	 * @throws IOException if there is an error writing to the appendable.
	 */
	public static <A extends Appendable> A formatAttribute(final A appendable, final Map.Entry<?, ?> attribute, final char quote) throws IOException {
		return formatAttribute(appendable, attribute, EQUALS_SIGN_CHAR, quote);
	}

	/**
	 * Formats a single name-value attribute.
	 * @param <A> The type of the appendable.
	 * @param appendable The formatting destination.
	 * @param attribute The attribute to format.
	 * @param assignment The character for assigning the value to the attribute.
	 * @param quote The quote character to use for the value, or {@link Characters#UNDEFINED_CHAR} if the value should not be quoted.
	 * @return The appendable used for formatting.
	 * @throws IOException if there is an error writing to the appendable.
	 */
	public static <A extends Appendable> A formatAttribute(final A appendable, final Map.Entry<?, ?> attribute, final char assignment, final char quote)
			throws IOException {
		return formatAttribute(appendable, attribute.getKey(), attribute.getValue(), assignment, quote);
	}

	/**
	 * Formats a single name-value attribute using {@value Characters#EQUALS_SIGN_CHAR} for assignment and {@value Characters#QUOTATION_MARK_CHAR} for value
	 * quotation.
	 * @param <A> The type of the appendable.
	 * @param appendable The formatting destination.
	 * @param name The name to format.
	 * @param value The value to format.
	 * @return The appendable used for formatting.
	 * @throws IOException if there is an error writing to the appendable.
	 */
	public static <A extends Appendable> A formatAttribute(final A appendable, final Object name, final Object value) throws IOException {
		return formatAttribute(appendable, name, value, QUOTATION_MARK_CHAR);
	}

	/**
	 * Formats a single name-value attribute using {@value Characters#EQUALS_SIGN_CHAR} for assignment.
	 * @param <A> The type of the appendable.
	 * @param appendable The formatting destination.
	 * @param name The name to format.
	 * @param value The value to format.
	 * @param quote The quote character to use for the value, or {@link Characters#UNDEFINED_CHAR} if the value should not be quoted.
	 * @return The appendable used for formatting.
	 * @throws IOException if there is an error writing to the appendable.
	 */
	public static <A extends Appendable> A formatAttribute(final A appendable, final Object name, final Object value, final char quote) throws IOException {
		return formatAttribute(appendable, name, value, EQUALS_SIGN_CHAR, quote);
	}

	/**
	 * Formats a single name-value attribute.
	 * @param <A> The type of the appendable.
	 * @param appendable The formatting destination.
	 * @param name The name to format.
	 * @param value The value to format.
	 * @param assignment The character for assigning the value to the attribute.
	 * @param quote The quote character to use for the value, or {@link Characters#UNDEFINED_CHAR} if the value should not be quoted.
	 * @return The appendable used for formatting.
	 * @throws IOException if there is an error writing to the appendable.
	 */
	public static <A extends Appendable> A formatAttribute(final A appendable, final Object name, final Object value, final char assignment, final char quote)
			throws IOException {
		appendable.append(Objects.toString(name)); //name
		appendable.append(assignment); //=
		if(quote != UNDEFINED_CHAR) { //if the attribute value is quoted
			appendable.append(quote); //"
		}
		appendable.append(Objects.toString(value)); //value
		if(quote != UNDEFINED_CHAR) { //if the attribute value is quoted
			appendable.append(quote); //"
		}
		return appendable; //return the appendable we used
	}

}
