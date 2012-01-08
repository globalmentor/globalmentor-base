/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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

import static java.util.Collections.*;

import static com.globalmentor.java.Characters.*;
import static com.globalmentor.java.Conditions.*;

import com.globalmentor.java.*;
import com.globalmentor.model.NameValuePair;

/**
 * Utilities for formatting text.
 * @author Garret Wilson
 */
public class TextFormatter
{

	/**
	 * Appends the string representations of the given objects separated by commas.
	 * @param appendable The formatting destination.
	 * @param items The objects to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString
	 */
	/*TODO bring back when it's known why this causes an ambiguity with other methods.
		public static <A extends Appendable> A formatList(final A appendable, final Object... items) throws IOException
		{
			return formatList(appendable, COMMA_CHAR, items);	//format the list using a comma as a separator
		}
	*/

	/**
	 * Formats an array of bytes into a sequence of hex characters, with each character pair representing the hexadecimal value of the byte.
	 * @param appendable The formatting destination.
	 * @param bytes The values to convert.
	 * @return A lowercase string with hexadecimal digits, each pair representing a byte in the byte array.
	 * @throws IOException if there is an error writing to the appendable.
	 */
	public static <A extends Appendable> A formatHex(final A appendable, final byte[] bytes) throws IOException //TODO make generic to allow different bases 
	{
		for(final byte b : bytes) //for each byte
		{
			appendable.append(Integers.toHexString(b, 2)); //convert the byte to a two-character hex string and add it to our appendable TODO make more efficient			
		}
		return appendable; //return the appendable we used
	}

	/**
	 * Formats an array of bytes into a sequence of hex characters, with each character pair representing the hexadecimal value of the byte.
	 * @param bytes The values to convert.
	 * @return A lowercase string with hexadecimal digits, each pair representing a byte in the byte array.
	 */
	public static String formatHex(final byte[] bytes) //TODO make generic to allow different bases 
	{
		try
		{
			return formatHex(new StringBuilder(), bytes).toString();
		}
		catch(final IOException ioException)
		{
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a {@value Characters#COMMA_CHAR}. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 * @see Characters#COMMA_CHAR
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final Iterable<T> items) throws IOException
	{
		return formatList(appendable, COMMA_CHAR, items);
	}

	/**
	 * Appends the string representations of the given items separated by a {@value Characters#COMMA_CHAR}. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 * @see Characters#COMMA_CHAR
	 */
	public static <T> String formatList(final Iterable<T> items)
	{
		try
		{
			return formatList(new StringBuilder(), items).toString();
		}
		catch(final IOException ioException)
		{
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator character. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final char separator, final Iterable<T> items) throws IOException
	{
		final Iterator<T> iterator = items.iterator();
		boolean hasNext = iterator.hasNext();
		while(hasNext)
		{
			final T object = iterator.next();
			appendable.append(Objects.toString(object)); //append the item
			hasNext = iterator.hasNext();
			if(separator != UNDEFINED_CHAR && hasNext) //if we have a separator and there is another item
			{
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
	public static <T> String formatList(final char separator, final Iterable<T> items)
	{
		try
		{
			return formatList(new StringBuilder(), separator, items).toString();
		}
		catch(final IOException ioException)
		{
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator string. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param separator The separator to be inserted between the item strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final String separator, final Iterable<T> items) throws IOException
	{
		final Iterator<T> iterator = items.iterator();
		boolean hasNext = iterator.hasNext();
		while(hasNext)
		{
			final T object = iterator.next();
			appendable.append(Objects.toString(object)); //append the item
			hasNext = iterator.hasNext();
			if(separator != null && hasNext) //if we have a separator and there is another item
			{
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
	public static <T> String formatList(final String separator, final Iterable<T> items)
	{
		try
		{
			return formatList(new StringBuilder(), separator, items).toString();
		}
		catch(final IOException ioException)
		{
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator character. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param separator The separator character to be inserted between the object strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final char separator, final T... items) throws IOException
	{
		return formatList(appendable, items, separator, null);
	}

	/**
	 * Appends the string representations of the given items separated by a {@value Characters#COMMA_CHAR}. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 * @see Characters#COMMA_CHAR
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final T... items) throws IOException
	{
		return formatList(appendable, COMMA_CHAR, items);
	}

	/**
	 * Appends the string representations of the given items separated by a {@value Characters#COMMA_CHAR}. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 * @see Characters#COMMA_CHAR
	 */
/*TODO fix; ambiguous
	public static <T> String formatList(final T... items)
	{
		try
		{
			return formatList(new StringBuilder(), items).toString();
		}
		catch(final IOException ioException)
		{
			throw unexpected(ioException);
		}
	}
*/

	/**
	 * Appends the string representations of the given items separated by a separator character. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param separator The separator character to be inserted between the object strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 */
	public static <T> String formatList(final char separator, final T... items)
	{
		try
		{
			return formatList(new StringBuilder(), separator, items).toString();
		}
		catch(final IOException ioException)
		{
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator character. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param items The items to be formatted.
	 * @param separator The separator character to be inserted between the object strings, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param ignoreItem The item to ignore, or <code>null</code> if no items should be ignored.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final T[] items, final char separator, final T ignoreItem) throws IOException
	{
		int appendedItemCount = 0;
		for(final T item : items) //for each item
		{
			if(ignoreItem != null && item == ignoreItem) //ignore certain items if requested 
			{
				continue;
			}
			appendable.append(Objects.toString(item)); //append the item
			if(appendedItemCount > 0 && separator != UNDEFINED_CHAR) //if we have already appended items and we have a separator
			{
				appendable.append(separator); //append the separator
			}
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
	public static <T> String formatList(final T[] items, final char separator, final T ignoreItem)
	{
		try
		{
			return formatList(new StringBuilder(), items, separator, ignoreItem).toString();
		}
		catch(final IOException ioException)
		{
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator string. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final String separator, final T... items) throws IOException
	{
		return formatList(appendable, items, separator, null);
	}

	/**
	 * Appends the string representations of the given items separated by a separator string. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 */
	public static <T> String formatList(final String separator, final T... items)
	{
		try
		{
			return formatList(new StringBuilder(), separator, items).toString();
		}
		catch(final IOException ioException)
		{
			throw unexpected(ioException);
		}
	}

	/**
	 * Appends the string representations of the given items separated by a separator string. <code>null</code> objects are represented by
	 * {@value Java#NULL_KEYWORD}.
	 * @param <T> The type of item being formatted.
	 * @param appendable The formatting destination.
	 * @param items The items to be formatted.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if there should be no separator.
	 * @param ignoreItem The item to ignore, or <code>null</code> if no items should be ignored.
	 * @return The appendable containing the new information.
	 * @throws IOException if there is an error writing to the appendable.
	 * @see Object#toString()
	 */
	public static <A extends Appendable, T> A formatList(final A appendable, final T[] items, final String separator, final T ignoreItem) throws IOException
	{
		int appendedItemCount = 0;
		for(final T item : items) //for each item
		{
			if(ignoreItem != null && item == ignoreItem) //ignore certain items if requested 
			{
				continue;
			}
			if(appendedItemCount > 0 && separator != null) //if we have already appended items and we have a separator
			{
				appendable.append(separator); //append the separator
			}
			appendable.append(Objects.toString(item)); //append the item
			appendedItemCount++; //show that we appended another item
		}
		return appendable; //return the appendable we used
	}

	/**
	 * Formats a series of name-value pairs using the format: <var>name</var>="<var>value</value>", <var>name</var>="<var>value</value>"
	 * @param appendable The formatting destination.
	 * @param attributes The attributes to format.
	 * @return The appendable used for formatting.
	 * @throws IOException if there is an error writing to the appendable.
	 */
	public static <A extends Appendable> A formatAttributes(final A appendable, final NameValuePair<?, ?>... attributes) throws IOException
	{
		return formatAttributes(appendable, COMMA_CHAR, EQUALS_SIGN_CHAR, QUOTATION_MARK_CHAR, emptySet(), attributes); //format the attributes using the standard formatting characters
	}

	/**
	 * Formats a series of name-value pairs.
	 * @param appendable The formatting destination.
	 * @param separator The character for separating the parameters, or {@link Characters#UNDEFINED_CHAR} if there should be no separator.
	 * @param assignment The character for assigning the pairs.
	 * @param quote The quote character to use for the value.
	 * @param unquotedNames The set of names that should not be quoted.
	 * @param attributes The attributes to format.
	 * @return The appendable used for formatting.
	 * @throws IOException if there is an error writing to the appendable.
	 */
	public static <A extends Appendable> A formatAttributes(final A appendable, final char separator, final char assignment, final char quote,
			final Set<?> unquotedNames, final NameValuePair<?, ?>... attributes) throws IOException
	{
		int appendedItemCount = 0;
		for(final NameValuePair<?, ?> attribute : attributes) //for each attribute
		{
			final Object name = attribute.getName(); //get the attribute name
			final boolean isQuoted = !unquotedNames.contains(name); //see if we should quote this attribute
			appendable.append(Objects.toString(name)); //name
			appendable.append(assignment); //=
			if(isQuoted) //if this attribute is quoted
			{
				appendable.append(quote); //"
			}
			appendable.append(Objects.toString(attribute.getValue())); //value
			if(isQuoted) //if this attribute is quoted
			{
				appendable.append(quote); //"
			}
			if(appendedItemCount > 0 && separator != UNDEFINED_CHAR) //if we have already appended items and we have a separator
			{
				appendable.append(separator); //append a separator
			}
			appendedItemCount++; //show that we appended another item
		}
		return appendable; //return the appendable we used
	}

}
