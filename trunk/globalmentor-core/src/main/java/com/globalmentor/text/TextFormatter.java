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

import java.util.*;

import static java.util.Collections.*;

import static com.globalmentor.java.Characters.*;
import static com.globalmentor.java.StringBuilders.*;

import com.globalmentor.java.Characters;
import com.globalmentor.java.Integers;
import com.globalmentor.model.NameValuePair;

/**
 * Utilities for formatting text.
 * @author Garret Wilson
 */
public class TextFormatter
{

	/**
	 * Appends the string representations of the given objects separated by commas.
	 * @param stringBuilder The string builder into which the result should be placed.
	 * @param items The objects to be formatted.
	 * @return The string buffer containing the new information.
	 * @see Object#toString
	 */
	/*TODO bring back when it's known why this causes an ambiguity with other methods.
		public static StringBuilder formatList(final StringBuilder stringBuilder, final Object... items)
		{
			return formatList(stringBuilder, COMMA_CHAR, items);	//format the list using a comma as a separator
		}
	*/

	/**
	 * Formats an array of bytes into a sequence of hex characters, with each character pair representing the hexadecimal value of the byte.
	 * @param stringBuilder The string builder into which the result should be placed.
	 * @param bytes The values to convert.
	 * @return A lowercase string with hexadecimal digits, each pair representing a byte in the byte array.
	 */
	public static StringBuilder formatHex(final StringBuilder stringBuilder, final byte[] bytes) //TODO make generic to allow different bases 
	{
		for(final byte b : bytes) //for each byte
		{
			stringBuilder.append(Integers.toHexString(b, 2)); //convert the byte to a two-character hex string and add it to our string buffer TODO make more efficient			
		}
		return stringBuilder; //return the string builder we used
	}

	/**
	 * Appends the string representations of the given items separated by a {@value Characters#COMMA_CHAR}.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param stringBuilder The string builder into which the result should be placed.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#NULL_CHAR} (Unicode code point 0) if there should be
	 *          no separator.
	 * @param items The items to be formatted.
	 * @return The string builder containing the new information.
	 * @see Object#toString()
	 * @see Characters#COMMA_CHAR
	 */
	public static <T> StringBuilder formatList(final StringBuilder stringBuilder, final Iterable<T> items)
	{
		return formatList(stringBuilder, COMMA_CHAR, items);
	}

	/**
	 * Appends the string representations of the given items separated by a separator character.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param stringBuilder The string builder into which the result should be placed.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#NULL_CHAR} (Unicode code point 0) if there should be
	 *          no separator.
	 * @param items The items to be formatted.
	 * @return The string builder containing the new information.
	 * @see Object#toString()
	 */
	public static <T> StringBuilder formatList(final StringBuilder stringBuilder, final char separator, final Iterable<T> items)
	{
		final Iterator<T> iterator = items.iterator();
		boolean hasNext = iterator.hasNext();
		while(hasNext)
		{
			final T object = iterator.next();
			stringBuilder.append(object); //append the item
			hasNext = iterator.hasNext();
			if(separator != NULL_CHAR && hasNext) //if we have a separator and there is another item
			{
				stringBuilder.append(separator); //append the separator
			}
		}
		return stringBuilder; //return the string builder, now containing the new information
	}

	/**
	 * Appends the string representations of the given items separated by a separator string.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param stringBuilder The string builder into which the result should be placed.
	 * @param separator The separator to be inserted between the item strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string builder containing the new information.
	 * @see Object#toString()
	 */
	public static <T> StringBuilder formatList(final StringBuilder stringBuilder, final String separator, final Iterable<T> items)
	{
		final Iterator<T> iterator = items.iterator();
		boolean hasNext = iterator.hasNext();
		while(hasNext)
		{
			final T object = iterator.next();
			stringBuilder.append(object); //append the item
			hasNext = iterator.hasNext();
			if(separator != null && hasNext) //if we have a separator and there is another item
			{
				stringBuilder.append(separator); //append the separator
			}
		}
		return stringBuilder; //return the string builder, now containing the new information
	}

	/**
	 * Appends the string representations of the given items separated by a separator character.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param stringBuilder The string builder into which the result should be placed.
	 * @param separator The separator character to be inserted between the object strings, or {@link Characters#NULL_CHAR} (Unicode code point 0) if there should
	 *          be no separator.
	 * @param items The items to be formatted.
	 * @return The string builder containing the new information.
	 * @see Object#toString()
	 */
	public static <T> StringBuilder formatList(final StringBuilder stringBuilder, final char separator, final T... items)
	{
		return formatList(stringBuilder, items, separator, null);
	}

	/**
	 * Appends the string representations of the given items separated by a separator character.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param stringBuilder The string builder into which the result should be placed.
	 * @param items The items to be formatted.
	 * @param separator The separator character to be inserted between the object strings, or {@link Characters#NULL_CHAR} (Unicode code point 0) if there should
	 *          be no separator.
	 * @param ignoreItem The item to ignore, or <code>null</code> if no items should be ignored.
	 * @return The string builder containing the new information.
	 * @see Object#toString()
	 */
	public static <T> StringBuilder formatList(final StringBuilder stringBuilder, final T[] items, final char separator, final T ignoreItem)
	{
		if(items.length > 0) //if there are items
		{
			for(final T item : items) //for each item
			{
				if(ignoreItem != null && item == ignoreItem) //ignore certain items if requested 
				{
					continue;
				}
				stringBuilder.append(item); //append the item
				if(separator != NULL_CHAR) //if we have a separator
				{
					stringBuilder.append(separator); //append the separator
				}
			}
			deleteEnd(stringBuilder); //remove the last separator
		}
		return stringBuilder; //return the string builder we used
	}

	/**
	 * Appends the string representations of the given items separated by a separator string.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param stringBuilder The string builder into which the result should be placed.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string builder containing the new information.
	 * @see Object#toString()
	 */
	public static <T> StringBuilder formatList(final StringBuilder stringBuilder, final String separator, final T... items)
	{
		return formatList(stringBuilder, items, separator, null);
	}

	/**
	 * Appends the string representations of the given items separated by a separator string.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param stringBuilder The string builder into which the result should be placed.
	 * @param items The items to be formatted.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if there should be no separator.
	 * @param ignoreItem The item to ignore, or <code>null</code> if no items should be ignored.
	 * @return The string builder containing the new information.
	 * @see Object#toString()
	 */
	public static <T> StringBuilder formatList(final StringBuilder stringBuilder, final T[] items, final String separator, final T ignoreItem)
	{
		final int length = items.length; //see how many items there are
		for(int i = 0; i < length; ++i) //for each item
		{
			int appendedItemCount = 0;
			for(final T item : items) //for each item
			{
				if(ignoreItem != null && item == ignoreItem) //ignore certain items if requested 
				{
					continue;
				}
				if(appendedItemCount > 0) //if we have appended items
				{
					stringBuilder.append(separator); //append the separator
				}
				stringBuilder.append(item); //append the item
			}
		}
		return stringBuilder; //return the string builder we used
	}

	/**
	 * Appends the string representations of the given items separated by a {@value Characters#COMMA_CHAR}.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#NULL_CHAR} (Unicode code point 0) if there should be
	 *          no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 * @see Characters#COMMA_CHAR
	 */
	public static <T> String formatList(final Iterable<T> items)
	{
		return formatList(new StringBuilder(), items).toString();
	}

	/**
	 * Appends the string representations of the given items separated by a separator character.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param separator The separator character to be inserted between the item strings, or {@link Characters#NULL_CHAR} (Unicode code point 0) if there should be
	 *          no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 */
	public static <T> String formatList(final char separator, final Iterable<T> items)
	{
		return formatList(new StringBuilder(), separator, items).toString();
	}

	/**
	 * Appends the string representations of the given items separated by a separator string.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param separator The separator to be inserted between the item strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 */
	public static <T> String formatList(final String separator, final Iterable<T> items)
	{
		return formatList(new StringBuilder(), separator, items).toString();
	}

	/**
	 * Appends the string representations of the given items separated by a separator character.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param separator The separator character to be inserted between the object strings, or {@link Characters#NULL_CHAR} (Unicode code point 0) if there should
	 *          be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 */
	public static <T> String formatList(final char separator, final T... items)
	{
		return formatList(new StringBuilder(), separator, items).toString();
	}

	/**
	 * Appends the string representations of the given items separated by a separator string.
	 * <p>
	 * <code>null</code> objects are handled as per {@link StringBuilder#append(Object)}.
	 * </p>
	 * @param <T> The type of item being formatted.
	 * @param separator The separator string to be inserted between the object strings, or <code>null</code> if there should be no separator.
	 * @param items The items to be formatted.
	 * @return The string containing the formatted list.
	 * @see Object#toString()
	 */
	public static <T> String formatList(final String separator, final T... items)
	{
		return formatList(new StringBuilder(), separator, items).toString();
	}

	/**
	 * Formats a series of name-value pairs using the format: <var>name</var>="<var>value</value>", <var>name</var>="<var>value</value>"
	 * @param stringBuilder The formatting destination.
	 * @param attributes The attributes to format.
	 * @return The string builder used for formatting.
	 */
	public static StringBuilder formatAttributes(final StringBuilder stringBuilder, final NameValuePair<?, ?>... attributes)
	{
		return formatAttributes(stringBuilder, COMMA_CHAR, EQUALS_SIGN_CHAR, QUOTATION_MARK_CHAR, emptySet(), attributes); //format the attributes using the standard formatting characters
	}

	/**
	 * Formats a series of name-value pairs.
	 * @param stringBuilder The formatting destination.
	 * @param separator The character for separating the parameters, or {@link Characters#NULL_CHAR} (Unicode code point 0) if there should be no separator.
	 * @param assignment The character for assigning the pairs.
	 * @param quote The quote character to use for the value.
	 * @param unquotedNames The set of names that should not be quoted.
	 * @param attributes The attributes to format.
	 * @return The string builder used for formatting.
	 */
	public static StringBuilder formatAttributes(final StringBuilder stringBuilder, final char separator, final char assignment, final char quote,
			final Set<?> unquotedNames, final NameValuePair<?, ?>... attributes)
	{
		if(attributes.length > 0) //if there are attributes
		{
			for(final NameValuePair<?, ?> attribute : attributes) //for each attribute
			{
				final Object name = attribute.getName(); //get the attribute name
				final boolean isQuoted = !unquotedNames.contains(name); //see if we should quote this attribute
				stringBuilder.append(name); //name
				stringBuilder.append(assignment); //=
				if(isQuoted) //if this attribute is quoted
				{
					stringBuilder.append(quote); //"
				}
				stringBuilder.append(attribute.getValue()); //value
				if(isQuoted) //if this attribute is quoted
				{
					stringBuilder.append(quote); //"
				}
				stringBuilder.append(separator); //append a separator
			}
			deleteEnd(stringBuilder); //remove the last separator
		}
		return stringBuilder; //return the string builder we used
	}

}
