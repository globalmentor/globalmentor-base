/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

/**Utilities for formatting text.
@author Garret Wilson
*/
public class TextFormatter
{

	/**Appends the string representations of the given objects separated by commas.
	@param stringBuilder The string builder into which the result should be placed.
	@param items The objects to be formatted.
	@return The string buffer containing the new information.
	@see Object#toString
	*/
/*TODO bring back when it's known why this causes an ambiguity with other methods.
	public static StringBuilder formatList(final StringBuilder stringBuilder, final Object... items)
	{
		return formatList(stringBuilder, COMMA_CHAR, items);	//format the list using a comma as a delimiter
	}
*/

	/**Formats an array of bytes into a sequence of hex characters,
	 	with each character pairrepresenting the hexadecimal value of the byte.
	@param stringBuilder The string builder into which the result should be placed.
	@param bytes The values to convert.
	@return A lowercase string with hexadecimal digits, each pair representing a byte in the byte array.
	*/
	public static StringBuilder formatHex(final StringBuilder stringBuilder, final byte[] bytes)	//TODO make generic to allow different bases 
	{
		for(final byte b:bytes)	//for each byte
		{
		  stringBuilder.append(Integers.toHexString(b, 2));  //convert the byte to a two-character hex string and add it to our string buffer TODO make more efficient			
		}
		return stringBuilder;	//return the string builder we used
	}

	/**Appends the string representations of the given objects separated by a delimiter character.
	@param stringBuilder The string builder into which the result should be placed.
	@param delimiter The separator character to be inserted between the object strings. 
	@param items The objects to be formatted.
	@return The string buffer containing the new information.
	@see Object#toString
	*/
	public static <T> StringBuilder formatList(final StringBuilder stringBuilder, final char delimiter, final T... items)
	{
		if(items.length>0)	//if there are items
		{
			for(final T item:items)	//for each item
			{
				stringBuilder.append(item).append(delimiter);	//append the item and the delimiter
			}
			deleteEnd(stringBuilder);	//remove the last delimiter
		}
		return stringBuilder;	//return the string builder we used
	}

	/**Appends the string representations of the given objects separated by a delimiter character.
	@param stringBuilder The string builder into which the result should be placed.
	@param delimiter The separator character to be inserted between the object strings. 
	@param collection The objects to be formatted.
	@return The string builder containing the new information.
	@see Object#toString()
	*/
	public static StringBuilder formatList(final StringBuilder stringBuilder, final char delimiter, final Iterable<?> iterable)
	{
		return formatList(stringBuilder, String.valueOf(delimiter), iterable);	//format the list with a string delimiter
	}

	/**Appends the string representations of the given ints separated by a delimiter character.
	@param stringBuilder The string builder into which the result should be placed.
	@param delimiter The separator character to be inserted between the int strings, or {@link Characters#NULL_CHAR} (Unicode code point 0) if no delimiter should be used. 
	@param items The items to be formatted.
	@return The string builder containing the new information.
	@see Integer#toString(int)
	@see Characters#NULL_CHAR
	*/
	public static StringBuilder formatList(final StringBuilder stringBuilder, final char delimiter, final int... items)
	{
		final int itemCount=items.length;	//find out how many items there are.
		for(int i=0; i<itemCount; ++i)	//for each item
		{
			stringBuilder.append(items[i]);	//append the item
			if(delimiter!=NULL_CHAR && i<itemCount-1)	//if there is a delimiter and we're not at the last item
			{
				stringBuilder.append(delimiter);	//append the delimiter
			}
		}
		return stringBuilder;	//return the string builder we used
	}

	/**Appends the string representations of the given longs separated by a delimiter character.
	@param stringBuilder The string builder into which the result should be placed.
	@param delimiter The separator character to be inserted between the int strings, or {@link Characters#NULL_CHAR} (Unicode code point 0) if no delimiter should be used. 
	@param items The items to be formatted.
	@return The string builder containing the new information.
	@see Integer#toString(int)
	@see Characters#NULL_CHAR
	*/
	public static StringBuilder formatList(final StringBuilder stringBuilder, final char delimiter, final long... items)
	{
		final int itemCount=items.length;	//find out how many items there are.
		for(int i=0; i<itemCount; ++i)	//for each item
		{
			stringBuilder.append(items[i]);	//append the item
			if(delimiter!=NULL_CHAR && i<itemCount-1)	//if there is a delimiter and we're not at the last item
			{
				stringBuilder.append(delimiter);	//append the delimiter
			}
		}
		return stringBuilder;	//return the string builder we used
	}

	/**Appends the string representations of the given objects separated by a delimiter string.
	@param stringBuilder The string builder into which the result should be placed.
	@param delimiter The separator to be inserted between the object strings. 
	@param collection The objects to be formatted.
	@return The string buffer containing the new information.
	@see Object#toString
	*/
	public static StringBuilder formatList(final StringBuilder stringBuilder, final String delimiter, final Iterable<?> iterable)
	{
		for(final Object item:iterable)	//for each item
		{
			stringBuilder.append(item).append(delimiter);	//append the item and the delimiter
		}
		final int length=stringBuilder.length();	//get the length of the string builder
		if(length>0)	//if items were added
		{
			stringBuilder.delete(length-delimiter.length(), length);	//delete the last delimiter
		}
		return stringBuilder;	//return the string builder we used
	}

	/**Appends the string representations of the given objects separated by a delimiter string.
	@param stringBuilder The string builder into which the result should be placed.
	@param delimiter The separator string to be inserted between the object strings. 
	@param items The objects to be formatted.
	@return The string buffer containing the new information.
	@see Object#toString
	*/
	public static StringBuilder formatList(final StringBuilder stringBuilder, final String delimiter, final Object... items)
	{
		for(int i=0; i<items.length; ++i)	//look at each object
		{
			stringBuilder.append(items[i]);	//add the string representation of this object to the string buffer
			if(i<items.length-1)	//if this isn't the last object
			{
				stringBuilder.append(delimiter);	//append the delimiter
			}
		}
		return stringBuilder;	//return the string builder, now containing the new information
	}	

	/**Formats a series of name-value pairs using the format:
	 <var>name</var>="<var>value</value>", <var>name</var>="<var>value</value>"
	@param stringBuilder The formatting destination.
	@param attributes The attributes to format. 
	@return The string builder used for formatting.
	*/
	public static StringBuilder formatAttributes(final StringBuilder stringBuilder, final NameValuePair<?, ?>... attributes)
	{
		return formatAttributes(stringBuilder, COMMA_CHAR, EQUALS_SIGN_CHAR, QUOTATION_MARK_CHAR, emptySet(), attributes);	//format the attributes using the standard formatting characters
	}

	/**Formats a series of name-value pairs.
	@param stringBuilder The formatting destination.
	@param delimiter The character for delimiting the parameters.
	@param assignment The character for assigning the pairs.
	@param quote The quote character to use for the value.
	@param unquotedNames The set of names that should not be quoted.
	@param attributes The attributes to format. 
	@return The string builder used for formatting.
	*/
	public static StringBuilder formatAttributes(final StringBuilder stringBuilder, final char delimiter, final char assignment, final char quote, final Set<?> unquotedNames, final NameValuePair<?, ?>... attributes)
	{
		if(attributes.length>0)	//if there are attributes
		{
			for(final NameValuePair<?, ?> attribute:attributes)	//for each attribute
			{
				final Object name=attribute.getName();	//get the attribute name
				final boolean isQuoted=!unquotedNames.contains(name);	//see if we should quote this attribute
				stringBuilder.append(name);	//name
				stringBuilder.append(assignment);	//=
				if(isQuoted)	//if this attribute is quoted
				{
					stringBuilder.append(quote);	//"
				}
				stringBuilder.append(attribute.getValue());	//value
				if(isQuoted)	//if this attribute is quoted
				{
					stringBuilder.append(quote);	//"
				}
				stringBuilder.append(delimiter);	//append a delimiter
			}
			deleteEnd(stringBuilder);	//remove the last delimiter
		}
		return stringBuilder;	//return the string builder we used
	}

}
