package com.garretwilson.javascript;

import static java.lang.reflect.Array.*;
import java.util.*;

import static com.garretwilson.javascript.JavaScriptConstants.*;
import static com.garretwilson.lang.StringBuilderUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.text.CharacterConstants.*;
import com.garretwilson.text.W3CDateFormat;

/**Utilities for encoding and decoding JavaScript Object Notation (JSON).
In addition to standard JSON, any {@link Date} object will be formatted as a string value according to the W3C Note, 
	"Date and Time Formats", <a href="http://www.w3.org/TR/NOTE-datetime">http://www.w3.org/TR/NOTE-datetime</a>,
	a profile of ISO 8601.
@author Garret Wilson
@see <a href="http://www.json.org/">Introducing JSON</a>
@see <a href="http://www.w3.org/TR/NOTE-datetime">Date and Time Formats</a>
@see W3CDateFormat.Style#DATE_TIME
*/
public class JSON
{

	/**Appends an object value.
	Supported value types are:
	<ul>
		<li>{@link CharSequence} (string)</li>
		<li>{@link Boolean} (boolean)</li>
		<li>{@link Number} (number)</li>
		<li>{@link Map} (associative array)</li>
		<li>[] (array)</li>
		<li>{@link Date} (W3C date/time)</li>
		<li>{@link Object} (string)</li>
		<li><code>null</code></li>
	</ul>
	@param stringBuilder The string builder to accept the string.
	@param object The object value to be appended, or <code>null</code>.
	@return The string builder.
	*/
	public static StringBuilder appendValue(final StringBuilder stringBuilder, final Object value)
	{
		if(value!=null)	//if the value is not null
		{
			if(value instanceof CharSequence)	//string
			{
				appendStringValue(stringBuilder, (CharSequence)value);	
			}
			else if(value instanceof Boolean)	//boolean
			{
				appendBooleanValue(stringBuilder, (Boolean)value);
			}
			else if(value instanceof Number)	//number
			{
				appendNumberValue(stringBuilder, (Number)value);
			}
			else if(value instanceof Map)	//if the value is a map
			{
				appendAssociativeArrayValue(stringBuilder, (Map<?, ?>)value);	//append the map as an associative array
			}
			else if(value.getClass().isArray())	//if the value is an array (we can't use instanceof Object[], because this may be an array of something besides Object)
			{
				appendArrayValue(stringBuilder, value);	//append the array
			}
			else if(value instanceof Date)	//date
			{
				appendStringValue(stringBuilder, W3CDateFormat.format((Date)value, W3CDateFormat.Style.DATE_HOURS_MINUTES_SECONDS));
			}
			else	//if we can't determine the type of object
			{
				appendStringValue(stringBuilder, value.toString());	//append a string form of the value	
			}
		}
		else	//if the value is null
		{
			stringBuilder.append(NULL);	//append null
		}
		return stringBuilder;	//return the string builder
	}
	
	
	/**Appends a string value in the form <code>"<var>string</var>"</code>.
	The character sequence will first be encoded as necessary.
	@param stringBuilder The string builder to accept the string.
	@param charSequence The string characters to be appended.
	@return The string builder.
	@exception NullPointerException if the given character sequence is <code>null</code>.
	@see #encodeStringValue(StringBuilder)
	*/
	public static StringBuilder appendStringValue(final StringBuilder stringBuilder, final CharSequence charSequence)
	{
		return stringBuilder.append(QUOTATION_MARK_CHAR).append(encodeStringValue(new StringBuilder(charSequence))).append(QUOTATION_MARK_CHAR);	//append and return "encodedString"
	}

	/**Appends a boolean value.
	@param stringBuilder The string builder to accept the value.
	@param bool The boolean value to append.
	@return The string builder.
	@exception NullPointerException if the given boolean value is <code>null</code>.
	*/
	public static StringBuilder appendBooleanValue(final StringBuilder stringBuilder, final Boolean bool)
	{
		return stringBuilder.append(checkInstance(bool, "Boolean value cannot be null."));	//append and return boolean
	}

	/**Appends a number value.
	@param stringBuilder The string builder to accept the value.
	@param number The number to append.
	@return The string builder.
	@exception NullPointerException if the given number is <code>null</code>.
	*/
	public static StringBuilder appendNumberValue(final StringBuilder stringBuilder, final Number number)
	{
		return stringBuilder.append(checkInstance(number, "Number value cannot be null."));	//append and return number
	}

	/**Appends an array value.
	@param stringBuilder The string builder to accept the value.
	@param array The array to append.
	@return The string builder.
	@exception NullPointerException if the given array is <code>null</code>.
	@exception IllegalArgumentException if the given object is not an array.
	*/
	public static StringBuilder appendArrayValue(final StringBuilder stringBuilder, final Object array)
	{
		stringBuilder.append(ARRAY_BEGIN_CHAR);	//[
		final int arrayLength=getLength(array);	//see how long the array is
		if(arrayLength==0)	//if the array is empty
		{
			stringBuilder.append(ARRAY_END_CHAR);	//]
		}
		else	//if the array isn't empty
		{
			for(int i=0; i<arrayLength; ++i)	//for each array element
			{
				appendValue(stringBuilder, get(array, i));	//append this element value
				stringBuilder.append(ARRAY_DELIMITER);	//,
			}
			final int lastIndex=stringBuilder.length()-1;	//get the index of the last character, which is the delimiter
			stringBuilder.replace(lastIndex, lastIndex+1, String.valueOf(ARRAY_END_CHAR));	//replace the last delimiter with ']'
		}
		return stringBuilder;	//return the string builder
	}

	/**Appends an object (associative array) value in the form <code>{"<var>key</var>":<var>value</var>,...}</code>.
	The provided map must not have any <code>null</code> keys.
	@param <K> The type of keys stored in the map.
	@param <V> The type of values stored in the map.
	@param stringBuilder The string builder to accept the associative array.
	@param map The map containing the associative array values.
	@return The string builder.
	@exception NullPointerException if one of the keys of the given map is is <code>null</code>.
	@see #appendValue(StringBuilder, Object)
	*/
	public static <K, V> StringBuilder appendAssociativeArrayValue(final StringBuilder stringBuilder, final Map<K, V> map)
	{
		stringBuilder.append(ASSOCIATIVE_ARRAY_BEGIN_CHAR);	//{
		final Set<Map.Entry<K, V>> mapEntrySet=map.entrySet();	//get the set of map entries
		if(mapEntrySet.isEmpty())	//if the map is empty
		{
			stringBuilder.append(ASSOCIATIVE_ARRAY_END_CHAR);	//}
		}
		else	//if the map isn't empty
		{
			for(final Map.Entry<K, V> mapEntry:mapEntrySet)	//for each map entry
			{
				appendStringValue(stringBuilder, mapEntry.getKey().toString());	//key
				stringBuilder.append(ASSOCIATIVE_ARRAY_KEY_VALUE_DELIMITER);	//:
				appendValue(stringBuilder, mapEntry.getValue());	//value
				stringBuilder.append(ASSOCIATIVE_ARRAY_DELIMITER);	//,
			}
			final int lastIndex=stringBuilder.length()-1;	//get the index of the last character, which is the delimiter
			stringBuilder.replace(lastIndex, lastIndex+1, String.valueOf(ASSOCIATIVE_ARRAY_END_CHAR));	//replace the last delimiter with '}'
		}
		return stringBuilder;	//return the string builder
	}

	/**Encodes a string value.
	The characters {@value JavaScriptConstants#STRING_ENCODE_CHARS} will be replaced with {@value JavaScriptConstants#STRING_ENCODE_REPLACEMENT_STRINGS}, respectively. 
	@param charSequence The characters to encode.
	@return A string containing encoded characters.
	@exception NullPointerException if the given character sequence is <code>null</code>.
	*/
	public static String encodeStringValue(final CharSequence charSequence)
	{
		final StringBuilder stringBuilder=new StringBuilder(checkInstance(charSequence, "String value cannot be null."));	//create a new string builder with the contents of the character sequence
		replace(stringBuilder, STRING_ENCODE_CHARS, STRING_ENCODE_REPLACEMENT_STRINGS);	//replace the encode characters with their encoded replacements
		return stringBuilder.toString();	//return the encoded string
	}
	
}