package com.garretwilson.text;

import static com.garretwilson.lang.StringBuilderUtilities.*;
import static com.garretwilson.text.CharacterConstants.*;
import com.garretwilson.util.NameValuePair;

/**Utilities for formatting text.
@author Garret Wilson
*/
public class FormatUtilities
{

	/**Appends the string representations of the given objects separated by commas.
	@param stringBuilder The string builder into which the result should be placed.
	@param items The objects to be formatted.
	@return The string buffer containing the new information.
	@see Object#toString
	*/
	public static StringBuilder formatList(final StringBuilder stringBuilder, final Object... items)
	{
		return formatList(stringBuilder, COMMA_CHAR, items);	//format the list using a comma as a delimiter
	}
	
	/**Appends the string representations of the given objects separated by a delimiter character.
	@param stringBuilder The string builder into which the result should be placed.
	@param delimiter The separator character to be inserted between the object strings. 
	@param items The objects to be formatted.
	@return The string buffer containing the new information.
	@see Object#toString
	*/
	public static StringBuilder formatList(final StringBuilder stringBuilder, final char delimiter, final Object... items)
	{
		if(items.length>0)	//if there are items
		{
			for(final Object item:items)	//for each item
			{
				stringBuilder.append(item).append(delimiter);	//append the item and the delimiter
			}
			deleteLastChar(stringBuilder);	//remove the last delimiter
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
		return formatAttributes(stringBuilder, COMMA_CHAR, EQUALS_SIGN_CHAR, QUOTATION_MARK_CHAR, attributes);	//format the attributes using the standard formatting characters
	}

	/**Formats a series of name-value pairs.
	@param stringBuilder The formatting destination.
	@param delimiter The character for delimiting the parameters.
	@param assignment The character for assigning the pairs.
	@param quote The quote character to use for the value.
	@param attributes The attributes to format. 
	@return The string builder used for formatting.
	*/
	public static StringBuilder formatAttributes(final StringBuilder stringBuilder, final char delimiter, final char assignment, final char quote, final NameValuePair<?, ?>... attributes)
	{
		if(attributes.length>0)	//if there are attributes
		{
			for(final NameValuePair<?, ?> attribute:attributes)	//for each attribute
			{
				stringBuilder.append(attribute.getName());	//name
				stringBuilder.append(assignment);	//=
				stringBuilder.append(quote).append(attribute.getValue()).append(quote);	//"value"
				stringBuilder.append(delimiter);	//append a delimiter
			}
			deleteLastChar(stringBuilder);	//remove the last delimiter
		}
		return stringBuilder;	//return the string builder we used
	}

}
