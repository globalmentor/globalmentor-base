package com.garretwilson.lang;

import java.util.EnumSet;

/**Utilities for working with enums.
@author Garret Wilson
*/
public class EnumUtilities
{

	/**Creates a set of enums using varargs.
	This method exists because the existing method {@link EnumSet#of(Enum, Enum...)} requires knowledge ahead of time of whether there is at least one enum element to be added to the set.
	@param <E> The type of enum to be stored in the set.
	@param enumClass The enum class.
	@param enumElements The elements to be contained in the set.
	@return A set of enums containing the given enum values.
	*/
	public static <E extends Enum<E>> EnumSet<E> createEnumSet(final Class<E> enumClass, final E... enumElements)
	{
		final EnumSet<E> set=EnumSet.noneOf(enumClass);	//create an empty enum set
		for(final E enumElement:enumElements)	//for each of the given enum values
		{
			set.add(enumElement);	//add this enum element to the set
		}
		return set;	//return the set we created and populated
	}

	/**Returns a form of the enum name appropriate for a resource key.
	The name is converted to lowercaes and all underscore characters ('_') are replaced by a period ('.').
	For example, <code>FILE_NOT_FOUND</code> would produce <code>file.not.found</code>.
	@param e The enum instance to convert to a resource key.
	@return A string representing the enum instance in a style appropriate for use as a resource key.
	@see Enum#name()
	*/
	public static String getResourceKey(final Enum<?> e)
	{
		return e.name().toLowerCase().replace('_', '.');	//convert the name to lowercase and replace underscores with periods
	}

}
