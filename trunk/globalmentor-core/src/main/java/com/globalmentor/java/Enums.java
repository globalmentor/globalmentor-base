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

package com.globalmentor.java;

import java.util.EnumSet;

import com.globalmentor.lex.Identifier;
import com.globalmentor.text.ASCII;

import static com.globalmentor.java.Java.*;
import static com.globalmentor.java.StringBuilders.*;

/**
 * Utilities for working with enums.
 * @author Garret Wilson
 */
public class Enums
{

	/**
	 * Creates a set of enums using varargs. This method exists because the existing method {@link EnumSet#of(Enum, Enum...)} requires knowledge ahead of time of
	 * whether there is at least one enum element to be added to the set.
	 * @param <E> The type of enum to be stored in the set.
	 * @param enumClass The enum class.
	 * @param enumElements The elements to be contained in the set.
	 * @return A set of enums containing the given enum values.
	 * @throws NullPointerException if the given enum class and/or enum elements is <code>null</code>.
	 */
	public static <E extends Enum<E>> EnumSet<E> createEnumSet(final Class<E> enumClass, final E... enumElements)
	{
		final EnumSet<E> set = EnumSet.noneOf(enumClass); //create an empty enum set
		for(final E enumElement : enumElements) //for each of the given enum values
		{
			set.add(enumElement); //add this enum element to the set
		}
		return set; //return the set we created and populated
	}

	/**
	 * Returns a form of the enum name appropriate for serialization.
	 * <p>
	 * If the enum is a lexical {@link Identifier}, the name is converted to lowercase and all underscore characters ('_') are replaced by hyphens ('-'). For
	 * example, <code>FILE_NOT_FOUND</code> would produce <code>file-not-found</code>.
	 * </p>
	 * @param e The enum instance to convert to a serialization form.
	 * @return A string representing the enum instance in a style appropriate for use in serialization.
	 * @see Enum#name()
	 * @see Identifier
	 */
	public static <E extends Enum<?>> String getSerializationName(final E e)	//JDK 6/7 does not work with some enum generics if Enum<E> is used; Eclipse 4.2.1 works fine
	{
		String name = e.name();
		if(e instanceof Identifier) //if the enum is an identifier
		{
			final StringBuilder stringBuilder = new StringBuilder(e.name()); //start with the name of the enum
			ASCII.toLowerCase(stringBuilder); //convert the name to lowercase (enums names will only consist of ASCII characters)
			replace(stringBuilder, '_', '-'); //replace underscores with hyphens
			name = stringBuilder.toString();
		}
		return name;
	}

	/**
	 * Returns the appropriate enum that has been serialized.
	 * <p>
	 * If the enum is a lexical {@link Identifier}, the name is converted to uppercase and all hyphen characters ('-') are replaced by underscores ('_') in order
	 * to determine the original enum name. For example, <code>file-not-found</code> would produce <code>FILE_NOT_FOUND</code>. This method assumes that the
	 * original enum name does not contain lowercase letters.
	 * </p>
	 * @param enumType The class object of the enum type from which to return an enum.
	 * @param serializationName The serialization form of the name of the enum to return.
	 * @return The enum constant of the specified enum type with the specified serialization name.
	 * @throws NullPointerException if the enum type and/or the serialization name is <code>null</code>.
	 * @throws IllegalArgumentException if the specified enum type has no constant with the specified serialization name, or the specified class object does not
	 *           represent an enum type.
	 * @see Enum#valueOf(Class, String)
	 * @see Identifier
	 */
	public static <E extends Enum<E>> E getSerializedEnum(final Class<E> enumType, String serializationName)
	{
		if(Identifier.class.isAssignableFrom(enumType)) //if the enum is an identifier
		{
			final StringBuilder stringBuilder = new StringBuilder(serializationName); //start with the serialization name
			replace(stringBuilder, '-', '_'); //convert hyphens to underscores
			ASCII.toUpperCase(stringBuilder); //convert the serialization name back to uppercase
			serializationName = stringBuilder.toString();
		}
		return Enum.valueOf(enumType, serializationName); //try to retrieve a corresponding enum from the original form of the name
	}

	/**
	 * Returns an identifying string for the enum that includes the enum class and the enum name. This ID is useful for resource keys, for example. The ID will be
	 * in the form <code><var>com.example.EnumClass</var>.<var>NAME</var></code>.
	 * @param e The enum instance for which to return an ID.
	 * @throws NullPointerException if the given enum is <code>null</code>.
	 * @see Classes#getPropertyName(Class, String)
	 * @see Enum#getClass()
	 * @see Enum#name()
	 */
	public static <E extends Enum<E>> String getPropertyName(final E e)
	{
		return getPropertyName(e, null); //get an ID with no property
	}

	/**
	 * Returns an identifying string for the enum that includes the enum class, the enum name, and an optional property or aspect (such as "label" or "glyph").
	 * This ID is useful for resource keys, for example. The ID will be in the form
	 * <code><var>com.example.EnumClass</var>.<var>NAME</var>.<var>property</var></code>.
	 * @param e The enum instance for which to return an ID.
	 * @param property The name of the enum property, or <code>null</code> if no property is desired.
	 * @return A string identification of the enum.
	 * @throws NullPointerException if the given enum is <code>null</code>.
	 * @see Classes#getPropertyName(Class, String)
	 * @see Enum#getClass()
	 * @see Enum#name()
	 */
	public static <E extends Enum<E>> String getPropertyName(final E e, final String property)
	{
		final String classPropertyName = Classes.getPropertyName(e.getClass(), e.name()); //com.example.EnumClass.NAME
		return property != null ? classPropertyName + OBJECT_PREDICATE_SEPARATOR + property : classPropertyName; //if a property was given, append it
	}
}
