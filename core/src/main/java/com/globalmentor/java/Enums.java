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

package com.globalmentor.java;

import java.util.*;
import java.util.function.Function;

import javax.annotation.*;

import com.globalmentor.lex.*;

import static com.globalmentor.java.Java.*;
import static com.globalmentor.lex.CompoundTokenization.*;

/**
 * Utilities for working with enums.
 * @author Garret Wilson
 */
public final class Enums {

	private Enums() {
	}

	/**
	 * Returns the enum constant specified by the given name for the indicated enum type, if the name is in fact of the indicated enum type.
	 * @apiNote This method is equivalent to calling {@link Enum#valueOf(Class, String)} except that it returns {@link Optional#empty()} instead of throwing an
	 *          exception if the name is unrecognized for the indicated enum type.
	 * @param <E> The enum type whose constant is to be returned.
	 * @param enumClass The class identifying the type of enum.
	 * @param name The name of the enum constant to return.
	 * @return The enum constant of the specified enum type with the specified name, which will be empty if the specified enum type has no constant with the
	 *         specified name.
	 * @throws NullPointerException if the enum class and/or name is <code>null</code>.
	 */
	public static <E extends Enum<E>> Optional<E> asEnum(final Class<E> enumClass, final String name) {
		try {
			return Optional.of(Enum.valueOf(enumClass, name));
		} catch(final IllegalArgumentException illegalArgumentException) {
			return Optional.empty();
		}
	}

	/**
	 * Creates a set of enums using varargs.
	 * @apiNote This method exists because the existing method {@link EnumSet#of(Enum, Enum...)} requires knowledge ahead of time of whether there is at least one
	 *          enum element to be added to the set.
	 * @param <E> The type of enum to be stored in the set.
	 * @param enumClass The enum class.
	 * @param enumElements The elements to be contained in the set.
	 * @return A set of enums containing the given enum values.
	 * @throws NullPointerException if the given enum class and/or enum elements is <code>null</code>.
	 * @see EnumSet#of(Enum, Enum...)
	 */
	@SafeVarargs
	public static <E extends Enum<E>> EnumSet<E> createEnumSet(final Class<E> enumClass, final E... enumElements) {
		final EnumSet<E> set = EnumSet.noneOf(enumClass); //create an empty enum set
		for(final E enumElement : enumElements) { //for each of the given enum values
			set.add(enumElement); //add this enum element to the set
		}
		return set; //return the set we created and populated
	}

	/**
	 * Returns a form of the enum name appropriate for serialization.
	 * <p>If the enum is a lexical {@link Identifier}, the name is converted to lowercase and all underscore characters ('_') are replaced by hyphens ('-'), i.e.
	 * from <code>CONSTANT_CASE</code> to <code>kebab-case</code>. For example, <code>FILE_NOT_FOUND</code> would produce <code>file-not-found</code>.</p>
	 * @implNote JDK 6/7 did not work with some enum generics if {@code <E extends Enum<E>>} was used in the signature, but in Eclipse 4.2.1 it worked fine.
	 *           Nevertheless using {@code Enum<?>} seems more flexible in general as a parameter.
	 * @param e The enum instance to convert to a serialization form.
	 * @return A string representing the enum instance in a style appropriate for use in serialization.
	 * @see Enum#name()
	 * @see Identifier
	 * @see CompoundTokenization#CONSTANT_CASE
	 * @see CompoundTokenization#KEBAB_CASE
	 */
	public static String getSerializationName(final Enum<?> e) {
		final String name = e.name();
		return e instanceof Identifier //if the enum is an identifier, return its serialized form
				? CONSTANT_CASE.to(KEBAB_CASE, name)
				: name;
	}

	/**
	 * Returns the appropriate enum that has been serialized.
	 * <p>If the enum is a lexical {@link Identifier}, the name is converted to uppercase and all hyphen characters ('-') are replaced by underscores ('_'), i.e.
	 * from <code>kebab-case</code> to <code>CONSTANT_CASE</code>, in order to determine the original enum name. For example, <code>file-not-found</code> would
	 * produce <code>FILE_NOT_FOUND</code>. This method assumes that the original enum name does not contain lowercase letters.</p>
	 * @param <E> The type of the enum.
	 * @param enumType The class object of the enum type from which to return an enum.
	 * @param serializationName The serialization form of the name of the enum to return.
	 * @return The enum constant of the specified enum type with the specified serialization name.
	 * @throws NullPointerException if the enum type and/or the serialization name is <code>null</code>.
	 * @throws IllegalArgumentException if the specified enum type has no constant with the specified serialization name, or the specified class object does not
	 *           represent an enum type.
	 * @see Enum#valueOf(Class, String)
	 * @see Identifier
	 * @see CompoundTokenization#KEBAB_CASE
	 * @see CompoundTokenization#CONSTANT_CASE
	 */
	public static <E extends Enum<E>> E getSerializedEnum(@Nonnull final Class<E> enumType, @Nonnull String serializationName) {
		final String name = Identifier.class.isAssignableFrom(enumType) //if the enum is an identifier, use the deserialized name form
				? KEBAB_CASE.to(CONSTANT_CASE, serializationName)
				: serializationName;
		return Enum.valueOf(enumType, name); //try to retrieve a corresponding enum from the original form of the name
	}

	/**
	 * Creates a function for mapping from a serialized form of some enum type to an instance of that enum type.
	 * @implSpec Deserialization is performed using {@link #getSerializedEnum(Class, String)}.
	 * @param <E> The type of the enum.
	 * @param enumType The class object of the enum type from which to return an enum.
	 * @return A function for mapping to an instance of the specified enum type from a serialization name.
	 * @throws NullPointerException if the enum type is <code>null</code>.
	 * @see #getSerializedEnum(Class, String)
	 */
	public static <E extends Enum<E>> Function<String, E> fromSerializionOf(@Nonnull final Class<E> enumType) {
		return serializationName -> getSerializedEnum(enumType, serializationName);
	}

	/**
	 * Returns an identifying string for the enum that includes the enum class and the enum name. The ID will be in the form
	 * <code><var>com.example.EnumClass</var>.<var>NAME</var></code>.
	 * @apiNote This ID is useful for resource keys, for example.
	 * @param <E> The type of the enum.
	 * @param e The enum instance for which to return an ID.
	 * @return The identifying string for the given enum.
	 * @throws NullPointerException if the given enum is <code>null</code>.
	 * @see Classes#getPropertyName(Class, String)
	 * @see Enum#getClass()
	 * @see Enum#name()
	 */
	public static <E extends Enum<E>> String getPropertyName(final E e) {
		return getPropertyName(e, null); //get an ID with no property
	}

	/**
	 * Returns an identifying string for the enum that includes the enum class, the enum name, and an optional property or aspect (such as "label" or "glyph").
	 * The ID will be in the form <code><var>com.example.EnumClass</var>.<var>NAME</var>.<var>property</var></code>.
	 * @apiNote This ID is useful for resource keys, for example.
	 * @param <E> The type of the enum.
	 * @param e The enum instance for which to return an ID.
	 * @param property The name of the enum property, or <code>null</code> if no property is desired.
	 * @return A string identification of the enum.
	 * @throws NullPointerException if the given enum is <code>null</code>.
	 * @see Classes#getPropertyName(Class, String)
	 * @see Enum#getClass()
	 * @see Enum#name()
	 */
	public static <E extends Enum<E>> String getPropertyName(final E e, final String property) {
		final String classPropertyName = Classes.getPropertyName(e.getClass(), e.name()); //com.example.EnumClass.NAME
		return property != null ? classPropertyName + OBJECT_PREDICATE_SEPARATOR + property : classPropertyName; //if a property was given, append it
	}

}
