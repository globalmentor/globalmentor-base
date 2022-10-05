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

package com.globalmentor.lex;

import com.globalmentor.java.Enums;

/**
 * Constants and utilities for lexical analysis.
 * @author Garret Wilson
 */
public class Lex {

	/**
	 * Returns a serialized form of an identifier. Usually the identifier is an {@link Enum}, resulting in a special token serialized form.
	 * @param identifier The identifier to serialize.
	 * @return The serialized form of the identifier.
	 * @throws NullPointerException if the given identifier is <code>null</code>.
	 * @see Enums#getSerializationName(Enum)
	 */
	public static String serialize(final Identifier identifier) {
		if(identifier instanceof Enum<?>) {
			final Enum<?> enumIdentifier = (Enum<?>)identifier;
			return Enums.getSerializationName(enumIdentifier);
		} else {
			return identifier.toString();
		}
	}

	/**
	 * Deserializes an identifier from its lexical form. Usually the identifier is an {@link Enum} and is deserialized from a special token serialized form.
	 * <p>
	 * This implementation currently only recognizes {@link Enum} types.
	 * </p>
	 * @param <I> The identifier class.
	 * @param identifierClass The class of identifier; usually a type of {@link Enum}.
	 * @param lexicalForm The lexical form of the identifier being deserialized.
	 * @return The identifier represented by the given lexical form.
	 * @throws NullPointerException if the given class and/or lexical form is <code>null</code>.
	 * @throws IllegalArgumentException if the identifier class is not a recognized type to be deserialized.
	 * @see Enums#getSerializedEnum(Class, String)
	 */
	public static <I extends Identifier> I deserialize(final Class<I> identifierClass, final String lexicalForm) {
		if(Enum.class.isAssignableFrom(identifierClass)) {
			@SuppressWarnings({"unchecked", "rawtypes"})
			final I identifier = (I)Enums.getSerializedEnum((Class<? extends Enum>)identifierClass, lexicalForm);
			return identifier;
		}
		throw new IllegalArgumentException("No known way to deserialize identifier type " + identifierClass);
	}

}
