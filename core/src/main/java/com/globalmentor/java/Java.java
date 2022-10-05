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

import static com.globalmentor.java.StringBuilders.*;

/**
 * Various utilities to assist programming with language-specific Java features.
 * @author Garret Wilson
 */
public class Java {

	/** The name extension for Java files. */
	public static final String JAVA_FILENAME_EXTENSION = "java";

	/** The string representing the null keyword. */
	public static final String NULL_KEYWORD = "null";

	/** The character used to separate internal classes in Java class names. */
	public static final char INTERNAL_CLASS_SEPARATOR = '$';

	/** The character used to separate packages in Java class names. */
	public static final char PACKAGE_SEPARATOR = '.';

	/** The character used to separate an object and its predicate in an identifier string. */
	public static final char OBJECT_PREDICATE_SEPARATOR = '.';

	/**
	 * The system property name that represents whether internationalization is turned on.
	 * @see javax.swing.text.DefaultStyledDocument
	 */
	public static final String I18N_PROPERTY_NAME = "i18n";

	/** The Java URI scheme identifier. */
	public static final String JAVA_URI_SCHEME = "java";

	/** This class cannot be publicly instantiated. */
	private Java() {
	}

	/**
	 * Compares one object with another, taking into account that one or both objects may be <code>null</code>. If one object is <code>null</code> but the other
	 * is not, the <code>null</code> object is considered to be less than the non-<code>null</code> object.
	 * @param <T> The type of object being compared.
	 * @param comparable1 The first object to compare.
	 * @param comparable2 The second object to compare.
	 * @return A negative integer, zero, or a positive integer if the first object is less than, equal to, or greater than the specified annotation, respectively,
	 *         with a <code>null</code> considered less than a non-<code>null</code> value.
	 * @see Comparable#compareTo(Object)
	 */
	public static final <T extends Comparable<T>> int compareTo(final T comparable1, final T comparable2) { //TODO move to Comparables
		if(comparable1 != null && comparable2 != null) //if both objects are non-null
			return comparable1.compareTo(comparable2); //compare the objects
		else if(comparable1 == comparable2) //if both objects are null (we know at this point that one object is null, so if the objects are equal then both are null)
			return 0; //the objects are equal
		else
			//if one objects is null and the other isn't
			return comparable1 == null ? -1 : 1; //the null object is lower
	}

	/**
	 * Constructs a proper name from the given name by capitalizing the first letter of the name.
	 * @param name The name to convert to a proper name.
	 * @return A proper name appropriate for the given name.
	 * @see #getVariableName(String)
	 */
	public static String getProperName(final String name) {
		if(!Character.isUpperCase(name.charAt(0))) { //if the first letter is not in uppercase
			final StringBuilder stringBuilder = new StringBuilder(name); //create a new string builder containing the name
			stringBuilder.setCharAt(0, Character.toUpperCase(stringBuilder.charAt(0))); //make sure the first letter is in uppercase
			return stringBuilder.toString(); //convert the string buffer to a string and return it
		} else
			//if the first letter is already in uppercase
			return name; //return the name itself; it's already a proper name
	}

	/**
	 * Constructs a variable name from the given name by decapitalizing all of the beginning uppercase letters of the name.
	 * @param name The name to convert to a variable name.
	 * @return A variable name appropriate for the given name.
	 * @see #getProperName(String)
	 */
	public static String getVariableName(final String name) {
		final StringBuilder stringBuilder = new StringBuilder(name); //create a new string builder with which to examine and modify the name
		final int length = stringBuilder.length(); //get the length of the string builder
		for(int i = 0; i < length; ++i) { //for each character
			final char character = stringBuilder.charAt(i); //get the current character
			if(Character.isUpperCase(character)) { //if this is an uppercase character TODO check for extended characters
				stringBuilder.setCharAt(i, Character.toLowerCase(character)); //convert the character to lowercase
			} else { //if the character is already lowercase
				break; //we've converted all the beginning uppercase characters to lowercase
			}
		}
		return stringBuilder.toString(); //return the variable name we created
	}

	/** Illegal variable characters to be replaced when creating a valid variable name. */
	private static final Characters ILLEGAL_VARIABLE_NAME_CHARACTERS = Characters.of(PACKAGE_SEPARATOR, INTERNAL_CLASS_SEPARATOR);

	/**
	 * Creates a safe Java variable name by replacing all illegal characters with the underscore ('_') character. This version only checks for the '.' and '$'
	 * characters.
	 * @param string A string to convert to a variable
	 * @return The string converted to a safe Java variable name.
	 */
	public static String createVariableName(final String string) {
		final StringBuilder stringBuilder = new StringBuilder(string);
		replace(stringBuilder, ILLEGAL_VARIABLE_NAME_CHARACTERS, '_'); //replace every '.' and '$' with '_'
		return stringBuilder.toString();
	}

}
