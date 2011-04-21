/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.StringBuilders.*;

/**
 * Various utilities to assist programming with language-specific Java features.
 * @author Garret Wilson
 */
public class Java
{

	/** The name extension for Java files. */
	public final static String JAVA_NAME_EXTENSION = "java";

	/** The string representing the null keyword. */
	public final static String NULL_KEYWORD = "null";

	/** The character used to separate internal classes in Java class names. */
	public final static char INTERNAL_CLASS_SEPARATOR = '$';

	/** The character used to separate packages in Java class names. */
	public final static char PACKAGE_SEPARATOR = '.';

	/** The character used to separate an object and its predicate in an identifier string. */
	public final static char OBJECT_PREDICATE_SEPARATOR = '.';

	/**
	 * The system property name that represents whether internationalization is turned on.
	 * @see javax.swing.text.DefaultStyledDocument
	 */
	public final static String I18N_PROPERTY_NAME = "i18n";

	/** The Java URI scheme identifier. */
	public final static String JAVA_URI_SCHEME = "java";

	/** This class cannot be publicly instantiated. */
	private Java()
	{
	}

	/**
	 * Compares one object with another, taking into account that one or both objects may be <code>null</code>. If one object is <code>null</code> but the other
	 * is not, the <code>null</code> object is considered to be less than the non-<code>null</code> object.
	 * @param <T> The type of object being compared.
	 * @param comparable1 The first object to compare.
	 * @param comparable2 The second object to compare.
	 * @return A negative integer, zero, or a positive integer if the first object is less than, equal to, or greater than the specified annotation, respectively,
	 *         with a <code>null</code> considered less than a non-<code>null</code> value.
	 * @see Comparable#compareTo
	 */
	public final static <T extends Comparable<T>> int compareTo(final T comparable1, final T comparable2)
	{
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
	public static String getProperName(final String name)
	{
		if(!Character.isUpperCase(name.charAt(0))) //if the first letter is not in uppercase
		{
			final StringBuilder stringBuilder = new StringBuilder(name); //create a new string builder containing the name
			stringBuilder.setCharAt(0, Character.toUpperCase(stringBuilder.charAt(0))); //make sure the first letter is in uppercase
			return stringBuilder.toString(); //convert the string buffer to a string and return it
		}
		else
			//if the first letter is already in uppercase
			return name; //return the name itself; it's already a proper name
	}

	/**
	 * Constructs a variable name from the given name by decapitalizing all of the beginning uppercase letters of the name.
	 * @param name The name to convert to a variable name.
	 * @return A variable name appropriate for the given name.
	 * @see #getProperName(String)
	 */
	public static String getVariableName(final String name)
	{
		final StringBuilder stringBuilder = new StringBuilder(name); //create a new string builder with which to examine and modify the name
		final int length = stringBuilder.length(); //get the length of the string builder
		for(int i = 0; i < length; ++i) //for each character
		{
			final char character = stringBuilder.charAt(i); //get the current character
			if(Character.isUpperCase(character)) //if this is an uppercase character TODO check for extended characters
			{
				stringBuilder.setCharAt(i, Character.toLowerCase(character)); //convert the character to lowercase
			}
			else
			//if the character is already lowercase
			{
				break; //we've converted all the beginning uppercase characters to lowercase
			}
		}
		return stringBuilder.toString(); //return the variable name we created
	}

	/** Illegal variable characters to be replaced when creating a valid variable name. */
	private final static Characters ILLEGAL_VARIABLE_NAME_CHARACTERS = new Characters(PACKAGE_SEPARATOR, INTERNAL_CLASS_SEPARATOR);

	/**
	 * Creates a safe Java variable name by replacing all illegal characters with the underscore ('_') character. This version only checks for the '.' and '$'
	 * characters.
	 * @param string A string to convert to a variable
	 */
	public static String createVariableName(final String string)
	{
		final StringBuilder stringBuilder = new StringBuilder(string);
		replace(stringBuilder, ILLEGAL_VARIABLE_NAME_CHARACTERS, '_'); //replace every '.' and '$' with '_'
		return stringBuilder.toString();
	}

	/**
	 * Returns the class of the caller on the stack before the class of this method's caller.
	 * <p>
	 * All callers from this class are ignored to allow for the various convenience forms of this method.
	 * </p>
	 * @return the class of the last line of program execution before a method in the class of the caller to this method.
	 * @throws IllegalStateException if there was no caller in the stack before the caller to this method.
	 */
	public static Class<?> getCallingClass()
	{
		return getCallingClass(null); //get the calling class, ignoring no classes
	}

	/**
	 * Returns the class of the caller on the stack before the class of this method's caller, ignoring any occurrences of the given class, if any.
	 * <p>
	 * All callers from this class are ignored to allow for the various convenience forms of this method.
	 * </p>
	 * @param ignoreClass The class to ignore in the stack, or <code>null</code> if the first caller before this method's caller should be returned and no other
	 *          classes should be ignored.
	 * @return the class of the last line of program execution before a method in the class of the caller to this method, skipping the ignore class, if any.
	 * @throws IllegalStateException if no ignore class was provided and there was no caller in the stack before the caller to this method.
	 * @throws IllegalArgumentException if an ignore class was provided and there was no caller in the stack before the caller to this method that wasn't from the
	 *           given class.
	 */
	public static Class<?> getCallingClass(final Class<?> ignoreClass)
	{
		try
		{
			return Class.forName(getCallingClassStackTraceElement(ignoreClass).getClassName()); //get the calling class stack trace and determine the class
		}
		catch(final ClassNotFoundException classNotFoundException) //since the class is calling us, the class should exist and already be loaded
		{
			throw new AssertionError(classNotFoundException);
		}
	}

	/**
	 * Returns a stack trace element describing the caller on the stack before the class of this method's caller.
	 * <p>
	 * All callers from this class are ignored to allow for the various convenience forms of this method.
	 * </p>
	 * @return A stack trace element representing the last line of program execution before a method in the class of the caller to this method.
	 * @throws IllegalStateException if there was no caller in the stack before the caller to this method.
	 */
	public static StackTraceElement getCallingClassStackTraceElement()
	{
		return getCallingClassStackTraceElement(null); //get the previous stack trace element, ignoring no classes
	}

	/**
	 * Returns a stack trace element describing the caller on the stack before the class of this method's caller, ignoring any occurrences of the given class, if
	 * any.
	 * <p>
	 * All callers from this class are ignored to allow for the various convenience forms of this method.
	 * </p>
	 * @param ignoreClass The class to ignore in the stack, or <code>null</code> if the first caller before this method's caller should be returned and no other
	 *          classes should be ignored.
	 * @return A stack trace element representing the last line of program execution before a method in the class of the caller to this method, skipping the
	 *         ignore class, if any.
	 * @throws IllegalStateException if no ignore class was provided and there was no caller in the stack before the caller to this method.
	 * @throws IllegalArgumentException if an ignore class was provided and there was no caller in the stack before the caller to this method that wasn't from the
	 *           given class.
	 */
	public static StackTraceElement getCallingClassStackTraceElement(final Class<?> ignoreClass)
	{
		final String thisClassName = Java.class.getName(); //get the name of this class so we can ignore it
		final String ignoreClassName = ignoreClass != null ? ignoreClass.getName() : null;
		final StackTraceElement[] stack = new Throwable().getStackTrace(); //get the current stack
		final int stackLength = stack.length;
		assert stackLength > 1 : "There should have been at least two stack entries: this method, and the one before it.";
		int i;
		for(i = 0; i < stackLength && thisClassName.equals(stack[i].getClassName()); ++i)
			; //skip all the callers from this class
		assert i < stackLength : "There should have been a caller that is not in this class, as this class has no main() method.";
		final String callerClassName = stack[i].getClassName(); //the current stack trace element should now be that of the caller
		StackTraceElement beforeCallerStackTraceElement = null;
		for(i = i + 1; i < stackLength; ++i) //start looking one stack element before the caller
		{
			final StackTraceElement stackTraceElement = stack[i];
			final String stackTraceElementClassName = stackTraceElement.getClassName();
			if(beforeCallerStackTraceElement == null) //if we haven't found the stack trace element before the caller
			{
				if(!callerClassName.equals(stackTraceElementClassName)) //if this location was before the caller
				{
					if(ignoreClassName == null || ignoreClassName.equals(callerClassName)) //if we shouldn't go back any further (either because there is no extra class to ignore, or because it is the same class as the caller)
					{
						return stackTraceElement; //return this location
					}
					else
					//if we should keep looking back before some class
					{
						beforeCallerStackTraceElement = stackTraceElement; //note that we found the class before the caller
					}
				}
			}
			if(beforeCallerStackTraceElement != null) //if we've already found the class before the caller (we can only get here if an ignore class was provided)
			{
				if(!ignoreClassName.equals(stackTraceElementClassName)) //if this location was before the before class
				{
					return stackTraceElement; //we found the class before the before class
				}
			}
		}
		if(ignoreClass != null)
		{
			throw new IllegalArgumentException("No caller exists on the stack before class " + ignoreClassName);
		}
		else
		{
			throw new IllegalStateException("No caller exists on the stack before class " + callerClassName);
		}
	}

}