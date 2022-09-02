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

package com.globalmentor.java;

import com.globalmentor.text.Case;

/**
 * Utilities for manipulating integer objects.
 * @author Garret Wilson
 */
public class Integers {

	/** This class cannot be publicly instantiated. */
	private Integers() {
	}

	/**
	 * Compares an integer object with the value of a literal integer.
	 * @param integerObject The object that supposedly holds an integer.
	 * @param integer The literal integer value to compare to the integer object.
	 * @return <code>true</code> if the object is an <code>Integer</code>, the value of which equals that of <code>integer</code>.
	 */
	public static boolean equals(final Object integerObject, final int integer) {
		//return false if the object really isn't an integer, of the object is null
		return integerObject instanceof Integer ? ((Integer)integerObject).intValue() == integer : false;
	}

	/**
	 * Parses an integer value in the given string. If the string contains a decimal point followed by zeros (a double representation of an integer value), this
	 * method correctly returns the integer value without throwing an exception; this is the one difference between this method and
	 * <code>Integer.parseInt()</code>.
	 * @param string The string containing the integer value, which may or may not have a decimal point, but if it does the decimal should be followed by zeros.
	 * @return The integer value represented by the string.
	 * @throws NumberFormatException Thrown if the string does not contain a parsable integer.
	 * @see Integer#parseInt(String)
	 */
	public static int parseIntValue(String string) throws NumberFormatException {
		final int decimalIndex = string.indexOf('.'); //see if there is a decimal point in the string TODO use a constant
		if(decimalIndex >= 0) { //if there is a decimal point
			try {
				final int fraction = Integer.parseInt(string.substring(decimalIndex + 1)); //try to convert everything after the decimal point to an integer
				if(fraction == 0) //if the number has no fraction (it's in the form X.0...)
					return Integer.parseInt(string.substring(0, decimalIndex)); //convert the part before the decimal to an integer
			} catch(Exception e) {
			} //if any exceptions occur, ignore them and allow the Java routines to thrown a conversion exception, below
		}
		return Integer.parseInt(string); //if there's no need for special cases (this string appears to represent a normal integer), convert the string normally
	}

	/**
	 * Parses a string and returns its integer value, or a default value if the string does not contain an integer.
	 * @param string The string which might contain an integer value.
	 * @param defaultValue The default value if the string does not contain a valid integer.
	 * @return The integer value of the string, or the default value if the string does not contain a valid integer or if the string is <code>null</code>.
	 */
	public static int parseIntDefault(final String string, final int defaultValue) {
		if(string != null) { //if this is a valid string
			try {
				return Integer.parseInt(string); //return the integer value of the string
			} catch(NumberFormatException numberFormatException) {
			} //if the string does not contain an integer
			{
				return defaultValue; //return the default value
			}
		} else
			//if the string is null
			return defaultValue; //return the default value
	}

	/**
	 * Parses the given string and returns an ordinal value.
	 * @param ordinal The string containing an ordinal value, such as "first" or "second".
	 * @return The 1-based order of the string.
	 * @throws NumberFormatException Thrown if the given string does not contain a valid ordinal value.
	 */
	public static int parseOrdinal(final String ordinal) throws NumberFormatException {
		final int order = parseOrdinalValue(ordinal); //get the order
		if(order >= 0) //if a valid order was returned
			return order; //return the order
		else
			//if the order isn't valid
			throw new NumberFormatException(ordinal); //show that this was not an ordinal
	}

	/**
	 * Parses the given string and returns an ordinal value without throwing an exception if the string is invalid.
	 * @param ordinal The string containing an ordinal value, such as "first" or "second".
	 * @return The 1-based order of the string, or <code>-1</code> if the string does not contain a valid ordinal value.
	 */
	public static int parseOrdinalValue(final String ordinal) { //TODO the name of this should probably be changed
		//TODO add more complex parsing routines for the ordinals past 10
		final String[] ordinals = new String[] { "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh" };
		for(int i = 0; i < ordinals.length; ++i) { //look at each of the ordinals
			if(ordinals[i].equalsIgnoreCase(ordinal)) //if this string matches
				return i + 1; //return the order
		}
		return -1; //show that the string doesn't seem to contain an ordinal value
	}

	/**
	 * Parses the given text string and returns a number without throwing an exception if the string is invalid.
	 * @param numberText The string containing a number value, such as "one" or "two".
	 * @return The number contained of the string, or <code>-1</code> if the string does not contain a valid text number.
	 */
	public static int parseNumberTextValue(final String numberText) { //TODO the name of this should probably be changed; TODO i18n
		//TODO add more complex parsing routines for numbers past 30; i18n;
		final String[] numbers = new String[] { "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve",
				"thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "twenty-one", "twenty-two", "twenty-three", "twenty-four",
				"twenty-five", "twenty-six", "twenty-seven", "twenty-eight", "twenty-nine", "thirty" };
		for(int i = 0; i < numbers.length; ++i) { //look at each of the numbers
			if(numbers[i].equalsIgnoreCase(numberText)) //if this string matches
				return i; //return the number value
		}
		return -1; //show that the string doesn't seem to contain a number string value we have
	}

	/**
	 * Returns a string representation of a given integer in the given radix, modified to match the the given length.
	 * @param value An integer to be converted to a string.
	 * @param radix The radix to use in the string representation.
	 * @param length The number of digits the returned string should have.
	 * @return A string representation of the argument in the specified radix.
	 * @see Integer#toString(int, int)
	 */
	public static String toString(final int value, final int radix, final int length) {
		//convert the integer to a string, then make the string the correct length by padding the beginning with zeros
		return Strings.forceLength(Integer.toString(value, radix), length, '0', 0);
	}

	/**
	 * Converts an integer into a hex string with the specified number of digits.
	 * @param value The value to convert.
	 * @param length The number of digits the returned string should have.
	 * @return Lowercase hex version of the given value with the correct number of digits, using zeros to pad the left of the string to the correct length.
	 * @see Integer#toHexString(int)
	 */
	public static String toHexString(final int value, final int length) {
		return toHexString(value, length, Case.LOWERCASE); //default to lowercase hex characters
	}

	/**
	 * Converts an integer into a hex string with the specified number of digits.
	 * @param value The value to convert.
	 * @param length The number of digits the returned string should have.
	 * @param hexCase Whether the hex characters should be lowercase or uppercase.
	 * @return The hex version of the given value with the correct number of digits, using zeros to pad the left of the string to the correct length.
	 * @see Integer#toHexString(int)
	 */
	public static String toHexString(final int value, final int length, final Case hexCase) {
		String hexString = Integer.toHexString(value); //convert the integer to hex
		if(hexCase == Case.UPPERCASE) { //if uppercase is requested
			hexString = hexString.toUpperCase();
		}
		return Strings.forceLength(hexString, length, '0', 0); //make the string the correct length by padding the beginning with zeros
	}

	/**
	 * Creates an array of primitive values from the given array of objects.
	 * @param integers The objects to place in an primitive value array.
	 * @return An array of primitive values representing the given objects.
	 */
	public static int[] toIntArray(final Integer... integers) {
		final int count = integers.length; //find out how many integers there are
		final int[] ints = new int[count]; //create an array of ints
		for(int i = count - 1; i >= 0; --i) { //for each index
			ints[i] = integers[i].intValue(); //extract the int value and place it in the new array
		}
		return ints; //return the array we created and filled
	}

	/**
	 * Creates an array of objects from the given array of primitive values.
	 * @param ints The primitive values to place in an object array.
	 * @return An array of object values representing the given primitive values.
	 */
	public static Integer[] toIntegerArray(final int... ints) {
		final int count = ints.length; //find out how many ints there are
		final Integer[] integers = new Integer[count]; //create an array of integers
		for(int i = count - 1; i >= 0; --i) { //for each index
			integers[i] = Integer.valueOf(ints[i]); //create an object from this integer and place it in the new array
		}
		return integers; //return the array we created and filled
	}

}