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

/**Utilities for working with Roman numerals.
	<p>Integers are used to represent Roman numerals. As a Java integer is 32
	bits long, a string would need over two million Roman numeral 'M's to surpass
	this limit.</p>
	<p>Conversion algoritms inspired from "When in Rome...." by Jonathan Knudsen
	at <a href="http://java.oreilly.com/bite-size/java_0698.html">
		http://java.oreilly.com/bite-size/java_0698.html</a>.</p>
@author Garret Wilson
@see <a href="http://java.oreilly.com/bite-size/java_0698.html">When in Rome....</a>
@see <a href="http://www.damtp.cam.ac.uk/user/bp10004/cgi_roman.html">Roman numeral conversion</a>
*/
public class RomanNumerals
{
	/**The Roman numerals in lowercase.*/
	private static final char[] LOWERCASE_ROMAN_NUMERALS={'i', 'v', 'x', 'l', 'c', 'd', 'm'};
	/**The Roman numerals in uppercase.*/
	private static final char[] UPPERCASE_ROMAN_NUMERALS={'I', 'V', 'X', 'L', 'C', 'D', 'M'};
	/**The Roman numeral values that correspond to the numerals.*/
	private static final int[] ROMAN_NUMERAL_VALUES={1, 5, 10, 50, 100, 500, 1000};
//TODO fix private static final int kLargestDigitIndex = kLowers.length / 2;

	/**Default constructor.*/
	public RomanNumerals()
	{
	}

	/**Returns the value of the given Roman numeral character.
	@param c The character containing a Roman numeral character.
	@return The value of the given Roman numeral character, or -1 if the character
		isn't recognized.
	*/
	public static int getValue(char c)
	{
		c=Character.toLowerCase(c); //convert the character to lowercase
		for(int i=0; i<LOWERCASE_ROMAN_NUMERALS.length; ++i)  //look at each numeral
		{
			if(LOWERCASE_ROMAN_NUMERALS[i]==c)  //if we find the letter
				return ROMAN_NUMERAL_VALUES[i]; //return the corresponding value
		}
		return -1;  //show that we couldn't find the value for the character
	}

	/**Parses the given Roman numerals and returns their integer value.
	@param romanNumerals The characters representing the Roman numerals.
	@return The value of the Roman numerals.
	@exception NumberFormatException Thrown if the given string does not contain
		a valid Roman numeral value.
	*/
	public static int parseRomanNumerals(final String romanNumerals)
	{
		int value=0; //we'll store the value here
		final int length=romanNumerals.length();  //get the text length
		for(int i=0; i<length; ++i) //look at each Roman numeral
		{
		  final char c=romanNumerals.charAt(i); //get the character to convert
			final int thisValue=getValue(c); //get the value of this character
			if(thisValue>=0)  //if this character represents a valid value
			{
				if(i<length-1)  //if we're not at the end of the string
				{
					final int nextValue=getValue(romanNumerals.charAt(i+1)); //find the value after this character as well
					if(nextValue>thisValue) //if the next value is larger than this one (don't check its validity---we'll do that on the next time around)
					{
						value-=thisValue; //subtract this value from the entire value
						continue; //go to the next value
					}
				}
				value+=thisValue; //add this value to the total value
			}
			else  //if this is not a valid value
				throw new NumberFormatException(romanNumerals); //indicate that this string doesn't represent a Roman numeral
		}
		return value; //return the value we calculated
	}

}