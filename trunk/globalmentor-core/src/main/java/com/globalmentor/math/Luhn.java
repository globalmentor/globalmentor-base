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

package com.globalmentor.math;

/**Methods implementing the Luhn Algorithm or "modulus 10".
	Used, for example, by the
	Primary Account Number (PAN) of an identification card as defined in ISO/IEC 7812-1:2000(E),
	"Identification cards — Identification of issuers — Part 1: Numbering system".
See ISO/IEC 7812-1:2000(E), "Annex B: Luhn formula for computing modulus 10 'double-add-double' check digit".
@author Garret Wilson
@see <a href="http://en.wikipedia.org/wiki/Luhn_formula">Luhn algorithm</a>
*/
public class Luhn
{

	/**Determines if the given sequence of digits ends with the correct check digit.
	Each digit must be a value from '0' to '9', inclusive.
	If the sequence of digits is empty, this method returns <code>false</code>.
	@param digits The digits to be verified against a trailing check digit.
	@return <code>true</code> if this character sequence ends with a check digit that correctly checks the other characters according to the Luhn algorithm.
	@exception NullPointerException if the given sequence of digits is <code>null</code>.
	@exception IllegalArgumentException if one of the the given digits is not a character between '0' to '9', inclusive.
	*/
	public final static boolean isDigitsCheck(final CharSequence digits)
	{
		final int length=digits.length();	//get the length of digits
		final int lastDigitIndex=length-1;	//get the index of the last digit
		return length>0 ? getCheckDigit(digits, 0, lastDigitIndex)==digits.charAt(lastDigitIndex) : false;
	}	

	/**Calculates the check digit of the entire given sequence of digits.
	Each digit must be a value from '0' to '9', inclusive.
	@param digits The digits for which a check digit should be calculated.
	@return The character representing the check digit generated for the given digits according to the Luhn algorithm.
	@exception NullPointerException if the given sequence of digits is <code>null</code>.
	@exception IllegalArgumentException if one of the the given digits is not a character between '0' to '9', inclusive.
	*/
	public final static char getCheckDigit(final CharSequence digits)
	{
		return getCheckDigit(digits, 0, digits.length());	//calculate a check digit for the entire sequence of digits
	}

	/**Calculates the check digit of the given sequence of digits within the given range.
	Each digit must be a value from '0' to '9', inclusive.
	If the sequence of digits is empty, this method returns '0'.
	@param digits The digits for which a check digit should be calculated.
	@param startIndex The starting index for which a check digit should be calculated.
	@param endIndex One more than the last index to include in a check digit calculation.
	@return The character representing the check digit generated for the given digits according to the Luhn algorithm.
	@exception NullPointerException if the given sequence of digits is <code>null</code>.
	@exception IndexOutOfBoundsException if the given range references one or more indexes outside the character sequence.
	@exception IllegalArgumentException if one of the the given digits is not a character between '0' to '9', inclusive.
	*/
	public final static char getCheckDigit(final CharSequence digits, final int start, final int end)
	{
		final int length=end-start;	//get the length of the digits
		final int[] digitValues=new int[length];	//create an array of values
		for(int i=start; i<end; ++i)	//for each digit character
		{
			digitValues[i-start]=digits.charAt(i)-'0';	//convert this digit to its corresponding value
		}
		return (char)('0'+getCheckDigitValue(digitValues));	//calculate the check digit and 
	}

	/**Determines the value of the check digit for the given digit values.
	If the sequence of digits is empty, this method returns zero.
	@param digitValues The values digits represent.
	@return The check digit value the Luhn algorithm specifies.
	@exception NullPointerException if the given array of values is <code>null</code>.
	@exception IllegalArgumentException if one of the the given digit values is 10 or greater.
	*/
	public final static int getCheckDigitValue(final int[] digitValues)
	{
		int length=digitValues.length;	//get the length of values
		if(length>0)	//if there are values
		{
			int sum=0;	//start out with no sum
			int i=length-1;	//start at the last value
			while(i>0)	//leave the first value uncalculated so that we can always be sure there is a previous value
			{
				sum+=doubleAdd(digitValues[i])+digitValues[i-1];	//double and add this digit value and then add the previous digit value
				i-=2;	//skip backwards two values
			}
			if(i==0)	//if we didn't yet include the first value
			{
				sum+=doubleAdd(digitValues[i]);	//double and add this digit value				
			}
			final int mod10=sum%10;	//determine the remainder of dividing by 10
			return mod10>0 ? 10-mod10 : mod10;	//calculate the tens complement if there is a remainder		
		}
		else	//if there are no values
		{
			return 0;	//the check digit value is zero
		}
	}

	/**Doubles the given digit value and adds the two decimal digits of the outcome.
	@param digitValue The digit value to double and then add.
	@return The sum of the decimal digits of the doubled value.
	@exception IllegalArgumentException if the given digit value is 10 or greater.
	*/
	public final static int doubleAdd(final int digitValue)
	{
		if(digitValue<5)	//if doubling would remain under 10
		{
			return digitValue<<1;	//double the value; there's nothing to add
		}
		else if(digitValue<10)	//for all other values under 10
		{
			return ((digitValue-5)<<1)+1;	//this formula is equivalent to digitValue+1+((digitValue*2)-10) for values 5 and higher
		}
		else	//if this is a two-digit value, that isn't allowed
		{
			throw new IllegalArgumentException("Digit "+digitValue+" must be less than 10.");
		}
	}
}
