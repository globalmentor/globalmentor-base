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

package com.globalmentor.country.us;

import static com.globalmentor.java.Objects.*;
import static com.globalmentor.java.Conditions.*;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.globalmentor.java.Integers;
import com.globalmentor.text.ArgumentSyntaxException;

/**
 * Encapsulation of a United States Social Security Number (SSN).
 * <p>
 * A Social Security Number is a nine-digit number composed a of three groups:
 * </p>
 * <ol>
 * <li>A three-digit <dfn>Area Number</dfn>.</li>
 * <li>A two-digit <dfn>Group Number</dfn>.</li>
 * <li>A three-digit <dfn>Serial Number</dfn>.</li>
 * </ol>
 * <p>
 * A SSN is accepted in the form "AAAGGSSSS" or "AAA-GG-SSSS".
 * @see <a href="http://en.wikipedia.org/wiki/Social_security_number">Wikipedia: Social Security Number</a>
 * @see <a href="http://www.ssa.gov/history/ssn/geocard.html">The SSN Numbering Scheme</a>
 * @author Garret Wilson
 */
public class SSN {

	/** The length of the area number. */
	public final static int AREA_NUMBER_LENGTH = 3;

	/** The length of the group number. */
	public final static int GROUP_NUMBER_LENGTH = 2;

	/** The length of the serial number. */
	public final static int SERIAL_NUMBER_LENGTH = 4;

	/** The canonical delimiter for social security numbers. */
	public final static char DELIMITER = '-';

	/**
	 * A regular expression pattern matching a SSN in the form "XXXXXXXXX" or "XXX-XX-XXXX". Three regular expression groups (1, 2, and 3; or 4, 5, and 6;
	 * depending on format) are formed representing the area number, the group number, and the serial number.
	 */
	public final static Pattern PATTERN = Pattern.compile("(?:(\\d{3})(\\d{2})(\\d{4}))|(?:(\\d{3})\\-(\\d{2})\\-(\\d{4}))");

	/** The area number. */
	private final int areaNumber;

	/** @return The area number. */
	public int getAreaNumber() {
		return areaNumber;
	}

	/** @return The three-digit area number string. */
	public String getAreaNumberString() {
		return Integers.toString(getAreaNumber(), 10, AREA_NUMBER_LENGTH);
	}

	/** The group number. */
	private final int groupNumber;

	/** @return The group number. */
	public int getGroupNumber() {
		return groupNumber;
	}

	/** @return The two-digit group number string. */
	public String getGroupNumberString() {
		return Integers.toString(getGroupNumber(), 10, GROUP_NUMBER_LENGTH);
	}

	/** The serial number. */
	private final int serialNumber;

	/** @return The serial number. */
	public int getSerialNumber() {
		return serialNumber;
	}

	/** @return The four-digit serial number string. */
	public String getSerialNumberString() {
		return Integers.toString(getSerialNumber(), 10, SERIAL_NUMBER_LENGTH);
	}

	/** The value of the social security number. */
	private final int value;

	/** @return The value of the social security number. */
	public int getValue() {
		return value;
	}

	/**
	 * SSN value constructor.
	 * @param ssn The social security number value.
	 * @throws IllegalArgumentException if the resulting string is longer than nine digits.
	 * @throws ArgumentSyntaxException if the resulting area number is 000.
	 * @throws ArgumentSyntaxException if the resulting group number is 00.
	 * @throws ArgumentSyntaxException if the resulting serial number is 0000.
	 */
	public SSN(final int ssn) throws ArgumentSyntaxException {
		this(Integers.toString(checkArgumentRange(ssn, 0, 999999999), 10, AREA_NUMBER_LENGTH + GROUP_NUMBER_LENGTH + SERIAL_NUMBER_LENGTH)); //create a string from the SSN value
	}

	/**
	 * Character sequence constructor.
	 * @param ssn A SSN character sequence in the form "XXXXXXXXX" or "XXX-XX-XXXX".
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 * @throws ArgumentSyntaxException if the character sequence is not in the form "XXXXXXXXX" or "XXX-XX-XXXX".
	 * @throws ArgumentSyntaxException if the area number is 000.
	 * @throws ArgumentSyntaxException if the group number is 00.
	 * @throws ArgumentSyntaxException if the serial number is 0000.
	 */
	public SSN(final CharSequence ssn) throws ArgumentSyntaxException {
		final Matcher matcher = PATTERN.matcher(checkInstance(ssn, "Social security number cannot be null.")); //create a matcher from the SSN pattern
		if(matcher.matches()) { //if the SSN matches the pattern
			final int groupDelta = matcher.group(1) != null ? 0 : 3; //see if we should use the first or second set of groups
			areaNumber = Integer.parseInt(matcher.group(1 + groupDelta)); //save the area number
			if(areaNumber == 0) { //if the area number is zero
				throw new ArgumentSyntaxException("SSN area number cannot be 000.");
			}
			groupNumber = Integer.parseInt(matcher.group(2 + groupDelta)); //save the group number
			if(groupNumber == 0) { //if the group number is zero
				throw new ArgumentSyntaxException("SSN group number cannot be 00.");
			}
			serialNumber = Integer.parseInt(matcher.group(3 + groupDelta)); //save the serial number
			if(serialNumber == 0) { //if the serial number is zero
				throw new ArgumentSyntaxException("SSN serial number cannot be 0000.");
			}
			value = Integer.parseInt(getPlainString()); //save the integer value of the SSN
		} else { //if the SSN doesn't match the pattern
			throw new ArgumentSyntaxException("SSN " + ssn + " is not a valid social security number in the form \"XXXXXXXXX\" or \"XXX-XX-XXXX\".");
		}
	}

	/** @return A hash code representing this object. */
	public int hashCode() {
		return getValue(); //return the value itself
	}

	/**
	 * Determines if this object is equivalent to another object. This method considers another object equivalent if it is another SSN with the same value.
	 * @return <code>true</code> if the given object is an equivalent SSN.
	 */
	public boolean equals(final Object object) {
		return object instanceof SSN && getValue() == ((SSN)object).getValue(); //if the object is a SSN, compare values
	}

	/**
	 * Compares this object with the specified object for order.
	 * @param ssn The object to be compared.
	 * @return A negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	 */
	public int compareTo(final SSN ssn) {
		return getValue() - ssn.getValue(); //compare values
	}

	/**
	 * Returns a plain, unformatted string representation of this social security number in the form "XXXXXXXXX".
	 * @return A string in the form "XXXXXXXXX" representing this social security number.
	 */
	public String getPlainString() {
		return getAreaNumberString() + getGroupNumberString() + getSerialNumberString(); //area number + group number + serial Number
	}

	/**
	 * Returns a canonical string representation of this social security number in the form "XXX-XX-XXXX".
	 * @return A string in the form "XXX-XX-XXXX" representing this social security number.
	 */
	public String getCanonicalString() {
		return getAreaNumberString() + DELIMITER + getGroupNumberString() + DELIMITER + getSerialNumberString(); //"AAA-GG-SSSS"
	}

	/**
	 * Returns a string representation of this social security number. This implementation delegates to {@link #getCanonicalString()}.
	 * @return A string in the form "XXX-XX-XXXX" representing this social security number.
	 */
	public String toString() {
		return getCanonicalString(); //returnthe canonical string
	}

}
