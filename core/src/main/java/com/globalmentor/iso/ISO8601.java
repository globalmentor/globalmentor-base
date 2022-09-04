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

package com.globalmentor.iso;

import com.globalmentor.java.Characters;

/**
 * Definitions related to ISO 8601-2004, "Data elements and interchange formats — Information interchange — Representation of dates and times".
 * @author Garret Wilson
 */
public class ISO8601 {

	/** The delimiter (for "period") that introduces a duration. */
	public static final char PERIOD_BEGIN = 'P';
	/** The delimiter that introduces a time component in a temporal. */
	public static final char TIME_BEGIN = 'T';
	/** The delimiter that separates year components in a temporal. */
	public static final char DATE_DELIMITER = '-';
	/** The delimiter that separates time components in a temporal. */
	public static final char TIME_DELIMITER = ':';
	/** The delimiter that separates time subseconds from seconds. */
	public static final char TIME_SUBSECONDS_DELIMITER = '.';
	/** The UTC designator for "Zulu Time", 'Z'. */
	public static final char UTC_DESIGNATOR = 'Z';

	/** The possible signs of a number. */
	public static final Characters SIGNS = Characters.of('-', '+');
	/**
	 * The most common decimal sign used as an ISO 8601 decimal fraction separator.
	 * @apiNote Although ISO 8601-200) specifies that "the comma is the preferred sign", the full stop character seems overwhelmingly to be used more in practice.
	 *          See survey and discussion at <a href="https://stackoverflow.com/q/20699705">Why does the ISO-8601 specification appear to be universally ignored
	 *          when it comes to decimals?</a>
	 */
	public static final char DECIMAL_SIGN = '.';
	/** The possible decimal representations in a number. */
	public static final Characters DECIMAL_SIGNS = Characters.of(DECIMAL_SIGN, ',');

	/** The designator for years in a duration expression. */
	public static final char YEAR_DESIGNATOR = 'Y';
	/** The designator for months in a duration expression. */
	public static final char MONTH_DESIGNATOR = 'M';
	/** The designator for days in a duration expression. */
	public static final char DAY_DESIGNATOR = 'D';
	/** The designator for hours in a duration expression. */
	public static final char HOUR_DESIGNATOR = 'H';
	/** The designator for minutes in a duration expression. */
	public static final char MINUTE_DESIGNATOR = 'M';
	/** The designator for seconds in a duration expression. */
	public static final char SECOND_DESIGNATOR = 'S';

}
