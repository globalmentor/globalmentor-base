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

package com.globalmentor.text.elff;

/**
 * An enumeration encapsulating the WebTrends true/false values. The string representation of this enumeration is that expected by WebTrends log files.
 * @author Garret Wilson
 */
public enum WebTrendsYesNo {

	/** The value for "Yes" used by WebTrends for true values. */
	YES("Yes"),

	/** The value for "No" used by WebTrends for false values. */
	NO("No");

	/** The string representation of this value. */
	private final String string;

	/**
	 * String representation constructor.
	 * @param string The string representation of this instance.
	 */
	private WebTrendsYesNo(final String string) {
		this.string = string;
	}

	/** @return The string representation of this value. */
	public String toString() {
		return string;
	}

	/**
	 * Converts the given value to a yes/no enumeration instance.
	 * @param value The boolen value to convert.
	 * @return {@link #YES} for <code>true</code> or {@link #NO} for <code>false</code>.
	 */
	public static WebTrendsYesNo asYesNo(final boolean value) {
		return value ? YES : NO; //return yes or no based upon the value
	}
}
