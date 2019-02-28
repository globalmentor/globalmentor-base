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

package com.globalmentor.util.regex;

import java.util.regex.Matcher;

/**
 * Utilities for regular expression matchers.
 * @author Garret Wilson
 */
public class Matchers {

	/**
	 * Returns an array of integer versions from the available groups of the given matcher. <code>null</code> groups will be counted as zero. That is, if a
	 * matcher contains groups "2", "3", and "null", the array <code>{2, 3, 0}</code> will be returned.
	 * @param matcher The matcher for which an integer array of group values should be returned.
	 * @return An array of integers representing the integer values of the matcher groups.
	 * @throws IllegalArgumentException if one of the matcher's groups is not <code>null</code> and cannot be parsed as an integer.
	 */
	public static int[] getIntGroups(final Matcher matcher) {
		return getIntGroups(matcher, 1); //get all group values, starting with the first group
	}

	/**
	 * Returns an array of integer versions from the available groups of the given matcher, starting at the requested group number. <code>null</code> groups will
	 * be counted as zero. That is, if a matcher contains groups "2", "3", and "null", the array <code>{2, 3, 0}</code> will be returned.
	 * @param matcher The matcher for which an integer array of group values should be returned.
	 * @param beginGroupNumber The one-based group number at which to begin retrieving values.
	 * @return An array of integers representing the integer values of the matcher groups.
	 * @throws IllegalArgumentException if one of the matcher's groups is not <code>null</code> and cannot be parsed as an integer.
	 */
	public static int[] getIntGroups(final Matcher matcher, final int beginGroupNumber) {
		final int groupCount = matcher.groupCount() - (beginGroupNumber - 1); //see how many groups there are, compensating for the beginning group number, which is one-based
		final int[] array = new int[groupCount]; //create an array of values
		for(int groupIndex = 0; groupIndex < groupCount; ++groupIndex) { //for each group
			final int groupNumber = beginGroupNumber + groupIndex; //see which group number this is, using the required regular expression group one-based indexing
			final String group = matcher.group(groupNumber); //get this group
			array[groupIndex] = group != null ? Integer.parseInt(group) : 0; //parse this group, using zero for a null group
		}
		return array; //return the array of integer
	}

}
