package com.garretwilson.util.regex;

import java.util.regex.Matcher;

/**Utilities for regular expression matchers.
@author Garret Wilson
*/
public class MatcherUtilities
{

	/**Returns an array of integer versions from the available groups of the given matcher.
	<code>null</code> groups will be counted as zero.
	That is, if a matcher contains groups "2", "3", and "null", the array <code>{2, 3, 0}</code> will be returned.  
	@param matcher The matcher for which an integer array of group values should be returned.
	@return An array of integers representing the integer values of the matcher groups.
	@exception IllegalArgumentException if one of the matcher's groups is not <code>null</code> and cannot be parsed as an integer.
	*/
	public static int[] getIntGroups(final Matcher matcher)
	{
		return getIntGroups(matcher, 1);	//get all group values, starting with the first group
	}

	/**Returns an array of integer versions from the available groups of the given matcher, starting at the requested group number.
	<code>null</code> groups will be counted as zero.
	That is, if a matcher contains groups "2", "3", and "null", the array <code>{2, 3, 0}</code> will be returned.  
	@param matcher The matcher for which an integer array of group values should be returned.
	@param beginGroupNumber The one-based group number at which to begin retrieving values.
	@return An array of integers representing the integer values of the matcher groups.
	@exception IllegalArgumentException if one of the matcher's groups is not <code>null</code> and cannot be parsed as an integer.
	*/
	public static int[] getIntGroups(final Matcher matcher, final int beginGroupNumber)
	{
		final int groupCount=matcher.groupCount()-(beginGroupNumber-1);	//see how many groups there are, compensating for the beginning group number, which is one-based
		final int[] array=new int[groupCount];	//create an array of values
		for(int groupIndex=0; groupIndex<groupCount; ++groupIndex)	//for each group
		{
			final int groupNumber=beginGroupNumber+groupIndex;	//see which group number this is, using the required regular expression group one-based indexing
			final String group=matcher.group(groupNumber);	//get this group
			array[groupIndex]=group!=null ? Integer.parseInt(group) : 0;	//parse this group, using zero for a null group
		}
		return array;	//return the array of integer
	}

}
