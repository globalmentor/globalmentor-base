package com.garretwilson.util;

/**Indicates a class that determines whether a given object will pass through
	the filter be filtered out.
@author Garret Wilson
*/
public interface Filter
{

	/**Determines whether a given object should pass through the filter or be
		filtered out.
	@param object The object to filter.
	@return <code>true</code> if the object should pass through the filter, else
		<code>false</code> if the object should be filtered out.
	*/
	public boolean isPass(final Object object);
}
