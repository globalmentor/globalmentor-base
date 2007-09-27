package com.garretwilson.urf.select;

import com.garretwilson.urf.URFResource;

/**A selector.
@author Garret Wilson
*/
public interface Selector extends URFResource
{

	/**Determines if this selector selects a given object.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	*/
	public boolean selects(final Object object);

}