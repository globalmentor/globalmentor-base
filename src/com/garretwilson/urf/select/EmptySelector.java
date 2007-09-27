package com.garretwilson.urf.select;

import java.net.URI;

/**A selector that selects no objects.
@author Garret Wilson
*/
public class EmptySelector extends AbstractSelector
{

	/**Default constructor.*/
	public EmptySelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The uri for the new resource.
	*/
	public EmptySelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Determines if this selector selects a given object.
	This version always returns <code>false</code>.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>false</code>
	*/
	public boolean selects(final Object object)
	{
		return false;	//the empty selector selects nothing
	}
}