package com.garretwilson.urf.select;

import java.net.URI;

/**A selector that selects all objects.
@author Garret Wilson
*/
public class UniversalSelector extends AbstractSelector
{

	/**Default constructor.*/
	public UniversalSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public UniversalSelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Determines if this selector selects a given object.
	This version always returns <code>true</code>.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code>
	*/
	public boolean selects(final Object object)
	{
		return true;	//the universal selector selects everything
	}
}