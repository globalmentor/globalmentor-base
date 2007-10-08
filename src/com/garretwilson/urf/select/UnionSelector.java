package com.garretwilson.urf.select;

import java.net.URI;

/**A selector that selects an object based upon the union of subselectors.
@author Garret Wilson
*/
public class UnionSelector extends AbstractOperatorSelector
{

	/**Default constructor.*/
	public UnionSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public UnionSelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Determines if this selector selects a given object.
	This version returns the logical union of its subselectors.
	@param resource The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given resource.
	@see #getSelectors()
	*/
	public boolean selects(final Object object)
	{
		for(final Selector selector:getSelectors())	//for each subselector
		{
			if(selector.selects(object))	//if the subselector selects the object
			{
				return true;	//one match is all we need
			}
		}
		return false;	//indicate that the object was not selected
	}
}