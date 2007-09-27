package com.garretwilson.urf.select;

import java.net.URI;

/**A selector that selects an object based upon the intersection of subselectors.
@author Garret Wilson
*/
public class IntersectionSelector extends AbstractOperatorSelector
{

	/**Default constructor.*/
	public IntersectionSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public IntersectionSelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Determines if this selector selects a given object.
	This version returns the logical intersection of its subselectors, or <code>false</code> if there are no subselectors.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@see #getSelects()
	*/
	public boolean selects(final Object object)
	{
		boolean selects=false;	//there has to be at least one subselector before this selector can select anything
		for(final Selector selector:getSelects())	//for each subselector
		{
			selects=selector.selects(object);	//see if the subselector selects the object
			if(!selects)	//if any subselector doesn't select the object
			{
				break;	//one negative spoils an entire intersection
			}
		}
		return selects;	//return whether or not the intersection was successful
	}
}