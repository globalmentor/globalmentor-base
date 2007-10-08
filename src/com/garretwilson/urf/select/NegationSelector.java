package com.garretwilson.urf.select;

import java.net.URI;
import java.util.Iterator;

/**A selector that selects an object based upon the negation of a subselector.
@author Garret Wilson
*/
public class NegationSelector extends AbstractOperatorSelector
{

	/**Default constructor.*/
	public NegationSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public NegationSelector(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Determines if this selector selects a given object.
	This version returns the logical negation of its subselector, or <code>false</code> if there is no subselector.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@see #getSelector()
	*/
	public boolean selects(final Object object)
	{
		final Iterator<Selector> selectIterator=getSelectors().iterator();	//get an iterator to the the selectors
		if(selectIterator.hasNext())	//if there is a selector
		{
			return !selectIterator.next().selects(object);	//return the negation of its selection
		}
		return false;	//if there is no subselector, there is no selection
	}
}