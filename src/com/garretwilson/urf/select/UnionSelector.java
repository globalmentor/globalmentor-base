package com.garretwilson.urf.select;

import java.net.URI;

/**A selector that selects an object based upon the union of subselectors.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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