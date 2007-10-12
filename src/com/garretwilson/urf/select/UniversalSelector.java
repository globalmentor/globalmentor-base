package com.garretwilson.urf.select;

import java.net.URI;

/**A selector that selects all objects.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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