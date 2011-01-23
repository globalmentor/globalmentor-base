package com.globalmentor.urf.select;

import java.net.URI;

/**A selector that selects an object based upon the negation of a subselector.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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
		final Selector selector=getSelector();	//get the selector
		if(selector!=null)	//if there is a selector
		{
			return !selector.selects(object);	//return the negation of its selection
		}
		return false;	//if there is no subselector, there is no selection
	}
}