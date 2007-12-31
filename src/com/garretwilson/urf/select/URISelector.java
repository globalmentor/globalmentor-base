package com.garretwilson.urf.select;

import java.net.URI;

import com.garretwilson.lang.Objects;
import com.garretwilson.net.Resource;

import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.select.Select.*;

/**A selector that selects a resource based upon its URI.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class URISelector extends AbstractSelector
{

	/**Default constructor.*/
	public URISelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public URISelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Returns the URI identified by this selector.
	@return This selector's URI designation, or <code>null</code> if this selector has no <code>selectURI</code> property with a URI value.
	@see Select#SELECT_URI_PROPERTY_URI
	*/
	public URI getSelectURI()
	{
		return asURI(getPropertyValue(SELECT_URI_PROPERTY_URI));	//get the selectURI property as a URI
	}

	/**Sets the URI identified by this selector.
	@param selectURI The URI to be selected.
	@see Select#SELECT_URI_PROPERTY_URI
	*/
	public void setSelectURI(final URI selectURI)
	{
		setPropertyValue(SELECT_URI_PROPERTY_URI, selectURI);	//set the given select URI
	}

	/**Determines if this selector selects a given object.
	A selector with no select URI will match only anonymous resources.
	This version returns the object is a {@link Resource} and the URI of the given resource matches the URI specified by this selector, if any.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@see #getSelectURI()
	*/
	public boolean selects(final Object object)
	{
		return object instanceof Resource && Objects.equals(getSelectURI(), ((Resource)object).getURI());	//if the object is a resource, compare the resource's URI with the select URI, if any
	}
}