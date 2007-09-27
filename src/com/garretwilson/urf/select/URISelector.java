package com.garretwilson.urf.select;

import java.net.URI;

import com.garretwilson.lang.ObjectUtilities;
import com.garretwilson.net.Resource;

import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.select.Select.*;

/**A selector that selects a resource based upon its URI.
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
	@return This selector's URI designation, or <code>null</code> if this selector has no <code>select:selectURI</code> property with a URI value.
	*/
	public URI getSelectURI()
	{
		return asURI(getPropertyValue(SELECT_URI_PROPERTY_URI));	//get the selectURI property as a URI
	}

	/**Sets the URI identified by this selector.
	@param selectURI The URI to be selected.
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
		return object instanceof Resource && ObjectUtilities.equals(getSelectURI(), ((Resource)object).getURI());	//if the object is a resource, compare the resource's URI with the select URI, if any
	}
}