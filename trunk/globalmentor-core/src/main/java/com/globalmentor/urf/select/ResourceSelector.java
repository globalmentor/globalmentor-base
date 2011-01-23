package com.globalmentor.urf.select;

import java.net.URI;

import com.globalmentor.net.Resource;
import com.globalmentor.urf.URFResource;
import static com.globalmentor.urf.select.Select.*;

/**A selector that selects a resource based upon URI identity.
Resources are considered identical if they have the same URI.
This selector differs from {@link URISelector} in that this selector provides the actual resource (with its identifying URI), not just its URI.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class ResourceSelector extends AbstractSelector
{

	/**Default constructor.*/
	public ResourceSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public ResourceSelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Returns the resource identified by this selector.
	@return This selector's resource designation, or <code>null</code> if this selector has no <code>selectResource</code> property.
	@see Select#SELECT_RESOURCE_PROPERTY_URI
	*/
	public URFResource getSelectResource()
	{
		return getPropertyValue(SELECT_RESOURCE_PROPERTY_URI);	//get the selectResource property
	}

	/**Sets the resource identified by this selector.
	@param selectResource The resource to be selected.
	@see Select#SELECT_RESOURCE_PROPERTY_URI
	*/
	public void setSelectResource(final URFResource selectResource)
	{
		setPropertyValue(SELECT_RESOURCE_PROPERTY_URI, selectResource);	//set the given select resource
	}

	/**Determines if this selector selects a given object.
	A selector with no select resource or an anonymous select resource will not match any resources.
	This version returns <code>true</code> the object is a {@link Resource} and the URI of the given resource matches the URI of the resource specified by this selector, if any.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@see #getSelectResource()
	*/
	public boolean selects(final Object object)
	{
		if(object instanceof Resource)	//if the object is a resource
		{
			final URFResource selectResource=getSelectResource();	//get the select resource
			if(selectResource!=null)	//if there is a select resource
			{
				final URI selectResourceURI=selectResource.getURI();	//get the URI of the select resource
				if(selectResourceURI!=null)	//if the select resource has a URI
				{
					return selectResourceURI.equals(((Resource)object).getURI());	//compare the resource's URI with the URI of the select resource
				}
			}
		}
		return false;	//we couldn't compare resource URIs and therefore the object isn't selected
	}
}