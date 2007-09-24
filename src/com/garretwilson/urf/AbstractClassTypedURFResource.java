package com.garretwilson.urf;

import java.net.URI;

import static com.garretwilson.lang.ClassUtilities.*;
import static com.garretwilson.urf.URF.*;

/**An URF resource that automatically adds an <code>urf:type</code> property of the local name of its Java class.
@author Garret Wilson
*/
public abstract class AbstractClassTypedURFResource extends DefaultURFResource
{

	/**URI and type namespace URI.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeNamespaceURI The namespace URI of the URI of the type to be added.
	@exception NullPointerException if the given type type namespace URI is <code>null</code>.
	*/
	public AbstractClassTypedURFResource(final URI uri, final URI typeNamespaceURI)
	{
		super(uri);  //construct the parent class
		addType(new DefaultURFResource(createResourceURI(typeNamespaceURI, getLocalName(getClass()))));	//add the default type based upon the given type namespace URI and the local name of the class
	}

}