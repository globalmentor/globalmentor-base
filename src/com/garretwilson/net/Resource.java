package com.garretwilson.net;

import java.net.URI;

/**Represents a generic resource with an identifying URI.
@author Garret Wilson
*/
public interface Resource
{

	/**The Java property name of a resource's URI.*/
	public final static String URI_PROPERTY_NAME="uri";

	/**@return The resource identifier URI, or <code>null</code> if the identifier is not known.*/
	public URI getURI();

	/**Sets the URI of the resource.
	@param uri The new URI, or <code>null</code> if the identifier is not known.
	*/
//TODO del	public void setReferenceURI(final URI uri);	//TODO later create a MoveableResource and only require this method with that interface

}