package com.garretwilson.urf;

import java.net.URI;

/**A factory to create resources.
@author Garret Wilson
@see URF
*/
public interface URFResourceFactory
{

	/**Creates a resource with the provided URI based upon the type URI, if any.
	If a type URI is provided, a corresponding type property will be added to the resource.
	@param resourceURI The URI of the resource to create, or <code>null</code> if the resource created should be anonymous.
	@param typeURI The URI of the resource type, or <code>null</code> if the type is not known.
	@return The resource created with this URI, with the given type added if a type was given, or <code>null</code> if no suitable resource could be created.
	*/
	public URFResource createResource(final URI resourceURI, final URI typeURI);

}