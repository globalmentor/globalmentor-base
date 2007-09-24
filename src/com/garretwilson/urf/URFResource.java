package com.garretwilson.urf;

import java.net.URI;

import com.garretwilson.net.Resource;

/**An URF resource.
@author Garret Wilson
*/
public interface URFResource extends Resource, URFScope
{

	/**Retrieves the types declared for this resource, if any.
	@return An iterable to all types declared for this resource.
	@see URF#TYPE_PROPERTY_URI
	*/
	public Iterable<URFResource> getTypes();

	/**Determines whether this resource has a type with the given URI.
	@param typeURI The URI of the type for which to search.
	@return <code>true</code> if this resource has a type with the given URI.
	@see URF#TYPE_PROPERTY_URI
	*/
	public boolean hasTypeURI(final URI typeURI);
}
