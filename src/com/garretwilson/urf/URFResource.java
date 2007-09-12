package com.garretwilson.urf;

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

}
