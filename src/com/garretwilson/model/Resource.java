package com.garretwilson.model;

import java.net.URI;

/**Represents a generic resource with an identifying reference URI.
@author Garret Wilson
*/
public interface Resource	//G***del if not needed extends Comparable
{

	/**@return The resource identifier URI, or <code>null</code> if the identifier is not known.*/
	public URI getReferenceURI();

	/**Sets the reference URI of the resource.
	@param uri The new reference URI, or <code>null</code> if the identifier is not known.
	*/
	public void setReferenceURI(final URI uri);

}