package com.garretwilson.util;

import java.net.URI;

/**Represents a generic resource with an identifying reference URI.
@author Garret Wilson
*/
public interface Resource extends Comparable
{

	/**@return The non-<code>null</code> resource identifier URI.*/
	public URI getReferenceURI();

}