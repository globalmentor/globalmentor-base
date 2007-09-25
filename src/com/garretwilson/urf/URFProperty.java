package com.garretwilson.urf;

import java.net.URI;

/**An encapsulation of a parent scope, property URI, value, and the associated property-value scope.
@author Garret Wilson
*/
public interface URFProperty extends URFValueContext
{

	/**@return The scope to which the property belongs.*/
	public URFScope getSubjectScope();

	/**@return The URI of the property.*/
	public URI getPropertyURI();

}
