package com.garretwilson.model;

import com.garretwilson.io.URIAccessible;

/**A model of a resource.
@author Garret Wilson
*/
public interface ResourceModel extends Model, URIAccessible
{
	/**@return The resource being modeled, or <code>null</code> if there is no resource.*/
	public Resource getResource();
}
