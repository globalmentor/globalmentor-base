package com.garretwilson.model;

/**A model of a resource.
@author Garret Wilson
*/
public interface ResourceModel extends Model
{
	/**@return The resource being modeled, or <code>null</code> if there is no resource.*/
	public Resource getResource();
}
