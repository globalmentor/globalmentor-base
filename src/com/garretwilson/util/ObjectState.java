package com.garretwilson.util;

/**Provides state information of an existing object, such as:
<ul>
	<li>Descriptive properties of the object.</li>
	<li>Whether the object has been modified.</li>
</ul>
@author Garret Wilson
*/
public interface ObjectState extends Modifiable
{

	/**@return The non-<code>null</code> object being described.*/
	public Object getObject();
	
	/**Sets the object being described.
	@param object The object being described.
	*/
//G***del if not needed	public void setObject(final Object object);

	/**Gets a property of the object state.
	@param key The key to the property.
	@return The value of the object state's property, or <code>null</code> if
		that property does not exist.
	*/
	public Object getProperty(final Object key);

	/**Sets the value of an object state property.
	If the property represented by the key already exists, it will be replaced.
	@param key The non-<code>null</code> property key.
	@param value The property value.
	@return The old property value associated with the key, or <code>null</code>
		if no value was associated with the key previously.
	*/
	public Object setProperty(final Object key, final Object value);

	/**Removes a property of the object state.
	If the property represented by the key does not exist, no action is taken.
	@param key The non-<code>null</code> property key.
	@return The removed property value, or <code>null</code> if there was no
		property.
	*/
	public Object removeProperty(final Object key);

}