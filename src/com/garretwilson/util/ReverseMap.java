package com.garretwilson.util;

import java.util.Map;

/**A map that also allows lookup of the map keys keyed to the values. 
<p>The keys and values in this map have a one-to-one relationship. Associating
	multiple values with a key will likely result in errant functionality.</p>
@author Garret Wilson
*/
public interface ReverseMap extends Map
{

	/**Returns the key that represents the given value. 
	@param value The value whose associated key is to be returned.
	@return The key to which this map reverse maps the specified value, or
		<code>null</code> if the map contains no reverse mapping for this value.
	@exception ClassCastException Thrown if the key is of an inappropriate type
		for this map (optional).
	@exception NullPointerException Thrown if the value is <code>null</code> and
		this map does not not permit <code>null</code> keys (optional).
	@see #containsValue(Object)
	*/
	public Object getKey(final Object value);

}
