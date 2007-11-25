package com.garretwilson.util;

import java.util.Map;

/**A map that also allows lookup of the map keys keyed to the values. 
<p>The keys and values in this map have a one-to-one relationship. Associating multiple values with a key will likely result in errant functionality.</p>
@param <K> The type of map key.
@param <V> The type of map value.
@author Garret Wilson
*/
public interface ReverseMap<K, V> extends Map<K, V>
{

	/**Returns the key that represents the given value. 
	@param value The value whose associated key is to be returned.
	@return The key to which this map reverse maps the specified value, or <code>null</code> if the map contains no reverse mapping for this value.
	@exception ClassCastException Thrown if the value is of an inappropriate type for this map (optional).
	@exception NullPointerException Thrown if the value is <code>null</code> and this map does not not permit <code>null</code> values (optional).
	@see #containsValue(Object)
	*/
	public K getKey(final V value);

	/**Removes the mapping for a value from this map if it is present.
	@param value The value whose mapping is to be removed from the map.
	@return The previous key associated with the value, or <code>null</code> if there was no mapping for the value.
	@exception UnsupportedOperationException if the remove operation is not supported by this map
	@exception ClassCastException if the value is of an inappropriate type for this map (optional).
	@exception NullPointerException if the specified value is <code>null</code> and this map does not permit <code>null</code> values (optional).
	*/
	public K removeValue(final V value);

}
