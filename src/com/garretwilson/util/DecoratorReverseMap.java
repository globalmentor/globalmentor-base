package com.garretwilson.util;

import java.util.Map;

import static com.garretwilson.lang.ObjectUtilities.*;

/**A map that also allows lookup of the map keys keyed to the values by decorating two maps. 
@param <K> The type of map key.
@param <V> The type of map value.
<p>The keys and values in this map have a one-to-one relationship. Associating multiple values with a key will likely result in errant functionality.</p>
@author Garret Wilson
*/
public class DecoratorReverseMap<K, V> extends MapDecorator<K, V> implements ReverseMap<K, V>	//TODO programmatically verify the one-to-one relationship
{

	/**The map containing reverse-lookup values.*/
	private final Map<V, K> reverseMap;

	/**Constructs a reverse map by decorating two other maps.
	@param map The main map to be decorated.
	@param reverseMap The map to contain reverse lookup values.
	@exception NullPointerException if the given map and/or reverse map is <code>null</code>.
	*/
	public DecoratorReverseMap(final Map<K, V> map, final Map<V, K> reverseMap)
	{
		super(map);	//create the parent class
		this.reverseMap=checkNull(reverseMap, "Reverse map cannot be null.");
	}

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
	public K getKey(final V value)
	{
		return reverseMap.get(value);	//return the key keyed to the given value in the key map
	}

	/**Returns <code>true</code> if this map maps a key to the specified value.
	<p>This version uses an internal reverse map to provide faster lookups than
		the default linear-time lookup.</p>
	@param value The value whose presence in this map is to be tested.
	@return <code>true</code> if this map maps a key to the specified value.
	*/
	public boolean containsValue(final Object value)
	{
		return reverseMap.containsKey(value);	//see if this value is stored in the key map
	}

	/**Associates the specified value with the specified key in this map,
		and associates the specified key with the specified value in the internal
		reverse map.
	<p>If the map previously contained a mapping for this key, the old
		value is replaced.</p>
	@param key The key with which the specified value is to be associated.
	@param value The value to be associated with the specified key.
	@return The previous value associated with specified key, or <code>null</code>
		if there was no mapping for key. A <code>null</code> return can
		also indicate that the map previously associated
		<code>null</code> with the specified key.
	*/
	public V put(final K key, final V value)
	{
		final V oldValue=super.put(key, value);	//store the value in the map, keyed to the key
		reverseMap.put(value, key);	//store the key in the key map, keyed to the value
		return oldValue;	//return the old value previously mapped to the key, if any
	}

	/**Removes all mappings from this map.*/
	public void clear()
	{
		super.clear();	//do the default clearing
		reverseMap.clear();	//clear the key map as well
	}

}
