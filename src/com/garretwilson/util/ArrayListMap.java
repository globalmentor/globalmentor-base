package com.garretwilson.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**An decorator map that stores an {@link ArrayList} of values for each key, with special methods for retrieving single values.
@param <K> The type of map key.
@param <V> The type of map value.
@author Garret Wilson
*/
public class ArrayListMap<K, V> extends AbstractDecoratorCollectionMap<K, V, List<V>>
{

	/**Map constructor.
	@param map The map this map should decorate.
	@exception NullPointerException if the provided map is <code>null</code>.
	*/
	public ArrayListMap(final Map<K, List<V>> map)
	{
		super(map);	//construct the parent class
	}

	/**Creates a collection in which to store values.
	This version returns an {@link ArrayList}.
	*/
	public List<V> createCollection()
	{
		return new ArrayList<V>();
	}
}