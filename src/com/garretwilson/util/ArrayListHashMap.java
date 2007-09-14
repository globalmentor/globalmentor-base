package com.garretwilson.util;

import java.util.*;

/**An implementation of a {@link HashMap} that stores an {@link ArrayList} of values for each key, with special methods for retrieving single values.
@param <K> The type of map key.
@param <V> The type of map value.
@author Garret Wilson
*/
public class ArrayListHashMap<K, V> extends ArrayListMap<K, V> 
{

	/**Default constructor that decorates a {@link HashMap}.*/
	public ArrayListHashMap()
	{
		super(new HashMap<K, List<V>>());	//create a new hash map to decorate
	}
}
