package com.garretwilson.util;

import java.util.*;

/**An implementation of a {@link HashMap} that stores an {@link ArrayList} of values for each key, with special methods for retrieving single values.
@author Garret Wilson
*/
public class ArrayListHashMap<K, V> extends AbstractDecoratorCollectionMap<K, V, List<V>> 
{

	/**Default constructor that decorates a {@link HashMap}.*/
	public ArrayListHashMap()
	{
		super(new HashMap<K, List<V>>());	//create a new hash map to decorate
	}

	/**Creates a collection in which to store values.
	This version returns an {@link ArrayList}.
	*/
	public List<V> createCollection()
	{
		return new ArrayList<V>();
	}
}
