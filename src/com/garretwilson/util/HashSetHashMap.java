package com.garretwilson.util;

import java.util.*;

/**An implementation of a {@link HashMap} that stores an {@link HashSet} of values for each key, with special methods for retrieving single values.
@author Garret Wilson
*/
public class HashSetHashMap<K, V> extends AbstractDecoratorCollectionMap<K, V, Set<V>> 
{

	/**Default constructor that decorates a {@link HashMap}.*/
	public HashSetHashMap()
	{
		super(new HashMap<K, Set<V>>());	//create a new hash map to decorate
	}

	/**Creates a collection in which to store values.
	This version returns an {@link HashSet}.
	*/
	public Set<V> createCollection()
	{
		return new HashSet<V>();
	}
}
