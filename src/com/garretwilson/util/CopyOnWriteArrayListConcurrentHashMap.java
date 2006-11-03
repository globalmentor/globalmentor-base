package com.garretwilson.util;

import java.util.*;
import java.util.concurrent.*;

/**An implementation of a {@link ConcurrentHashMap} that stores a {@link CopyOnWriteArrayList} of values for each key, with special methods for retrieving single values.
@author Garret Wilson
*/
public class CopyOnWriteArrayListConcurrentHashMap<K, V> extends AbstractDecoratorCollectionMap<K, V, List<V>> 
{

	/**Default constructor that decorates a {@link ConcurrentHashMap}.*/
	public CopyOnWriteArrayListConcurrentHashMap()
	{
		super(new ConcurrentHashMap<K, List<V>>());	//create a new concurrent hash map to decorate
	}

	/**Creates a collection in which to store values.
	This version returns an {@link ArrayList}.
	*/
	protected List<V> createCollection()
	{
		return new ArrayList<V>();
	}
}
