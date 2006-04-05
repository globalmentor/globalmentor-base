package com.garretwilson.util;

import java.util.*;
import java.util.concurrent.*;

/**An implementation of a map that stores a list of values for each key, with special methods for retrieving single values.
{@link CopyOnWriteArrayList}s are stored in a {@link ConcurrentHashMap}.
@author Garret Wilson
*/
public class CopyOnWriteArrayListConcurrentHashMap<K, V> extends AbstractDecoratorListMap<K, V> 
{

	/**Default constructor that decorates a {@link ConcurrentHashMap}.*/
	public CopyOnWriteArrayListConcurrentHashMap()
	{
		super(new ConcurrentHashMap<K, List<V>>());	//create a new concurrent hash map to decorate
	}

	/**Creates a list in which to store values.
	This version returns an {@link ArrayList}.
	*/
	protected List<V> createList()
	{
		return new ArrayList<V>();
	}
}
