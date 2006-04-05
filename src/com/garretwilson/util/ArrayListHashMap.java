package com.garretwilson.util;

import java.util.*;

/**An implementation of a map that stores a list of values for each key, with special methods for retrieving single values.
{@link ArrayList}s are stored in a {@link HashMap}.
@author Garret Wilson
*/
public class ArrayListHashMap<K, V> extends AbstractDecoratorListMap<K, V> 
{

	/**Default constructor that decorates a {@link HashMap}.*/
	public ArrayListHashMap()
	{
		super(new HashMap<K, List<V>>());	//create a new hash map to decorate
	}

	/**Creates a list in which to store values.
	This version returns an {@link ArrayList}.
	*/
	protected List<V> createList()
	{
		return new ArrayList<V>();
	}
}
