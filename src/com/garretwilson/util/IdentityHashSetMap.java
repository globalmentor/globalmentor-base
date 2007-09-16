package com.garretwilson.util;

import java.util.*;

/**An decorator map that stores an {@link IdentityHashSet} of values for each key, with special methods for retrieving single values.
@param <K> The type of map key.
@param <V> The type of map value.
@author Garret Wilson
*/
public class IdentityHashSetMap<K, V> extends AbstractDecoratorCollectionMap<K, V, Set<V>>
{

	/**Map constructor.
	@param map The map this map should decorate.
	@exception NullPointerException if the provided map is <code>null</code>.
	*/
	public IdentityHashSetMap(final Map<K, Set<V>> map)
	{
		super(map);	//construct the parent class
	}

	/**Creates a collection in which to store values.
	This version returns an {@link IdentityHashSet}.
	*/
	public Set<V> createCollection()
	{
		return new IdentityHashSet<V>();
	}
}