/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.model;

import java.util.*;

import com.globalmentor.java.Longs;

/**
 * Convenience class for keeping track of a count. This is useful, for example, as a value object in a map.
 * 
 * <p>
 * This class is not thread safe.
 * </p>
 * 
 * @author Garret Wilson
 */
public class Counter
{

	/** The current count. */
	private long count;

	/** @return The current count. */
	public long getCount()
	{
		return count;
	}

	/**
	 * Default constructor with a count of zero.
	 */
	public Counter()
	{
		this(0L);
	}

	/**
	 * Count constructor.
	 * 
	 * @param count The current count.
	 */
	public Counter(final long count)
	{
		this.count = count;
	}

	/**
	 * Increments the counter and returns the new value.
	 * @return The new, incremented count value.
	 */
	public long increment()
	{
		return ++count;
	}

	/**
	 * {@inheritDoc} This implementation outputs the current count.
	 * @see #getCount()
	 **/
	@Override
	public String toString()
	{
		return Long.toString(getCount());
	}

	/**
	 * Increments the occurrence count of the given key using the given map. In other words, the associated count for the key will be increased by one. If the key
	 * does not yet exist in the map, it will be added with a count of <code>1</code>.
	 * <p>
	 * This is a convenience method for keeping track of the count of some key in a map.
	 * </p>
	 * <p>
	 * This implementation does not allow <code>null</code> counter values.
	 * </p>
	 * @param map The map containing the counts.
	 * @param key The key being counted.
	 * @return The new count of the key in the map.
	 */
	public static <K> long incrementCounterMapCount(final Map<K, Counter> map, final K key)
	{
		Counter counter = map.get(key); //get the current count
		if(counter == null) //if the key does not exist in the map
		{
			counter = new Counter(); //create a new counter and put it in the map keyed to the key 
			map.put(key, counter);
		}
		return counter.increment(); //increment the counter and return the new count
	}

	/**
	 * Comparator that compares map entries of a counter map based upon the count of each entry.
	 * <p>
	 * This implementation does not allow <code>null</code> counter values.
	 * </p>
	 * @author Garret Wilson
	 * @param <K> The type of key used in the map.
	 */
	public static class CounterMapEntryComparator<K> implements Comparator<Map.Entry<K, Counter>>
	{
		@Override
		public int compare(final Map.Entry<K, Counter> entry1, final Map.Entry<K, Counter> entry2)
		{
			return Longs.compare(entry1.getValue().getCount(), entry2.getValue().getCount());
		}
	}
}
