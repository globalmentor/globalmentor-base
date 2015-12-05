/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import com.globalmentor.java.AbstractLong;
import com.globalmentor.java.CloneSupported;
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
public class Count extends AbstractLong<Count> implements CloneSupported {

	/** The current count. */
	private long count;

	/** @return The current count. */
	public long getCount() {
		return count;
	}

	/** {@inheritDoc} This version delegates to {@link #getCount()}. */
	@Override
	public long longValue() {
		return getCount();
	}

	/**
	 * Default constructor with a count of zero.
	 */
	public Count() {
		this(0L);
	}

	/**
	 * Count constructor.
	 * 
	 * @param count The current count.
	 */
	public Count(final long count) {
		this.count = count;
	}

	/**
	 * Increments the counter and returns the new value.
	 * @return The new, incremented count value.
	 */
	protected long increment() {
		return ++count;
	}

	/**
	 * Decrements the counter and returns the new value.
	 * @return The new, decremented count value.
	 */
	protected long decrement() {
		return --count;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return super.clone();
	}

	/**
	 * {@inheritDoc} This implementation outputs the current count.
	 * @see #getCount()
	 **/
	@Override
	public String toString() {
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
	public static <K> long incrementCounterMapCount(final Map<K, Count> map, final K key) {
		Count counter = map.get(key); //get the current counter
		if(counter == null) { //if the key does not exist in the map
			counter = new Count(); //create a new counter and put it in the map keyed to the key 
			map.put(key, counter);
		}
		return counter.increment(); //increment the counter and return the new count
	}

	/**
	 * Decrements the occurrence count of the given key using the given map. In other words, the associated count for the key will be decreased by one. If the
	 * counter gets to zero, it will be removed from the map.
	 * <p>
	 * This is a convenience method for keeping track of the count of some key in a map.
	 * </p>
	 * <p>
	 * This implementation does not allow <code>null</code> counter values. This implementation does not support counts less than zero.
	 * </p>
	 * @param map The map containing the counts.
	 * @param key The key being counted.
	 * @return The new count of the key in the map.
	 * @throws IllegalStateException if the key does not exist in the map (i.e. the count is zero).
	 */
	public static <K> long decrementCounterMapCount(final Map<K, Count> map, final K key) {
		final Count counter = map.get(key); //get the current counter
		if(counter == null) { //if the key does not exist in the map
			throw new IllegalStateException("Key " + key + " does not exist in the counter map (it has an effective zero count).");
		}
		final long count = counter.decrement(); //decrement the counter
		if(count == 0L) { //if we reached zero
			map.remove(key); //remove the counter from the map altogether
		}
		return count; //return the new count
	}

	/**
	 * Returns the current count of the given key in the given map. If the key does not exist in the map, the count is considered zero.
	 * <p>
	 * This is a convenience method for keeping track of the count of some key in a map.
	 * </p>
	 * <p>
	 * This implementation does not allow <code>null</code> counter values.
	 * </p>
	 * @param map The map containing the counts.
	 * @param key The key being counted.
	 * @return The count value of the counter of the key in the map.
	 */
	public static <K> long getCount(final Map<K, Count> map, final K key) {
		final Count counter = map.get(key); //get the current counter
		return counter != null ? counter.getCount() : 0L; //return the count, considering no counter to indicate a count of zero
	}

	/**
	 * Comparator that compares map entries of a counter map based upon the count of each entry.
	 * <p>
	 * This implementation does not allow <code>null</code> counter values.
	 * </p>
	 * @author Garret Wilson
	 * @param <K> The type of key used in the map.
	 */
	public static class CounterMapEntryComparator<K> implements Comparator<Map.Entry<K, Count>> {

		@Override
		public int compare(final Map.Entry<K, Count> entry1, final Map.Entry<K, Count> entry2) {
			return Longs.compare(entry1.getValue().getCount(), entry2.getValue().getCount());
		}
	}
}
