/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.util;

import static com.globalmentor.java.Objects.*;

import java.util.EventObject;

/**An event that is fired when a value is fetched in the cache.
@param <K> The type of key used to lookup data in the cache.
@param <V> The type of value stored in the cache.
@see Cache
@see CacheFetchListener
@author Garret Wilson
*/
public class CacheFetchEvent<K, V> extends EventObject
{

	/**The cache which fetched the value.
	@return The object on which the Event initially occurred.
	*/
	@SuppressWarnings("unchecked")
	public Cache<K, V> getSource()
	{
		return (Cache<K, V>)super.getSource();
  }

	/**The key to use in looking up the cached value.*/
	private final K key;

		/**@return The key to use in looking up the cached value.*/
		public K getKey() {return key;}

	/**The fetched value.*/
	private final V value;

		/**@return The fetched value.*/
		public V getValue() {return value;}

	/**Source and property name constructor with old and new values.
	The target will be set to be the same as the given source.
	@param source The bean that fired the event.
	@param key The key to use in looking up the cached value.
	@param value The fetched value.
	@exception NullPointerException if the given source, key, and/or value is <code>null</code>.
	*/
	public CacheFetchEvent(final Cache<K, V> source, final K key, final V value)
	{
		super(checkInstance(source, "Event source object cannot be null."));	//construct the parent class
		this.key=checkInstance(key, "Key cannot be null.");
		this.value=checkInstance(value, "Value cannot be null.");
	}

}
