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

import java.io.IOException;

/**A cache that can fetch information when needed.
@param <K> The type of key used to lookup data in the cache.
@param <V> The type of value stored in the cache.
@author Garret Wilson
*/
public interface Cache<K, V>
{

	/**@return Whether fetching new values is synchronous.*/
	public boolean isFetchSynchronous();

	/**Sets whether fetching new values is synchronous.
	Changing this value does not change the sychronicity of fetches that have already started but have not yet completed.
	@param fetchSynchronous Whether fetches for new values should occur synchronously.
	*/
	public void setFetchSynchronous(final boolean fetchSynchronous);

	/**@return The life of an object in the cache, in milliseconds.*/
	public long getExpiration();

	/**Sets the life of an object in the cache.
	@param expiration The length of time, in milliseconds, to keep cached information.
	*/
	public void setExpiration(final long expiration);

		/**Adds a listener to listen for a value being fetched.
	@param key The key of the value to be fetched.
	@param listener The listener to be notified when the value is fetched.
	*/
	public void addCacheFetchListener(final K key, final CacheFetchListener<K, V> listener);

	/**Removes a listener to listen for a value being fetched.
	@param key The key of the value to be fetched.
	@param listener The listener to be notified when the value is fetched.
	*/
	public void removeCacheFetchListener(final K key, final CacheFetchListener<K, V> listener);

	/**Determined if a non-stale value is in the cache.
	@param key The key to use in looking up the cached value.
	@return Whether the value associated with the given key is in the cache and not stale.
	@exception IOException if there was an error checking the cached information for staleness.
	*/
	public boolean isCached(final K key) throws IOException;

	/**Retrieves a value from the cache.
	Values are fetched from the backing store if needed, and this method blocks until the data is fetched.
	@param key The key to use in looking up the cached value.
	@return The cached value.
	@exception IOException if there was an error fetching the value from the backing store.
	@see #fetch(Object)
	*/
	public V get(final K key) throws IOException;
	
	/**Retrieves a value from the cache.
	Values are fetched from the backing store if needed, with fetching optionally deferred until later.
	@param key The key to use in looking up the cached value.
	@param deferFetch Whether fetching, if needed, should be deffered and performed in an asynchronous thread.
	@return The cached value, or <code>null</code> if fetching was deferred.
	@exception IOException if there was an error fetching the value from the backing store.
	@see #fetch(Object)
	*/
	public V get(final K key, final boolean deferFetch) throws IOException;

	/**Removes a value from the cache.
	@param key The key to use in looking up the cached value.
	@return The previously cached value, even if stale, or <code>null</code> if there was no cached value.
	@exception IOException if there was an error removing the value from the cache.
	*/
	public V uncache(final K key) throws IOException;

}
