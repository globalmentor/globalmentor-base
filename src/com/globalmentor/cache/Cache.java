/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.cache;

import java.io.IOException;

/**A cache that can fetch information when needed.
@param <Q> The type of query used to request data from the cache.
@param <V> The type of value stored in the cache.
@author Garret Wilson
*/
public interface Cache<Q, V>
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
	@param query The query for requesting a value from the cache.
	@param listener The listener to be notified when the value is fetched.
	*/
	public void addCacheFetchListener(final Q key, final CacheFetchListener<Q, V> listener);

	/**Removes a listener to listen for a value being fetched.
	@param query The query for requesting a value from the cache.
	@param listener The listener to be notified when the value is fetched.
	*/
	public void removeCacheFetchListener(final Q key, final CacheFetchListener<Q, V> listener);

	/**Determined if a non-stale value is in the cache.
	@param query The query for requesting a value from the cache.
	@return Whether the value associated with the given query is in the cache and not stale.
	@exception IOException if there was an error checking the cached information for staleness.
	*/
	public boolean isCached(final Q query) throws IOException;

	/**Determined if a non-stale value is in the cache.
	@param key The key to use in looking up the cached value.
	@return Whether the value associated with the given key is in the cache and not stale.
	@exception IOException if there was an error checking the cached information for staleness.
	*/
//TODO del	public boolean isCached(final K key) throws IOException;

	/**Retrieves a value from the cache.
	Values are fetched from the backing store if needed, and this method blocks until the data is fetched.
	@param query The query for requesting a value from the cache.
	@return The cached value.
	@exception IOException if there was an error fetching the value from the backing store.
	@see #fetch(Object)
	*/
	public V get(final Q query) throws IOException;
	
	/**Retrieves a value from the cache.
	Values are fetched from the backing store if needed, with fetching optionally deferred until later.
	@param query The query for requesting a value from the cache.
	@param deferFetch Whether fetching, if needed, should be deffered and performed in an asynchronous thread.
	@return The cached value, or <code>null</code> if fetching was deferred.
	@exception IOException if there was an error fetching the value from the backing store.
	@see #fetch(Object)
	*/
	public V get(final Q query, final boolean deferFetch) throws IOException;

	/**Removes a value from the cache.
	@param query The query for requesting a value from the cache.
	@return The previously cached value, even if stale, or <code>null</code> if there was no cached value.
	@exception IOException if there was an error removing the value from the cache.
	*/
	public V uncache(final Q query) throws IOException;

}
