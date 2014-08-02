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
import java.util.*;
import static java.util.Collections.*;
import java.util.concurrent.locks.*;

import static com.globalmentor.collections.Collections.*;
import static com.globalmentor.java.Objects.*;

import com.globalmentor.collections.*;
import com.globalmentor.log.Log;
import com.globalmentor.util.*;

/**An abstract cache that requires a subclass implementing data retrieval methods.
@param <K> The type of key used to look up data in the cache.
@param <Q> The type of query used to request data from the cache.
@param <V> The type of value stored in the cache.
@param <D> The type of data that holds the value.
@author Garret Wilson
*/
public abstract class AbstractCache<K, Q extends AbstractCache.Query<K>, V, D extends Cache.Data<V>> implements Cache<Q, V>
{

	/**Whether fetching new values is synchronous.*/
	private boolean fetchSynchronous;

		/**@return Whether fetching new values is synchronous.*/
		public boolean isFetchSynchronous() {return fetchSynchronous;}

		/**Sets whether fetching new values is synchronous.
		Changing this value does not change the sychronicity of fetches that have already started but have not yet completed.
		@param fetchSynchronous Whether fetches for new values should occur synchronously.
		*/
		public void setFetchSynchronous(final boolean fetchSynchronous) {this.fetchSynchronous=fetchSynchronous;}

	/**The life of an object in the cache, in milliseconds.*/
	private long expiration;

		/**@return The life of an object in the cache, in milliseconds.*/
		public long getExpiration() {return expiration;}

		/**Sets the life of an object in the cache.
		@param expiration The length of time, in milliseconds, to keep cached information.
		*/
		public void setExpiration(final long expiration) {this.expiration=expiration;}

	/**The lock forcing synchronous fetching if enabled.*/
	protected final Lock fetchLock=new ReentrantLock();

	/**The read/write lock soft value map containing cached values.*/
	protected final ReadWriteLockMap<K, D> cacheMap=new DecoratorReadWriteLockMap<K, D>(new PurgeOnWriteSoftValueHashMap<K, D>());

	/**The map of cache fetch listeners keyed to cache keys.*/
	private final ReadWriteLockCollectionMap<K, CacheFetchListener<Q, V>, List<CacheFetchListener<Q, V>>> cacheFetchListenerMap=new DecoratorReadWriteLockCollectionMap<K, CacheFetchListener<Q, V>, List<CacheFetchListener<Q, V>>>(new ArrayListHashMap<K, CacheFetchListener<Q, V>>());

	/**Adds a listener to listen for a value being fetched.
	@param query The query for requesting a value from the cache.
	@param listener The listener to be notified when the value is fetched.
	*/
	public void addCacheFetchListener(final Q query, final CacheFetchListener<Q, V> listener)
	{
		cacheFetchListenerMap.addItem(query.getKey(), listener);	//add the listener to the collection map
	}

	/**Removes a listener to listen for a value being fetched.
	@param query The query for requesting a value from the cache.
	@param listener The listener to be notified when the value is fetched.
	*/
	public void removeCacheFetchListener(final Q query, final CacheFetchListener<Q, V> listener)
	{
		cacheFetchListenerMap.removeItem(query.getKey(), listener);	//remove the listener from the collection map
	}

	/**Constructor.
	@param fetchSynchronous Whether fetches for new values should occur synchronously.
	@param expiration The length of time, in milliseconds, to keep cached information.
	*/
	public AbstractCache(final boolean fetchSynchronous, final long expiration)
	{
		this.fetchSynchronous=fetchSynchronous;
		this.expiration=expiration;
	}

	/**Determined if a non-stale value is in the cache.
	@param query The query for requesting a value from the cache.
	@return Whether the value associated with the given query is in the cache and not stale.
	@throws IOException if there was an error checking the cached information for staleness.
	@see #isStaleData(Query, Data)
	*/
	public boolean isCached(final Q query) throws IOException
	{
		final D cachedData=cacheMap.get(query.getKey());	//get cached information from the map
		return cachedData!=null && !isStaleData(query, cachedData);	//return whether there is a cached value that isn't stale
	}

	/**Determined if the given data is still cached for the given query.
	This method provides a way to tell if previously retrieved data is still valid.  
	@param query The query for requesting a value from the cache.
	@param data Data that was once and may still be associated with the given query in the cache.
	@return Whether the given data is still associated with the given query is in the cache and not stale.
	@throws NullPointerException if the given data is <code>null</code>.
	@throws IOException if there was an error checking the cached data for staleness.
	@see #isStaleData(Query, Data)
	*/
/*TODO del
	public boolean isDataCached(final Q query, final Data<V> data) throws IOException
	{
		checkInstance(data, "Data cannot be null.");
		final D cachedData=cacheMap.get(query.getKey());	//get cached information from the map
		return cachedData==data && !isStaleData(query, cachedData);	//return whether this data is still cached and isn't stale
	}
*/

	/**Retrieves a value from the cache.
	Values are fetched from the backing store if needed, and this method blocks until the data is fetched.
	@param query The query for requesting a value from the cache.
	@return The cached value.
	@throws IOException if there was an error fetching the value from the backing store.
	@see #isStaleData(Query, Data)
	@see #fetchData(Query)
	*/
	public final V get(final Q query) throws IOException
	{
		return get(query, false);	//get without deferring fetching
	}

	/**Retrieves a value from the cache.
	Values are fetched from the backing store if needed, with fetching optionally deferred until later.
	@param query The query for requesting a value from the cache.
	@param deferFetch Whether fetching, if needed, should be deffered and performed in an asynchronous thread.
	@return The cached value, or <code>null</code> if fetching was deferred.
	@throws IOException if there was an error fetching the value from the backing store.
	@see #isStaleData(Query, Data)
	@see #fetchData(Query)
	*/
	public final V get(final Q query, final boolean deferFetch) throws IOException
	{
		final Data<V> cachedData=getData(query, deferFetch);	//get the cached data, if appropriate
		return cachedData!=null ? cachedData.getValue() : null;
	}

	/**Retrieves data from the cache.
	Data is fetched from the backing store if needed, and this method blocks until the data is fetched.
	@param query The query for requesting data from the cache.
	@return The cached data.
	@throws IOException if there was an error fetching the data from the backing store.
	@see #isStaleData(Query, Data)
	@see #fetchData(Query)
	*/
	public final Data<V> getData(final Q query) throws IOException
	{
		return getData(query, false);	//get without deferring fetching
	}
	
	/**Retrieves data from the cache.
	Data is fetched from the backing store if needed, with fetching optionally deferred until later.
	@param query The query for requesting data from the cache.
	@param deferFetch Whether fetching, if needed, should be deffered and performed in an asynchronous thread.
	@return The cached data, or <code>null</code> if fetching was deferred.
	@throws IOException if there was an error fetching the value from the backing store.
	@see #isStaleData(Query, Data)
	@see #fetchData(Query)
	*/
	public Data<V> getData(final Q query, final boolean deferFetch) throws IOException
	{
		final K key=query.getKey();	//get a key for this query
		final D cachedData;
		cachedData=cacheMap.get(key);	//get cached data from the map
		if(cachedData!=null)	//if we have cached data
		{
			if(!isStaleData(query, cachedData))	//if the cached data isn't stale
			{
				return cachedData;	//return the value that was cached
			}
		}
		if(cachedData!=null)	//if we had cached a value
		{
			uncache(query);	//uncache the information (a benign race condition here could have us remove new valid data that has just come in, but that has a low probability and would only result in an extra cache miss in the future)
		}
		if(deferFetch)	//if we should defer fetching
		{
			new Thread(new Fetcher(query)).start();	//start fetching in another thread
			return null;	//indicate that the value is not yet available
		}
		else	//if we shouldn't defer fetching
		{
			return retrieveData(query);	//fetch now and return the value
		}
	}

	/**Fetches data from the backing store, stores the data in the cache, and notifies any listeners.
	@param query The query for requesting a value from the cache.
	@return The fetched and cached data.
	@throws IOException if there was an error fetching the value from the backing store.
	@see #fetchData(Query)
	*/
	protected final Data<V> retrieveData(final Q query) throws IOException
	{
		final boolean isFetchSynchronous=isFetchSynchronous();	//see if cache fetching should be synchronous
		if(isFetchSynchronous)	//if cache fetching should be synchronous
		{
			fetchLock.lock();	//force synchronous fetching
		}
		try
		{
			final K key=query.getKey();	//get a key for this query
			final D cachedData=fetchData(query);	//fetch new cached info for the key
			cacheMap.put(key, cachedData);	//cache the information
			final Collection<CacheFetchListener<Q, V>> cacheFetchListeners;	//we'll determine the cache fetch listeners; do this outside the read lock, in case a listener wants to remove itself from the list
			cacheFetchListenerMap.readLock().lock();	//get a read lock on the listeners
			try
			{
				if(cacheFetchListenerMap.hasItems(key))	//if there are listeners for this key (this is expensive to look up the collection twice, but less expensive than creating an event and iterators; this assumes that most of the time there will be no listeners)
				{
					cacheFetchListeners=new ArrayList<CacheFetchListener<Q, V>>();	//create a list of listeners
					addAll(cacheFetchListeners, cacheFetchListenerMap.getItems(key));	//add the listeners to our list
				}
				else	//if there are no listeners
				{
					cacheFetchListeners=emptyList();	//use an empty list
				}
			}
			finally
			{
				cacheFetchListenerMap.readLock().unlock();	//always release the read lock				
			}
			if(!cacheFetchListeners.isEmpty())	//if there are cache fetch listeners
			{
				final CacheFetchEvent<Q, V> cacheFetchEvent=new CacheFetchEvent<Q, V>(this, query, cachedData.getValue());	//create an event to send to the listeners
				for(final CacheFetchListener<Q, V> listener:cacheFetchListeners)	//for each listener wanting to know when data is fetched for this key
				{
					listener.fetched(cacheFetchEvent);	//inform the listener that the data was fetched
				}
			}
			return cachedData;	//return the cached data we fetched
		}
		finally
		{
			if(isFetchSynchronous)	//if cache fetching was synchronous
			{
				fetchLock.unlock();	//always release the fetch lock
			}
		}
	}

	/**Removes a value from the cache.
	@param query The query for requesting a value from the cache.
	@return The previously cached value, even if stale, or <code>null</code> if there was no cached value.
	@throws IOException if there was an error removing the value from the cache.
	*/
	public final V uncache(final Q query) throws IOException
	{
		final D cachedData=cacheMap.remove(query.getKey());	//remove the cached information
//TODO fix; there seems to be no way to know if someone is still using the file		discard(key, cachedValue);	//discard the cached information, which we can do separately now that
		return cachedData!=null ? cachedData.getValue() : null;	//if there was cached info, returned the cached value
	}

	/**Determines if a given cached value is stale.
	This version checks to see if the age of the cached information is greater than {@link #getExpiration()}.
	@param query The query for requesting a value from the cache.
	@param cachedData The information that is cached.
	@return <code>true</code> if the cached information has become stale.
	@throws IOException if there was an error checking the cached information for staleness.
	@see Data#getCachedTime()
	*/
	protected boolean isStaleData(final Q query, final D cachedData) throws IOException
	{
		return System.currentTimeMillis()-cachedData.getCachedTime()>getExpiration();	//if the age is greater than the expiration time, the information is stale
	}

	/**Performs any operations that need to be done when cached information is discarded (for example, if the cached information is stale).
	This version does nothing.
	@param key The key for the cached information.
	@param cachedData The information that is cached.
	@throws IOException if there was an error discarding the cached information.
	*/
	public void discard(final K key, final D cachedData) throws IOException
	{
	}

	/**Fetches data from the backing store.
	@param query The query for requesting a value from the cache.
	@return New information to cache.
	@throws IOException if there was an error fetching the value from the backing store.
	*/
	protected abstract D fetchData(final Q query) throws IOException;

	/**The runnable that can fetch in a separate thread.
	@author Garret Wilson
	*/
	private class Fetcher implements Runnable
	{
		
		/**The query for requesting a value from the cache.*/
		private final Q query;

		/**Query constructor.
		@param query The query for requesting a value from the cache.
		@throws NullPointerException if the given query is <code>null</code>.
		*/
		public Fetcher(final Q query)
		{
			this.query=checkInstance(query, "Query cannot be null.");
		}

		/**Fetches the value from the cache.
		@see AbstractCache#retrieveData(Cache.Query)
		*/
		public void run()
		{
			try
			{
				retrieveData(query);	//perform the fetch
			}
			catch(final IOException ioException)
			{
				cacheFetchListenerMap.remove(query.getKey());	//remove all the listeners so that they won't cause memory leaks TODO report the error to the listeners
				Log.warn(ioException);	//TODO del when error reporting is implemented
			}
		}
	}

	/**The query used to request information from the cache.
	@param <KK> The type of key used to look up data in the cache.
	@author Garret Wilson
	*/
	public interface Query<KK>
	{
		/**@return A key for looking up data for the query.*/ 
		public KK getKey();
	}

}
