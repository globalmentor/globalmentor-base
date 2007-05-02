package com.garretwilson.util;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.locks.*;

/**An abstract cache that requires a subclass implementing data retrieval methods.
Fetching by default does not occur synchronously.
@param <K> The type of key used to lookup data in the cache.
@param <V> The type of value stored in the cache.
@author Garret Wilson
*/
public abstract class AbstractCache<K, V>
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

	/**The lock controlling access to the cache.*/
	protected final ReadWriteLock cacheLock=new ReentrantReadWriteLock();

	/**The soft value map containing cached values. This map is made thread-safe through the use of {@link #cacheLock}.*/
	protected final Map<K, CachedValue> cacheMap=new SoftValueHashMap<K, CachedValue>();

	/**Constructor.
	@param fetchSynchronous Whether fetches for new values should occur synchronously.
	@param expiration The length of time, in milliseconds, to keep cached information.
	*/
	public AbstractCache(final boolean fetchSynchornous, final long expiration)
	{
		this.fetchSynchronous=fetchSynchornous;
		this.expiration=expiration;
	}
	
	/**Retrieves a value from the cache.
	Values are fetched from the backing store if needed.
	@param key The key to use in looking up the cached value.
	@return The cached value.
	@exception IOException if there was an error fetching the value from the backing store.
	{@link #isStale(Object, long)}
	@see #fetch()
	*/
	public V get(final K key) throws IOException
	{
		cacheLock.readLock().lock();	//lock the cache for reading
		try
		{
			CachedValue cachedValue=cacheMap.get(key);	//get cached value from the map
			if(cachedValue!=null)	//if we have a cached value
			{
				final V value=cachedValue.getValue();	//get the value that is cached
				if(!isStale(value, System.currentTimeMillis()-cachedValue.getCreatedTime()))	//there is a cached value that isn't stale
				{
					return value;	//return the value that was cached
				}
			}
		}
		finally
		{
			cacheLock.readLock().unlock();	//always release the read lock
		}
		final boolean isFetchSynchronous=true;	//TODO get this from somewhere
		if(isFetchSynchronous)	//if cache fetching should be synchronous
		{
			fetchLock.lock();	//force synchronous fetching
		}
		try
		{
			final V value=fetch(key);	//fetch a new value for the key
			cacheLock.writeLock().lock();	//lock the cache for writing
			try
			{
				cacheMap.put(key, new CachedValue(value));	//cache the value
				return value;	//return the value we fetched
			}
			finally
			{
				cacheLock.writeLock().unlock();	//always release the write lock
			}		
		}
		finally
		{
			if(isFetchSynchronous)	//if cache fetching was synchronous
			{
				fetchLock.unlock();	//always release the fetch lock
			}
		}
	}

	/**Determines if a given cached value is stale.
	This version checks to see if the given age is greater than {@link #getExpiration()}.
	@param value The value currently cached.
	@param age The amount of time the information has been cached, in milliseconds.
	@return <code>true</code> if the cached information has become stale.
	*/
	public boolean isStale(final V value, final long age)
	{
		return age>getExpiration();	//if the age is greater than the expiration time, the information is stale
	}

	/**Fetches data from the backing store.
	@param key The key describing the value to fetch.
	@return A new value from backing store.
	@exception IOException if there was an error fetching the value from the backing store.
	*/
	public abstract V fetch(final K key) throws IOException;

	/**Class for storing a value along with its expiration information.
	@author Garret Wilson
	*/
	protected class CachedValue
	{
	
		/**The time the cached information was created.*/
		private final long createdTime;
	
			/**@return The time the cached information was created.*/
			public long getCreatedTime() {return createdTime;}

		/**The value being stored.*/
		private final V value;

			/**@return The value being stored.*/
			public V getValue() {return value;}

		/**Value constructor.
		@param value The value to store.
		*/
		public CachedValue(final V value)
		{
			createdTime=System.currentTimeMillis();	//record the time this information was created
			this.value=value;	//save the value
		}
	}

}
