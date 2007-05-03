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
public abstract class AbstractCache<K, V, I extends AbstractCache.CachedInfo<V>>
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
	protected final Map<K, I> cacheMap=new SoftValueHashMap<K, I>();

	/**Constructor.
	@param fetchSynchronous Whether fetches for new values should occur synchronously.
	@param expiration The length of time, in milliseconds, to keep cached information.
	*/
	public AbstractCache(final boolean fetchSynchronous, final long expiration)
	{
		this.fetchSynchronous=fetchSynchronous;
		this.expiration=expiration;
	}
	
	/**Retrieves a value from the cache.
	Values are fetched from the backing store if needed.
	@param key The key to use in looking up the cached value.
	@return The cached value.
	@exception IOException if there was an error fetching the value from the backing store.
	@see #isStale(Object, com.garretwilson.util.AbstractCache.CachedInfo)
	@see #fetch(Object)
	*/
	public V get(final K key) throws IOException
	{
		cacheLock.readLock().lock();	//lock the cache for reading
		try
		{
			final I cachedInfo=cacheMap.get(key);	//get cached value from the map
			if(cachedInfo!=null)	//if we have a cached value
			{
				if(!isStale(key, cachedInfo))	//if the cached value isn't stale
				{
					return cachedInfo.getValue();	//return the value that was cached
				}
			}
		}
		finally
		{
			cacheLock.readLock().unlock();	//always release the read lock
		}
/*TODO fix; there seems to be no way to know if someone is still using the file
		if(cachedValue!=null)	//if we had cached a value
		{
			cacheLock.writeLock().lock();	//lock the cache for writing
			try
			{
				cacheMap.remove(key);	//remove the cached information
			}
			finally
			{
				cacheLock.writeLock().unlock();	//always release the write lock
			}		
			discard(key, cachedValue);	//discard the cached information, which we can do separately now that 
		}
*/
		final boolean isFetchSynchronous=true;	//TODO get this from somewhere
		if(isFetchSynchronous)	//if cache fetching should be synchronous
		{
			fetchLock.lock();	//force synchronous fetching
		}
		try
		{
			final I newCachedInfo=fetch(key);	//fetch new cached info for the key
			cacheLock.writeLock().lock();	//lock the cache for writing
			try
			{
				cacheMap.put(key, newCachedInfo);	//cache the information
				return newCachedInfo.getValue();	//return the value we fetched
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
	This version checks to see if the age of the cached information is greater than {@link #getExpiration()}.
	@param key The key for the cached information.
	@param cachedInfo The information that is cached.
	@return <code>true</code> if the cached information has become stale.
	@exception IOException if there was an error fetching the value from the backing store.
	@see CachedInfo#getCachedTime()
	*/
	public boolean isStale(final K key, final I cachedInfo) throws IOException
	{
		return System.currentTimeMillis()-cachedInfo.getCachedTime()>getExpiration();	//if the age is greater than the expiration time, the information is stale
	}

	/**Performs any operations that need to be done when cached information is discarded (for example, if the cached information is stale).
	This version does nothing.
	@param key The key for the cached information.
	@param cachedInfo The information that is cached.
	@exception IOException if there was an error discarding the cached information.
	*/
	public void discard(final K key, final I cachedInfo) throws IOException
	{
	}

	/**Fetches data from the backing store.
	@param key The key describing the value to fetch.
	@return New information to cache.
	@exception IOException if there was an error fetching the value from the backing store.
	*/
	public abstract I fetch(final K key) throws IOException;

	/**Class for storing a value along with its expiration information.
	@param <V> The type of value stored in the cache.
	@author Garret Wilson
	*/
	public static class CachedInfo<VV>
	{
		/**The time the cached information was created.*/
		private final long cachedTime;
	
			/**@return The time the cached information was created.*/
			public long getCachedTime() {return cachedTime;}

		/**The value being stored.*/
		private final VV value;

			/**@return The value being stored.*/
			public VV getValue() {return value;}

		/**Value constructor.
		@param value The value to store.
		*/
		public CachedInfo(final VV value)
		{
			cachedTime=System.currentTimeMillis();	//record the time this information was created
			this.value=value;	//save the value
		}
	}

}