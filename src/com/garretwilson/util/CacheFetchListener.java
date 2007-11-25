package com.garretwilson.util;

import java.util.EventListener;

/**Indicates the implementing class can listen for a value being fetched in a cache.
@param <K> The type of key used to lookup data in the cache.
@param <V> The type of value stored in the cache.
@see Cache
@see CacheFetchEvent
@author Garret Wilson
*/
public interface CacheFetchListener<K, V> extends EventListener
{

	/**Called when a value is fetched in the cache.
	@param cacheFetchEvent The event identifying the value fetched.
	*/
	public void fetched(final CacheFetchEvent<K, V> cacheFetchEvent);

}