package com.garretwilson.util;

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
