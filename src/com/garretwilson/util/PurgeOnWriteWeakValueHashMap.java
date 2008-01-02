package com.garretwilson.util;

import java.lang.ref.*;
import java.util.*;

import static com.globalmentor.java.Objects.*;

/**A map that uses weak references to store map values.
Values are only purged when map write operations occur.
<code>null</code> values are not supported.
@param <K> The type of key.
@param <V> The type of value.
@author Garret Wilson
*/
public class PurgeOnWriteWeakValueHashMap<K, V> extends AbstractPurgeOnWriteReferenceValueMap<K, V, PurgeOnWriteWeakValueHashMap.WeakValueReference<K, V>>
{

	/**Empty map constructor with the specified initial capacity and load factor.
	@param initialCapacity The initial capacity.
	@param loadFactor The load factor.
	@exception IllegalArgumentException if the initial capacity is negative or the load factor is nonpositive.
	*/
	public PurgeOnWriteWeakValueHashMap(final int initialCapacity, final float loadFactor)
	{
		this(new HashMap<K, WeakValueReference<K, V>>(initialCapacity, loadFactor));	//construct the class with a new hash map
	}

	/**Empty map constructor with the specified initial capacity and a default load factor.
	@param initialCapacity The initial capacity.
	@exception IllegalArgumentException if the initial capacity is negative or the load factor is nonpositive.
	*/
	public PurgeOnWriteWeakValueHashMap(final int initialCapacity)
	{
		this(new HashMap<K, WeakValueReference<K, V>>(initialCapacity));	//construct the class with a new hash map
	}

	/**Default constructor with a default initial capacity and load factor.*/
	public PurgeOnWriteWeakValueHashMap()
	{
		this(new HashMap<K, WeakValueReference<K, V>>());	//construct the class with a new hash map		
	}

	/**Decorated map constructor.
	@param map The map to decorate.
	@exception NulPOinter
	*/
	protected PurgeOnWriteWeakValueHashMap(final Map<K, WeakValueReference<K, V>> map)
	{
		super(map);	//construct the parent class
	}

  /**Creates the appropriate reference for associating the given value with the given key.
	The reference will be registered with the given queue.
	@param key The key with which the value is being associated.
	@param value The value to be stored.
	@param referenceQueue The queue with which the reference will be registered.
	@return A reference to the given value that also indicates the key being used.
	*/
  protected PurgeOnWriteWeakValueHashMap.WeakValueReference<K, V> createReference(final K key, final V value, final ReferenceQueue<V> referenceQueue)
  {
  	return new WeakValueReference<K, V>(key, value, referenceQueue);	//create and return a new weak value reference 
  }

  /**A weak reference that keeps track of the key with which a value was associated. 
	@param <K> The type of key.
	@param <V> The type of value.
	@author Garret Wilson
	*/
  public static class WeakValueReference<K, V> extends WeakReference<V> implements AbstractPurgeOnWriteReferenceValueMap.Keyed<K>
  {

  	/**They key with which the referent value was associated.*/
  	private final K key;

	  	/**@return They key with which the referent value was associated.*/
	  	public K getKey() {return key;}

  	/**Creates a new weak reference that remembers the given key, refers to the given object, and is registered with the given queue.
		@param key The key with which the value is being associated.
		@param value The value to which the new weak reference will refer.
		@param referenceQueue The queue with which the reference is to be registered.
		@exception NullPointerException if the given reference queue is <code>null</code>.
  	*/
    public WeakValueReference(final K key, final V value, ReferenceQueue<? super V> referenceQueue)
    {
    	super(value, checkInstance(referenceQueue, "Reference queue cannot be null."));	//construct the parent class
    	this.key=key;	//store the key
    }

  }
}
