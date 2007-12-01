package com.garretwilson.util;

import java.lang.ref.*;
import java.util.*;
import static java.util.Collections.*;

import static com.garretwilson.lang.ObjectUtilities.*;

/**A map that uses references to store map values.
Cleared references are only purged when map write operations occur.
<code>null</code> values are not supported.
@param <K> The type of key.
@param <V> The type of value.
@param <R> The type of reference.
@author Garret Wilson
*/
public abstract class AbstractPurgeOnWriteReferenceValueMap<K, V, R extends Reference<V> & AbstractPurgeOnWriteReferenceValueMap.Keyed<K>> implements Map<K, V>
{

	/**The decorated map.*/
	private final Map<K, R> map;

	/**Decorated map constructor.
	@param map The map being decorated.
	@exception NullPointerException if the given map is <code>null</code>.
	*/
	public AbstractPurgeOnWriteReferenceValueMap(final Map<K, R> map)
	{
		this.map=checkInstance(map, "Map cannot be null.");
	}

  /**
   * Returns the number of key-value mappings in this map.  If the
   * map contains more than <tt>Integer.MAX_VALUE</tt> elements, returns
   * <tt>Integer.MAX_VALUE</tt>.
   *
   * @return the number of key-value mappings in this map.
   */
  public int size() {return map.size();}

  /**
   * Returns <tt>true</tt> if this map contains no key-value mappings.
   *
   * @return <tt>true</tt> if this map contains no key-value mappings.
   */
  public boolean isEmpty() {return map.isEmpty();}

  /**
   * Returns <tt>true</tt> if this map contains a mapping for the specified
   * key.  More formally, returns <tt>true</tt> if and only if
   * this map contains a mapping for a key <tt>k</tt> such that
   * <tt>(key==null ? k==null : key.equals(k))</tt>.  (There can be
   * at most one such mapping.)
   *
   * @param key key whose presence in this map is to be tested.
   * @return <tt>true</tt> if this map contains a mapping for the specified
   *         key.
   * 
   * @throws ClassCastException if the key is of an inappropriate type for
   * 		  this map (optional).
   * @throws NullPointerException if the key is <tt>null</tt> and this map
   *            does not permit <tt>null</tt> keys (optional).
   */
  public boolean containsKey(Object key)
  {
  	final R reference=map.get(key);	//get the reference if there is one
  	return reference!=null && reference.get()!=null;	//see if there is a referenced object
  }

  /**
   * Returns <tt>true</tt> if this map maps one or more keys to the
   * specified value.  More formally, returns <tt>true</tt> if and only if
   * this map contains at least one mapping to a value <tt>v</tt> such that
   * <tt>(value==null ? v==null : value.equals(v))</tt>.  This operation
   * will probably require time linear in the map size for most
   * implementations of the <tt>Map</tt> interface.
   *
   * @param value value whose presence in this map is to be tested.
   * @return <tt>true</tt> if this map maps one or more keys to the
   *         specified value.
   * @throws ClassCastException if the value is of an inappropriate type for
   * 		  this map (optional).
   * @throws NullPointerException if the value is <tt>null</tt> and this map
   *            does not permit <tt>null</tt> values (optional).
   */
  public boolean containsValue(Object value)
  {
  	for(final R reference:map.values())	//look at each value reference in the map
  	{
  		if(value.equals(reference.get()))	//if the value equals the referenced value
  		{
  			return true;	//this map contains the value
  		}
  	}
  	return false;	//we couldn't find the requested value
  }

  /**
   * Returns the value to which this map maps the specified key.  Returns
   * <tt>null</tt> if the map contains no mapping for this key.  A return
   * value of <tt>null</tt> does not <i>necessarily</i> indicate that the
   * map contains no mapping for the key; it's also possible that the map
   * explicitly maps the key to <tt>null</tt>.  The <tt>containsKey</tt>
   * operation may be used to distinguish these two cases.
   *
   * <p>More formally, if this map contains a mapping from a key
   * <tt>k</tt> to a value <tt>v</tt> such that <tt>(key==null ? k==null :
   * key.equals(k))</tt>, then this method returns <tt>v</tt>; otherwise
   * it returns <tt>null</tt>.  (There can be at most one such mapping.)
   *
   * @param key key whose associated value is to be returned.
   * @return the value to which this map maps the specified key, or
   *	       <tt>null</tt> if the map contains no mapping for this key.
   * 
   * @throws ClassCastException if the key is of an inappropriate type for
   * 		  this map (optional).
   * @throws NullPointerException if the key is <tt>null</tt> and this map
   *		  does not permit <tt>null</tt> keys (optional).
   * 
   * @see #containsKey(Object)
   */
  public V get(Object key)
  {
  	final R reference=map.get(key);	//get the reference if there is one
  	return reference!=null ? reference.get() : null;	//if there is a reference, return the referenced object
  }

  // Modification Operations

  /**
   * Associates the specified value with the specified key in this map
   * (optional operation).  If the map previously contained a mapping for
   * this key, the old value is replaced by the specified value.  (A map
   * <tt>m</tt> is said to contain a mapping for a key <tt>k</tt> if and only
   * if {@link #containsKey(Object) m.containsKey(k)} would return
   * <tt>true</tt>.)) 
   *
   * @param key key with which the specified value is to be associated.
   * @param value value to be associated with the specified key.
   * @return previous value associated with specified key, or <tt>null</tt>
   *	       if there was no mapping for key.  A <tt>null</tt> return can
   *	       also indicate that the map previously associated <tt>null</tt>
   *	       with the specified key, if the implementation supports
   *	       <tt>null</tt> values.
   * 
   * @throws UnsupportedOperationException if the <tt>put</tt> operation is
   *	          not supported by this map.
   * @throws ClassCastException if the class of the specified key or value
   * 	          prevents it from being stored in this map.
   * @throws IllegalArgumentException if some aspect of this key or value
   *	          prevents it from being stored in this map.
   * @throws NullPointerException if this map does not permit <tt>null</tt>
   *            keys or values, and the specified key or value is
   *            <tt>null</tt>.
   */
  public V put(final K key, final V value)
  {
  	final R reference=map.put(key, createReference(key, value, referenceQueue));	//create a reference and set the value
  	if(reference!=null)	//if there was a reference
  	{
    	final V oldValue=reference!=null ? reference.get() : null;	//get the old value, if any
  		reference.enqueue();	//add the reference to the queue for purging
  		purgeExcept(reference);	//purge all references except the one we just removed
  		return oldValue;	//return the old value
  	}
  	else	//if there was no reference
  	{
  		purge();	//purge everything
  		return null;	//indicate that there was no previous value
  	}
  }

  /**
   * Removes the mapping for this key from this map if it is present
   * (optional operation).   More formally, if this map contains a mapping
   * from key <tt>k</tt> to value <tt>v</tt> such that
   * <code>(key==null ?  k==null : key.equals(k))</code>, that mapping
   * is removed.  (The map can contain at most one such mapping.)
   *
   * <p>Returns the value to which the map previously associated the key, or
   * <tt>null</tt> if the map contained no mapping for this key.  (A
   * <tt>null</tt> return can also indicate that the map previously
   * associated <tt>null</tt> with the specified key if the implementation
   * supports <tt>null</tt> values.)  The map will not contain a mapping for
   * the specified  key once the call returns.
   *
   * @param key key whose mapping is to be removed from the map.
   * @return previous value associated with specified key, or <tt>null</tt>
   *	       if there was no mapping for key.
   *
   * @throws ClassCastException if the key is of an inappropriate type for
   * 		  this map (optional).
   * @throws NullPointerException if the key is <tt>null</tt> and this map
   *            does not permit <tt>null</tt> keys (optional).
   * @throws UnsupportedOperationException if the <tt>remove</tt> method is
   *         not supported by this map.
   */
  public V remove(Object key)
  {
  	final R reference=map.remove(key);	//remove the keyed reference, if any
  	if(reference!=null)	//if there was a reference
  	{
    	final V oldValue=reference!=null ? reference.get() : null;	//get the old value, if any
  		reference.enqueue();	//add the reference to the queue for purging
  		purgeExcept(reference);	//purge all references except the one we just removed
  		return oldValue;	//return the old value
  	}
  	else	//if there was no reference
  	{
  		purge();	//purge everything
  		return null;	//indicate that there was no previous value  		
  	}
  }


  // Bulk Operations

  /**
   * Copies all of the mappings from the specified map to this map
   * (optional operation).  The effect of this call is equivalent to that
   * of calling {@link #put(Object,Object) put(k, v)} on this map once
   * for each mapping from key <tt>k</tt> to value <tt>v</tt> in the 
   * specified map.  The behavior of this operation is unspecified if the
   * specified map is modified while the operation is in progress.
   *
   * @param t Mappings to be stored in this map.
   * 
   * @throws UnsupportedOperationException if the <tt>putAll</tt> method is
   * 		  not supported by this map.
   * 
   * @throws ClassCastException if the class of a key or value in the
   * 	          specified map prevents it from being stored in this map.
   * 
   * @throws IllegalArgumentException some aspect of a key or value in the
   *	          specified map prevents it from being stored in this map.
   * @throws NullPointerException if the specified map is <tt>null</tt>, or if
   *         this map does not permit <tt>null</tt> keys or values, and the
   *         specified map contains <tt>null</tt> keys or values.
   */
  public void putAll(Map<? extends K, ? extends V> t)
  {
  	for(final Map.Entry<? extends K, ? extends V> entry:t.entrySet())	//for each entry in the new map
  	{
  		put(entry.getKey(), entry.getValue());	//put this key/value pair in the map
  	}
  }

  /**
   * Removes all mappings from this map (optional operation).
   *
   * @throws UnsupportedOperationException clear is not supported by this
   * 		  map.
   */
  public void clear()
  {
  	for(final Map.Entry<K, R> referenceEntry:map.entrySet())	//for each reference entry in the map
  	{
  		referenceEntry.getValue().enqueue();	//enqueue this value
  	}
  	map.clear();	//clear the map
  	Reference<? extends V> reference;	//remove all the references from the queue
  	do
  	{
  		reference=referenceQueue.poll();
  	}
  	while(reference!=null);
  }


  // Views

  /**
   * Returns a set view of the keys contained in this map.  The set is
   * backed by the map, so changes to the map are reflected in the set, and
   * vice-versa.  If the map is modified while an iteration over the set is
   * in progress (except through the iterator's own <tt>remove</tt>
   * operation), the results of the iteration are undefined.  The set
   * supports element removal, which removes the corresponding mapping from
   * the map, via the <tt>Iterator.remove</tt>, <tt>Set.remove</tt>,
   * <tt>removeAll</tt> <tt>retainAll</tt>, and <tt>clear</tt> operations.
   * It does not support the add or <tt>addAll</tt> operations.
   *
   * @return a set view of the keys contained in this map.
   */
  public Set<K> keySet() {return map.keySet();}

  /**
   * Returns a collection view of the values contained in this map.  The
   * collection is backed by the map, so changes to the map are reflected in
   * the collection, and vice-versa.  If the map is modified while an
   * iteration over the collection is in progress (except through the
   * iterator's own <tt>remove</tt> operation), the results of the
   * iteration are undefined.  The collection supports element removal,
   * which removes the corresponding mapping from the map, via the
   * <tt>Iterator.remove</tt>, <tt>Collection.remove</tt>,
   * <tt>removeAll</tt>, <tt>retainAll</tt> and <tt>clear</tt> operations.
   * It does not support the add or <tt>addAll</tt> operations.
   *
   * @return a collection view of the values contained in this map.
   */
  public Collection<V> values()
  {
  	final Collection<R> references=map.values();	//get all the value references
  	if(!references.isEmpty())	//if there are references, assume there's probably at least one value that hasn't been garbaged collected
  	{
	  	final Collection<V> values=new ArrayList<V>(references.size());	//create a list for the worst-cast scenario of no garbage-collected values
	  	for(final R reference:references)	//for each value reference
	  	{
	  		final V value=reference.get();	//get this value
	  		if(value!=null)	//if this value hasn't been garbage collected
	  		{
	  			values.add(value);	//add this value to our list
	  		}
	  	}
	  	return values;	//return the collected values
  	}
  	else	//if there are no references
  	{
  		return emptyList();	//return an empty list
  	}
  }

  /**
   * Returns a set view of the mappings contained in this map.  Each element
   * in the returned set is a {@link Map.Entry}.  The set is backed by the
   * map, so changes to the map are reflected in the set, and vice-versa.
   * If the map is modified while an iteration over the set is in progress
   * (except through the iterator's own <tt>remove</tt> operation, or through
   * the <tt>setValue</tt> operation on a map entry returned by the iterator)
   * the results of the iteration are undefined.  The set supports element
   * removal, which removes the corresponding mapping from the map, via the
   * <tt>Iterator.remove</tt>, <tt>Set.remove</tt>, <tt>removeAll</tt>,
   * <tt>retainAll</tt> and <tt>clear</tt> operations.  It does not support
   * the <tt>add</tt> or <tt>addAll</tt> operations.
   *
   * @return a set view of the mappings contained in this map.
   */
  public Set<Map.Entry<K, V>> entrySet()
  {
  	final Set<Map.Entry<K, R>> referenceEntrySet=map.entrySet();	//get the set of reference entries
  	final Set<Map.Entry<K, V>> entrySet=new HashSet<Entry<K,V>>(referenceEntrySet.size());	//create a set to hold the entries
  	for(final Map.Entry<K, R> referenceEntry:referenceEntrySet)	//for each reference entry
  	{
  		entrySet.add(new ReferenceEntryDecorator(referenceEntry));	//create a new decorator for the reference entry
  	}
  	return entrySet;	//return the new set of entries
  }

  /**
   * A map entry (key-value pair).  The <tt>Map.entrySet</tt> method returns
   * a collection-view of the map, whose elements are of this class.  The
   * <i>only</i> way to obtain a reference to a map entry is from the
   * iterator of this collection-view.  These <tt>Map.Entry</tt> objects are
   * valid <i>only</i> for the duration of the iteration; more formally,
   * the behavior of a map entry is undefined if the backing map has been
   * modified after the entry was returned by the iterator, except through
   * the <tt>setValue</tt> operation on the map entry.
   *
   * @see Map#entrySet()
   * @since 1.2
   */
  private class ReferenceEntryDecorator implements Entry<K,V>
  {
  	private final Entry<K, R> referenceEntry;	//the decorated reference entry
 
  	public ReferenceEntryDecorator(final Entry<K, R> referenceEntry)
  	{
  		this.referenceEntry=checkInstance(referenceEntry, "Reference entry cannot be null.");
  	}
  	
  	
	  /**
	 * Returns the key corresponding to this entry.
	 *
	 * @return the key corresponding to this entry.
	       * @throws IllegalStateException implementations may, but are not
	       *         required to, throw this exception if the entry has been
	       *         removed from the backing map
	 */
	  public K getKey() {return referenceEntry.getKey();};
	
	  	/**
	 * Returns the value corresponding to this entry.  If the mapping
	 * has been removed from the backing map (by the iterator's
	 * <tt>remove</tt> operation), the results of this call are undefined.
	 *
	 * @return the value corresponding to this entry.
	       * @throws IllegalStateException implementations may, but are not
	       *         required to, throw this exception if the entry has been
	       *         removed from the backing map
	 */
	  public V getValue()
	  {
	  	final R reference=referenceEntry.getValue();	//get the reference if there is one
	  	return reference!=null ? reference.get() : null;	//if there is a reference, return the referenced object
	  }
	
	  	/**
	 * Replaces the value corresponding to this entry with the specified
	 * value (optional operation).  (Writes through to the map.)  The
	 * behavior of this call is undefined if the mapping has already been
	 * removed from the map (by the iterator's <tt>remove</tt> operation).
	 *
	 * @param value new value to be stored in this entry.
	 * @return old value corresponding to the entry.
	       * 
	 * @throws UnsupportedOperationException if the <tt>put</tt> operation
	 *	      is not supported by the backing map.
	 * @throws ClassCastException if the class of the specified value
	 * 	      prevents it from being stored in the backing map.
	 * @throws    IllegalArgumentException if some aspect of this value
	 *	      prevents it from being stored in the backing map.
	 * @throws NullPointerException if the backing map does not permit
	 *	      <tt>null</tt> values, and the specified value is
	 *	      <tt>null</tt>.
	       * @throws IllegalStateException implementations may, but are not
	       *         required to, throw this exception if the entry has been
	       *         removed from the backing map
	       */
	  public V setValue(V value)
	  {
	  	final R reference=referenceEntry.setValue(createReference(getKey(), value, referenceQueue));	//create a reference and set the value
	  	if(reference!=null)	//if there was a reference
	  	{
	    	final V oldValue=reference!=null ? reference.get() : null;	//get the old value, if any
	  		reference.enqueue();	//add the reference to the queue for purging
	  		purgeExcept(reference);	//purge all references except the one we just removed
	  		return oldValue;	//return the old value
	  	}
	  	else	//if there was no reference
	  	{
	  		purge();	//purge everything
	  		return null;	//indicate that there was no previous value  		
	  	}
	  }
	
	/**
	 * Compares the specified object with this entry for equality.
	 * Returns <tt>true</tt> if the given object is also a map entry and
	 * the two entries represent the same mapping.  More formally, two
	 * entries <tt>e1</tt> and <tt>e2</tt> represent the same mapping
	 * if<pre>
	       *     (e1.getKey()==null ?
	       *      e2.getKey()==null : e1.getKey().equals(e2.getKey()))  &&
	       *     (e1.getValue()==null ?
	       *      e2.getValue()==null : e1.getValue().equals(e2.getValue()))
	       * </pre>
	 * This ensures that the <tt>equals</tt> method works properly across
	 * different implementations of the <tt>Map.Entry</tt> interface.
	 *
	 * @param o object to be compared for equality with this map entry.
	 * @return <tt>true</tt> if the specified object is equal to this map
	 *         entry.
	       */
	  public boolean equals(Object o) {return referenceEntry.equals(o);}
	
	/**
	 * Returns the hash code value for this map entry.  The hash code
	 * of a map entry <tt>e</tt> is defined to be: <pre>
	 *     (e.getKey()==null   ? 0 : e.getKey().hashCode()) ^
	 *     (e.getValue()==null ? 0 : e.getValue().hashCode())
	       * </pre>
	 * This ensures that <tt>e1.equals(e2)</tt> implies that
	 * <tt>e1.hashCode()==e2.hashCode()</tt> for any two Entries
	 * <tt>e1</tt> and <tt>e2</tt>, as required by the general
	 * contract of <tt>Object.hashCode</tt>.
	 *
	 * @return the hash code value for this map entry.
	 * @see Object#hashCode()
	 * @see Object#equals(Object)
	 * @see #equals(Object)
	 */
	  public int hashCode() {return referenceEntry.hashCode();}
  }

  // Comparison and hashing

  /**
   * Compares the specified object with this map for equality.  Returns
   * <tt>true</tt> if the given object is also a map and the two Maps
   * represent the same mappings.  More formally, two maps <tt>t1</tt> and
   * <tt>t2</tt> represent the same mappings if
   * <tt>t1.entrySet().equals(t2.entrySet())</tt>.  This ensures that the
   * <tt>equals</tt> method works properly across different implementations
   * of the <tt>Map</tt> interface.
   *
   * @param o object to be compared for equality with this map.
   * @return <tt>true</tt> if the specified object is equal to this map.
   */
  public boolean equals(Object o) {return map.equals(o);}

  /**
   * Returns the hash code value for this map.  The hash code of a map
   * is defined to be the sum of the hashCodes of each entry in the map's
   * entrySet view.  This ensures that <tt>t1.equals(t2)</tt> implies
   * that <tt>t1.hashCode()==t2.hashCode()</tt> for any two maps
   * <tt>t1</tt> and <tt>t2</tt>, as required by the general
   * contract of Object.hashCode.
   *
   * @return the hash code value for this map.
   * @see Map.Entry#hashCode()
   * @see Object#hashCode()
   * @see Object#equals(Object)
   * @see #equals(Object)
   */
  public int hashCode() {return map.hashCode();}

  private final ReferenceQueue<V> referenceQueue=new ReferenceQueue<V>();

  /**Purges all enqueued references.*/
  public void purge()
  {
  	purgeExcept(null);
  }

  /**Purges all enqueued references except for the given reference.
  @param exceptReference The reference not to purge, or <code>null</code> if all enqueued references should be purged.
  */
  @SuppressWarnings("unchecked")
	protected void purgeExcept(final R exceptReference)
  {
  	R reference=(R)referenceQueue.poll();	//see if there are any references ready for purging
  	while(reference!=null && reference!=exceptReference)
  	{
  		map.remove(reference.getKey());	//remove this value from the underlying map
    	reference=(R)referenceQueue.poll();	//see if there are any other references to purge
  	}
  }
  
  /**Creates the appropriate reference for associating the given value with the given key.
	The reference will be registered with the given queue.
	@param key The key with which the value is being associated.
	@param value The value to be stored.
	@param referenceQueue The queue with which the reference will be registered.
	@return A reference to the given value that also indicates the key being used.
	*/
  protected abstract R createReference(final K key, final V value, final ReferenceQueue<V> referenceQueue);

  /**An object that remembers the key associated with it.
  @author Garret Wilson
  */
  protected interface Keyed<K>
  {
  	/**@return They key with which the referent value was associated.*/
  	public K getKey();
  }

}
