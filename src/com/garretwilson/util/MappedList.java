package com.garretwilson.util;

import java.util.List;

/**A list that allows quick lookup of its elements.
This interface does not implement <code>Map</code>, because the <code>Map</code> interface
has <code>remove(Object)</code> semantics that conflict with those of the corresponding <code>List</code> method. 
@author Garret Wilson
@see java.util.List
@see java.util.Map
*/
public interface MappedList<K, E> extends List<E>
{
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
  public boolean containsKey(Object key);

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
  public E get(Object key);

  /**Removes the value from the list mapped to the given key value.
  @param key The key whose mapping is to be removed from the map
  	and whose corresponding value is to be removed from the list.
  @return previous value associated with specified key, or <code>null</code>
  	if there was no mapping for key.
  @throws ClassCastException if the key is of an inappropriate type for
  	this mapped list (optional).
  @throws NullPointerException if the key is <code>null</code> and this mapped list
  	does not permit <code>null</code> keys (optional).
	@throws UnsupportedOperationException if the <code>remove</code> method is
		not supported by this mapped list.
	*/
  public E removeKey(Object key);

}
