package com.garretwilson.util;

import java.util.*;

/**A class that implements the <code>Set</code> interface, backed by a
	<code>WeakHashMap</code>. This means that members of the set will
	be collected by the garbage collector when they are no longer in ordinary
	use.
<p>This class was created referencing <code>java.util.HashSet</code>
	1.25 01/12/03.</p>
@author Garret Wilson
@see HashSet
@see WeakHashMap
*/
public class WeakHashSet extends AbstractSet implements Set
{

	/**The weak hash map that backs this set.*/
	private final transient WeakHashMap map;

	/**The dummy value used in the weak hash map.*/
	private final static Object DUMMY_VALUE=new Object();

	/**Constructs a new, empty set. The backing <code>WeakHashMap</code> instance
		has default initial capacity (16) and load factor (0.75).
	 */
	public WeakHashSet()
	{
		map=new WeakHashMap();	//create a default weak hash map
	}

	/**Constructs a new set containing the elements in the specified
		collection.  The <code>WeakHashMap</code> is created with default load
		factor (0.75) and an initial capacity sufficient to contain the elements in
		the specified collection.
	@param collection The collection whose elements are to be placed into this set.
	@throws NullPointerException Thrown if the specified collection is <code>null</code>.
	 */
	public WeakHashSet(final Collection collection)
	{
		map=new WeakHashMap(Math.max((int)(collection.size()/.75f)+1, 16));	//create a new hash map big enough to hold the collection 
		addAll(collection);	//add the contents of the collection
	}

	/**Constructs a new, empty set. The backing <code>WeakHashMap</code> instance
		has the specified initial capacity and the specified load factor.
	@param initialCapacity The initial capacity of the hash map.
	@param loadFactor The load factor of the hash map.
	@throws IllegalArgumentException Thrown if the initial capacity is less
		than zero, or if the load factor is nonpositive.
	*/
	public WeakHashSet(final int initialCapacity, final float loadFactor)
	{
		map=new WeakHashMap(initialCapacity, loadFactor);	//create a hash map with the given parameters
	}

	/**Constructs a new, empty set. The backing <code>WeakHashMap</code> instance
		has the specified initial capacity and default load factor, which is
		<code>0.75</code>.
	@param initialCapacity The initial capacity of the hash table.
	@throws IllegalArgumentException Thrown if the initial capacity is less
		than zero.
	*/
	public WeakHashSet(final int initialCapacity)
	{
		map=new WeakHashMap(initialCapacity);	//create the hash map with the given initial capacity
	}

	/**
	 * Returns the number of elements in this set (its cardinality).  If this
	 * set contains more than <tt>Integer.MAX_VALUE</tt> elements, returns
	 * <tt>Integer.MAX_VALUE</tt>.
	 *
	 * @return the number of elements in this set (its cardinality).
	 */
	public int size() {return map.size();}

	/**
	 * Returns <tt>true</tt> if this set contains no elements.
	 *
	 * @return <tt>true</tt> if this set contains no elements.
	 */
	public boolean isEmpty() {return map.isEmpty();}

	/**
	 * Returns <tt>true</tt> if this set contains the specified element.  More
	 * formally, returns <tt>true</tt> if and only if this set contains an
	 * element <code>e</code> such that <code>(o==null ? e==null :
	 * o.equals(e))</code>.
	 *
	 * @param o element whose presence in this set is to be tested.
	 * @return <tt>true</tt> if this set contains the specified element.
	 * @throws ClassCastException if the type of the specified element
	 * 	       is incompatible with this set (optional).
	 * @throws NullPointerException if the specified element is null and this
	 *         set does not support null elements (optional).
	 */
	public boolean contains(Object o) {return map.containsKey(o);}

	/**
	 * Returns an iterator over the elements in this set.  The elements are
	 * returned in no particular order (unless this set is an instance of some
	 * class that provides a guarantee).
	 *
	 * @return an iterator over the elements in this set.
	 */
	public Iterator iterator() {return map.keySet().iterator();}


	// Modification Operations

	/**
	 * Adds the specified element to this set if it is not already present
	 * (optional operation).  More formally, adds the specified element,
	 * <code>o</code>, to this set if this set contains no element
	 * <code>e</code> such that <code>(o==null ? e==null :
	 * o.equals(e))</code>.  If this set already contains the specified
	 * element, the call leaves this set unchanged and returns <tt>false</tt>.
	 * In combination with the restriction on constructors, this ensures that
	 * sets never contain duplicate elements.<p>
	 *
	 * The stipulation above does not imply that sets must accept all
	 * elements; sets may refuse to add any particular element, including
	 * <tt>null</tt>, and throwing an exception, as described in the
	 * specification for <tt>Collection.add</tt>.  Individual set
	 * implementations should clearly document any restrictions on the the
	 * elements that they may contain.
	 *
	 * @param o element to be added to this set.
	 * @return <tt>true</tt> if this set did not already contain the specified
	 *         element.
	 * 
	 * @throws UnsupportedOperationException if the <tt>add</tt> method is not
	 * 	       supported by this set.
	 * @throws ClassCastException if the class of the specified element
	 * 	       prevents it from being added to this set.
	 * @throws NullPointerException if the specified element is null and this
	 *         set does not support null elements.
	 * @throws IllegalArgumentException if some aspect of the specified element
	 *         prevents it from being added to this set.
	 */
	public boolean add(Object o) {return map.put(o, DUMMY_VALUE)==null;}


	/**
	 * Removes the specified element from this set if it is present (optional
	 * operation).  More formally, removes an element <code>e</code> such that
	 * <code>(o==null ?  e==null : o.equals(e))</code>, if the set contains
	 * such an element.  Returns <tt>true</tt> if the set contained the
	 * specified element (or equivalently, if the set changed as a result of
	 * the call).  (The set will not contain the specified element once the
	 * call returns.)
	 *
	 * @param o object to be removed from this set, if present.
	 * @return true if the set contained the specified element.
	 * @throws ClassCastException if the type of the specified element
	 * 	       is incompatible with this set (optional).
	 * @throws NullPointerException if the specified element is null and this
	 *         set does not support null elements (optional).
	 * @throws UnsupportedOperationException if the <tt>remove</tt> method is
	 *         not supported by this set.
	 */
	public boolean remove(Object o) {return map.remove(o)==DUMMY_VALUE;}


	// Bulk Operations


	/**
	 * Removes all of the elements from this set (optional operation).
	 * This set will be empty after this call returns (unless it throws an
	 * exception).
	 *
	 * @throws UnsupportedOperationException if the <tt>clear</tt> method
	 * 		  is not supported by this set.
	 */
	public void clear() {map.clear();}

}
