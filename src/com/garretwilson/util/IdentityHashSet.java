package com.garretwilson.util;

import java.util.*;

/**A set that is backed by an identity hash map.
<p><strong>This class is <em>not</em> a general-purpose <code>Set</code>
	implementation!  While this class implements the <code>Map</code> interface,
	it intentionally violates <code>Set</code>'s general contract, which mandates
	the use of the <code>>equals()</code> method when comparing objects.  This
	class is designed for use only in the rare cases wherein reference-equality
	semantics are required.</strong></p>
<p>This class is based upon <code>java.util.HashSet</code> 1.28, 01/23/03 by 
	Josh Bloch, Copyright 2003 Sun Microsystems, Inc.</p>
@author Garret Wilson
@see java.util.HashSet
@see java.util.IdentityHashMap
*/
public class IdentityHashSet extends AbstractSet implements Set, Cloneable
{
    private transient IdentityHashMap map;

    // Dummy value to associate with an Object in the backing Map
    private static final Object PRESENT = new Object();

    /**
     * Constructs a new, empty set; the backing <tt>HashMap</tt> instance has
     * default initial capacity (16) and load factor (0.75).
     */
    public IdentityHashSet() {
	map = new IdentityHashMap();
    }

    /**
     * Constructs a new set containing the elements in the specified
     * collection.  The <tt>HashMap</tt> is created with default load factor
     * (0.75) and an initial capacity sufficient to contain the elements in
     * the specified collection.
     *
     * @param c the collection whose elements are to be placed into this set.
     * @throws NullPointerException   if the specified collection is null.
     */
    public IdentityHashSet(Collection c) {
	map = new IdentityHashMap(Math.max((int) (c.size()/.75f) + 1, 16));
	addAll(c);
    }

    /**
     * Constructs a new, empty set; the backing <tt>HashMap</tt> instance has
     * the specified initial capacity and default load factor, which is
     * <tt>0.75</tt>.
     *
     * @param      initialCapacity   the initial capacity of the hash table.
     * @throws     IllegalArgumentException if the initial capacity is less
     *             than zero.
     */
    public IdentityHashSet(int initialCapacity) {
	map = new IdentityHashMap(initialCapacity);
    }

    /**
     * Returns an iterator over the elements in this set.  The elements
     * are returned in no particular order.
     *
     * @return an Iterator over the elements in this set.
     * @see ConcurrentModificationException
     */
    public Iterator iterator() {
	return map.keySet().iterator();
    }

    /**
     * Returns the number of elements in this set (its cardinality).
     *
     * @return the number of elements in this set (its cardinality).
     */
    public int size() {
	return map.size();
    }

    /**
     * Returns <tt>true</tt> if this set contains no elements.
     *
     * @return <tt>true</tt> if this set contains no elements.
     */
    public boolean isEmpty() {
	return map.isEmpty();
    }

    /**
     * Returns <tt>true</tt> if this set contains the specified element.
     *
     * @param o element whose presence in this set is to be tested.
     * @return <tt>true</tt> if this set contains the specified element.
     */
    public boolean contains(Object o) {
	return map.containsKey(o);
    }

    /**
     * Adds the specified element to this set if it is not already
     * present.
     *
     * @param o element to be added to this set.
     * @return <tt>true</tt> if the set did not already contain the specified
     * element.
     */
    public boolean add(Object o) {
	return map.put(o, PRESENT)==null;
    }

    /**
     * Removes the specified element from this set if it is present.
     *
     * @param o object to be removed from this set, if present.
     * @return <tt>true</tt> if the set contained the specified element.
     */
    public boolean remove(Object o) {
	return map.remove(o)==PRESENT;
    }

    /**
     * Removes all of the elements from this set.
     */
    public void clear() {
	map.clear();
    }

    /**
     * Returns a shallow copy of this <tt>HashSet</tt> instance: the elements
     * themselves are not cloned.
     *
     * @return a shallow copy of this set.
     */
    public Object clone() {
	try { 
	    IdentityHashSet newSet = (IdentityHashSet)super.clone();
	    newSet.map = (IdentityHashMap)map.clone();
	    return newSet;
	} catch (CloneNotSupportedException e) { 
	    throw new InternalError();
	}
    }
}