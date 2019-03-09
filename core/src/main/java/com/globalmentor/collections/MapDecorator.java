
package com.globalmentor.collections;

import java.util.*;

import static java.util.Objects.*;

/**
 * A map that wraps an existing map, providing access through the {@link Map} interface.
 * @param <K> The type of map key.
 * @param <V> The type of map value.
 * @author Garret Wilson
 */
public class MapDecorator<K, V> implements Map<K, V> {

	/** The map this class decorates. */
	protected final Map<K, V> map;

	/**
	 * Map constructor.
	 * @param map The map this map should decorate.
	 * @throws NullPointerException if the provided map is <code>null</code>.
	 */
	public MapDecorator(final Map<K, V> map) {
		this.map = requireNonNull(map, "Map cannot be null"); //save the map
	}

	/**
	 * Returns the number of key-value mappings in this map. If the map contains more than <code>Integer.MAX_VALUE</code> elements, returns <code>Integer.MAX_VALUE</code>
	 * .
	 *
	 * @return the number of key-value mappings in this map.
	 */
	public int size() {
		return map.size();
	}

	/**
	 * Returns <code>true</code> if this map contains no key-value mappings.
	 *
	 * @return <code>true</code> if this map contains no key-value mappings.
	 */
	public boolean isEmpty() {
		return map.isEmpty();
	}

	/**
	 * Returns <code>true</code> if this map contains a mapping for the specified key. More formally, returns <code>true</code> if and only if this map contains a mapping
	 * for a key <code>k</code> such that <code>(key==null ? k==null : key.equals(k))</code>. (There can be at most one such mapping.)
	 *
	 * @param key key whose presence in this map is to be tested.
	 * @return <code>true</code> if this map contains a mapping for the specified key.
	 * 
	 * @throws ClassCastException if the key is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the key is <code>null</code> and this map does not permit <code>null</code> keys (optional).
	 */
	public boolean containsKey(Object key) {
		return map.containsKey(key);
	}

	/**
	 * Returns <code>true</code> if this map maps one or more keys to the specified value. More formally, returns <code>true</code> if and only if this map contains at
	 * least one mapping to a value <code>v</code> such that <code>(value==null ? v==null : value.equals(v))</code>. This operation will probably require time linear in
	 * the map size for most implementations of the <code>Map</code> interface.
	 *
	 * @param value value whose presence in this map is to be tested.
	 * @return <code>true</code> if this map maps one or more keys to the specified value.
	 * @throws ClassCastException if the value is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the value is <code>null</code> and this map does not permit <code>null</code> values (optional).
	 */
	public boolean containsValue(Object value) {
		return map.containsValue(value);
	}

	/**
	 * Returns the value to which this map maps the specified key. Returns <code>null</code> if the map contains no mapping for this key. A return value of
	 * <code>null</code> does not <i>necessarily</i> indicate that the map contains no mapping for the key; it's also possible that the map explicitly maps the key to
	 * <code>null</code>. The <code>containsKey</code> operation may be used to distinguish these two cases.
	 *
	 * <p>
	 * More formally, if this map contains a mapping from a key <code>k</code> to a value <code>v</code> such that <code>(key==null ? k==null :
	 * key.equals(k))</code>, then this method returns <code>v</code>; otherwise it returns <code>null</code>. (There can be at most one such mapping.)
	 *
	 * @param key key whose associated value is to be returned.
	 * @return the value to which this map maps the specified key, or <code>null</code> if the map contains no mapping for this key.
	 * 
	 * @throws ClassCastException if the key is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the key is <code>null</code> and this map does not permit <code>null</code> keys (optional).
	 * 
	 * @see #containsKey(Object)
	 */
	public V get(Object key) {
		return map.get(key);
	}

	// Modification Operations

	/**
	 * Associates the specified value with the specified key in this map (optional operation). If the map previously contained a mapping for this key, the old
	 * value is replaced by the specified value. (A map <code>m</code> is said to contain a mapping for a key <code>k</code> if and only if {@link #containsKey(Object)
	 * m.containsKey(k)} would return <code>true</code>.))
	 *
	 * @param key key with which the specified value is to be associated.
	 * @param value value to be associated with the specified key.
	 * @return previous value associated with specified key, or <code>null</code> if there was no mapping for key. A <code>null</code> return can also indicate that the
	 *         map previously associated <code>null</code> with the specified key, if the implementation supports <code>null</code> values.
	 * 
	 * @throws UnsupportedOperationException if the <code>put</code> operation is not supported by this map.
	 * @throws ClassCastException if the class of the specified key or value prevents it from being stored in this map.
	 * @throws IllegalArgumentException if some aspect of this key or value prevents it from being stored in this map.
	 * @throws NullPointerException if this map does not permit <code>null</code> keys or values, and the specified key or value is <code>null</code>.
	 */
	public V put(K key, V value) {
		return map.put(key, value);
	}

	/**
	 * Removes the mapping for this key from this map if it is present (optional operation). More formally, if this map contains a mapping from key <code>k</code> to
	 * value <code>v</code> such that <code>(key==null ?  k==null : key.equals(k))</code>, that mapping is removed. (The map can contain at most one such mapping.)
	 *
	 * <p>
	 * Returns the value to which the map previously associated the key, or <code>null</code> if the map contained no mapping for this key. (A <code>null</code> return
	 * can also indicate that the map previously associated <code>null</code> with the specified key if the implementation supports <code>null</code> values.) The map
	 * will not contain a mapping for the specified key once the call returns.
	 *
	 * @param key key whose mapping is to be removed from the map.
	 * @return previous value associated with specified key, or <code>null</code> if there was no mapping for key.
	 *
	 * @throws ClassCastException if the key is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the key is <code>null</code> and this map does not permit <code>null</code> keys (optional).
	 * @throws UnsupportedOperationException if the <code>remove</code> method is not supported by this map.
	 */
	public V remove(Object key) {
		return map.remove(key);
	}

	// Bulk Operations

	/**
	 * Copies all of the mappings from the specified map to this map (optional operation). The effect of this call is equivalent to that of calling
	 * {@link #put(Object,Object) put(k, v)} on this map once for each mapping from key <code>k</code> to value <code>v</code> in the specified map. The behavior of this
	 * operation is unspecified if the specified map is modified while the operation is in progress.
	 *
	 * @param t Mappings to be stored in this map.
	 * 
	 * @throws UnsupportedOperationException if the <code>putAll</code> method is not supported by this map.
	 * 
	 * @throws ClassCastException if the class of a key or value in the specified map prevents it from being stored in this map.
	 * 
	 * @throws IllegalArgumentException some aspect of a key or value in the specified map prevents it from being stored in this map.
	 * @throws NullPointerException if the specified map is <code>null</code>, or if this map does not permit <code>null</code> keys or values, and the specified map
	 *           contains <code>null</code> keys or values.
	 */
	public void putAll(Map<? extends K, ? extends V> t) {
		map.putAll(t);
	}

	/**
	 * Removes all mappings from this map (optional operation).
	 *
	 * @throws UnsupportedOperationException clear is not supported by this map.
	 */
	public void clear() {
		map.clear();
	}

	// Views

	/**
	 * Returns a set view of the keys contained in this map. The set is backed by the map, so changes to the map are reflected in the set, and vice-versa. If the
	 * map is modified while an iteration over the set is in progress (except through the iterator's own <code>remove</code> operation), the results of the iteration
	 * are undefined. The set supports element removal, which removes the corresponding mapping from the map, via the <code>Iterator.remove</code>,
	 * <code>Set.remove</code>, <code>removeAll</code> <code>retainAll</code>, and <code>clear</code> operations. It does not support the add or <code>addAll</code> operations.
	 *
	 * @return a set view of the keys contained in this map.
	 */
	public Set<K> keySet() {
		return map.keySet();
	}

	/**
	 * Returns a collection view of the values contained in this map. The collection is backed by the map, so changes to the map are reflected in the collection,
	 * and vice-versa. If the map is modified while an iteration over the collection is in progress (except through the iterator's own <code>remove</code> operation),
	 * the results of the iteration are undefined. The collection supports element removal, which removes the corresponding mapping from the map, via the
	 * <code>Iterator.remove</code>, <code>Collection.remove</code>, <code>removeAll</code>, <code>retainAll</code> and <code>clear</code> operations. It does not support the add or
	 * <code>addAll</code> operations.
	 *
	 * @return a collection view of the values contained in this map.
	 */
	public Collection<V> values() {
		return map.values();
	}

	/**
	 * Returns a set view of the mappings contained in this map. Each element in the returned set is a {@link Map.Entry}. The set is backed by the map, so changes
	 * to the map are reflected in the set, and vice-versa. If the map is modified while an iteration over the set is in progress (except through the iterator's
	 * own <code>remove</code> operation, or through the <code>setValue</code> operation on a map entry returned by the iterator) the results of the iteration are
	 * undefined. The set supports element removal, which removes the corresponding mapping from the map, via the <code>Iterator.remove</code>, <code>Set.remove</code>,
	 * <code>removeAll</code>, <code>retainAll</code> and <code>clear</code> operations. It does not support the <code>add</code> or <code>addAll</code> operations.
	 *
	 * @return a set view of the mappings contained in this map.
	 */
	public Set<Map.Entry<K, V>> entrySet() {
		return map.entrySet();
	}

	// Comparison and hashing

	/**
	 * Compares the specified object with this collection for equality.
	 * <p>
	 *
	 * While the <code>Collection</code> interface adds no stipulations to the general contract for the <code>Object.equals</code>, programmers who implement the
	 * <code>Collection</code> interface "directly" (in other words, create a class that is a <code>Collection</code> but is not a <code>Set</code> or a <code>List</code>) must
	 * exercise care if they choose to override the <code>Object.equals</code>. It is not necessary to do so, and the simplest course of action is to rely on
	 * <code>Object</code>'s implementation, but the implementer may wish to implement a "value comparison" in place of the default "reference comparison." (The
	 * <code>List</code> and <code>Set</code> interfaces mandate such value comparisons.)
	 * <p>
	 *
	 * The general contract for the <code>Object.equals</code> method states that equals must be symmetric (in other words, <code>a.equals(b)</code> if and only if
	 * <code>b.equals(a)</code>). The contracts for <code>List.equals</code> and <code>Set.equals</code> state that lists are only equal to other lists, and sets to other
	 * sets. Thus, a custom <code>equals</code> method for a collection class that implements neither the <code>List</code> nor <code>Set</code> interface must return
	 * <code>false</code> when this collection is compared to any list or set. (By the same logic, it is not possible to write a class that correctly implements both
	 * the <code>Set</code> and <code>List</code> interfaces.)
	 *
	 * @param o Object to be compared for equality with this collection.
	 * @return <code>true</code> if the specified object is equal to this collection
	 * 
	 * @see Object#equals(Object)
	 * @see Set#equals(Object)
	 * @see List#equals(Object)
	 */
	public boolean equals(Object o) {
		return map.equals(o);
	}

	/**
	 * Returns the hash code value for this collection. While the <code>Collection</code> interface adds no stipulations to the general contract for the
	 * <code>Object.hashCode</code> method, programmers should take note that any class that overrides the <code>Object.equals</code> method must also override the
	 * <code>Object.hashCode</code> method in order to satisfy the general contract for the <code>Object.hashCode</code>method. In particular, <code>c1.equals(c2)</code>
	 * implies that <code>c1.hashCode()==c2.hashCode()</code>.
	 *
	 * @return the hash code value for this collection
	 * 
	 * @see Object#hashCode()
	 * @see Object#equals(Object)
	 */
	public int hashCode() {
		return map.hashCode();
	}
}
