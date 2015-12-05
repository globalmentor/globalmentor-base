/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.collections;

import java.util.*;

import com.globalmentor.model.IDed;

/**
 * A list that allows quick lookup of {@link IDed} objects. This list will give unpredictable results if multiple objects with the same ID are added to the
 * list. This list does not support <code>null</code> values.
 * @author Garret Wilson
 */
public class DecoratorIDedMappedList<K, E extends IDed<K>> extends ListDecorator<E> implements MappedList<K, E> { //TODO finish this class if needed

	/** The map associating IDs with list items. */
	private final Map<K, E> map;

	/** @return The map associating IDs with list items. */
	protected Map<K, E> getMap() {
		return map;
	}

	/**
	 * Decorates the given list by mapping its values. Any values present in the map will be removed. Any values already present in the list will be mapped.
	 * @param map The map which stores the ID to list item associations.
	 * @param list The list in which to store the list items.
	 * @throws IllegalArgumentException if more than one item with the same ID exists in the list.
	 */
	public DecoratorIDedMappedList(final Map<K, E> map, final List<E> list) {
		super(list); //decorate the given list
		this.map = map; //save our map
		map.clear(); //make sure we don't have any values in the map
		for(final E element : list) { //for all elements already in the list
			final K key = element.getID(); //use the element's ID as the key
			if(!map.containsKey(key)) { //if the element isn't yet in the map
				map.put(key, element); //store the element in the map
			} else { //if the element is already in the map
				throw new IllegalArgumentException("Duplicate ID \"" + key + "\"."); //TODO maybe do this in the add() method as well
			}
		}
	}

	/**
	 * 
	 * Returns <tt>true</tt> if this list contains the specified element. More formally, returns <tt>true</tt> if and only if this list contains at least one
	 * element <tt>e</tt> such that <tt>(o==null&nbsp;?&nbsp;e==null&nbsp;:&nbsp;o.equals(e))</tt>.
	 *
	 * @param o element whose presence in this list is to be tested.
	 * @return <tt>true</tt> if this list contains the specified element.
	 * @throws ClassCastException if the type of the specified element is incompatible with this list (optional).
	 * @throws NullPointerException if the specified element is null and this list does not support null elements (optional).
	 */
	//TODO fix  boolean contains(Object o);

	/**
	 * Appends the specified element to the end of this list, updating the map.
	 * @param o element to be appended to this list.
	 * @return <code>true</code> (as per the general contract of the <code>Collection.add</code> method).
	 * @throws UnsupportedOperationException if the <code>add</code> method is not supported by this list.
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list.
	 * @throws NullPointerException if the specified element is null and this list does not support null elements.
	 * @throws IllegalArgumentException if some aspect of this element prevents it from being added to this list.
	 */
	public boolean add(E o) {
		final boolean result = super.add(o); //add the element normally
		if(result) { //if things went well
			getMap().put(o.getID(), o); //add the element to the map, in case it wasn't added already
		}
		return result; //show how things went
	}

	/**
	 * Removes the first occurrence in this list of the specified element (optional operation). If this list does not contain the element, it is unchanged. More
	 * formally, removes the element with the lowest index i such that <tt>(o==null ? get(i)==null : o.equals(get(i)))</tt> (if such an element exists).
	 *
	 * @param o element to be removed from this list, if present.
	 * @return <tt>true</tt> if this list contained the specified element.
	 * @throws ClassCastException if the type of the specified element is incompatible with this list (optional).
	 * @throws NullPointerException if the specified element is null and this list does not support null elements (optional).
	 * @throws UnsupportedOperationException if the <tt>remove</tt> method is not supported by this list.
	 */
	public boolean remove(Object o) {
		final boolean result = super.remove(o); //remove the object normally
		if(result) { //if we succeeded
			getMap().remove(o); //remove the objet from the map
		}
		return result; //return the result
	}

	/**
	 * Appends all of the elements in the specified collection to the end of this list, in the order that they are returned by the specified collection's iterator
	 * (optional operation). The behavior of this operation is unspecified if the specified collection is modified while the operation is in progress. (Note that
	 * this will occur if the specified collection is this list, and it's nonempty.)
	 *
	 * @param c collection whose elements are to be added to this list.
	 * @return <tt>true</tt> if this list changed as a result of the call.
	 * 
	 * @throws UnsupportedOperationException if the <tt>addAll</tt> method is not supported by this list.
	 * @throws ClassCastException if the class of an element in the specified collection prevents it from being added to this list.
	 * @throws NullPointerException if the specified collection contains one or more null elements and this list does not support null elements, or if the
	 *           specified collection is <tt>null</tt>.
	 * @throws IllegalArgumentException if some aspect of an element in the specified collection prevents it from being added to this list.
	 * @see #add(Object)
	 */
	public boolean addAll(Collection<? extends E> c) {
		final boolean result = super.addAll(c); //add all normally
		if(result) { //if things went well
			for(final E element : c) { //for each element in the collection
				getMap().put(element.getID(), element); //add the element to the map, in case it wasn't added already
			}
		}
		return result; //return the result
	}

	/**
	 * Inserts all of the elements in the specified collection into this list at the specified position (optional operation). Shifts the element currently at that
	 * position (if any) and any subsequent elements to the right (increases their indices). The new elements will appear in this list in the order that they are
	 * returned by the specified collection's iterator. The behavior of this operation is unspecified if the specified collection is modified while the operation
	 * is in progress. (Note that this will occur if the specified collection is this list, and it's nonempty.)
	 *
	 * @param index index at which to insert first element from the specified collection.
	 * @param c elements to be inserted into this list.
	 * @return <tt>true</tt> if this list changed as a result of the call.
	 * 
	 * @throws UnsupportedOperationException if the <tt>addAll</tt> method is not supported by this list.
	 * @throws ClassCastException if the class of one of elements of the specified collection prevents it from being added to this list.
	 * @throws NullPointerException if the specified collection contains one or more null elements and this list does not support null elements, or if the
	 *           specified collection is <tt>null</tt>.
	 * @throws IllegalArgumentException if some aspect of one of elements of the specified collection prevents it from being added to this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt; size()).
	 */
	public boolean addAll(int index, Collection<? extends E> c) {
		final boolean result = super.addAll(index, c); //add all normally
		if(result) { //if things went well
			for(final E element : c) { //for each element in the collection
				getMap().put(element.getID(), element); //add the element to the map, in case it wasn't added already
			}
		}
		return result; //return the result  	
	}

	/**
	 * Removes from this list all the elements that are contained in the specified collection (optional operation).
	 *
	 * @param c collection that defines which elements will be removed from this list.
	 * @return <tt>true</tt> if this list changed as a result of the call.
	 * 
	 * @throws UnsupportedOperationException if the <tt>removeAll</tt> method is not supported by this list.
	 * @throws ClassCastException if the types of one or more elements in this list are incompatible with the specified collection (optional).
	 * @throws NullPointerException if this list contains one or more null elements and the specified collection does not support null elements (optional).
	 * @throws NullPointerException if the specified collection is <tt>null</tt>.
	 * @see #remove(Object)
	 * @see #contains(Object)
	 */
	public boolean removeAll(Collection<?> c) {
		final boolean result = super.removeAll(c); //remove all normally
		if(result) { //if things went well
			for(final Object element : c) { //for each element in the collection
				getMap().remove(element); //remove the element from the map, in case it wasn't removed already
			}
		}
		return result; //return the result  	
	}

	/**
	 * Retains only the elements in this list that are contained in the specified collection (optional operation). In other words, removes from this list all the
	 * elements that are not contained in the specified collection.
	 *
	 * @param c collection that defines which elements this set will retain.
	 * 
	 * @return <tt>true</tt> if this list changed as a result of the call.
	 * 
	 * @throws UnsupportedOperationException if the <tt>retainAll</tt> method is not supported by this list.
	 * @throws ClassCastException if the types of one or more elements in this list are incompatible with the specified collection (optional).
	 * @throws NullPointerException if this list contains one or more null elements and the specified collection does not support null elements (optional).
	 * @throws NullPointerException if the specified collection is <tt>null</tt>.
	 * @see #remove(Object)
	 * @see #contains(Object)
	 */
	public boolean retainAll(Collection<?> c) {
		final boolean result = super.retainAll(c); //retain all normally
		if(result) { //if things went well
			for(final E element : getMap().values()) { //for each element in the map
				if(!c.contains(element)) { //if the collection does not contain this value
					getMap().remove(element); //remove the element from the map
				}
			}
		}
		return result; //return the result  	
	}

	/**
	 * Removes all of the elements from this list (optional operation). This list will be empty after this call returns (unless it throws an exception).
	 *
	 * @throws UnsupportedOperationException if the <tt>clear</tt> method is not supported by this list.
	 */
	public void clear() {
		super.clear(); //clear normally
		getMap().clear(); //clear our map
	}

	// Comparison and hashing

	/**
	 * Replaces the element at the specified position in this list with the specified element (optional operation).
	 *
	 * @param index index of element to replace.
	 * @param element element to be stored at the specified position.
	 * @return the element previously at the specified position.
	 * 
	 * @throws UnsupportedOperationException if the <tt>set</tt> method is not supported by this list.
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list.
	 * @throws NullPointerException if the specified element is null and this list does not support null elements.
	 * @throws IllegalArgumentException if some aspect of the specified element prevents it from being added to this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt;= size()).
	 */
	public E set(int index, E element) {
		final E oldElement = super.set(index, element); //set the element normally
		getMap().remove(oldElement); //remove the old element from the map
		getMap().put(element.getID(), element); //add the element to the map, in case it wasn't added already
		return oldElement; //return the old element
	}

	/**
	 * Inserts the specified element at the specified position in this list (optional operation). Shifts the element currently at that position (if any) and any
	 * subsequent elements to the right (adds one to their indices).
	 *
	 * @param index index at which the specified element is to be inserted.
	 * @param element element to be inserted.
	 * 
	 * @throws UnsupportedOperationException if the <tt>add</tt> method is not supported by this list.
	 * @throws ClassCastException if the class of the specified element prevents it from being added to this list.
	 * @throws NullPointerException if the specified element is null and this list does not support null elements.
	 * @throws IllegalArgumentException if some aspect of the specified element prevents it from being added to this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt; size()).
	 */
	public void add(int index, E element) {
		super.add(index, element); //add the element normally
		getMap().put(element.getID(), element); //add the element to the map
	}

	/**
	 * Removes the element at the specified position in this list (optional operation). Shifts any subsequent elements to the left (subtracts one from their
	 * indices). Returns the element that was removed from the list.
	 *
	 * @param index the index of the element to removed.
	 * @return the element previously at the specified position.
	 * 
	 * @throws UnsupportedOperationException if the <tt>remove</tt> method is not supported by this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index &lt; 0 || index &gt;= size()).
	 */
	public E remove(int index) {
		final E oldElement = super.remove(index); //remove the element normally
		getMap().remove(oldElement); //remove the old element from the map
		return oldElement; //return the old element
	}

	/**
	 * Returns <tt>true</tt> if this map contains a mapping for the specified key. More formally, returns <tt>true</tt> if and only if this map contains a mapping
	 * for a key <tt>k</tt> such that <tt>(key==null ? k==null : key.equals(k))</tt>. (There can be at most one such mapping.)
	 *
	 * @param key key whose presence in this map is to be tested.
	 * @return <tt>true</tt> if this map contains a mapping for the specified key.
	 * 
	 * @throws ClassCastException if the key is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the key is <tt>null</tt> and this map does not permit <tt>null</tt> keys (optional).
	 */
	public boolean containsKey(Object key) {
		return getMap().containsKey(key);
	}

	/**
	 * Returns the value to which this map maps the specified key. Returns <tt>null</tt> if the map contains no mapping for this key. A return value of
	 * <tt>null</tt> does not <i>necessarily</i> indicate that the map contains no mapping for the key; it's also possible that the map explicitly maps the key to
	 * <tt>null</tt>. The <tt>containsKey</tt> operation may be used to distinguish these two cases.
	 *
	 * <p>
	 * More formally, if this map contains a mapping from a key <tt>k</tt> to a value <tt>v</tt> such that <tt>(key==null ? k==null :
	 * key.equals(k))</tt>, then this method returns <tt>v</tt>; otherwise it returns <tt>null</tt>. (There can be at most one such mapping.)
	 *
	 * @param key key whose associated value is to be returned.
	 * @return the value to which this map maps the specified key, or <tt>null</tt> if the map contains no mapping for this key.
	 * 
	 * @throws ClassCastException if the key is of an inappropriate type for this map (optional).
	 * @throws NullPointerException if the key is <tt>null</tt> and this map does not permit <tt>null</tt> keys (optional).
	 * 
	 * @see #containsKey(Object)
	 */
	public E get(Object key) {
		return getMap().get(key);
	}

	/**
	 * Removes the value from the list mapped to the given key value.
	 * @param key The key whose mapping is to be removed from the map and whose corresponding value is to be removed from the list.
	 * @return previous value associated with specified key, or <code>null</code> if there was no mapping for key.
	 * @throws ClassCastException if the key is of an inappropriate type for this mapped list (optional).
	 * @throws NullPointerException if the key is <code>null</code> and this mapped list does not permit <code>null</code> keys (optional).
	 * @throws UnsupportedOperationException if the <code>remove</code> method is not supported by this mapped list.
	 */
	public E removeKey(Object key) {
		final E element = get(key); //get the element, if it exists
		if(element != null) { //if there is such an element
			getMap().remove(key); //remove the object from the map
			remove(element); //remove the element from the list
		}
		return element; //return the element, if any, that was removed 
	}
}
