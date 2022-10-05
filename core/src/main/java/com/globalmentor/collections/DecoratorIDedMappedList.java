/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
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
 * list.
 * @implSpec This list does not support <code>null</code> values.
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

	//TODO fix
	//		@Override
	//		public boolean contains(Object o) {
	//		}

	@Override
	public boolean add(E o) {
		final boolean result = super.add(o); //add the element normally
		if(result) { //if things went well
			getMap().put(o.getID(), o); //add the element to the map, in case it wasn't added already
		}
		return result; //show how things went
	}

	@Override
	public boolean remove(Object o) {
		final boolean result = super.remove(o); //remove the object normally
		if(result) { //if we succeeded
			getMap().remove(o); //remove the object from the map
		}
		return result; //return the result
	}

	@Override
	public boolean addAll(Collection<? extends E> c) {
		final boolean result = super.addAll(c); //add all normally
		if(result) { //if things went well
			for(final E element : c) { //for each element in the collection
				getMap().put(element.getID(), element); //add the element to the map, in case it wasn't added already
			}
		}
		return result; //return the result
	}

	@Override
	public boolean addAll(int index, Collection<? extends E> c) {
		final boolean result = super.addAll(index, c); //add all normally
		if(result) { //if things went well
			for(final E element : c) { //for each element in the collection
				getMap().put(element.getID(), element); //add the element to the map, in case it wasn't added already
			}
		}
		return result; //return the result  	
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		final boolean result = super.removeAll(c); //remove all normally
		if(result) { //if things went well
			for(final Object element : c) { //for each element in the collection
				getMap().remove(element); //remove the element from the map, in case it wasn't removed already
			}
		}
		return result; //return the result  	
	}

	@Override
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

	@Override
	public void clear() {
		super.clear(); //clear normally
		getMap().clear(); //clear our map
	}

	// Comparison and hashing

	@Override
	public E set(int index, E element) {
		final E oldElement = super.set(index, element); //set the element normally
		getMap().remove(oldElement); //remove the old element from the map
		getMap().put(element.getID(), element); //add the element to the map, in case it wasn't added already
		return oldElement; //return the old element
	}

	@Override
	public void add(int index, E element) {
		super.add(index, element); //add the element normally
		getMap().put(element.getID(), element); //add the element to the map
	}

	@Override
	public E remove(int index) {
		final E oldElement = super.remove(index); //remove the element normally
		getMap().remove(oldElement); //remove the old element from the map
		return oldElement; //return the old element
	}

	@Override
	public boolean containsKey(Object key) {
		return getMap().containsKey(key);
	}

	@Override
	public E get(Object key) {
		return getMap().get(key);
	}

	@Override
	public E removeKey(Object key) {
		final E element = get(key); //get the element, if it exists
		if(element != null) { //if there is such an element
			getMap().remove(key); //remove the object from the map
			remove(element); //remove the element from the list
		}
		return element; //return the element, if any, that was removed 
	}
}
