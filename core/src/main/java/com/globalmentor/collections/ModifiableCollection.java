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

import com.globalmentor.model.DefaultModifiable;
import com.globalmentor.model.Modifiable;

/**
 * A collection that implements <code>Modifiable</code> so that it can keep track of whether it has been modified.
 * @apiNote This class is meant as a wrapper to an existing collection.
 * @param <E> the type of elements in this collection.
 * @author Garret Wilson
 * @see Modifiable
 * @deprecated
 */
@Deprecated
public class ModifiableCollection<E> extends DefaultModifiable implements Collection<E> {

	/** The collection this class wraps. */
	private final Collection<E> collection;

	/**
	 * Collection constructor.
	 * @param collection The collection this collection should wrap.
	 */
	public ModifiableCollection(final Collection<E> collection) {
		this.collection = collection; //save the collection
	}

	@Override
	public int size() {
		return collection.size();
	}

	@Override
	public boolean isEmpty() {
		return collection.isEmpty();
	}

	@Override
	public boolean contains(Object o) {
		return collection.contains(o);
	}

	@Override
	public Iterator<E> iterator() {
		return collection.iterator();
	}

	@Override
	public Object[] toArray() {
		return collection.toArray();
	}

	@Override
	public <T> T[] toArray(T[] a) {
		return collection.toArray(a);
	}

	// Modification Operations

	@Override
	public boolean add(E o) {
		final boolean modified = collection.add(o); //do the default functionality
		if(modified) //if we modified the collection
			setModified(true); //show that we've been modified
		return modified; //return whether we were modified
	}

	@Override
	public boolean remove(Object o) {
		final boolean modified = collection.remove(o); //do the default functionality
		if(modified) //if we modified the collection
			setModified(true); //show that we've been modified
		return modified; //return whether we were modified
	}

	// Bulk Operations

	@Override
	public boolean containsAll(Collection<?> c) {
		return collection.containsAll(c);
	}

	@Override
	public boolean addAll(Collection<? extends E> c) {
		final boolean modified = collection.addAll(c); //do the default functionality
		if(modified) //if we modified the collection
			setModified(true); //show that we've been modified
		return modified; //return whether we were modified
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		final boolean modified = collection.removeAll(c); //do the default functionality
		if(modified) //if we modified the collection
			setModified(true); //show that we've been modified
		return modified; //return whether we were modified
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		final boolean modified = collection.retainAll(c); //do the default functionality
		if(modified) //if we modified the collection
			setModified(true); //show that we've been modified
		return modified; //return whether we were modified
	}

	@Override
	public void clear() {
		collection.clear(); //do the default functionality
		setModified(true); //show that we've been modified
	}

	// Comparison and hashing

	@Override
	public boolean equals(Object o) {
		return collection.equals(o);
	}

	@Override
	public int hashCode() {
		return collection.hashCode();
	}

}
