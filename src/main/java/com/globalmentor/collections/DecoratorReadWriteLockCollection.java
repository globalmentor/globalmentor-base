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
import java.util.concurrent.locks.*;

import static java.util.Objects.*;

/**
 * A thread-safe collection decorator that allows many readers but only one writer to access a collection at a time. For operations that iterate over live
 * collection data, a read or write lock should be acquired before the call to acquire the data and held until the data is consumed.
 * @param <E> The type of elements in the collection.
 * @author Garret Wilson
 */
public class DecoratorReadWriteLockCollection<E> extends ReadWriteLockDecorator implements ReadWriteLockCollection<E> {

	/** The collection this class decorates. */
	private final Collection<E> collection;

	/** @return The collection this class decorates. */
	protected Collection<E> getCollection() {
		return collection;
	}

	/**
	 * Collection constructor with a default reentrant read/write lock.
	 * @param collection The collection this collection should decorate.
	 * @throws NullPointerException if the provided collection is <code>null</code>.
	 */
	public DecoratorReadWriteLockCollection(final Collection<E> collection) {
		this(collection, new ReentrantReadWriteLock()); //create the collection with a default lock
	}

	/**
	 * Collection and read/write lock constructor.
	 * @param collection The collection this collection should decorate.
	 * @param lock The lock for controlling access to the collection.
	 * @throws NullPointerException if the provided collection and/or lock is <code>null</code>.
	 */
	public DecoratorReadWriteLockCollection(final Collection<E> collection, final ReadWriteLock lock) {
		super(lock); //construct the parent class
		this.collection = requireNonNull(collection, "Collection cannot be null"); //save the collection
	}

	/**
	 * Returns the number of elements in this collection. If this collection contains more than <code>Integer.MAX_VALUE</code> elements, returns
	 * <code>Integer.MAX_VALUE</code>.
	 * 
	 * @return the number of elements in this collection
	 */
	public int size() {
		readLock().lock();
		try {
			return getCollection().size();
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns <code>true</code> if this collection contains no elements.
	 *
	 * @return <code>true</code> if this collection contains no elements
	 */
	public boolean isEmpty() {
		readLock().lock();
		try {
			return getCollection().isEmpty();
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns <code>true</code> if this collection contains the specified element. More formally, returns <code>true</code> if and only if this collection contains at
	 * least one element <code>e</code> such that <code>(o==null ? e==null : o.equals(e))</code>.
	 *
	 * @param o element whose presence in this collection is to be tested.
	 * @return <code>true</code> if this collection contains the specified element
	 * @throws ClassCastException if the type of the specified element is incompatible with this collection (optional).
	 * @throws NullPointerException if the specified element is null and this collection does not support null elements (optional).
	 */
	public boolean contains(Object o) {
		readLock().lock();
		try {
			return getCollection().contains(o);
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns an iterator over the elements in this collection. There are no guarantees concerning the order in which the elements are returned (unless this
	 * collection is an instance of some class that provides a guarantee).
	 * 
	 * @return an <code>Iterator</code> over the elements in this collection
	 */
	public Iterator<E> iterator() {
		readLock().lock();
		try {
			return getCollection().iterator();
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns an array containing all of the elements in this collection. If the collection makes any guarantees as to what order its elements are returned by
	 * its iterator, this method must return the elements in the same order.
	 * <p>
	 *
	 * The returned array will be "safe" in that no references to it are maintained by this collection. (In other words, this method must allocate a new array
	 * even if this collection is backed by an array). The caller is thus free to modify the returned array.
	 * <p>
	 *
	 * This method acts as bridge between array-based and collection-based APIs.
	 *
	 * @return an array containing all of the elements in this collection
	 */
	public Object[] toArray() {
		readLock().lock();
		try {
			return getCollection().toArray();
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Returns an array containing all of the elements in this collection; the runtime type of the returned array is that of the specified array. If the
	 * collection fits in the specified array, it is returned therein. Otherwise, a new array is allocated with the runtime type of the specified array and the
	 * size of this collection.
	 * <p>
	 *
	 * If this collection fits in the specified array with room to spare (i.e., the array has more elements than this collection), the element in the array
	 * immediately following the end of the collection is set to <code>null</code>. This is useful in determining the length of this collection <i>only</i> if the
	 * caller knows that this collection does not contain any <code>null</code> elements.)
	 * <p>
	 *
	 * If this collection makes any guarantees as to what order its elements are returned by its iterator, this method must return the elements in the same order.
	 * <p>
	 *
	 * Like the <code>toArray</code> method, this method acts as bridge between array-based and collection-based APIs. Further, this method allows precise control
	 * over the runtime type of the output array, and may, under certain circumstances, be used to save allocation costs
	 * <p>
	 *
	 * Suppose <code>l</code> is a <code>List</code> known to contain only strings. The following code can be used to dump the list into a newly allocated array of
	 * <code>String</code>:
	 *
	 * <pre>
	 * 
	 * String[] x = (String[])v.toArray(new String[0]);
	 * </pre>
	 * <p>
	 *
	 * Note that <code>toArray(new Object[0])</code> is identical in function to <code>toArray()</code>.
	 *
	 * @param a the array into which the elements of this collection are to be stored, if it is big enough; otherwise, a new array of the same runtime type is
	 *          allocated for this purpose.
	 * @return an array containing the elements of this collection
	 * 
	 * @throws ArrayStoreException the runtime type of the specified array is not a supertype of the runtime type of every element in this collection.
	 * @throws NullPointerException if the specified array is <code>null</code>.
	 */
	public <T> T[] toArray(T[] a) {
		readLock().lock();
		try {
			return getCollection().toArray(a);
		} finally {
			readLock().unlock();
		}
	}

	// Modification Operations

	/**
	 * Ensures that this collection contains the specified element (optional operation). Returns <code>true</code> if this collection changed as a result of the call.
	 * (Returns <code>false</code> if this collection does not permit duplicates and already contains the specified element.)
	 * <p>
	 *
	 * Collections that support this operation may place limitations on what elements may be added to this collection. In particular, some collections will refuse
	 * to add <code>null</code> elements, and others will impose restrictions on the type of elements that may be added. Collection classes should clearly specify in
	 * their documentation any restrictions on what elements may be added.
	 * <p>
	 *
	 * If a collection refuses to add a particular element for any reason other than that it already contains the element, it <i>must</i> throw an exception
	 * (rather than returning <code>false</code>). This preserves the invariant that a collection always contains the specified element after this call returns.
	 *
	 * @param o element whose presence in this collection is to be ensured.
	 * @return <code>true</code> if this collection changed as a result of the call
	 * 
	 * @throws UnsupportedOperationException <code>add</code> is not supported by this collection.
	 * @throws ClassCastException class of the specified element prevents it from being added to this collection.
	 * @throws NullPointerException if the specified element is null and this collection does not support null elements.
	 * @throws IllegalArgumentException some aspect of this element prevents it from being added to this collection.
	 */
	public boolean add(E o) {
		writeLock().lock();
		try {
			return getCollection().add(o);
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * Removes a single instance of the specified element from this collection, if it is present (optional operation). More formally, removes an element
	 * <code>e</code> such that <code>(o==null ?  e==null :
	 * o.equals(e))</code>, if this collection contains one or more such elements. Returns true if this collection contained the specified element (or equivalently,
	 * if this collection changed as a result of the call).
	 *
	 * @param o element to be removed from this collection, if present.
	 * @return <code>true</code> if this collection changed as a result of the call
	 * 
	 * @throws ClassCastException if the type of the specified element is incompatible with this collection (optional).
	 * @throws NullPointerException if the specified element is null and this collection does not support null elements (optional).
	 * @throws UnsupportedOperationException remove is not supported by this collection.
	 */
	public boolean remove(Object o) {
		writeLock().lock();
		try {
			return getCollection().remove(o);
		} finally {
			writeLock().unlock();
		}
	}

	// Bulk Operations

	/**
	 * Returns <code>true</code> if this collection contains all of the elements in the specified collection.
	 *
	 * @param c collection to be checked for containment in this collection.
	 * @return <code>true</code> if this collection contains all of the elements in the specified collection
	 * @throws ClassCastException if the types of one or more elements in the specified collection are incompatible with this collection (optional).
	 * @throws NullPointerException if the specified collection contains one or more null elements and this collection does not support null elements (optional).
	 * @throws NullPointerException if the specified collection is <code>null</code>.
	 * @see #contains(Object)
	 */
	public boolean containsAll(Collection<?> c) {
		readLock().lock();
		try {
			return getCollection().containsAll(c);
		} finally {
			readLock().unlock();
		}
	}

	/**
	 * Adds all of the elements in the specified collection to this collection (optional operation). The behavior of this operation is undefined if the specified
	 * collection is modified while the operation is in progress. (This implies that the behavior of this call is undefined if the specified collection is this
	 * collection, and this collection is nonempty.)
	 *
	 * @param c elements to be inserted into this collection.
	 * @return <code>true</code> if this collection changed as a result of the call
	 * 
	 * @throws UnsupportedOperationException if this collection does not support the <code>addAll</code> method.
	 * @throws ClassCastException if the class of an element of the specified collection prevents it from being added to this collection.
	 * @throws NullPointerException if the specified collection contains one or more null elements and this collection does not support null elements, or if the
	 *           specified collection is <code>null</code>.
	 * @throws IllegalArgumentException some aspect of an element of the specified collection prevents it from being added to this collection.
	 * @see #add(Object)
	 */
	public boolean addAll(Collection<? extends E> c) {
		writeLock().lock();
		try {
			return getCollection().addAll(c);
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * 
	 * Removes all this collection's elements that are also contained in the specified collection (optional operation). After this call returns, this collection
	 * will contain no elements in common with the specified collection.
	 *
	 * @param c elements to be removed from this collection.
	 * @return <code>true</code> if this collection changed as a result of the call
	 * 
	 * @throws UnsupportedOperationException if the <code>removeAll</code> method is not supported by this collection.
	 * @throws ClassCastException if the types of one or more elements in this collection are incompatible with the specified collection (optional).
	 * @throws NullPointerException if this collection contains one or more null elements and the specified collection does not support null elements (optional).
	 * @throws NullPointerException if the specified collection is <code>null</code>.
	 * @see #remove(Object)
	 * @see #contains(Object)
	 */
	public boolean removeAll(Collection<?> c) {
		writeLock().lock();
		try {
			return getCollection().removeAll(c);
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * Retains only the elements in this collection that are contained in the specified collection (optional operation). In other words, removes from this
	 * collection all of its elements that are not contained in the specified collection.
	 *
	 * @param c elements to be retained in this collection.
	 * @return <code>true</code> if this collection changed as a result of the call
	 * 
	 * @throws UnsupportedOperationException if the <code>retainAll</code> method is not supported by this Collection.
	 * @throws ClassCastException if the types of one or more elements in this collection are incompatible with the specified collection (optional).
	 * @throws NullPointerException if this collection contains one or more null elements and the specified collection does not support null elements (optional).
	 * @throws NullPointerException if the specified collection is <code>null</code>.
	 * @see #remove(Object)
	 * @see #contains(Object)
	 */
	public boolean retainAll(Collection<?> c) {
		writeLock().lock();
		try {
			return getCollection().retainAll(c);
		} finally {
			writeLock().unlock();
		}
	}

	/**
	 * Removes all of the elements from this collection (optional operation). This collection will be empty after this method returns unless it throws an
	 * exception.
	 *
	 * @throws UnsupportedOperationException if the <code>clear</code> method is not supported by this collection.
	 */
	public void clear() {
		writeLock().lock();
		try {
			getCollection().clear();
		} finally {
			writeLock().unlock();
		}
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
		readLock().lock();
		try {
			return getCollection().equals(o);
		} finally {
			readLock().unlock();
		}
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
		readLock().lock();
		try {
			return getCollection().hashCode();
		} finally {
			readLock().unlock();
		}
	}

}
