/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

import java.lang.reflect.Array;

/**
 * Various utilities for manipulating arrays.
 * @author Garret Wilson
 * @see java.util.Arrays
 */
public class Arrays {

	/** An object array that contains no elements. */
	public static final Object[] EMPTY_OBJECT_ARRAY = new Object[] {};

	/**
	 * Creates a new array and appends the value to the contents of the given array.
	 * @param array The array holding the original values.
	 * @param value The value to append to the array.
	 * @return A new array containing the contents of the first array followed by the contents of the second array.
	 */
	public static int[] append(final int[] array, final int value) {
		return append(array, new int[] { value }); //create an array containing the value and append it to the first array
	}

	/**
	 * Creates a new array and appends the contents of <var>array2</var> to the contents of <var>array1</var>.
	 * @param array1 The first array.
	 * @param array2 The array to append to the first array.
	 * @return A new array containing the contents of the first array followed by the contents of the second array.
	 */
	public static int[] append(final int[] array1, final int[] array2) {
		final int[] array = new int[array1.length + array2.length]; //create an array large enough to hold both arrays
		System.arraycopy(array1, 0, array, 0, array1.length); //copy the first array to the new array
		System.arraycopy(array2, 0, array, array1.length, array2.length); //copy the second array to the new array
		return array; //return the new array we created
	}

	/**
	 * Convenience method that returns the given object if and only if it is an instance of an array of the given class. This method is equivalent to
	 * <code><var>object</var> instanceof <var>Type[]</var> ? (Type[])object : null</code>.
	 * @param <T> The array type of object to check for.
	 * @param object The object to examine.
	 * @param elementClass The class of which elements the object hold if the object is an array instance.
	 * @return The object if it is an instance of an array containing objects of the given class, otherwise <code>null</code>.
	 */
	@SuppressWarnings("unchecked")
	public static <T> T[] asArrayInstance(final Object object, final Class<T> elementClass) {
		return object != null && elementClass.isInstance(object.getClass().getComponentType()) ? (T[])object : null; //if each array element of the object is of the correct type, cast to the correct type of array
	}

	/**
	 * Checks to see if the elements within an array are instances of any object, and throws a {@link NullPointerException} if any element is <code>null</code>.
	 * @param <T> The type of array element to check.
	 * @param array The array the elements of which to check.
	 * @return The given array.
	 * @throws NullPointerException if the given array or any element within the array is <code>null</code>.
	 */
	public static <T> T[] checkInstances(final T[] array) {
		return checkInstances(array, null); //check for null with no description
	}

	/**
	 * Checks to see if a given variable is an instance of any object, and throws a {@link NullPointerException} if the variable is <code>null</code>.
	 * @param <T> The type of array element to check.
	 * @param array The array the elements of which to check.
	 * @param description A description of the element to be used when generating an exception, or <code>null</code> for no description.
	 * @return The given variable.
	 * @throws NullPointerException if the given array or any element within the array is <code>null</code>.
	 */
	public static <T> T[] checkInstances(final T[] array, final String description) {
		for(final T element : array) { //for each element in the array
			if(element == null) { //if the element is null
				throw new NullPointerException(description);
			}
		}
		return array; //return the array
	}

	/**
	 * Checks to make sure that a given index range is within the given sized array.
	 * @param length The length of the array.
	 * @param start The first index in the range.
	 * @param end The index after the last requested position in the array.
	 * @return The length of the checked range.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 */
	public static int checkIndexRange(final int length, int start, int end) {
		final int rangeLength = end - start;
		if(rangeLength < 0) { //if the first index is greater than the last index
			throw new IllegalArgumentException("Start range index " + start + " greater than end range index" + end);
		}
		if(start < 0) { //if the first index is too low
			throw new IllegalArgumentException("Start range index " + start + " cannot be less than zero.");
		}
		if(end > length) { //if the last index is too high
			throw new IllegalArgumentException("End range index " + end + " cannot be greater than length " + length);
		}
		return rangeLength;
	}

	/**
	 * Checks to make sure that a given index range is within the given sized array.
	 * @param array The array against which the range should be checked.
	 * @param start The first index in the range.
	 * @param end The index after the last requested position in the array.
	 * @return The length of the checked range.
	 * @throws NullPointerException if the given array is <code>null</code>.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 */
	public static int checkIndexRange(final Object[] array, int start, int end) {
		return checkIndexRange(array.length, start, end);
	}

	/**
	 * Creates a new integer array with the specified size with content from the original array. Effectively changes the size of the original array by making a
	 * copy. The number of elements copied is equal to the minimum of the size of the original array and the size of the new array.
	 * @param length The size at which the new array should be created.
	 * @param a The array whose content will be copied, or <code>null</code> if there are no values to copy..
	 * @return A new array with the content of the old array at the corresponding indexes.
	 */
	public static int[] create(final int length, final int[] a) {
		final int[] newArray = new int[length]; //create a new array
		if(a != null) { //if an array was provided for content
			final int copyLength = Math.min(a.length, length); //see how many elements we should copy
			System.arraycopy(a, 0, newArray, 0, copyLength); //copy the specified number of elements
		}
		return newArray; //return the new array
	}

	/**
	 * Creates a shallow copy of the given array containing the specified range.
	 * @param source The array to copy.
	 * @param start The offset from which to start copying elements.
	 * @return A new array containing the specified range of elements from the source array.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 */
	public static <T> T[] createCopy(final T[] source, final int start) {
		return createCopy(source, start, source.length);
	}

	/**
	 * Creates a shallow copy of the given array containing the specified range.
	 * @param source The array to copy.
	 * @param start The offset from which to start copying elements.
	 * @param end The index after the last element to copy.
	 * @return A new array containing the specified range of elements from the source array.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 */
	public static <T> T[] createCopy(final T[] source, final int start, final int end) {
		checkIndexRange(source.length, start, end); //check the validity of the given range
		final int length = end - start;
		final T[] destination = createArray(source, length); //create a destination array of the correct length
		System.arraycopy(source, start, destination, 0, length); //copy the specified number of elements to the destination copy
		return destination; //return the new copy
	}

	/**
	 * Creates a copy of the given array containing the specified range.
	 * @param source The array to copy.
	 * @param start The offset from which to start copying elements.
	 * @param end The index after the last element to copy.
	 * @return A new array containing the specified range of elements from the source array.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 */
	public static char[] createCopy(final char[] source, final int start, final int end) {
		checkIndexRange(source.length, start, end); //check the validity of the given range
		final int length = end - start;
		final char[] destination = new char[length]; //create a destination array of the correct length
		System.arraycopy(source, start, destination, 0, length); //copy the specified number of elements to the destination copy
		return destination; //return the new copy
	}

	/**
	 * Creates a copy of the given array containing the specified range.
	 * @param source The array to copy.
	 * @param start The offset from which to start copying elements.
	 * @param end The number of elements to copy.
	 * @return A new array containing the specified range of elements from the source array.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 */
	public static int[] createCopy(final int[] source, final int start, final int end) {
		checkIndexRange(source.length, start, end); //check the validity of the given range
		final int length = end - start;
		final int[] destination = new int[length]; //create a destination array of the correct length
		System.arraycopy(source, start, destination, 0, length); //copy the specified number of elements to the destination copy
		return destination; //return the new copy
	}

	/**
	 * Returns a new array containing the given elements. The returned array is identical to the implicit array passed into this method.
	 * @param <T> The type of elements contained in the array.
	 * @param elements The elements the array should contain.
	 * @return An array containing the provided elements.
	 */
	public static <T> T[] createArray(final T... elements) {
		return elements; //return the varargs as an array
	}

	/**
	 * Creates an array of the given type and initializes it with the given elements.
	 * @param <T> The type of elements contained in the array.
	 * @param elementType A class instance indicating the type of elements to use in the array.
	 * @param elements The elements with which to initialize the array.
	 * @return A new array of the requested type containing the provided elements.
	 */
	public static <T> T[] createArray(final Class<T> elementType, final T... elements) {
		final T[] array = createArray(elementType, elements.length); //create an array of the given type large enough to hold the given elements
		for(int i = elements.length - 1; i >= 0; --i) { //for each element in the array
			array[i] = elements[i]; //copy the element to the new array
		}
		return array; //return the created array
	}

	/**
	 * Creates an array of the specified length containing elements of the given type.
	 * @param <T> The type of elements contained in the array.
	 * @param elementType A class instance indicating the type of elements to use in the array.
	 * @param length The requested length of the created array.
	 * @return A new array of the requested length containing components of the requested type.
	 */
	@SuppressWarnings("unchecked")
	public static <T> T[] createArray(final Class<T> elementType, final int length) {
		return (T[])Array.newInstance(elementType, length); //create a new array of the specified length to contain the given type
	}

	/**
	 * Creates an array of the specified length containing elements of the given type and fills the array with the given value.
	 * @param <T> The type of elements contained in the array.
	 * @param elementType A class instance indicating the type of elements to use in the array.
	 * @param length The requested length of the created array.
	 * @param value The value to be stored in all elements of the array.
	 * @return A new array of the requested length containing components of the requested type, filled with the given value.
	 * @see #fill(Object[], Object)
	 */
	public static <T> T[] createArray(final Class<T> elementType, final int length, final T value) {
		final T[] array = createArray(elementType, length); //create an array of the appropriate length
		fill(array, value); //fill the array
		return array; //return the array we created
	}

	/**
	 * Creates an array of the specified length containing the same types of components as does the provided array.
	 * @param <T> The type of elements contained in the array.
	 * @param array The template array indicating the types of components.
	 * @param length The requested length of the created array.
	 * @return A new array of the requested length containing the same components as the original array.
	 */
	@SuppressWarnings("unchecked")
	public static <T> T[] createArray(final T[] array, final int length) {
		return (T[])createArray(array.getClass().getComponentType(), length); //create a new array based upon the component type of the template array
	}

	/**
	 * Returns an array with at least given length. If the given array already has at least the minimum number of elements, it is returned. Otherwise, a new array
	 * is created containing the same types of components as does the provided array.
	 * <p>
	 * Array elements will not be copied if a new array is created.
	 * </p>
	 * @param <T> The type of elements contained in the array.
	 * @param array The template array indicating the types of components.
	 * @param minLength The minimum length of the returned array.
	 * @return An array of at least the requested length containing the same components as the original array; either the original array or a new array.
	 * @see #createArray(Object[], int)
	 */
	public static <T> T[] getArray(T[] array, final int minLength) {
		if(array.length < minLength) { //if the given array is not large enough
			array = createArray(array, minLength); //create a new array that is large enough
		}
		return array; //return the array
	}

	/**
	 * Fills the given array with the specified value and returns the array.
	 * @param array The array to be filled.
	 * @param value The value to be stored in all elements of the array.
	 * @return The array that was filled.
	 */
	public static <T> T[] fill(final T[] array, final T value) {
		return fill(array, 0, array.length, value); //fill the entire range of the array
	}

	/**
	 * Assigns the specified value to each element of the specified range of the specified array. The range to be filled extends from index <code>start</code> ,
	 * inclusive, to index <code>start</code>, exclusive.
	 * @param array The array to be filled.
	 * @param start The index of the first element (inclusive) to be filled with the specified value.
	 * @param end The index of the last element (exclusive) to be filled with the specified value.
	 * @param value The value to be stored in all elements of the array.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 * @return The array that was filled.
	 */
	public static <T> T[] fill(final T[] array, final int start, final int end, T value) {
		checkIndexRange(array.length, start, end); //check the given range
		for(int i = start; i < end; array[i++] = value)
			; //store the value in each index of the array
		return array; //return the array
	}

	/**
	 * Fills the given array with the specified value and returns the array.
	 * @param array The array to be filled.
	 * @param value The value to be stored in all elements of the array.
	 * @return The array that was filled.
	 */
	public static double[] fill(final double[] array, final double value) {
		return fill(array, 0, array.length, value); //fill the entire range of the array
	}

	/**
	 * Assigns the specified value to each element of the specified range of the specified array. The range to be filled extends from index <code>start</code> ,
	 * inclusive, to index <code>end</code>, exclusive.
	 * @param array The array to be filled..
	 * @param start The index of the first element (inclusive) to be filled with the specified value.
	 * @param end The index of the last element (exclusive) to be filled with the specified value.
	 * @param value The value to be stored in all elements of the array.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 * @return The array that was filled.
	 */
	public static double[] fill(final double[] array, final int start, final int end, double value) {
		checkIndexRange(array.length, start, end); //check the given range
		for(int i = start; i < end; array[i++] = value)
			; //store the value in each index of the array
		return array; //return the array
	}

	/**
	 * Performs a simple sequential search for the object in the given array, using the object's <code>equals()</code> operator.
	 * @param <T> The type of object contained in the array.
	 * @param array The array to search.
	 * @param object The object to find.
	 * @return <code>true</code> if the object exists in the array, else <code>false</code>.
	 */
	public static <T> boolean contains(final T[] array, final T object) {
		return indexOf(array, object) >= 0; //see if the given object appears in the array
	}

	/**
	 * Performs a simple sequential search for the object in the given array, using the object's <code>equals()</code> operator.
	 * @param <T> The type of object contained in the array.
	 * @param array The array to search.
	 * @param object The object to find.
	 * @return The index of the first instance of an object that is equal to the specified object, or -1 if no such object exits.
	 */
	public static <T> int indexOf(final T[] array, final T object) {
		for(int i = 0; i < array.length; ++i) { //look at each object in the array
			if(object.equals(array[i])) //if the object equals this element of the array
				return i; //return this index
		}
		return -1; //show that we could not find the object in the array
	}

	/**
	 * Finds and returns the first instance of the given class in the array.
	 * @param <T> The type of object to return.
	 * @param array The array to search.
	 * @param objectClass The class of object to return.
	 * @return The first object that is the instance of the given class, or <code>null</code> if there is no instance of the given class in the array.
	 * @throws NullPointerException if the given array and/or object class is <code>null</code>.
	 */
	public static <T> T getInstance(final Object[] array, final Class<T> objectClass) {
		for(final Object object : array) { //look at each object in the array
			if(objectClass.isInstance(object)) { //if this object is an instance of the given class
				return objectClass.cast(object); //return the object
			}
		}
		return null; //an instance of the given class could not be found
	}

	/**
	 * Counts the number of non-<code>null</code> objects in an array.
	 * @param <T> The type of object contained in the array.
	 * @param array The array to iteratate.
	 * @return The number of objects in the array that are not <code>null</code>.
	 */
	public static <T> int getInstanceCount(final T[] array) {
		int count = 0; //we'll see how many instances are in the row
		for(int i = array.length - 1; i >= 0; --i) { //for each index
			if(array[i] != null) { //if there is an object at this position
				++count; //increase the count of instances
			}
		}
		return count; //return the number of non-null objects
	}

	/**
	 * Performs a simple sequential search for the value in the given array.
	 * @param array The array to search.
	 * @param value The value to find.
	 * @return The index of the first instance of an value, or -1 if no such value exits.
	 */
	public static <T> boolean contains(final char[] array, final char value) {
		return indexOf(array, value) >= 0; //see if the given value appears in the array
	}

	/**
	 * Performs a simple sequential search for the value in the given array.
	 * @param array The array to search.
	 * @param value The value to find.
	 * @return <code>true</code> if the value exists in the array, else <code>false</code>.
	 */
	public static int indexOf(final char[] array, final char value) {
		for(int i = 0; i < array.length; ++i) { //look at each object in the array
			if(value == array[i]) //if the value equals this element of the array
				return i; //return this index
		}
		return -1; //show that we could not find the value in the array
	}

	/**
	 * Performs a simple sequential search for the value in the given array.
	 * @param array The array to search.
	 * @param value The value to find.
	 * @return The index of the first instance of an value, or -1 if no such value exits.
	 */
	public static <T> boolean contains(final int[] array, final int value) {
		return indexOf(array, value) >= 0; //see if the given value appears in the array
	}

	/**
	 * Performs a simple sequential search for the value in the given array.
	 * @param array The array to search.
	 * @param value The value to find.
	 * @return <code>true</code> if the value exists in the array, else <code>false</code>.
	 */
	public static int indexOf(final int[] array, final int value) {
		for(int i = 0; i < array.length; ++i) { //look at each object in the array
			if(value == array[i]) //if the value equals this element of the array
				return i; //return this index
		}
		return -1; //show that we could not find the value in the array
	}

	/**
	 * Creates an array of the string representations of all non-<code>null</code> objects in the given array.
	 * @param array The array to convert to a string array.
	 * @return A string array, each element of which contains the string version of the corresponding object array element or <code>null</code> if the
	 *         corresponding object is <code>null</code>.
	 * @throws NullPointerException if the given array is <code>null</code>.
	 */
	public static String[] toStringArray(final Object[] array) {
		final int length = array.length; //get the length of the array
		final String[] strings = new String[length]; //create a string array
		for(int i = 0; i < length; ++i) { //for each index
			final Object object = array[i]; //get this object
			strings[i] = object != null ? object.toString() : null; //store the object or null in the new array
		}
		return strings; //return the new array of strings
	}

}