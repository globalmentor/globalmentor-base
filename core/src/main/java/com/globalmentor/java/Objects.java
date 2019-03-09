/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.lang.reflect.*;
import java.util.Optional;

import static java.util.Objects.*;

import static com.globalmentor.java.Classes.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Java.*;

/**
 * Various utilities to manipulate Java objects.
 * @author Garret Wilson
 */
public class Objects {

	/** A shared object array that contains no elements. */
	public static final Object[] NO_OBJECTS = new Object[0];

	/** This class cannot be publicly instantiated. */
	private Objects() {
	}

	/**
	 * Checks to see if a given variable is of the correct type and if not, throws a <code>ClassCastException</code>.
	 * @param <T> The type of variable to check.
	 * @param variable The variable to check, or <code>null</code>.
	 * @param type The type to verify.
	 * @return The given variable.
	 * @throws ClassCastException if the given variable is not <code>null</code> and not an instance of type <var>type</var>.
	 */
	public static <T> T checkType(final Object variable, final Class<T> type) {
		return checkType(variable, type, null); //check for type with no description
	}

	/**
	 * Checks to see if a given variable is of the correct type and if not, throws a <code>ClassCastException</code>.
	 * @param <T> The type of variable to check.
	 * @param variable The variable to check, or <code>null</code>.
	 * @param type The type to verify.
	 * @param description A description of the variable to be used when generating an exception, or <code>null</code> for no description.
	 * @return The given variable.
	 * @throws ClassCastException if the given variable is not <code>null</code> and not an instance of type <var>type</var>.
	 */
	@SuppressWarnings("unchecked")
	public static <T> T checkType(final Object variable, final Class<T> type, final String description) {
		if(variable != null && !type.isInstance(variable)) { //if the variable is not null but is of a different type
			throw new ClassCastException(description);
		}
		return (T)variable; //return the variable
	}

	/**
	 * Clones an object that supports cloning.
	 * @param <T> The type of the object.
	 * @param object The object that supports cloning through use of the {@link CloneSupported} interface.
	 * @return The cloned object.
	 * @throws IllegalStateException if the object's {@link CloneSupported#clone()} method throws a {@link CloneNotSupportedException}.
	 * @see CloneSupported#clone()
	 */
	@SuppressWarnings("unchecked")
	public static <T extends CloneSupported> T clone(final T object) {
		try {
			return (T)object.clone();
		} catch(final CloneNotSupportedException cloneNotSupportedException) {
			throw unexpected(cloneNotSupportedException);
		}
	}

	/**
	 * Compares two objects for order, taking into account <code>null</code>. If both objects are <code>null</code> they are considered equal. If only one object
	 * is <code>null</code>, comparison will be performed based upon whether <code>null</code> is considered higher or lower. Otherwise, the second object is
	 * compared to the first using the first object's {@link Comparable#compareTo(Object)} method.
	 * @param <T> The type of the object.
	 * @param object1 The first object to be compared.
	 * @param object2 The second object to be compared.
	 * @param nullBias A negative or positive integer indicating if <code>null</code> should be considered less than or greater than non-<code>null</code>
	 *          objects.
	 * @return A negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	 */
	public static <T extends Comparable<? super T>> int compare(final T object1, final T object2, final int nullBias) {
		if(object1 == null) { //if the first object is null
			return object2 == null ? 0 : nullBias; //if both objects are null, they are considered equal; otherwise, send back whatever null should be
		} else { //if the first object is not null
			return object2 != null ? object1.compareTo(object2) : -nullBias; //if both objects are non-null, they can be compared; otherwise, the second object is the only one null, and gets the negation of the null bias
		}
	}

	/**
	 * Convenience method that returns the given object if and only if it is an instance of the given class. This method is equivalent to
	 * <code><var>object</var> instanceof <var>Type</var> ? Optional.of((Type)object) : Optional.empty();</code>.
	 * @param <T> The type of object to check for.
	 * @param object The object to examine.
	 * @param instanceClass The class of which the object may be an instance.
	 * @return The object if it is an instance of the given class, otherwise <code>null</code>.
	 */
	public static <T> Optional<T> asInstance(final Object object, final Class<T> instanceClass) {
		return instanceClass.isInstance(object) ? Optional.of(instanceClass.cast(object)) : Optional.<T>empty();
	}

	/**
	 * Compares two objects to make sure that the objects are equal, or the objects are both set to <code>null</code>. If the first object is not
	 * <code>null</code>, it is compared to the second using the first object's {@link Object#equals(Object)} method. This is a convenience method to compare two
	 * objects using the {@link Object#equals(Object)} method when it's not known if one of the objects is <code>null</code>.
	 * @param object1 The first object to compare.
	 * @param object2 The second object to compare.
	 * @return <code>true</code> if the objects are equal according to the first object's {@link Object#equals(Object)} method or if both objects are
	 *         <code>null</code>.
	 * @see Object#equals(Object)
	 */
	public static final boolean equals(final Object object1, final Object object2) {
		//if the first object isn't null, compare it to the second; otherwise, see if the second object is null as well
		return object1 != null ? object1.equals(object2) : object2 == null;
	}

	/**
	 * Returns the first object that is an instance of {@link Object} (i.e. that is not <code>null</code>).
	 * @param <T> The type of the objects.
	 * @param objects The objects to investigate.
	 * @return The first object that is not <code>null</code>, or <code>null</code> if all objects are <code>null</code>.
	 */
	@SafeVarargs
	public static <T> T getInstance(final T... objects) {
		for(final T object : objects) { //look at all the references
			if(object != null) { //if the object isn't null (it is faster to just check for null rather than to delegate to the class-based getInstance() version
				return object; //return the object
			}
		}
		return null; //we couldn't find such an instance
	}

	/**
	 * Returns the first object that is an instance of the given object type.
	 * @param <T> The type of the objects.
	 * @param <C> The subtype of the given objects.
	 * @param objectClass The class of the type of object to return
	 * @param objects The objects to investigate.
	 * @return The first object that is the instance of the given type, or <code>null</code> if no object is an instance of the indicated type.
	 * @throws NullPointerException if the given object class is <code>null</code>.
	 */
	@SafeVarargs
	public static <T, C extends T> C getInstance(final Class<C> objectClass, final T... objects) {
		for(final T object : objects) { //look at all the references
			if(objectClass.isInstance(object)) { //if this object is an instance of the given class
				return objectClass.cast(object); //cast and return the object
			}
		}
		return null; //we couldn't find such an instance
	}

	/**
	 * Returns either and object or a default instance if the object is <code>null</code>.
	 * <p>
	 * This is equivalent to the JavaScript statement <code>var x = object || defaultInstance;</code>
	 * </p>
	 * @param <T> The type of the object.
	 * @param object The object to examine.
	 * @param defaultInstance The default instance to return if the object is <code>null</code>.
	 * @return The object, or the default instance of the object is <code>null</code>.
	 * @throws NullPointerException if the given default instance is <code>null</code>.
	 */
	public static <T> T toInstance(final T object, final T defaultInstance) {
		return object != null ? object : requireNonNull(defaultInstance);
	}

	/**
	 * Returns the property of an object based upon a given property name. A property getter in the form "get<var>PropertyName</var>" takes precedence over a
	 * property getter in the form "is<var>PropertyName</var>" having a {@link Boolean#TYPE}.
	 * @param object The object the property of which to retrieve.
	 * @param propertyName The name of the property to retrieve.
	 * @return The value of the given property.
	 * @throws NullPointerException if the given object is <code>null</code>.
	 * @throws NoSuchMethodException if the given object has no method with the name "get<var>PropertyName</var>", or the name "is<var>PropertyName</var>" having
	 *           a {@link Boolean#TYPE}.
	 * @throws IllegalAccessException if the getter method enforces Java language access control and the getter method is inaccessible.
	 * @throws InvocationTargetException if the getter method throws an exception.
	 * @throws ExceptionInInitializerError if the initialization provoked by the getter method fails.
	 */
	public static Object getProperty(final Object object, final String propertyName)
			throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
		final Method getterMethod = getGetterMethod(object.getClass(), propertyName); //get the getter property, if there is one
		if(getterMethod != null) { //if there is a getter method
			return getterMethod.invoke(object); //invoke the getter method and return the value
		} else { //if there is no getter method
			throw new NoSuchMethodException("Object with class " + object.getClass() + " has no getter method for property " + propertyName);
		}
	}

	/**
	 * Generates a hash code based upon a series of objects. This method is based upon the algorithm explained in <cite>Effective Java</cite> (2001) by Joshua
	 * Bloch (pp. 38-39) as well as the hash code generation of the Java runtime library. Any or all objects can be <code>null</code>. Arrays are deeply
	 * traversed. This methods delegates to {@link #getSeededHashCode(int, Object...)} with a seed value of 17.
	 * @param objects The objects to be used in generating a hash code.
	 * @return A hash code taking into account the given objects.
	 * @throws NullPointerException if the given array of objects is <code>null</code>.
	 * @deprecated Use {@link java.util.Objects#hash(Object...)} from Java 7 instead.
	 */
	@Deprecated
	public static int getHashCode(final Object... objects) {
		return getSeededHashCode(17, objects); //generate a hash code with a particular seed
	}

	/**
	 * Generates a hash code based upon a seed and a series of objects. This method is based upon the algorithm explained in <cite>Effective Java</cite> (2001) by
	 * Joshua Bloch (pp. 38-39) as well as the hash code generation of the Java runtime library. Any or all objects can be <code>null</code>. Arrays are deeply
	 * traversed.
	 * @param seed The seed with which to start.
	 * @param objects The objects to be used in generating a hash code.
	 * @return A hash code starting with the given seed and taking into account the given objects.
	 * @throws NullPointerException if the given array of objects is <code>null</code>.
	 */
	@Deprecated
	public static int getSeededHashCode(int seed, final Object... objects) {
		for(final Object object : objects) { //for each object
			final int hashCode = object != null ? (object.getClass().isArray() ? getHashCode((Object[])object) : object.hashCode()) : 0; //get the object's hash code, or zero if the object is null
			seed = 37 * seed + hashCode; //multiply by 37 and add the hash code of this object
		}
		return seed; //return the entire result
	}

	/**
	 * Generates a hash code based upon a series of integer values. This is a convenience varargs method that delegates to the
	 * {@link java.util.Arrays#hashCode(int[])} version.
	 * @param values The values to be used in generating a hash code.
	 * @return A hash code taking into account the given values.
	 * @throws NullPointerException if the given array of values is <code>null</code>.
	 * @see java.util.Arrays#hashCode(int[])
	 */
	@Deprecated
	public static int getIntHashCode(final int... values) {
		return java.util.Arrays.hashCode(values);
	}

	/**
	 * Generates a hash code based upon a series of long values. This is a convenience varargs method that delegates to the
	 * {@link java.util.Arrays#hashCode(long[])} version.
	 * @param values The values to be used in generating a hash code.
	 * @return A hash code taking into account the given values.
	 * @throws NullPointerException if the given array of values is <code>null</code>.
	 * @see java.util.Arrays#hashCode(long[])
	 */
	@Deprecated
	public static int getLongHashCode(final long... values) {
		return java.util.Arrays.hashCode(values);
	}

	/**
	 * Generates a hash code based upon a series of integer values. This is a convenience varargs method that delegates to the
	 * {@link java.util.Arrays#hashCode(double[])} version.
	 * @param values The values to be used in generating a hash code.
	 * @return A hash code taking into account the given values.
	 * @throws NullPointerException if the given array of values is <code>null</code>.
	 * @see java.util.Arrays#hashCode(double[])
	 */
	@Deprecated
	public static int getDoubleHashCode(final double... values) {
		return java.util.Arrays.hashCode(values);
	}

	/**
	 * Determines of which, if any, of the provided classes the given object is an instance.
	 * @param object The object to check as an instance; may be <code>null</code>.
	 * @param classes The classes of which to check the given object being an instance.
	 * @return The first of the listed classes of which the given object is an instance, or <code>null</code> if the object is not an instance of any of the
	 *         listed classes.
	 */
	public static Class<?> getInstanceOf(final Object object, final Class<?>... classes) {
		for(final Class<?> objectClass : classes) {
			if(objectClass.isInstance(object)) {
				return objectClass;
			}
		}
		return null;
	}

	/**
	 * Determines if the object is an instance of any of the provided classes.
	 * @param object The object to check as an instance; may be <code>null</code>.
	 * @param classes The classes of which to check the given object being an instance.
	 * @return <code>true</code> if the object is an instance of one of the listed classes.
	 */
	public static boolean isInstanceOf(final Object object, final Class<?>... classes) {
		return getInstanceOf(object, classes) != null;
	}

	/**
	 * Returns the string representation of the object or {@value Java#NULL_KEYWORD}.
	 * @param object An object to be represented by a string.
	 * @return The string representation of the object or {@value Java#NULL_KEYWORD} if the object is <code>null</code>.
	 */
	public static final String toString(final Object object) {
		return object != null ? object.toString() : NULL_KEYWORD; //return the object's string representation or "null"
	}

}
