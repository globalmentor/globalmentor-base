package com.garretwilson.lang;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import static com.garretwilson.lang.ClassUtilities.*;

/**Various utilities to manipulate Java objects.
@author Garret Wilson
*/
public class ObjectUtilities
{
	/**This class cannot be publicly instantiated.*/
	private ObjectUtilities() {}

	/**Checks to see if a given variable is an instance of any object, and throws a {@link NullPointerException} if the variable is <code>null</code>.
	@param <T> The type of variable to check.
	@param variable The variable to check.
	@return The given variable.
	@exception NullPointerException if the given variable is <code>null</code>.
	*/
	public static <T> T checkInstance(final T variable)
	{
		return checkInstance(variable, null);	//check for null with no description
	}
	
	/**Checks to see if a given variable is an instance of any object, and throws a {@link NullPointerException} if the variable is <code>null</code>.
	@param <T> The type of variable to check.
	@param variable The variable to check.
	@param description A description of the variable to be used when generating an exception,
		or <code>null</code> for no description.
	@return The given variable.
	@exception NullPointerException if the given variable is <code>null</code>.
	*/
	public static <T> T checkInstance(final T variable, final String description)
	{
		if(variable==null)	//if the variable is null
		{
			throw new NullPointerException(description);
		}
		return variable;	//return the variable
	}

	/**Checks to see if a given variable is of the correct type and if not, throws a <code>ClassCastException</code>.
	@param <T> The type of variable to check.
	@param variable The variable to check, or <code>null</code>.
	@param type The type to verify.
	@return The given variable.
	@exception ClassCastException if the given variable is not <code>null</code> and not an instance of type <var>type</var>.
	*/
	public static <T> T checkType(final Object variable, final Class<T> type)
	{
		return checkType(variable, type, null);	//check for type with no description
	}
	
	/**Checks to see if a given variable is of the correct type and if not, throws a <code>ClassCastException</code>.
	@param <T> The type of variable to check.
	@param variable The variable to check, or <code>null</code>.
	@param type The type to verify.
	@param description A description of the variable to be used when generating an exception, or <code>null</code> for no description.
	@return The given variable.
	@exception ClassCastException if the given variable is not <code>null</code> and not an instance of type <var>type</var>.
	*/
	@SuppressWarnings("unchecked")
	public static <T> T checkType(final Object variable, final Class<T> type, final String description)
	{
		if(variable!=null && !type.isInstance(variable))	//if the variable is not null but is of a different type
		{
			throw new ClassCastException(description);
		}
		return (T)variable;	//return the variable
	}

	/**Compares two objects for order, taking into account <code>null</code>.
	If both objects are <code>null</code> they are considered equal.
	If only one object is <code>null</code>, comparison will be performed based upon whether <code>null</code> is considered higher or lower.
	Otherwise, the second object is compared to the first using the first object's {@link Comparable#compareTo(Object)} method.
	@param object1 The first object to be compared.
	@param object2 The second object to be compared.
	@param nullBias A negative or positive integer indicating if <code>null</code> should be considered less than or greater than non-<code>null</code> objects.
	@return A negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	*/
	public static <T extends Comparable<T>> int compare(final T object1, final T object2, final int nullBias)
	{
		if(object1==null)	//if the first object is null
		{
			return object2==null ? 0 : nullBias;	//if both objects are null, they are considered equal; otherwise, send back whatever null should be
		}
		else	//if the first object is not null
		{
			return object2!=null ? object1.compareTo(object2) : -nullBias;	//if both objects are non-null, they can be compared; otherwise, the second object is the only one null, and gets the negation of the null bias
		}		
	}

	/**Convenience method that returns the given object if and only if it is an
		instance of the given class. This method is equivalent to
		<code><var>object</var> instanceof <var>Type</var> ? (Type)object : null</code>. 
	@param <T> The type of object to check for.
	@param object The object to examine.
	@param instanceClass The class of which the object may be an instance.
	@return The object if it is an instance of the given class, otherwise
		<code>null</code>.
	 */
	public static <T> T asInstance(final Object object, final Class<T> instanceClass)
	{
		return instanceClass.isInstance(object) ? instanceClass.cast(object) : null;	//cast and return the object if it is an instance of the class, otherwise null
	}

	/**Compares two object to make sure that the objects are equal, or the
		objects are both set to <code>null</code>. If the first object is not
		<code>null</code>, it is compared to the second using the first object's
		<code>equal()</code> method.
		This is a convenience method to compare two objects using the
		<code>equals()</code> method when it's not known if one of the objects
		is <code>null</code>.
	@param object1 The first object to compare.
	@param object2 The second object to compare.
	@return <code>true</code> if the objects are equal according to the first
		object's <code>equal()</code> method or if both objects are
		<code>null</code>.
	@see Object#equals
	*/
	public final static boolean equals(final Object object1, final Object object2)
	{
			//if the first object isn't null, compare it to the second; otherwise, see if the second object is null as well
		return object1!=null ? object1.equals(object2) : object2==null;
	}

	/**Returns the property of an object based upon a given property name.
	A property getter in the form "get<var>PropertyName</var>" takes precedence over a property getter in the form "is<var>PropertyName</var>" having a {@link Boolean#TYPE}. 
	@param object The object the property of which to retrieve.
	@param propertyName The name of the property to retrieve.
	@return The value of the given property.
	@exception NullPointerException if the given object is <code>null</code>.
	@exception NoSuchMethodException if the given object has no method with the name "get<var>PropertyName</var>", or the name "is<var>PropertyName</var>" having a {@link Boolean#TYPE}.
	@exception IllegalAccessException if the getter method enforces Java language access control and the getter method is inaccessible.
	@exception InvocationTargetException if the getter method throws an exception.
	@exception ExceptionInInitializerError if the initialization provoked by the getter method fails.
	*/
	public static Object getProperty(final Object object, final String propertyName) throws NoSuchMethodException, InvocationTargetException, IllegalAccessException
	{
		final Method getterMethod=getGetterMethod(object.getClass(), propertyName);	//get the getter property, if there is one
		if(getterMethod!=null)	//if there is a getter method
		{
			return getterMethod.invoke(object);	//invoke the getter method and return the value
		}
		else	//if there is no getter method
		{
			throw new NoSuchMethodException("Object with class "+object.getClass()+" has no getter method for property "+propertyName);
		}
	}

	/**Generates a hash code based upon a series of objects.
	This method is based upon the algorithm explained in <cite>Effective Java</cite> (2001) by Joshua Bloch (pp. 38-39) as well as the hash code generation of the Java runtime library.
	Any or all objects can be <code>null</code>.
	This methods delegates to {@link #hashCode(int, Object...)} with a seed value of 17. 
	@param objects The objects to be used in generating a hash code.
	@return A hash code taking into account the given objects.
	@exception NullPointerException if the given array of objects is <code>null</code>.
	*/
	public static int hashCode(final Object... objects)
	{
		return hashCode(17, objects);	//generate a hash code with a particular seed
	}

	/**Generates a hash code based upon a seed and a series of objects.
	This method is based upon the algorithm explained in <cite>Effective Java</cite> (2001) by Joshua Bloch (pp. 38-39) as well as the hash code generation of the Java runtime library.
	Any or all objects can be <code>null</code>.
	@param seed The seed with which to start.
	@param objects The objects to be used in generating a hash code.
	@return A hash code starting with the given seed and taking into account the given objects.
	@exception NullPointerException if the given array of objects is <code>null</code>.
	*/
	public static int hashCode(int seed, final Object... objects)
	{
		for(final Object object:objects)	//for each object
		{
			final int hashCode=object!=null ? object.hashCode() : 0;	//get the object's hash code, or zero if the object is null
			seed=37*seed+hashCode;	//multiply by 37 and add the hash code of this object
		}
		return seed;	//return the entire result
	}

	/**Returns the string representation of the object or "null".
	@param object An object to be represented by a string.
	@return The string representation of the object or "null" if the object is
		<code>null</code>.
	*/
	public final static String toString(final Object object)
	{
		return object!=null ? object.toString() : "null"; //return the object's string representation or "null" G***use a constant here
	}

}