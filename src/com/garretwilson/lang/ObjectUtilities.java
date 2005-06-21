package com.garretwilson.lang;

/**Various utilities to manipulate Java objects.
@author Garret Wilson
*/
public class ObjectUtilities
{
	/**This class cannot be publicly instantiated.*/
	private ObjectUtilities() {}


	/**Checks to see if a given variable is <code>null</code> and if so,
	 	throws a <code>NullPointerException</code>.
	@param <T> The type of variable to check.
	@param variable The variable to check.
	@return The given variable.
	@exception NullPointerException if the given variable is <code>null</code>.
	*/
	public static <T> T checkNull(final T variable)
	{
		return checkNull(variable, null);	//check for null with no description
	}
	
	/**Checks to see if a given variable is <code>null</code> and if so,
	 	throws a <code>NullPointerException</code>.
	@param <T> The type of variable to check.
	@param variable The variable to check.
	@param description A description of the variable to be used when generating an exception,
		or <code>null</code> for no description.
	@return The given variable.
	@exception NullPointerException if the given variable is <code>null</code>.
	*/
	public static <T> T checkNull(final T variable, final String description)
	{
		if(variable==null)	//if the variable is null
		{
			throw new NullPointerException(description);
		}
		return variable;	//return the variable
	}

	/**Checks to see if a given variable is of the correct type and if not,
	 	throws a <code>ClassCastException</code>.
	@param <T> The type of variable to check.
	@param variable The variable to check.
	@param type The type to verify.
	@return The given variable.
	@exception ClassCastException if the given variable is not <code>null</code> and not an instance of type <var>type</var>.
	*/
	public static <T> T checkType(final Object variable, final Class<T> type)
	{
		return checkType(variable, type, null);	//check for type with no description
	}
	
	/**Checks to see if a given variable is of the correct type and if not,
	 	throws a <code>NullPointerException</code>.
	 	throws a <code>ClassCastException</code>.
	@param <T> The type of variable to check.
	@param variable The variable to check.
	@param type The type to verify.
	@param description A description of the variable to be used when generating an exception,
		or <code>null</code> for no description.
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

	/**Convenience method that returns the given object if and only if it is an
		instance of the given class. This method is equivalent to
		<code><var>object</var> instanceof <var>instanceClass</var> ? object : null</code>. 
	@param object The object to examine.
	@param instanceClass The class of which the object may be an instance.
	@return The object if it is an instance of the given class, otherwise
		<code>null</code>.
	 */
	@SuppressWarnings("unchecked")
	public static <T> T asInstance(final Object object, final Class<T> instanceClass)
	{
		return instanceClass.isInstance(object) ? (T)object : null;	//return the object if it is an instance of the class, otherwise null
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