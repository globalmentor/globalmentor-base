package com.garretwilson.lang;

/**Various utilities to manipulate Java objects.
@author Garret Wilson
*/
public class ObjectUtilities
{
	/**This class cannot be publicly instantiated.*/
	private ObjectUtilities() {}

	/**Convenience method that returns the given object if and only if it is an
		instance of the given class. This method is equivalent to
		<code><var>object</var> instanceof <var>instanceClass</var> ? object : null</code>. 
	@param object The object to examine.
	@param instanceClass The class of which the object may be an instance.
	@return The object if it is an instance of the given class, otherwise
		<code>null</code>.
	 */
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