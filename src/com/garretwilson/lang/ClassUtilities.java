package com.garretwilson.lang;

import java.lang.reflect.*;

/**Utilities for manipulating Java classes.
@author Garret Wilson
*/
public class ClassUtilities implements JavaConstants
{

	/**This class cannot be publicly instantiated.*/
	private ClassUtilities() {}

	/**Returns the package name of the given class. For example, passing
		<code>String.class</code> will return "java.lang".
		<p>This method is redundant, returning the same value as
		<code>Class.getPackage().getName()</code>, but is necessary to work around
		SavaJe OS 1.1 returning <code>null</code> for <code>Class.getPackage()</code>.</p>
		<p>Currently the package names returned for internal classes will not be
		correct.</p>
	@param objectClass The class for which the Java package will be determined.
	@return A string representing the package name of the given class.
	*/
	public static String getPackageName(final Class objectClass)
	{
		final String className=objectClass.getName(); //get the class name
		final int classNameDividerIndex=className.lastIndexOf('.'); //G***testnig
		return classNameDividerIndex>=0 ? className.substring(0, classNameDividerIndex) : className;  //G***testing
	}

	/**Convenience function to locate and return the public default constructor of
		a particular class. This differs from <code>Class.getConstructor()</code> in
		that this method returns <code>null</code> instead of throwing an exception
		if the given constructor is not found.
		<p>An equivalent call with more exception-handling overhead would be to
		enclose <code>objectClass.getConstructor(new Class()[])</code> in a
		<code>try...catch()</code> block.</p>
	@param objectClass The class for which the default constructor should be found.
	@return The default constructor of the given class, or <code>null</code> if
		a default constructor does not exist.
	@exception SecurityException Thrown if access to the information is denied.
	@see Class#getConstructors
	*/
	public static Constructor getPublicDefaultConstructor(final Class objectClass) throws SecurityException
	{
		final Constructor[] constructors=objectClass.getConstructors(); //look at each constructor
		for(int i=constructors.length-1; i>=0; --i) //look at each constructor
		{
			final Constructor constructor=constructors[i];  //get a reference to this constructor
				//if this constructor has no parameters and is public
			if(constructor.getParameterTypes().length==0 && Modifier.isPublic(constructor.getModifiers()))
				return constructor; //we found the default constructor
		}
		return null;  //show that we could not find a default constructor
	}

	/**Returns the local name of the class, with the package name removed.
		Therefore <code>com.garretwilson.Foo$Bar</code> becomes <code>Foo$Bar</code>.
	@param objectClass The class for which a local name should be returned.
	@return The local name of the class within its package.
	@see #getSimpleName
	*/
	public static String getLocalName(final Class objectClass)
	{
			//return the class name, with everything before the last package separator removed
		return StringUtilities.removeBeforeLast(objectClass.getName(), PACKAGE_SEPARATOR);
	}

	/**Returns the local name of the class, with the package name removed. If
		the class represents an internal class, the external class name is removed
		as well. Therefore <code>com.garretwilson.Foo$Bar</code> becomes
		<code>Bar</code>.
	@param objectClass The class for which a simple name should be returned.
	@return The simple name of the class within its package, if any, and within
		its enclosing class, if any.
	@see #getLocalName
	*/
	public static String getSimpleName(final Class objectClass)
	{
			//return the local name, with everything before the last internal class separator removed
		return StringUtilities.removeBeforeLast(getLocalName(objectClass), INTERNAL_CLASS_SEPARATOR);
	}

}