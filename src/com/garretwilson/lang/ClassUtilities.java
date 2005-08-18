package com.garretwilson.lang;

import java.lang.reflect.*;

import static com.garretwilson.lang.JavaConstants.*;
import static com.garretwilson.lang.StringUtilities.*;

/**Utilities for manipulating Java classes.
@author Garret Wilson
*/
public class ClassUtilities
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
/*G***del; support for SavaJe isn't currently necessary
	public static String getPackageName(final Class objectClass)
	{
		final String className=objectClass.getName(); //get the class name
		final int classNameDividerIndex=className.lastIndexOf('.'); //G***testnig
		return classNameDividerIndex>=0 ? className.substring(0, classNameDividerIndex) : className;  //G***testing
	}
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

	/**Creates a property name by appending the property local name to the full class name.
	@param objectClass The class to supply the class name.
	@param localName The local name of the property.
	@return A full class name plus property name.
	@see #getFullName(Package, String)
	*/
	public static String getPropertyName(final Class objectClass, final String localName)
	{
		return objectClass.getName()+OBJECT_PREDICATE_SEPARATOR+localName;	//return the class name plus the local name separated by a package separator
	}

	/**Creates a full name analogous to a class name from the package of an
	 	existing class. For example, a class of <code>com.garretwilson.Foo</code>
	 	and a local name of <code>Bar</code> will result in a full name of
	 	<code>com.garretwilson.Bar</code>.
	@param objectClass The class to supply the package name.
	@param localName The local name for constructing the full name within the package.
	@return A full class name in the package of the given class with the given
		local name.
	@see #getFullName(Package, String)
	*/
	public static String getFullName(final Class objectClass, final String localName)
	{
		return getFullName(objectClass.getPackage(), localName);	//return the package plus the name separated by a package separator
	}

	/**Creates a full name given package and a local name.
	 	For example, a package of <code>com.garretwilson</code>
	 	and a local name of <code>Bar</code> will result in a full name of
	 	<code>com.garretwilson.Bar</code>.
	@param objectClass The class to supply the package name.
	@param localName The local name for constructing the full name within the package.
	@return A full class name in the given package and the given local name.
	*/
	public static String getFullName(final Package objectPackage, final String localName)
	{
		return objectPackage.getName()+PACKAGE_SEPARATOR+localName;	//return the package plus the name separated by a package separator
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
		return removeBeforeLast(objectClass.getName(), PACKAGE_SEPARATOR);
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
		return removeBeforeLast(getLocalName(objectClass), INTERNAL_CLASS_SEPARATOR);
	}

	/**Constructs a variable name from the class by by decapitalizing all of the beginning uppercase letters of the simple name of the class.
	@param objectClass The class for which a variable name should be returned.
	@return A variable name appropriate for the class.
	@see #getSimpleName(Class)
	@see JavaUtilities#getVariableName(String)
	*/
	public static String getVariableName(final Class objectClass)
	{
		return JavaUtilities.getVariableName(getSimpleName(objectClass));	//get the variable name form of the simple name of the class
	}

	/**Casts an object to the given class.
	This method is available in JDK 5.0 via {@link Class#cast(java.lang.Object)}, but is provided here for backwards-compatibility using RetroWeaver for example.
	@param <T> The type to which to cast.
	@param objectClass The class to which the object will be cast.
	@param object The object to be cast.
	@return The object after casting, or <code>null</code> if the object is <code>null</code>.
	@exception ClassCastException if the object is not null and is not assignable to the type T.
	*/
	@SuppressWarnings("unchecked")
	public static <T> T cast(final Class<T> objectClass, final Object object)
	{
		if(object==null || objectClass.isInstance(object))	//if the object is null or an instance of the class
		{
			return (T)object;	//cast and return the object
		}
		else	//if the object isn't null and is not an instance of the class
		{
			throw new ClassCastException(objectClass.getName());	//throw an exception
		}
	}

}