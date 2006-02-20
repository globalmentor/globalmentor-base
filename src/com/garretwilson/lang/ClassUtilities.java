package com.garretwilson.lang;

import java.lang.reflect.*;
import java.util.*;

import static com.garretwilson.lang.JavaConstants.*;
import static com.garretwilson.lang.JavaUtilities.*;
import static com.garretwilson.lang.StringUtilities.*;

/**Utilities for manipulating Java classes.
@author Garret Wilson
*/
public class ClassUtilities
{

	/**This class cannot be publicly instantiated.*/
	private ClassUtilities() {}

  /**Finds a defined constructor of a class.
	This method differs from {@link Class#getConstructor} in that if no matching constructor is found, <code>null</code> is returned rather than an exception being thrown.
	@param objectClass The class for which the constructor should be found.
	@param parameterTypes The constructor parameters.
	@return The <code>Method</code> object of the public constructor that matches the specified <code>parameterTypes</code>, or <code>null</code> if no such constructor exists.
	@exception SecurityException If a security manager, <em>s</em>, is present and any of the following conditions is met:
  <ul>
		<li>Invocation of <code>{@link SecurityManager#checkMemberAccess s.checkMemberAccess(this, Member.PUBLIC)}</code> denies access to the constructor.</li>
		<li>The caller's class loader is not the same as or an ancestor of the class loader for the current class and invocation of <code>{@link SecurityManager#checkPackageAccess s.checkPackageAccess()}</code> denies access to the package of this class.</li>
	</ul>
	*/
  public static <T> Constructor<T> getConstructor(final Class<T> objectClass, final Class ... parameterTypes) throws SecurityException
  {
  	try
		{
			return objectClass.getConstructor(parameterTypes);	//ask the class for the constructor
		}
  	catch(final NoSuchMethodException noSuchMethodException)	//if the constructor isn't found
		{
  		return null;	//indicate that the constructor couldn't be found
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

	/**Returns the getter method of a given class.
	@param objectClass The class for which a getter method should be returned.
	@param propertyName The property name, such as "propertyName".
	@return The method with the name "get<var>PropertyName</var>".
	@throws NoSuchMethodException if a matching method is not found or if the name is "<init>"or "<clinit>".
	*/
	public static Method getGetterMethod(final Class objectClass, final String propertyName) throws NoSuchMethodException
	{
		return objectClass.getMethod(getGetterMethodName(propertyName));	//return the getter method, if there is one
	}

	/**Returns the setter method of a given class.
	@param objectClass The class for which a setter method should be returned.
	@param propertyName The property name, such as "propertyName".
	@param valueClass The type of property value to be set. 
	@return The method with the name "set<var>PropertyName</var>" and the given value class as a parameter type.
	@throws NoSuchMethodException if a matching method is not found or if the name is "<init>"or "<clinit>".
	*/
	public static Method getSetterMethod(final Class objectClass, final String propertyName, final Class valueClass) throws NoSuchMethodException
	{
		return objectClass.getMethod(getSetterMethodName(propertyName), valueClass);	//return the setter method, if there is one
	}

	/**The name of the getter method corresponding to the given property.
	@param propertyName The property name, such as "propertyName".
	@return The name of the getter method in the form "get<var>PropertyName</var>".
	*/
	public static String getGetterMethodName(final String propertyName)
	{
		return "get"+getProperName(propertyName);	//return "getPropertyName" TODO use a constant
	}

	/**The name of the setter method corresponding to the given property.
	@param propertyName The property name, such as "propertyName".
	@return The name of the setter method in the form "set<var>PropertyName</var>".
	*/
	public static String getSetterMethodName(final String propertyName)
	{
		return "set"+getProperName(propertyName);	//return "setPropertyName" TODO use a constant
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

	/**Determines all super classes and interfaces of the given class.
	@param objectClass The class for which super classes and interfaces should be found.
	@param rootClass The root class or interface to retrieve, or <code>null</code> if all classes should be retrieved.
	@param includeSuperClasses Whether super classes should be returned.
	@param includeAbstract Whether abstract classes should be returned.
	@param includeInterfaces Whether implemented interfaces should be returned.
	@return The set of all super classes and implemented interfaces.
	*/
	public static Set<Class<?>> getClasses(final Class<?> objectClass, final Class<?> rootClass, final boolean includeSuperClasses, final boolean includeAbstract, final boolean includeInterfaces)
	{
		final Set<Class<?>> classes=new HashSet<Class<?>>();	//create a new set of classes
		getClasses(objectClass, rootClass, includeSuperClasses, includeAbstract, includeInterfaces, classes);	//get all the classes
		return classes;	//return the classes we retrieved
	}

	/**Determines all super classes and interfaces of the given class.
	@param objectClass The class for which super classes and interfaces should be found.
	@param rootClass The root class or interface to retrieve, or <code>null</code> if all classes should be retrieved.
	@param includeSuperClasses Whether super classes should be returned.
	@param includeAbstract Whether abstract classes should be returned.
	@param includeInterfaces Whether implemented interfaces should be returned.
	@param classes The set of classes to which the super classes and implemented interfaces should be added.
	*/
	protected static void getClasses(final Class<?> objectClass, final Class<?> rootClass, final boolean includeSuperClasses, final boolean includeAbstract, final boolean includeInterfaces, final Set<Class<?>> classes)
	{
		if(includeSuperClasses)	//if super classes should be included
		{
			final Class<?> superClass=objectClass.getSuperclass();	//get the super class
			if(superClass!=null)	//if there is a super class
			{
				if(rootClass==null || rootClass.isAssignableFrom(superClass))	//if the super class extends or implements the root class
				{
					if(includeAbstract || !Modifier.isAbstract(superClass.getModifiers()))	// make sure we should include abstract classes if this is an abstract class
					{
						classes.add(superClass);	//add the super class to the set
					}
					getClasses(superClass, rootClass, includeSuperClasses, includeAbstract, includeInterfaces, classes);	//get all the classes of the super class
				}
			}
		}
		if(includeInterfaces)	//if interfaces should be included
		{
			for(final Class<?> classInterface:objectClass.getInterfaces())	//look at each implemented interface
			{

				if(rootClass==null || rootClass.isAssignableFrom(classInterface))	//if this interface extends the root class
				{
					classes.add(classInterface);	//add the interface to the set
					getClasses(classInterface, rootClass, includeSuperClasses, includeAbstract, includeInterfaces, classes);	//get all the classes of the interface
				}
			}
		}
	}
}