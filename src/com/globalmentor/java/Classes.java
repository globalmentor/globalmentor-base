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

package com.globalmentor.java;

import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.util.*;
import static java.util.Collections.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.*;

import javax.mail.internet.*;

import static com.globalmentor.io.ContentTypes.*;
import static com.globalmentor.io.ContentTypeConstants.*;
import static com.globalmentor.io.FileConstants.*;
import static com.globalmentor.io.Files.*;
import static com.globalmentor.java.Java.*;
import static com.globalmentor.java.Strings.*;
import static com.globalmentor.net.URIs.*;
import static com.globalmentor.net.URLs.*;

import com.globalmentor.io.InputStreams;
import com.globalmentor.net.URIPath;
import com.globalmentor.util.NameValuePair;

/**Utilities for manipulating Java classes.
@author Garret Wilson
*/
public class Classes
{
	
	/**The getter prefix "get".*/
	public final static String GET_GETTER_PREFIX="get";

	/**The getter prefix "is".*/
	public final static String IS_GETTER_PREFIX="is";

	/**The getter prefix "set".*/
	public final static String SET_SETTER_PREFIX="set";

	/**The pattern recognizing a getter method name: "get" or "is" followed by any other characters (assuming they are Java characters), with the prefix in matching group 1 and the property name in matching group 2.*/
	public final static Pattern GETTER_METHOD_NAME_PATTERN=Pattern.compile("("+GET_GETTER_PREFIX+'|'+IS_GETTER_PREFIX+")(.+)");

	/**The pattern recognizing a setter method name: "set" followed by any other characters (assuming they are Java characters), with the prefix in matching group 1 and the property name in matching group 2.*/
	public final static Pattern SETTER_METHOD_NAME_PATTERN=Pattern.compile("("+SET_SETTER_PREFIX+")(.+)");

	/**This class cannot be publicly instantiated.*/
	private Classes() {}

	/**Determines the Java class represented by the given URI.
	A URI represents a Java class if it has a {@value Java#JAVA_URI_SCHEME} scheme
	in the form <code>java:/<var>com</var>/<var>example</var>/<var>package</var>/<var>Class</var></code>.	
	@param resourceURI The URI which is expected to represent a Java class, or <code>null</code>.
	@return The Java class represented by the given URI, or <code>null</code> if the URI is not a <code>java:</code> URI.
	@exception IllegalArgumentException if the given URI represents a Java class that does not have the correct syntax.
	@exception ClassNotFoundException if the class represented by the given URI could not be found.
	@see Java#JAVA_URI_SCHEME
	*/
	public static Class<?> asClass(final URI resourceURI) throws ClassNotFoundException
	{
		if(resourceURI!=null && JAVA_URI_SCHEME.equals(resourceURI.getScheme()))	//if an java: URI was given
		{
			final String classPath=resourceURI.getRawPath();	//get the path to the class
			if(classPath!=null)	//if there is a path
			{
				if(classPath.startsWith(ROOT_PATH))	//if the path is absolute
				{
					return Class.forName(uriDecode(classPath.substring(ROOT_PATH.length()).replace(PATH_SEPARATOR, PACKAGE_SEPARATOR)));	//skip the root path delimiter, replace path separators with package separators, and decode the string before trying to load the class
				}
				else	//if the path is not absolute
				{
					throw new IllegalArgumentException("Java URI "+resourceURI+" does not have an absolute path.");
				}				
			}
			else	//if there is no path
			{
				throw new IllegalArgumentException("Java URI "+resourceURI+" missing path.");
			}
		}
		return null;	//no class could be found
	}

	/**Creates a Java URI for a Java package using the {@value Java#JAVA_URI_SCHEME} scheme
	in the form <code>java:/<var>com</var>/<var>example</var>/<var>package</var>/</code>.
	@param objectPackage The package to use in creating the <code>java:</code> URI.
	@return A <code>java:</code> URI based upon the given class.
	@exception NullPointerException if the given class is <code>null</code>.
	*/
	public static URI createJavaURI(final Package objectPackage)
	{
		final String packagePath=URIPath.encodeSegment(objectPackage.getName()).replace(PACKAGE_SEPARATOR, PATH_SEPARATOR);	//get the package path by replacing the package separators with path separators after encoding
		return URI.create(JAVA_URI_SCHEME+SCHEME_SEPARATOR+ROOT_PATH+packagePath+PATH_SEPARATOR);	//create and return a new Java URI for the package
	}

	/**Creates a Java URI for a Java class using the {@value Java#JAVA_URI_SCHEME} scheme
	in the form <code>java:/<var>com</var>/<var>example</var>/<var>package</var>/<var>Class</var></code>.
	@param objectClass The class to use in creating the <code>java:</code> URI.
	@return A <code>java:</code> URI based upon the given class.
	@exception NullPointerException if the given class is <code>null</code>.
	*/
	public static URI createJavaURI(final Class<?> objectClass)
	{
		final String classPath=URIPath.encodeSegment(objectClass.getName()).replace(PACKAGE_SEPARATOR, PATH_SEPARATOR);	//get the class path by replacing the package separators with path separators after encoding
		return URI.create(JAVA_URI_SCHEME+SCHEME_SEPARATOR+ROOT_PATH+classPath);	//create and return a new Java URI for the class
	}

	/**Returns a content type identifying an object of the given class in the form <code>application/x-java-object;class=<var>package.Class</var></code>.
	@param objectClass The class for which a content type should be returned.
	@return A content type identifying an object of the given class in the form <code>application/x-java-object;class=<var>package.Class</var></code>.
	@exception IllegalArgumentException if the given object class is <code>null</code>.
	*/
	public static ContentType getObjectContentType(final Class<?> objectClass)
	{
		final ParameterList parameterList=new ParameterList();	//create a new parameter list
		parameterList.set("class", objectClass.getName());	//TODO testing
		return getContentTypeInstance(APPLICATION_PRIMARY_TYPE, X_JAVA_OBJECT, parameterList);	//create a content type appropriate for this object class TODO use a constant
	}

	/**Returns a constructor of a class that is compatible with the given parameter types.
	A constructor is considered compatible if each of the given parameter types can be assigned to the formal parameter type in the constructor.
	A constructor is first located the formal parameter types of which match the given parameters. If that fails, a compatible constructor is located.
	@param <T> The type of class.
	@param objectClass The class for which compatible constructors should be returned.
	@param parameterTypes The types of parameters to be used.
	@return A compatible constructors, or <code>null</code> if no compatible constructor could be found.
	@exception SecurityException If a security manager, <var>s</var>, is present and any of the following conditions is met:
  <ul>
		<li>Invocation of <code>{@link SecurityManager#checkMemberAccess(Class, int) s.checkMemberAccess(this, Member.PUBLIC)}</code> denies access to the constructor.</li>
		<li>The caller's class loader is not the same as or an ancestor of the class loader for the current class and invocation of <code>{@link SecurityManager#checkPackageAccess s.checkPackageAccess()}</code> denies access to the package of this class.</li>
	</ul>
	*/
	public static <T> Constructor<T> getCompatibleConstructor(final Class<T> objectClass, final Class<?> ... parameterTypes) throws SecurityException
	{
		Constructor<T> constructor=getConstructor(objectClass, parameterTypes);	//see if we can find an exact constructor
		if(constructor==null)	//if there is no exact constructor
		{
			final Constructor<T>[] compatibleConstructors=getCompatibleConstructors(objectClass, parameterTypes);	//get the compatible constructors, if any
			if(compatibleConstructors.length>0)	//if there is at least one compatible constructor
			{
				constructor=compatibleConstructors[0];	//use the first compatible constructor
			}
		}
		return constructor;	//return the compatible constructor, if we found one
	}

	/**Returns all constructors of a class that are compatible with the given parameter types.
	A constructor is considered compatible if each of the given parameter types can be assigned to the formal parameter type in the constructor.
	@param <T> The type of class.
	@param objectClass The class for which compatible constructors should be returned.
	@param parameterTypes The types of parameters to be used.
	@return An array of compatible constructors.
	@exception SecurityException If a security manager, <var>s</var>, is present and any of the following conditions is met:
  <ul>
		<li>Invocation of <code>{@link SecurityManager#checkMemberAccess(Class, int) s.checkMemberAccess(this, Member.PUBLIC)}</code> denies access to the constructor.</li>
		<li>The caller's class loader is not the same as or an ancestor of the class loader for the current class and invocation of <code>{@link SecurityManager#checkPackageAccess s.checkPackageAccess()}</code> denies access to the package of this class.</li>
	</ul>
	*/
	@SuppressWarnings("unchecked")	//casts are used because arrays are not generic-aware
	public static <T> Constructor<T>[] getCompatibleConstructors(final Class<T> objectClass, final Class ... parameterTypes) throws SecurityException
	{
		final int parameterCount=parameterTypes.length;	//get the number of requested parameters
		final Constructor[] constructors=objectClass.getConstructors();	//get all constructors for this class
		final List<Constructor<T>> compatibleConstructors=new ArrayList<Constructor<T>>(constructors.length);	//create a list sufficiently large to hold all constructors
		for(final Constructor constructor:constructors)	//for each constructor
		{
			final Class[] formalParameterTypes=constructor.getParameterTypes();	//get the formal parameter types
			if(formalParameterTypes.length==parameterCount)	//if this constructor has the correct number of formal parameters
			{
				boolean isCompatible=true;	//start out assuming this is a compatible constructor
				for(int i=parameterCount-1; isCompatible && i>=0; --i)	//for each parameter, as long we we think this is a compatible constructor
				{
					if(!isCompatible(((Class<?>)formalParameterTypes[i]), parameterTypes[i]))	//if we can't assign the requested parameter type to the formal parameter type
					{
						isCompatible=false;	//this is not a compatible constructor
					}
				}
				if(isCompatible)	//if this is a compatible constructor
				{
					compatibleConstructors.add((Constructor<T>)constructor);	//add this constructor to the list
				}
			}
		}
		return compatibleConstructors.toArray((Constructor<T>[])new Constructor[compatibleConstructors.size()]);	//return an array of compatible constructors
	}

	/**Determines if a class is compatible with a given class.
	This method functions identically to {@link Class#isAssignableFrom(Class)}, except that it allows the compatible class to be a non-primitive representation (e.g. {@link Integer})
	of a primitive type (e.g. <code>int</code>).
	This method is useful for determining if some type is compatible with a method signature.
	@param objectClass The class with which compatibility is being determined.
	@param compatibleClass The class the compatibility of which is questioned.
	@return <code>true</code> if the given class is compatible with the object class.
	*/
	public static boolean isCompatible(final Class<?> objectClass, final Class<?> compatibleClass)
	{
		if(objectClass.isPrimitive())	//if the class is a primitive, we'll have to do special checking
		{
			if(objectClass==compatibleClass)	//if the classes are the same
			{
				return true;	//they are compatible
			}
			if(Boolean.TYPE==objectClass)	//otherwise, check to see if a non-primitive version of a primitive class was given
			{
				return compatibleClass==Boolean.class;
			}
			else if(Byte.TYPE==objectClass)
			{
				return compatibleClass==Byte.class;
			}
			else if(Character.TYPE==objectClass)
			{
				return compatibleClass==Character.class;
			}
			else if(Double.TYPE==objectClass)
			{
				return compatibleClass==Double.class;
			}
			else if(Float.TYPE==objectClass)
			{
				return compatibleClass==Float.class;
			}
			else if(Integer.TYPE==objectClass)
			{
				return compatibleClass==Integer.class;
			}
			else if(Long.TYPE==objectClass)
			{
				return compatibleClass==Long.class;
			}
			else if(Short.TYPE==objectClass)
			{
				return compatibleClass==Short.class;
			}
			else if(Void.TYPE==objectClass)
			{
				return false;	//no class is compatible with void
			}
			else	//there should be no other primitive types in Java
			{
				throw new AssertionError("Unrecognized primitive type: "+objectClass);
			}
		}
		else	//if the class is not primitive
		{
			return objectClass.isAssignableFrom(compatibleClass);	//see if we can assign the compatible class to the object class
		}
	}
	
  /**Finds a defined constructor of a class.
	This method differs from {@link Class#getConstructor(Class[])} in that if no matching constructor is found, <code>null</code> is returned rather than a {@link NoSuchMethodException} being thrown.
	@param objectClass The class for which the constructor should be found.
	@param parameterTypes The constructor parameters.
	@return The <code>Method</code> object of the public constructor that matches the specified <code>parameterTypes</code>, or <code>null</code> if no such constructor exists.
	@exception SecurityException If a security manager, <var>s</var>, is present and any of the following conditions is met:
  <ul>
		<li>Invocation of <code>{@link SecurityManager#checkMemberAccess(Class, int) s.checkMemberAccess(this, Member.PUBLIC)}</code> denies access to the constructor.</li>
		<li>The caller's class loader is not the same as or an ancestor of the class loader for the current class and invocation of <code>{@link SecurityManager#checkPackageAccess s.checkPackageAccess()}</code> denies access to the package of this class.</li>
	</ul>
	*/
  public static <T> Constructor<T> getConstructor(final Class<T> objectClass, final Class<?> ... parameterTypes) throws SecurityException
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
		that this method returns <code>null</code> instead of throwing a {@link NoSuchMethodException}
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
	@SuppressWarnings("unchecked")	//all the constructors of the class should be constructors of the class type, even if the API doesn't indicate that for arrays
	public static <T> Constructor<T> getPublicDefaultConstructor(final Class<T> objectClass) throws SecurityException
	{
		final Constructor<T>[] constructors=(Constructor<T>[])objectClass.getConstructors(); //look at each constructor
		for(int i=constructors.length-1; i>=0; --i) //look at each constructor
		{
			final Constructor<T> constructor=constructors[i];  //get a reference to this constructor
				//if this constructor has no parameters and is public
			if(constructor.getParameterTypes().length==0 && Modifier.isPublic(constructor.getModifiers()))
				return constructor; //we found the default constructor
		}
		return null;  //show that we could not find a default constructor
	}

	/**Returns a <code>Method</code> object that reflects the specified public member method of the class or interface represented by this <code>Class</code> object.
	This method differs from {@link Class#getMethod(String, Class...)} in that if no matching method is found, <code>null</code> is returned rather than a {@link NoSuchMethodException} being thrown.
	@param objectClass The class for which the method should be found.
	@param name The name of the method.
	@param parameterTypes The list of parameters.
	@return The <code>Method</code> object that matches the specified <code>name</code> and <code>parameterTypes</code>, or <code>null</code> if a matching method is not found or if the name is is "&lt;init&gt;"or "&lt;clinit&gt;".
	@exception NullPointerException if <code>name</code> is <code>null</code>
	@exception  SecurityException If a security manager, <i>s</i>, is present and any of the following conditions is met:
	<ul>
		<li>invocation of <code>{@link SecurityManager#checkMemberAccess s.checkMemberAccess(this, Member.PUBLIC)}</code> deniesaccess to the method</li>
		<li> the caller's class loader is not the same as or an ancestor of the class loader for the current class and invocation of <code>{@link SecurityManager#checkPackageAccess s.checkPackageAccess()}</code> denies access to the package of this class</li>
	</ul>
	@since JDK1.1
	*/
  public static Method getMethod(final Class<?> objectClass, final String name, final Class<?> ... parameterTypes) throws SecurityException
  {
  	try
		{
			return objectClass.getMethod(name, parameterTypes);	//ask the class for the method
		}
  	catch(final NoSuchMethodException noSuchMethodException)	//if the method isn't found
		{
  		return null;	//indicate that the method couldn't be found
		}	  	
  }

	/**Returns the "get" getter method of a given class.
	This method differs from {@link Class#getMethod(String, Class...)} in that if no matching method is found, <code>null</code> is returned rather than a {@link NoSuchMethodException} being thrown.
	@param objectClass The class for which a getter method should be returned.
	@param propertyName The property name, such as "propertyName".
	@return The method with the name "get<var>PropertyName</var>", or <code>null</code> if such a method was not found.
	*/
	public static Method getGetPropertyMethod(final Class<?> objectClass, final String propertyName)
	{
		return getMethod(objectClass, getGetPropertyMethodName(propertyName));	//return the getter method, if there is one
	}

	/**Returns the "is" getter method of a given class.
	This method differs from {@link Class#getMethod(String, Class...)} in that if no matching method is found, <code>null</code> is returned rather than a {@link NoSuchMethodException} being thrown.
	@param objectClass The class for which a getter method should be returned.
	@param propertyName The property name, such as "propertyName".
	@return The method with the name "is<var>PropertyName</var>" having a {@link Boolean#TYPE} return type, or <code>null</code> if such a method was not found.
	*/
	public static Method getIsPropertyMethod(final Class<?> objectClass, final String propertyName)
	{
		final Method method=getMethod(objectClass, getIsPropertyMethodName(propertyName));	//get the getter method, if there is one
		return method!=null && Boolean.TYPE.equals(method.getReturnType()) ? method : null;	//if there is such a method, make sure it returns a boolean
	}

	/**Returns the getter method of a given class.
	This method first looks for a method with the name "get<var>PropertyName</var>", and then with the name "is<var>PropertyName</var>" having a {@link Boolean#TYPE} return type.
	This method differs from {@link Class#getMethod(String, Class...)} in that if no matching method is found, <code>null</code> is returned rather than a {@link NoSuchMethodException} being thrown.
	@param objectClass The class for which a getter method should be returned.
	@param propertyName The property name, such as "propertyName".
	@return The method with the name "get<var>PropertyName</var>", or the name "is<var>PropertyName</var>" having a {@link Boolean#TYPE}; or <code>null</code> if such a method was not found.
	*/
	public static Method getGetterMethod(final Class<?> objectClass, final String propertyName)
	{
		final Method getPropertyMethod=getMethod(objectClass, getGetPropertyMethodName(propertyName));	//get the getProperty method, if there is one
		return getPropertyMethod!=null ? getPropertyMethod : getIsPropertyMethod(objectClass, propertyName);	//if there is no getProperty method, check for a boolean isProperty method
	}

	/**Returns the setter method of a given class.
	This method differs from {@link Class#getMethod(String, Class...)} in that if no matching method is found, <code>null</code> is returned rather than a {@link NoSuchMethodException} being thrown.
	@param objectClass The class for which a setter method should be returned.
	@param propertyName The property name, such as "propertyName".
	@param valueClass The type of property value to be set. 
	@return The method with the name "set<var>PropertyName</var>" and the given value class as a parameter type, or <code>null</code> if such a method was not found.
	*/
	public static Method getSetterMethod(final Class<?> objectClass, final String propertyName, final Class<?> valueClass)
	{
		return getMethod(objectClass, getSetPropertyMethodName(propertyName), valueClass);	//return the setter method, if there is one
	}

	/**Returns a setter method compatible with a given value type, i.e. that could be used if the value is cast to the setter's parameter type.
	@param objectClass The class for which a setter method should be returned.
	@param propertyName The property name, such as "propertyName".
	@param valueClass The type of property value to be set. 
	@return The method with the name "set<var>PropertyName</var>" and a single parameter assignment-compatible with the given value class, or <code>null</code> if such a method was not found.
	*/
	public static Method getCompatibleSetterMethod(final Class<?> objectClass, final String propertyName, final Class<?> valueClass)
	{
		final String setterMethodName=getSetPropertyMethodName(propertyName);	//get the setter name to look for
		for(final Method method:objectClass.getMethods())	//look at each object method
		{
			if(method.getName().equals(setterMethodName))	//if this has the setter name
			{
				final Class<?>[] parameterTypes=method.getParameterTypes();	//get the parameter types for this method
				if(parameterTypes.length==1)	//if this setter has one parameter
				{
					final Class<?> parameterType=parameterTypes[0];	//get the single parameter type
					if(isCompatible(parameterType, valueClass))	//if we can assign the value class to the parameter type
					{
						return method;	//return this method
					}
				}
			}
		}
		return null;	//indicate that we couldn't find a compatible method
	}

	/**Determines if the given method is a getter method.
	@param method The method to check
	@return <code>true</code> if the method has a return type but no parameters, and the name of the method is in the form "get<var>PropertyName</var>" or "is<var>PropertyName</var>".
	*/
	public static boolean isGetterMethod(final Method method)
	{
		return isGetterMethodName(method.getName()) && method.getReturnType()!=null && method.getParameterTypes().length==0;	//see if the method has a getter name with a return type and no parameters
	}

	/**Determines if the given method name is that of a getter method.
	@param methodName The method name, such as "getPropertyName" or "isPropertyName".
	@return <code>true</code> if the name of the method is in the form "get<var>PropertyName</var>" or "is<var>PropertyName</var>".
	*/
	public static boolean isGetterMethodName(final String methodName)
	{
		return GETTER_METHOD_NAME_PATTERN.matcher(methodName).matches();	//see if the method name matches the getter method name pattern
	}

	/**Determines if the given method is a setter method.
	@param method The method name to check
	@return <code>true</code> if the method has no return type and a single parameter, and the name of the method is in the form "set<var>PropertyName</var>".
	*/
	public static boolean isSetterMethod(final Method method)
	{
		return isSetterMethodName(method.getName()) && method.getReturnType()==null && method.getParameterTypes().length==1;	//see if the method has a setter name with no return type and a single parameter		
	}

	/**Determines if the given method name is that of a setter method.
	@param methodName The method name, such as "setPropertyName".
	@return <code>true</code> if the name of the method is in the form "set<var>PropertyName</var>".
	*/
	public static boolean isSetterMethodName(final String methodName)
	{
		return SETTER_METHOD_NAME_PATTERN.matcher(methodName).matches();	//see if the method name matches the setter method name pattern
	}

	/**Determines the property name of the given getter method name.
	@param methodName The method name, such as "getPropertyName" or "isPropertyName".
	@return The property name in the form <var>propertyName</var>, or <code>null</code> if the name of the method is not in the form "get<var>PropertyName</var>" or "is<var>PropertyName</var>".
	*/
	public static String getGetterPropertyName(final String methodName)
	{
		final Matcher matcher=GETTER_METHOD_NAME_PATTERN.matcher(methodName);	//match the method name against the getter method name pattern
		return matcher.matches() ? Java.getVariableName(matcher.group(2)) : null;	//if there is a match, return the variable name of the matching group; otherwise return null
	}

	/**Determines the property name of the given getter method name.
	@param methodName The method name, such as "setPropertyName".
	@return The property name in the form <var>propertyName</var>, or <code>null</code> if the name of the method is not in the form "set<var>PropertyName</var>".
	*/
	public static String getSetterPropertyName(final String methodName)
	{
		final Matcher matcher=SETTER_METHOD_NAME_PATTERN.matcher(methodName);	//match the method name against the setter method name pattern
		return matcher.matches() ? Java.getVariableName(matcher.group(2)) : null;	//if there is a match, return the variable name of the matching group; otherwise return null
	}
	
	/**The name of the "get" getter method corresponding to the given property.
	@param propertyName The property name, such as "propertyName".
	@return The name of the getter method in the form "get<var>PropertyName</var>".
	*/
	public static String getGetPropertyMethodName(final String propertyName)
	{
		return GET_GETTER_PREFIX+getProperName(propertyName);	//return "getPropertyName"
	}

	/**The name of the "is" getter method corresponding to the given property.
	@param propertyName The property name, such as "propertyName".
	@return The name of the getter method in the form "is<var>PropertyName</var>".
	*/
	public static String getIsPropertyMethodName(final String propertyName)
	{
		return IS_GETTER_PREFIX+getProperName(propertyName);	//return "isPropertyName"
	}

	/**The name of the "set" setter method corresponding to the given property.
	@param propertyName The property name, such as "propertyName".
	@return The name of the setter method in the form "set<var>PropertyName</var>".
	*/
	public static String getSetPropertyMethodName(final String propertyName)
	{
		return SET_SETTER_PREFIX+getProperName(propertyName);	//return "setPropertyName"
	}

	/**Creates a property name by appending the property local name to the full class name.
	@param objectClass The class to supply the class name.
	@param localName The local name of the property.
	@return A full class name plus property name.
	@see #getFullName(Package, String)
	*/
	public static String getPropertyName(final Class<?> objectClass, final String localName)
	{
		return objectClass.getName()+OBJECT_PREDICATE_SEPARATOR+localName;	//return the class name plus the local name separated by a package separator
	}

	/**Creates a full name analogous to a class name from the package of an
	 	existing class. For example, a class of <code>com.example.Foo</code>
	 	and a local name of <code>Bar</code> will result in a full name of
	 	<code>com.example.Bar</code>.
	@param objectClass The class to supply the package name.
	@param localName The local name for constructing the full name within the package.
	@return A full class name in the package of the given class with the given
		local name.
	@see #getFullName(Package, String)
	*/
	public static String getFullName(final Class<?> objectClass, final String localName)
	{
		return getFullName(objectClass.getPackage(), localName);	//return the package plus the name separated by a package separator
	}

	/**Creates a full name given package and a local name.
	 	For example, a package of <code>com.example</code>
	 	and a local name of <code>Bar</code> will result in a full name of
	 	<code>com.example.Bar</code>.
	@param objectClass The class to supply the package name.
	@param localName The local name for constructing the full name within the package.
	@return A full class name in the given package and the given local name.
	*/
	public static String getFullName(final Package objectPackage, final String localName)
	{
		return objectPackage.getName()+PACKAGE_SEPARATOR+localName;	//return the package plus the name separated by a package separator
	}

	/**Returns the local name of the class, with the package name removed.
		Therefore <code>com.example.Foo$Bar</code> becomes <code>Foo$Bar</code>.
	@param objectClass The class for which a local name should be returned.
	@return The local name of the class within its package.
	@see #getSimpleName
	*/
	public static String getLocalName(final Class<?> objectClass)
	{
			//return the class name, with everything before the last package separator removed
		return removeBeforeLast(objectClass.getName(), PACKAGE_SEPARATOR);
	}

	/**Returns the local name of the class, with the package name removed. If
		the class represents an internal class, the external class name is removed
		as well. Therefore <code>com.example.Foo$Bar</code> becomes
		<code>Bar</code>.
	@param objectClass The class for which a simple name should be returned.
	@return The simple name of the class within its package, if any, and within
		its enclosing class, if any.
	@see #getLocalName
	*/
	public static String getSimpleName(final Class<?> objectClass)
	{
			//return the local name, with everything before the last internal class separator removed
		return removeBeforeLast(getLocalName(objectClass), INTERNAL_CLASS_SEPARATOR);
	}

	/**Constructs a variable name from the class by by decapitalizing all of the beginning uppercase letters of the simple name of the class.
	@param objectClass The class for which a variable name should be returned.
	@return A variable name appropriate for the class.
	@see #getSimpleName(Class)
	@see Java#getVariableName(String)
	*/
	public static String getVariableName(final Class<?> objectClass)
	{
		return Java.getVariableName(getSimpleName(objectClass));	//get the variable name form of the simple name of the class
	}

	/**A comparator that sorts ancestor classes primarily in terms of height (distance from a descendant class), secondarily in terms of concreteness (concrete class, abstract class, and then interface), and teriarily by class name.*/
	public final static Comparator<NameValuePair<Class<?>, Integer>> CONCRETE_CLASS_HEIGHT_COMPARATOR=new Comparator<NameValuePair<Class<?>,Integer>>()
			{
				/**Compares two classes based upon the classes and their height or distance from a particular class.
				Comparing is performed primarily in terms of height (distance from a descendant class), secondarily in terms of concreteness (concrete class, abstract class, and then interface), and teriarily by class name.
				@param classHeight1 The first class paired with its distance from a descendant class.
				@param classHeight2 The second class paired with its distance from a descendant class.
				@return The result of comparing the two classes.
				*/
				public int compare(final NameValuePair<Class<?>, Integer> classHeight1, final NameValuePair<Class<?>, Integer> classHeight2)
				{
					int result=classHeight1.getValue().intValue()-classHeight2.getValue().intValue();	//get the differences in heights
					if(result==0)	//if both classes are at the same height
					{
						final Class<?> class1=classHeight1.getClass();	//get the classes
						final Class<?> class2=classHeight2.getClass();
						if(class1.equals(class2))	//if this is the same class
						{
							return 0;	//the two are equal
						}
						final boolean isClass1Interface=class1.isInterface();	//see if the classes are interfaces
						final boolean isClass2Interface=class2.isInterface();
						if(isClass1Interface!=isClass2Interface)	//if one is an interface and the other isn't
						{
							return isClass1Interface ? 1 : -1;	//the interface gets the higher ordering
						}
						final boolean isClass1Abstract=Modifier.isAbstract(class1.getModifiers());	//see if the classes are abstract
						final boolean isClass2Abstract=Modifier.isAbstract(class2.getModifiers());
						if(isClass1Abstract!=isClass2Abstract)	//if one is abstract and the other isn't
						{
							return isClass1Abstract ? 1 : -1;	//the abstract class gets the higher ordering
						}
						result=class1.getName().compareTo(class2.getName());	//compare class names
					}
					return result;	//return whatever result we found
				};
			};

	/**Determines all super classes and interfaces of the given class, including the given class itself.
	Classes will be sorted primarily in terms of maximum height (distance from a descendant class), secondarily in terms of concreteness (concrete class, abstract class, and then interface), and teriarily by class name.
	@param objectClass The class for which super classes and interfaces should be found.
	@return The set of all super classes and implemented interfaces.
	@exception NullPointerException if the given object class is <code>null</code>.
	@see #CONCRETE_CLASS_HEIGHT_COMPARATOR
	*/
	public static List<Class<?>> getAncestorClasses(final Class<?> objectClass)
	{
		return getAncestorClasses(objectClass, Object.class);	//get all classes and interfaces
	}

	/**Determines all super classes and interfaces of the given class, excluding the given class itself.
	Classes will be sorted primarily in terms of maximum height (distance from a descendant class), secondarily in terms of concreteness (concrete class, abstract class, and then interface), and teriarily by class name.
	@param objectClass The class for which super classes and interfaces should be found.
	@return The set of all super classes and implemented interfaces.
	@exception NullPointerException if the given object class is <code>null</code>.
	@see #CONCRETE_CLASS_HEIGHT_COMPARATOR
	*/
	public static List<Class<?>> getProperAncestorClasses(final Class<?> objectClass)
	{
		return getProperAncestorClasses(objectClass, Object.class);	//get all classes and interfaces
	}

	/**Determines all super classes and interfaces of the given class, including the given class itself, up to the given class, in no guaranteed order.
	Classes will be sorted primarily in terms of maximum height (distance from a descendant class), secondarily in terms of concreteness (concrete class, abstract class, and then interface), and teriarily by class name.
	@param <R> The type of root class.
	@param objectClass The class for which super classes and interfaces should be found.
	@param rootClass The root class or interface to retrieve, or <code>null</code> if all classes should be retrieved.
	@return The set of all super classes and implemented interfaces.
	@exception NullPointerException if the given object class and/or root class is <code>null</code>.
	@see #CONCRETE_CLASS_HEIGHT_COMPARATOR
	*/
	public static <R> List<Class<? extends R>> getAncestorClasses(final Class<? extends R> objectClass, final Class<R> rootClass)
	{
		return getAncestorClasses(objectClass, rootClass, true, true, true, true, (Comparator<NameValuePair<Class<? extends R>, Integer>>)(Object)CONCRETE_CLASS_HEIGHT_COMPARATOR);	//get ancestor classes, including super classes, abstract classes, and interfaces TODO check cast
	}

	/**Determines all super classes and interfaces of the given class, excluding the given class itself, up to the given class, in no guaranteed order.
	Classes will be sorted primarily in terms of maximum height (distance from a descendant class), secondarily in terms of concreteness (concrete class, abstract class, and then interface), and teriarily by class name.
	@param <R> The type of root class.
	@param objectClass The class for which super classes and interfaces should be found.
	@param rootClass The root class or interface to retrieve, or <code>null</code> if all classes should be retrieved.
	@return The set of all super classes and implemented interfaces.
	@exception NullPointerException if the given object class and/or root class is <code>null</code>.
	@see #CONCRETE_CLASS_HEIGHT_COMPARATOR
	*/
	public static <R> List<Class<? extends R>> getProperAncestorClasses(final Class<? extends R> objectClass, final Class<R> rootClass)
	{
		return getAncestorClasses(objectClass, rootClass, false, true, true, true, (Comparator<NameValuePair<Class<? extends R>, Integer>>)(Object)CONCRETE_CLASS_HEIGHT_COMPARATOR);	//get ancestor classes, including super classes, abstract classes, and interfaces TODO check cast
	}

	/**Determines all super classes and interfaces of the given class.
	If a comparator is given, the classes and interfaces will be sorted based upon that comparator, using the farthest distance from the given class when sorting.
	@param <R> The type of root class.
	@param objectClass The class for which super classes and interfaces should be found.
	@param rootClass The root class or interface to retrieve.
	@param includeThisClasses Whether the object class itself should be returned.
	@param includeSuperClasses Whether super classes should be returned.
	@param includeAbstract Whether abstract classes should be returned.
	@param includeInterfaces Whether implemented interfaces should be returned.
	@param comparator The strategy for sorting the returned classes, or <code>null</code> if the order of classes is not important.
	@exception NullPointerException if the given object class and/or root class is <code>null</code>.
	@return The set of all super classes and implemented interfaces.
	*/
	@SuppressWarnings("unchecked")	//casts, along with not-type-specific addClass() method, required for compiling with Sun JDK 1.6.0_03-b05; not required for Eclipse 3.4M3
	public static <R> List<Class<? extends R>> getAncestorClasses(final Class<? extends R> objectClass, final Class<R> rootClass, final boolean includeThisClass, final boolean includeSuperClasses, final boolean includeAbstract, final boolean includeInterfaces, final Comparator<NameValuePair<Class<? extends R>, Integer>> comparator)
	{
		final Map<Class<? extends R>, NameValuePair<Class<? extends R>, Integer>> classHeightMap=new HashMap<Class<? extends R>, NameValuePair<Class<? extends R>, Integer>>();	//create a new map of class/height pairs
		if(includeThisClass && rootClass.isAssignableFrom(objectClass))	//if we should include this class
		{
			addClass(objectClass.asSubclass(rootClass), 0, (Map<Class<?>, NameValuePair<Class<?>, Integer>>)(Object)classHeightMap);	//add this class to the map at height 0
		}
		getAncestorClasses(objectClass, 1, rootClass, includeSuperClasses, includeAbstract, includeInterfaces, classHeightMap);	//get all the classes, starting one level above the class
		final List<Class<? extends R>> classList;	//we'll create a list to hold the classes
		if(comparator!=null)	//if a comparator was given
		{
			classList=new ArrayList<Class<? extends R>>(classHeightMap.size());	//create a list to hold the classes
			final List<NameValuePair<Class<? extends R>, Integer>> classHeightList=new ArrayList<NameValuePair<Class<? extends R>,Integer>>(classHeightMap.values());	//get all the class/height pairs
			if(comparator!=null)	//if a comparator was given
			{
				sort(classHeightList, comparator);	//sort the list using the comparator
			}
			for(final NameValuePair<Class<? extends R>, Integer> classHeight:classHeightList)	//for each class height in the list
			{
				classList.add(classHeight.getName());	//add this class to the list
			}
		}
		else	//if no comparator was given
		{
			classList=new ArrayList<Class<? extends R>>(classHeightMap.keySet());	//create a list from the set of keys			
		}
		return classList;	//return the list of classes
	}

	/**Determines super classes and interfaces of the given class.
	The returned set will not include the given object class.
	@param <R> The type of root class.
	@param objectClass The class for which super classes and interfaces should be found.
	@param height The zero-based distance towards the root away from the original class.
	@param rootClass The root class or interface to retrieve.
	@param includeSuperClasses Whether super classes should be returned.
	@param includeAbstract Whether abstract classes should be returned.
	@param includeInterfaces Whether implemented interfaces should be returned.
	@param classHeightMap The map of class/height pairs keyed to the class.
	*/
	@SuppressWarnings("unchecked")	//casts, along with not-type-specific addClass() method, required for compiling with Sun JDK 1.6.0_03-b05; not required for Eclipse 3.4M3
	protected static <R> void getAncestorClasses(final Class<? extends R> objectClass, final int height, final Class<R> rootClass, final boolean includeSuperClasses, final boolean includeAbstract, final boolean includeInterfaces, final Map<Class<? extends R>, NameValuePair<Class<? extends R>, Integer>> classHeightMap)
	{
		if(includeSuperClasses)	//if super classes should be included
		{
			final Class<?> superClass=objectClass.getSuperclass();	//get the super class
			if(superClass!=null)	//if there is a super class
			{
				if(rootClass.isAssignableFrom(superClass))	//if the super class extends or implements the root class
				{
					final Class<? extends R> superExtendsRootClass=superClass.asSubclass(rootClass);	//get the version of the super class that extends the root class
					if(includeAbstract || !Modifier.isAbstract(superClass.getModifiers()))	// make sure we should include abstract classes if this is an abstract class
					{
						addClass(superExtendsRootClass, height, (Map<Class<?>, NameValuePair<Class<?>, Integer>>)(Object)classHeightMap);	//add the super class to the map
					}
					getAncestorClasses(superExtendsRootClass, height+1, rootClass, includeSuperClasses, includeAbstract, includeInterfaces, classHeightMap);	//get all the classes of the super class
				}
			}
		}
		if(includeInterfaces)	//if interfaces should be included
		{
			for(final Class<?> classInterface:objectClass.getInterfaces())	//look at each implemented interface
			{
				if(rootClass.isAssignableFrom(classInterface))	//if this interface extends the root class
				{
					final Class<? extends R> interfaceExtendsRootClass=classInterface.asSubclass(rootClass);	//get the version of the interface that extends the root class
					addClass(interfaceExtendsRootClass, height, (Map<Class<?>, NameValuePair<Class<?>, Integer>>)(Object)classHeightMap);	//add the interface to the map
					getAncestorClasses(interfaceExtendsRootClass, height+1, rootClass, includeSuperClasses, includeAbstract, includeInterfaces, classHeightMap);	//get all the classes of the interface
				}
			}
		}
	}

	/**Adds a class to a map of class lists, removing any classes that were listed in any lists of lower heights.
	@param <R> The type of root class.
	@param objectClass The class to add.
	@param height The zero-based distance towards the root away from the original class.
	@param classHeightMap The map of class/height pairs keyed to the class.
	@exception NullPointerException if the given object class is <code>null</code>.
	*/
	private static <R> void addClass(final Class<? extends R> objectClass, final int height, final Map<Class<?>, NameValuePair<Class<?>, Integer>> classHeightMap)
//preferred signature, which works on Eclipse 3.4M3 but not on Sun JDK 1.6.0_03-b05:	private static <R> void addClass(final Class<? extends R> objectClass, final int height, final Map<Class<? extends R>, NameValuePair<Class<? extends R>, Integer>> classHeightMap)
	{
		final NameValuePair<Class<?>, Integer> oldClassHeight=classHeightMap.get(objectClass);	//get the old height
		if(oldClassHeight==null || oldClassHeight.getValue().intValue()<height)	//if there was no old height, or the old height is not as large as the new height
		{
			classHeightMap.put(objectClass, new NameValuePair<Class<?>, Integer>(objectClass, height));	//update the height for the class
		}
	}

//TODO make a soft reference that deletes the file when garbage-collected
	
	/**The shared, thread-safe map of temporary files keyed to resource names.*/
	private final static Map<String, File> resourceFileMap=new ConcurrentHashMap<String, File>();
	
	/**Provides access to a resouce in the classpath via a file object.
	The rules for searching resources associated with a given class are implemented by the defining
	{@linkplain ClassLoader class loader} of the class.
	The first time a particular resource is accessed a temporary file is created with the contents of the resource.
	The temporary file will be deleted when the JVM exits.
	This method does not guarantee that any two requests for the same resource will result in the same file object or filename.
	The calling method must not delete the file or modify the file in any way, as the file may be cached and used for subsequent calls to this method.
	@param objectClass The class the class loader of which will be used to provide access to the resource.
	@param name The name of the desired resource.
	@return A file object or <code>null</code> if no resource with the given name is found.
	@exception IOException if there is an I/O error accessing the resource.
	*/
	public static File getResource(final Class<?> objectClass, final String name) throws IOException
	{
//TODO del Debug.trace("ready to get file to resource", name);
		File file=resourceFileMap.get(name);	//get any cached temporary file
		if(file==null)	//if there is no cached file for this name (there is a benign race condition here; it is better to allow the possibility of multiple temporary files for a single resource than to slow down all accesses to resources while one loads)
		{
//		TODO del Debug.trace("must create new file");
			final URL resourceURL=objectClass.getResource(name);	//get a URL to the resource
//		TODO del Debug.trace("got URL to resource", resourceURL);
			if(resourceURL!=null)	//if there is such a resource
			{
				final String filename=getFileName(resourceURL);	//get the filename of the URL
//			TODO del Debug.trace("resource filename:", filename);
				final String baseName=removeExtension(filename);	//get the base name
//			TODO del Debug.trace("baseName:", baseName);
				final String extension=getExtension(filename);	//get the extension
//			TODO del Debug.trace("extension:", extension);
				file=File.createTempFile(baseName, new StringBuilder().append(EXTENSION_SEPARATOR).append(extension).toString());	//create a temp file with the base name as the prefix and the extension (with separator) as the suffix
				file.deleteOnExit();	//indicate that the temporary file should be deleted when the JVM exits
				final InputStream inputStream=resourceURL.openConnection().getInputStream();	//get an input stream to the resource
				try
				{
					final OutputStream outputStream=new FileOutputStream(file);	//create an output stream to the file
					try
					{
						InputStreams.copy(inputStream, outputStream);	//copy the resource input stream to the output stream to the temporary file
					}
					finally
					{
						outputStream.close();	//always close the output stream
					}
				}
				finally
				{
					inputStream.close();	//always close the input stream
				}
				resourceFileMap.put(name, file);	//cache the temporary file, now that the resource has been copied and the streams closed successfully
			}
		}
		return file;	//return the file to the resource contents, or null if there was no such resource
	}
	
	
}