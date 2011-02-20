/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf.ploop;

import java.lang.reflect.*;
import java.net.URI;
import java.util.*;
import static java.util.Collections.*;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.regex.Pattern;

import static com.globalmentor.collections.Arrays.*;
import static com.globalmentor.java.Classes.*;
import static com.globalmentor.java.Java.*;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.URF.*;

import com.globalmentor.collections.Collections;
import com.globalmentor.net.*;
import com.globalmentor.urf.*;
import com.globalmentor.util.*;

/*TODO fix for Guise
import com.guiseframework.style.AbstractModeledColor;
import com.guiseframework.style.Color;
*/

/**Processes PLOOP objects from an URF data model.
<p>This is a stateful processor and may only be used for one URF data model instance.</p>
<p>This processor is not thread safe.</p>
<p>This processor recognizes the {@link Resource} type; when this class is present with a single URI parameter constructor, that constructor will take precedence using the resource URI value.</p>
<p>This processor also recognizes the {@link URFResource} type and will transfer all non-PLOOP properties when an instance is encountered.</p>
@author Garret Wilson
*/
public class PLOOPURFProcessor
{

	/**The default arguments that can be used in calling class constructors.*/
	private final Set<Object> defaultConstructorArguments=new CopyOnWriteArraySet<Object>();

		/**@return The default arguments that can be used in calling class constructors.*/
		protected Set<Object> getDefaultConstructorArguments() {return defaultConstructorArguments;}

	/**The map of created objects keyed to the resources from which they were created.*/
	protected final Map<URFResource, Object> resourceObjectMap=new HashMap<URFResource, Object>();
		
	/**Default arguments constructor.
	@param defaultConstructorArguments The objects that can be used as default arguments in class constructors.
	*/
	public PLOOPURFProcessor(final Object... defaultConstructorArguments)
	{
		addAll(this.defaultConstructorArguments, defaultConstructorArguments);	//add all the given default constructor arguments to our set of default constructor arguments
	}

	/**Retrieves an object to represent the given URF resource.
	If the the object for the resource has already been created, the existing object will be returned.
	Otherwise the object is created using {@link #createObject(URFResource)} and a reference to the created object is stored for later retrieval inside {@link #setObjectProperties(Object, URFResource, Map)}.
	@param resource The resource describing the Java object to be created.
	@return A created and initialized object according to the given resource description. 
	@exception NullPointerException if the given resource is <code>null</code>.
 	@exception DataException if a resource does not specify Java type information.
 	@exception DataException if a resource is a Java-typed resource the class of which cannot be found.
 	@exception DataException if a resource indicates a Java class that has no appropriate constructor.
 	@exception DataException if a resource indicates a Java class that is an interface or an abstract class.
 	@exception DataException if a resource indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if a resource indicates a Java class the constructor of which throws an exception.
	*/
	public Object getObject(final URFResource resource) throws DataException, InvocationTargetException
	{
		Object object=resourceObjectMap.get(resource);	//look up an existing object if the description, if any
		if(object==null)	//if we don't have an object already
		{
			object=createObject(resource);	//create the object from the description
		}
		return object;	//return the created object
	}

	/**Retrieves objects representing the given URF instance.
	Objects for all Java typed resources will be returned. This is a convenient way to ensure all Java classes described by an URF instance have been created.
	If an object for a given resource has already been created, the existing object will be returned.
	Otherwise the object is created using {@link #createObject(URFResource)} and a reference to the created object is stored for later retrieval inside {@link #setObjectProperties(Object, URFResource, Map)}.
	@param urf The URF instance describing the Java objects to be created.
	@return A list of created and initialized objects according to the given URF instance. 
	@exception NullPointerException if the given URF data model is <code>null</code>.
 	@exception DataException if a resource does not specify Java type information.
 	@exception DataException if a resource is a Java-typed resource the class of which cannot be found.
 	@exception DataException if a resource indicates a Java class that has no appropriate constructor.
 	@exception DataException if a resource indicates a Java class that is an interface or an abstract class.
 	@exception DataException if a resource indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if a resource indicates a Java class the constructor of which throws an exception.
	*/
	public List<Object> getObjects(final URF urf) throws DataException, InvocationTargetException
	{
		final List<Object> objects=new ArrayList<Object>();	//create a new list of objects
		for(final URFResource resource:urf.getResources())	//for each resource
		{
			boolean hasJavaType=false;	//we'll see if this resource has a Java type
			final Iterator<URFResource> typeIterator=resource.getTypes().iterator();	//get an iterator to the types
			while(!hasJavaType && typeIterator.hasNext())	//while we haven't found a Java type and there are other types left
			{
				final URI typeURI=typeIterator.next().getURI();	//get the next type URI
				if(typeURI!=null && JAVA_URI_SCHEME.equals(typeURI.getScheme()))	//if this is an a java: URI
				{
					hasJavaType=true;	//show that this resource has a Java type					
				}
			}
			if(hasJavaType)	//if this resource has a Java type
			{
				objects.add(getObject(resource));	//get the instance of this resource
			}
		}
		return objects;
	}

	/**Retrieves an object of the given type or subtype from the given URF instance.
	All Java classes described by an URF instance will first be ensured to have been created.
	@param urf The URF instance describing the Java objects to be created.
	@param type The type of object to return.
	@return A created and initialized object according to the given URF description of the first object of the given type, or <code>null</code> if no object of the given type is described in the URF instance. 
	@exception NullPointerException if the given URF data model and/or type is <code>null</code>.
 	@exception DataException if a resource does not specify Java type information.
 	@exception DataException if a resource is a Java-typed resource the class of which cannot be found.
 	@exception DataException if a resource indicates a Java class that has no appropriate constructor.
 	@exception DataException if a resource indicates a Java class that is an interface or an abstract class.
 	@exception DataException if a resource indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if a resource indicates a Java class the constructor of which throws an exception.
	*/
	public <T> T getObject(final URF urf, final Class<T> type) throws DataException, InvocationTargetException
	{
		for(final Object object:getObjects(urf))	//get all objects and look at each one of them
		{
			if(type.isInstance(object))	//if the object is an instance of the type
			{
				return type.cast(object);	//cast the object to the correct type and return it
			}
		}
		return null;	//indicate that no matching object could be found
	}

	/**Creates and initializes an object to represent the given URF resource by taking the following steps:
	<ol>
		<li>If the resource is an {@link URFListResource}, a new {@link List} containing converted list element objects will be returned.</li>
		<li>If the resource is an {@link URFSetResource}, a new {@link Set} containing converted set element objects will be returned.</li>
		<li>If the resource is an {@link URFMapResource}, a new {@link Map} containing the converted keys and converted value objects will be returned.</li>
		<li>If the resource specifies a Java type, the indicated Java class is instantiated and initialized from the resource description.</li>
		<li>If the resource otherwise indicates a value that can be represented by a Java object (such as an integer), such an object will be returned.</li>
		<li>If the resource does not meet any of the above criteria, the resource itself will be returned.</li>
	</ul>
	@param urfResource The URF resource describing the Java object to be created.
	@return A created and initialized object according to the given resource description.
	@exception NullPointerException if the given resource is <code>null</code>. 
 	@exception DataException if the given resource does not specify Java type information.
 	@exception DataException if the given resource is a Java-typed resource the class of which cannot be found.
 	@exception DataException if the given resource indicates a Java class that has no appropriate constructor.
 	@exception DataException if the given resourceindicates a Java class that is an interface or an abstract class.
 	@exception DataException if the given resourceindicates a Java class the constructor of which is not accessible.
	@exception DataException If a particular property could not be accessed.
	@exception InvocationTargetException if the given resource indicates a Java class the constructor of which throws an exception.
	@see #convertObject(Object, Class)
	*/
	protected Object createObject(final URFResource resource) throws DataException, InvocationTargetException
	{
		if(resource instanceof URFListResource<?>)	//if the object is an URF array
		{
			final URFListResource<?> urfListResource=(URFListResource<?>)resource;	//cast the object to a list
			final List<Object> list=new ArrayList<Object>();	//create a new list TODO eventually create a list but later check to see if the setter will accept a collection
			urfListResource.readLock().lock();	//get a read lock
			try
			{
				for(final URFResource urfListElement:urfListResource)	//for each URF resource in the list
				{
					list.add(getObject(urfListElement));	//get or create an object from this URF list element and add it to our list
				}
			}
			finally
			{
				urfListResource.readLock().unlock();	//always release the read lock
			}
			return list;	//return the list of objects we created
		}
		else if(resource instanceof URFSetResource<?>)	//if the object is an URF set
		{
			final URFSetResource<?> urfSetResource=(URFSetResource<?>)resource;	//cast the object to a set
			final Set<Object> set=new HashSet<Object>();	//create a new set TODO eventually create a set but later check to see if the setter will accept a collection
			urfSetResource.readLock().lock();	//get a read lock
			try
			{
				for(final URFResource urfSetElement:urfSetResource)	//for each URF resource in the set
				{
					set.add(getObject(urfSetElement));	//get or create an object from this URF set element and add it to our set
				}
			}
			finally
			{
				urfSetResource.readLock().unlock();	//always release the read lock
			}
			return set;	//return the set of objects we created
		}
		else if(resource instanceof URFMapResource<?, ?>)	//if the object is an URF map
		{
			final URFMapResource<?, ?> urfMapResource=(URFMapResource<?, ?>)resource;	//cast the object to a map
			final Map<Object, Object> map=new HashMap<Object, Object>();	//create a new map
			urfMapResource.readLock().lock();	//get a read lock
			try
			{
				for(final Map.Entry<? extends URFResource, ? extends URFResource> mapEntry:urfMapResource.entrySet())	//for each map entry in the URF map
				{
					map.put(getObject(mapEntry.getKey()), getObject(mapEntry.getValue()));	//get or create objects for the key and value, and store them in the map 
				}
			}
			finally
			{
				urfMapResource.readLock().unlock();	//always release the read lock
			}
			return map;	//return the map of objects we created
		}
		else	//if this is another type of resource, see if we can create an object for it
		{
			final Object simpleObject=asObject(resource);	//see if we can turn the resource into a simple object (do this first, because it may be an enum)
			if(simpleObject!=null)	//if we know what type of object it is
			{
				return simpleObject;	//return the object
			}
			Class<?> valueClass=null;	//we'll try to find a Java class from one of the types
			for(final URFProperty typeProperty:resource.getProperties(TYPE_PROPERTY_URI))	//look at each type
			{
				final URFResource type=typeProperty.getValue();	//get this type
				try
				{
					valueClass=asClass(type);	//try to get a class from the type
				}
				catch(final ClassNotFoundException classNotFoundException)
				{
					throw new DataException(classNotFoundException);
				}
				if(valueClass!=null)	//if we know the value class, try to construct a Java object
				{
					final Map<URI, PropertyDescription> propertyDescriptionMap=getPropertyDescriptionMap(valueClass, resource);	//get the property descriptions from the resource description
					Constructor<?> constructor=null;	//we'll store an appropriate constructor here
					Object[] arguments=null;	//we'll keep track of the arguments here
					final Constructor<?>[] constructors=valueClass.getConstructors();	//get all available constructors
					final URFListResource<?> selector=asListInstance(typeProperty.getScope().getPropertyValue(SELECTOR_PROPERTY_URI));	//get the selector list, if any
					if(selector!=null)	//if a selector was specified
					{
						final int argumentCount=selector.size();	//see how many selector arguments there are
						for(final Constructor<?> candidateConstructor:constructors)	//look at each constructor to find one with the correct number of parameters
						{
							final Class<?>[] parameterTypes=candidateConstructor.getParameterTypes();	//get the parameter types for this constructor
							if(parameterTypes.length==argumentCount)	//if this constructor has the correct number of parameters
							{
								arguments=new Object[argumentCount];	//create an array sufficient for the arguments
								for(int parameterIndex=0; parameterIndex<argumentCount; ++parameterIndex)	//for each parameter, as long we we have matching parameters
								{
									final Class<?> parameterType=parameterTypes[parameterIndex];	//get this parameter type
									final Object argument=convertObject(getObject(selector.get(parameterIndex)), parameterType);	//get an object from the selector resource and covert it to the correct type
									if(argument!=null)	//if we successfully converted this constructor argument
									{
										arguments[parameterIndex]=argument;	//store the argument
									}
									else	//if we couldn't convert this constructor argument
									{
										arguments=null;	//these arguments won't work
										break;	//stop looking at this constructor
									}								
								}
							}
							if(arguments!=null)	//if these arguments worked
							{
								constructor=candidateConstructor;	//use this constructor
								break;	//stop looking for a matching constructor
							}
						}
						if(constructor==null)	//if we didn't find an appropriate constructor
						{
							throw new DataException("Value class "+valueClass+" does not have a constructor appropriate for the specified selector parameters: "+Collections.toString(selector));
						}
					}
					else	//if there is no type selector
					{
						if(Resource.class.isAssignableFrom(valueClass))	//if the value class is a Resource, see if we can create it with a single URI
						{
							constructor=getCompatibleConstructor(valueClass, URI.class);	//see if there is a single URI parameter constructor
							if(constructor!=null)	//if there is a single URI parameter constructor for the Resource
							{
								arguments=new Object[]{resource.getURI()};	//the resource URI will be constructor argument
							}
						}
						if(constructor==null)	//if we still don't have a constructor
						{
							constructor=getPublicDefaultConstructor(valueClass);	//see if there's a default constructor
							if(constructor!=null)	//if there is a default constructor
							{
								arguments=EMPTY_OBJECT_ARRAY;	//the default constructor has no arguments
							}
						}
					}
					if(constructor!=null)	//if we found a constructor
					{
						assert arguments!=null : "Found constructor but missing arguments.";
						try
						{
							final Object object=constructor.newInstance(arguments);	//invoke the constructor with the arguments
							setObjectProperties(object, resource, propertyDescriptionMap);	//initialize the object with the properties
							return object;	//return the constructed and initialized object
						}
						catch(final InstantiationException instantiationException)
						{
							throw new DataException(instantiationException);
						}
						catch(final IllegalAccessException illegalAccessException)
						{
							throw new DataException(illegalAccessException);
						}
					}
					else	//if we didn't find a constructor
					{
						throw new DataException("Value class "+valueClass+" does not have an appropriate constructor.");						
					}
				}
			}
			return resource;	//return the resource itself
		}
	}

	/**Initializes an object based upon the given description.
	The object being initialized will be stored locally keyed to the resource description for later lookup.
	This implementation also recognizes the {@link URFResource} type and will transfer all non-PLOOP properties when an instance is encountered.
	@param object The object to initialize.
	@param resource The description for the object.
	@exception NullPointerException if the given object and/or resource is <code>null</code>.
	@exception DataException if a particular value is not an appropriate argument for the corresponding property.
	@exception DataException If a particular property could not be accessed.
	@exception InvocationTargetException if a resource indicates a Java class the constructor of which throws an exception.
	@see #setObjectProperties(Object, Map)
	*/
	public void setObjectProperties(final Object object, final URFResource resource) throws DataException, InvocationTargetException
	{
		final Map<URI, PropertyDescription> propertyDescriptionMap=getPropertyDescriptionMap(object.getClass(), resource);	//get property descriptions from the resource description
		setObjectProperties(object, resource, propertyDescriptionMap);	//initialize the object from the property descriptions
	}

	/**Initializes an object based upon the given URI and property descriptions.
	The object being initialized will be stored locally keyed to the resource description for later lookup.
	This implementation also recognizes the {@link URFResource} type and will transfer all non-PLOOP properties to the object when an instance is encountered.
	@param object The object to initialize.
	@param resource The description for the object.
	@param propertyDescriptionMap The property descriptions for initializing the object.
	@exception NullPointerException if the given object, resource, and/or property description map is <code>null</code>.
 	@exception DataException if a resource is a Java-typed resource the class of which cannot be found.
	@exception DataException if a particular value is not an appropriate argument for the corresponding property.
	@exception DataException If a particular property could not be accessed.
	@exception InvocationTargetException a resource indicates a Java class the constructor of which throws an exception.
	*/
	protected void setObjectProperties(final Object object, final URFResource resource, final Map<URI, PropertyDescription> propertyDescriptionMap) throws DataException, InvocationTargetException
	{
		for(final PropertyDescription propertyDescription:propertyDescriptionMap.values())	//for each property description
		{
			final Method setter=propertyDescription.getSetter();	//get the setter method for this property
			if(setter!=null)	//if there is a setter for this property
			{
				setObjectProperty(object, setter, propertyDescription.getValue());	//set the value for the property
			}
		}
		if(object instanceof URFResource)	//if the object is an URF resource, add any non-PLOOP properties to the new URF resource
		{
			for(final URFProperty property:resource.getProperties())	//for each property of the source resource
			{
				final URI propertyURI=property.getPropertyURI();	//get the property URI
				if(!propertyDescriptionMap.containsKey(propertyURI))	//if this was not a PLOOP property
				{
					((URFResource)object).addPropertyValue(propertyURI, property.getValue());	//add this property and value to the created URF resource
				}
			}
		}
		resourceObjectMap.put(resource, object);	//associate the initialized object with its resource description
	}

	/**Sets the property of an object based upon the stored property value in a given resource description.
	If the given resource has no such property, no action will occur.
	@param object The object to initialize.
	@param resource The description for the object.
	@param propertyName The name of the property to set.
	@return <code>true</code> if the given resource description contained a corresponding property and that property was used to update the given object.
	@exception NullPointerException if the given object, resource, and/or property name is <code>null</code>.
 	@exception DataException if a resource is a Java-typed resource the class of which cannot be found.
 	@exception DataException if the object has no setter method for the given property.
	@exception DataException if the particular value is not an appropriate argument for the given property.
	@exception DataException If the given property could not be accessed.
	@exception InvocationTargetException if the given resource indicates a Java class the constructor of which throws an exception.
	*/
	public boolean setObjectProperty(final Object object, final URFResource resource, final String propertyName) throws DataException, InvocationTargetException
	{
		final URI propertyURI=createResourceURI(DEFAULT_NAMESPACE_URI, propertyName);	//get the property URI for the given property of the object
		final URFResource propertyValue=resource.getPropertyValue(propertyURI);	//get the value for this property
//TODO fix to support null values		if(propertyValue==null && !resource.hasProperty(propertyURI))	//if the resource does not have a value for the property
		if(propertyValue==null && !resource.hasProperty(propertyURI))	//if the resource does not have a value for the property
		{
			return false;	//indicate that there is no such property value
		}
		final PropertyDescription propertyDescription=getPropertyDescription(object.getClass(), propertyName, propertyValue);	//get the property description
		if(propertyDescription!=null)	//if we found a property description
		{
			final Method setterMethod=propertyDescription.getSetter();	//get the setter
			if(setterMethod!=null)	//if there is a setter method
			{
				setObjectProperty(object, setterMethod, propertyDescription.getValue());	//set the property
				return true;	//indicate that we set the property successfully
			}
		}
		throw new DataException("Object "+object+" has no property "+propertyName);
	}

	/**Sets the property of an object using the given setter method and value.
	@param object The object to initialize.
	@param setterMethod The method to use in setting the object property.
	@param value The value to set.
	@exception NullPointerException if the given object and/or setter method is <code>null</code>.
	@exception DataException if the given value is not an appropriate argument for the given property.
	@exception DataException If the given property could not be accessed.
	@exception InvocationTargetException if the given Java property method throws an exception.
	*/
	public static void setObjectProperty(final Object object, final Method setterMethod, final Object value) throws DataException, InvocationTargetException
	{
		try
		{
			setterMethod.invoke(object, value);	//invoke the setter on the object
		}
		catch(final IllegalAccessException illegalAccessException)	//if we can't access this setter
		{
			throw new DataException(illegalAccessException);
		}
	}

	/**Constructs a map of property descriptions for a class based upon a resource description, using the object's class to determine the property namespace.
	If there are duplicate properties, only one will be stored.
	@param objectClass The class of the object to be constructed.
	@param resource The description fo the object.
	@return A map of property descriptions keyed to property URIs.
 	@exception DataException if a resource does not specify Java type information.
 	@exception DataException if a resource is a Java-typed resource the class of which cannot be found.
 	@exception DataException if a resource indicates a Java class that has no appropriate constructor.
 	@exception DataException if a resource indicates a Java class that is an interface or an abstract class.
 	@exception DataException if a resource indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if a resource indicates a Java class the constructor of which throws an exception.
	*/
	protected Map<URI, PropertyDescription> getPropertyDescriptionMap(final Class<?> objectClass, final URFResource resource) throws DataException, InvocationTargetException
	{
		final Map<URI, PropertyDescription> propertyDescriptionMap=new HashMap<URI, PropertyDescription>((int)resource.getPropertyValueCount());	//create a map to hold property descriptions, with a least enough capacity to hold descriptions for all properties
		for(final URFProperty property:resource.getProperties())	//for each resource property
		{
			final URI propertyURI=property.getPropertyURI();	//get the property URI
			if(DEFAULT_NAMESPACE_URI.equals(getNamespaceURI(propertyURI)))	//if this property is in the default namespace
			{
				final String propertyName=getLocalName(property.getPropertyURI());	//get the local name of the property
				final PropertyDescription propertyDescription=getPropertyDescription(objectClass, propertyName, property.getValue());	//get a description for this property
				if(propertyDescription!=null)	//if this was a recognized property
				{
					propertyDescriptionMap.put(propertyURI, propertyDescription);	//store this property description in the map
				}
			}
		}
		return propertyDescriptionMap;	//return the property description map
	}
	
	/**Gets a description of a property of the object based upon the given property name and value.
	An URF property is considered to be a PLOOP property the property is in the namespace indicatd by the class package
	and there the class has either a getter or setter for the URF property's local name.
	The returned property description will indicate a setter method if the property is settable.
	@param objectClass The class of the object to be updated.
	@param propertyName The name of the property potentially representing an object property.
	@param propertyValueResource The value of the URF property potentially representing a object property.
	@return A description of the property, or <code>null</code> if the property is not recognized.
	@exception NullPointerException if the given object class and/or property name is <code>null</code>.
 	@exception DataException if a resource does not specify Java type information.
 	@exception DataException if a resource is a Java-typed resource the class of which cannot be found.
 	@exception DataException if a resource indicates a Java class that has no appropriate constructor.
 	@exception DataException if a resource indicates a Java class that is an interface or an abstract class.
 	@exception DataException if a resource indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if a resource indicates a Java class the constructor of which throws an exception.
	*/
	protected PropertyDescription getPropertyDescription(final Class<?> objectClass, final String propertyName, final URFResource propertyValueResource) throws DataException, InvocationTargetException
	{
		Object propertyValue=getObject(propertyValueResource);	//get the appropriate value for the property TODO get the type and save it somewhere, because this may return null
		final Class<?> propertyValueType=propertyValue.getClass();	//get the type of the value
			//try to find a compatible setter method; get the variable name from each supposed setter in case the setter has multiple capital letters, such as setID()
		final Method[] methods=objectClass.getMethods();	//get all the class methods
		for(final Method method:methods)	//for each method
		{
			if(propertyName.equals(getSetterPropertyName(method.getName())))	//if we could consider this method a setter for the variable we have 
			{
				final Class<?>[] parameterTypes=method.getParameterTypes();	//get the parameter types for this method
				if(parameterTypes.length==1)	//if this setter has one parameter
				{
					final Class<?> parameterType=parameterTypes[0];	//get the single parameter type
					final Object value=convertObject(propertyValue, parameterType);	//convert the object to the correct type
					if(value!=null)	//if we found a parameter to use for this method
					{
						return new PropertyDescription(parameterType, value, method);	//return a description of this property with the method and parameter
					}
				}
			}
		}
			//if no setter could be found, try to find a getter method to verify this is a property that can be set
			//get the variable name from each supposed getter in case the getter has multiple capital letters, such as getID()
		for(final Method method:methods)	//for each method
		{
			if(propertyName.equals(getGetterPropertyName(method.getName())))	//if we could consider this method a getter for the variable we have 
			{
				final Class<?> returnType=method.getReturnType();	//get the return type of the getter
				final Object value=convertObject(propertyValue, returnType);	//convert the object to the getter return type, if we can
				if(value!=null)	//if we can convert the property value to the getter return type
				{
					return new PropertyDescription(value!=null ? value.getClass() : returnType, value);	//return a description of this property with just the value TODO see why covariant return types aren't working correctly; for now, we'll just get the value type directly
				}
			}
		}
		return null;	//indicate that we don't recognize this property
	}

	/**Converts an object to the correct Java type if possible.
	If the object is already of the correct type, no action occurs.
	The following types can be converted to the listed types:
	<dl>
		<dt>{@link Boolean}</dt> <dd><code>boolean</code></dd>
		<dt>{@link Character}</dt> <dd><code>char</code></dd>
		<dt>{@link Number}</dt> <dd>{@link Long}, <code>long</code>, {@link Integer}, <code>int</code>, {@link Double}, <code>double</code>, {@link Float}, <code>float</code></dd>
		<dt>{@link String}</dt> <dd><code>char[]</code>, {@link Color}, {@link Enum}, {@link Pattern}, {@link URIPath}</dd>
	</dl>
	@param object The object to convert.
	@param requiredType The required type of the object.
	@return The object as the required type, or <code>null</code> if the object cannot be converted to the required type.
	@exception NullPointerException if the given object is <code>null</code>.
	@exception DataException if the given object should be able to be converted to the required type but something about its state, format, or contents prevented the conversion.
	*/
	protected Object convertObject(final Object object, final Class<?> requiredType) throws DataException	//TODO search for a string constructor or a static valueOf() method
	{
		final Class<?> objectType=object.getClass();	//get the type of the object
		if(requiredType.isAssignableFrom(objectType))	//if we expect this type (this algorithm could be improved to first try to find an exact match and then find a convertible match)
		{
			return object;	//use the object as-is
		}
		else	//if we expect for another object type
		{
			try
			{
				if(object instanceof Boolean)	//if the object is a Boolean
				{
					if(Boolean.TYPE.equals(requiredType))	//if the required type is boolean (we already checked for Boolean when we checked to see if the types were the same)
					{
						return object;	//return the Boolean object
					}
				}
				else if(object instanceof Character)	//if the object is a Character
				{
					if(Character.TYPE.equals(requiredType))	//if the required type is char (we already checked for Character when we checked to see if the types were the same)
					{
						return object;	//return the Character object
					}
				}
				else if(object instanceof Number)	//if the object is a Number
				{
					if(Long.TYPE.equals(requiredType) || Long.class.isAssignableFrom(requiredType))	//if the required type is long or Long 
					{
						return object instanceof Long ? object : Long.valueOf(((Number)object).longValue());	//return a Long version of the object
					}
					else if(Integer.TYPE.equals(requiredType) || Integer.class.isAssignableFrom(requiredType))	//if the required type is integer or integer 
					{
						return object instanceof Integer ? object : Integer.valueOf(((Number)object).intValue());	//return an Integer version of the object
					}
					else if(Double.TYPE.equals(requiredType) || Double.class.isAssignableFrom(requiredType))	//if the required type is double or Double
					{
						return object instanceof Double ? object : Double.valueOf(((Number)object).doubleValue());	//return a Double version of the object
					}
					else if(Float.TYPE.equals(requiredType) || Float.class.isAssignableFrom(requiredType))	//if the required type is float or Float
					{
						return object instanceof Double ? object : Float.valueOf(((Number)object).floatValue());	//return a Float version of the object
					}
					//TODO add BigInteger and BigDecimal types
				}
				else if(object instanceof String)	//if the object is a string, see if we can convert it to the correct type
				{
					final String stringObject=(String)object;	//cast the value to a String
					if(requiredType.isArray() && Character.TYPE.equals(requiredType.getComponentType()))	//if the required type is a character array
					{
						return stringObject.toCharArray();	//return the string as a character array
					}
/*TODO fix for Guise
					else if(Color.class.isAssignableFrom(requiredType))	//if the required type is Color
					{
						return AbstractModeledColor.valueOf(stringObject);	//compile a color from the string
					}
*/
					else if(Pattern.class.isAssignableFrom(requiredType))	//if the required type is Pattern
					{
						return Pattern.compile(stringObject);	//compile a pattern from the string
					}
					else if(URIPath.class.isAssignableFrom(requiredType))	//if the required type is URIPath
					{
						return new URIPath(stringObject);	//create a URI path from the string
					}
				}
			}
			catch(final IllegalArgumentException illegalArgumentException)	//if there was a conversion error
			{
				throw new DataException(illegalArgumentException);	//throw a data error
			}
		}
		return null;	//indicate we couldn't get an object of the correct type
	}

	/**Property information for an object's property.
	The information indicates the value of a property, and may also indicate a method for setting the given property.
	@author Garret Wilson
	 */
	protected static class PropertyDescription
	{

		/**The class representing the property type.*/
		private final Class<?> propertyClass;

			/**@return The class representing the property type.*/
			public Class<?> getPropertyClass() {return propertyClass;}

		/**The property value.*/
		private final Object value;

			/**@return The property value.*/
			public Object getValue() {return value;}

		/**The setter method to be invoked, or <code>null</code> if no setting method is known.*/
		private final Method setter;

			/**@return The setter method to be invoked, or <code>null</code> if no setting method is known.*/
			public Method getSetter() {return setter;}

		/**Value constructor.
		@param propertyClass The class representing the property type.
		@param value The property value.
		@exception NullPointerException if the given property URI and/or property class is <code>null</code>.
		*/
		public PropertyDescription(final Class<?> propertyClass, final Object value)
		{
			this(propertyClass, value, null);	//construct the class with no setter
		}

		/**Setter and value constructor.
		@param propertyURI The URI identifying the property.
		@param setter The setter method to be invoked, or <code>null</code> if no setting method is known.
		@param value The property value.
		@exception NullPointerException if the given property URI and/or property class is <code>null</code>.
		*/
		public PropertyDescription(final Class<?> propertyClass, final Object value, final Method setter)
		{
			this.propertyClass=checkInstance(propertyClass, "Property class cannot be null.");
			this.setter=setter;
			this.value=value;
		}

		/**@return A string representation of this property description.*/
		public String toString()
		{
			final StringBuilder stringBuilder=new StringBuilder();	//create a string builder for constructing the string
			final Object value=getValue();	//get this property's value
			if(value!=null)	//if there is a value
			{
				stringBuilder.append(value).append(' ');	//show a string representation of the value
			}
			stringBuilder.append('(').append(getPropertyClass()).append(')');	//(propertyClass)
			return stringBuilder.toString();	//return the string we constructed
		}
	}

}
