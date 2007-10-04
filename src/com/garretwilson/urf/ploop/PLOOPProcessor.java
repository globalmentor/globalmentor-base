package com.garretwilson.urf.ploop;

import java.lang.reflect.*;
import java.net.URI;
import java.util.*;
import static java.util.Collections.*;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.regex.Pattern;

import static com.garretwilson.lang.ClassUtilities.*;
import static com.garretwilson.lang.EnumUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.net.URIConstants.*;
import com.garretwilson.net.*;
import static com.garretwilson.net.URIUtilities.*;

import com.garretwilson.urf.*;
import com.garretwilson.util.CollectionUtilities;
import com.garretwilson.util.Debug;

import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.ploop.PLOOP.*;

import com.guiseframework.style.AbstractModeledColor;
import com.guiseframework.style.Color;

/**Processes PLOOP objects from an URF data model.
<p>This is a stateful processor and may only be used for one URF data model instance.</p>
<p>This processor is not thread safe.</p>
<p>This processor recognizes the {@link Resource} type; when this class is present with a single URI parameter constructor, that constructor will take precedence using the resource URI value.</p>
<p>This processor also recognizes the {@link URFResource} type and will transfer all non-PLOOP properties when an instance is encountered.</p>
@author Garret Wilson
*/
public class PLOOPProcessor
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
	public PLOOPProcessor(final Object... defaultConstructorArguments)
	{
		addAll(this.defaultConstructorArguments, defaultConstructorArguments);	//add all the given default constructor arguments to our set of default constructor arguments
	}

	/**Retrieves an object to represent the given URF resource.
	If the the object for the resource has already been created, the existing object will be returned.
	Otherwise the object is created using {@link #createObject(URFResource)} and a reference to the created object is stored for later retrieval inside {@link #setObjectProperties(Object, URFResource, Map)}.
	@param resource The resource describing the Java object to be created.
	@return A created and initialized object according to the given resource description. 
 	@exception IllegalArgumentException if a given resource does not specify Java type information.
 	@exception IllegalArgumentException if a given resource is a Java-typed resource the class of which cannot be found.
 	@exception IllegalArgumentException if a given resource indicates a Java class that has no appropriate constructor.
 	@exception IllegalArgumentException if a given resource indicates a Java class that is an interface or an abstract class.
 	@exception IllegalArgumentException if a given resource indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if a given resource indicates a Java class the constructor of which throws an exception.
	*/
	public Object getObject(final URFResource resource) throws InvocationTargetException
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
 	@exception IllegalArgumentException if a given resource does not specify Java type information.
 	@exception IllegalArgumentException if a given resource is a Java-typed resource the class of which cannot be found.
 	@exception IllegalArgumentException if a given resource indicates a Java class that has no appropriate constructor.
 	@exception IllegalArgumentException if a given resource indicates a Java class that is an interface or an abstract class.
 	@exception IllegalArgumentException if a given resource indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if a given resource indicates a Java class the constructor of which throws an exception.
	*/
	public List<Object> getObjects(final URF urf) throws InvocationTargetException
	{
		final List<Object> objects=new ArrayList<Object>();	//create a new list of objects
		for(final URFResource resource:urf.getResources())	//for each resource
		{
			boolean hasJavaType=false;	//we'll see if this resource has a Java type
			final Iterator<URFResource> typeIterator=resource.getTypes().iterator();	//get an iterator to the types
			while(!hasJavaType && typeIterator.hasNext())	//while we haven't found a Java type and there are other types left
			{
				final URI typeURI=typeIterator.next().getURI();	//get the next type URI
				if(typeURI!=null && isInfoNamespace(typeURI, INFO_SCHEME_JAVA_NAMESPACE))	//if this is an info:java/ URI
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
 	@exception IllegalArgumentException if a given resource does not specify Java type information.
 	@exception IllegalArgumentException if a given resource is a Java-typed resource the class of which cannot be found.
 	@exception IllegalArgumentException if a given resource indicates a Java class that has no appropriate constructor.
 	@exception IllegalArgumentException if a given resource indicates a Java class that is an interface or an abstract class.
 	@exception IllegalArgumentException if a given resource indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if a given resource indicates a Java class the constructor of which throws an exception.
	*/
	public <T> T getObject(final URF urf, final Class<T> type) throws InvocationTargetException
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
		<li>If the resource specifies a Java type, the indicated Java class is instantiated and initialized from the resource description.</li>
		<li>If the resource otherwise indicates a value that can be represented by a Java object (such as an integer), such an object will be returned.</li>
	</ul>
	If the resource does not meet any of the above criteria, an {@link IllegalArgumentException} is thrown.
	@param urfResource The URF resource describing the Java object to be created.
	@return A created and initialized object according to the given resource description. 
 	@exception IllegalArgumentException if the given resource does not specify Java type information.
 	@exception IllegalArgumentException if the given resource is a Java-typed resource the class of which cannot be found.
 	@exception IllegalArgumentException if the given resource indicates a Java class that has no appropriate constructor.
 	@exception IllegalArgumentException if the given resourceindicates a Java class that is an interface or an abstract class.
 	@exception IllegalArgumentException if the given resourceindicates a Java class the constructor of which is not accessible.
	@exception IllegalStateException If a particular property could not be accessed.
	@exception InvocationTargetException if the given resource indicates a Java class the constructor of which throws an exception.
	@see #convertObject(Object, Class)
	*/
	protected Object createObject(final URFResource resource) throws InvocationTargetException
	{
		if(resource instanceof URFListResource)	//if the object is an URF array
		{
System.out.println("ready to create a PLOOP list");
Debug.trace("ready to create a PLOOP list");
			final URFListResource<?> urfListResource=(URFListResource<?>)resource;	//cast the object to a list
				//TODO maybe get a read lock on the list
			final List<Object> list=new ArrayList<Object>();	//create a new list TODO eventually create a list but later check to see if the setter will accept a collection
			for(final URFResource urfListElement:urfListResource)	//for each URF resource in the list
			{
				final Object object=getObject(urfListElement);
System.out.println("converted list element "+urfListElement+" to "+object);
Debug.trace("converted list element", urfListElement, "to", object);
				list.add(object);	//get or create an object from this URF list element and add it to our list
//TODO bring back				list.add(getObject(urfListElement));	//get or create an object from this URF list element and add it to our list
			}
			return list;	//return the list of objects we created
		}
		else if(resource instanceof URFSetResource)	//if the object is an URF set
		{
			final URFSetResource<?> urfSetResource=(URFSetResource<?>)resource;	//cast the object to a set
				//TODO maybe get a read lock on the set
			final Set<Object> set=new HashSet<Object>();	//create a new set TODO eventually create a list but later check to see if the setter will accept a collection
			for(final URFResource urfSetElement:urfSetResource)	//for each URF resource in the set
			{
				set.add(getObject(urfSetElement));	//get or create an object from this URF set element and add it to our set
			}
			return set;	//return the set of objects we created
		}
		else	//if this is another type of resource, check the types for a Java type
		{
//TODO del Debug.trace("Creating object for URF object", URFUtilities.toString(resource));
			Class<?> valueClass=null;	//we'll try to find a Java class from one of the types
			final Iterator<URFResource> typeIterator=resource.getTypes().iterator();	//get an iterator to the types
			while(valueClass==null && typeIterator.hasNext())	//while we haven't found a value class and we're not out of types
			{
				final URFResource type=typeIterator.next();	//get the next type
				try
				{
					valueClass=asClass(type);	//try to get a class from the type
				}
				catch(final ClassNotFoundException classNotFoundException)
				{
					throw new IllegalArgumentException(classNotFoundException);
				}
			}
//		TODO del Debug.trace("Loading class", valueClassName);
//TODO del			final Object value;	//we'll determine the value by invoking the constructor
			if(valueClass!=null)	//if we know the value class, try to constructd a Java object
			{
				final Map<URI, PropertyDescription> propertyDescriptionMap=getPropertyDescriptionMap(valueClass, resource);	//get the property descriptions from the resource description
				final Constructor<?>[] constructors=valueClass.getConstructors();	//get all available constructors
				if(Resource.class.isAssignableFrom(valueClass))	//if the value class is a Resource, see if we can create it with a single URI
				{
					final Constructor<?> constructor=getCompatibleConstructor(valueClass, URI.class);	//see if there is a single URI parameter constructor
					if(constructor!=null)	//if there is a single URI parameter constructor for the Resource
					{
						try
						{
							final Object object=constructor.newInstance(resource.getURI());	//invoke the constructor with the resource reference URI
							setObjectProperties(object, resource, propertyDescriptionMap);	//initialize the object with the properties
							return object;	//return the constructed and initialized object
						}
						catch(final InstantiationException instantiationException)
						{
							throw new IllegalArgumentException(instantiationException);
						}
						catch(final IllegalAccessException illegalAccessException)
						{
							throw new IllegalArgumentException(illegalAccessException);
						}							
					}
				}
					//for all non-Resource types
				final List<PropertyDescription> readOnlyProperties=new ArrayList<PropertyDescription>(propertyDescriptionMap.size());	//the set of read-only properties, which we may use in the constructor
				for(final PropertyDescription propertyDescription:propertyDescriptionMap.values())	//for each property description
				{
Debug.trace("to determine read-only properties, looking at property description for", propertyDescription);
					if(propertyDescription.getSetter()==null)	//if there is no setter for this property, it is a read-only property; save it in case we can use it for the constructor
					{
Debug.trace("this is a read-only property");
						readOnlyProperties.add(propertyDescription);	//add this property to the list of read-only properties
					}
				}
	//TODO del when works				readOnlyProperties.add(new PropertyDescription(URFUtilities.createReferenceURI(GUISE_PROPERTY_NAMESPACE_URI, "session"), GuiseSession.class, getSession()));	//artificially populate the read-only property with a session TODO refactor this to allow such variables to be specfified in a general way
				
				int maxParameterCount=0;	//we'll determine the maximum number of parameters available
				for(final Constructor<?> constructor:constructors)	//look at each constructor to find one with the correct number of parameters
				{
					final Class<?>[] parameterTypes=constructor.getParameterTypes();	//get the parameter types for this constructor
					final int parameterCount=parameterTypes.length;	//see how how many parameters this constructor has
					if(parameterCount>maxParameterCount)	//if this parameter count is more than we know about
					{
						maxParameterCount=parameterCount;	//update the maximum parameter count
					}
				}
	//			TODO del Debug.trace("ready to create object of type", valueClassName, "with constructors with max parameters", maxParameterCount);
				for(int parameterCount=0; parameterCount<=maxParameterCount; ++parameterCount)	//find a constructor with the least number of parameters, starting with the default constructor, until we exhaust the available constructors
				{
					for(final Constructor<?> constructor:constructors)	//look at each constructor to find one with the correct number of parameters
					{
						final Class<?>[] parameterTypes=constructor.getParameterTypes();	//get the parameter types for this constructor
						if(parameterTypes.length==parameterCount)	//if this constructor has the correct number of parameters
						{
Debug.trace("Looking at constructor with parameter count:", parameterCount);
							boolean foundArguments=true;	//start out by assuming the parameters match
							final Object[] arguments=new Object[parameterCount];	//create an array sufficient for the arguments
							for(int parameterIndex=0; parameterIndex<parameterCount && foundArguments; ++parameterIndex)	//for each parameter, as long we we have matching parameters
							{
								final Class<?> parameterType=parameterTypes[parameterIndex];	//get this parameter type
Debug.trace("Parameter", parameterIndex, "type: ", parameterType);
								boolean foundArgument=false;	//we'll try to find an argument
								for(final PropertyDescription propertyDescription:readOnlyProperties)	//look at all the properties to find one for this parameter
								{
Debug.trace("checking to see if this is read-only property value class:", propertyDescription.getPropertyClass());
									if(parameterType.isAssignableFrom(propertyDescription.getPropertyClass()))	//if this read-only property will work for this parameter
									{
Debug.trace("matches!");
										arguments[parameterIndex]=propertyDescription.getValue();	//use this read-only property in the constructor
										foundArgument=true;	//show that we found an argument
										break;	//stop looking for the argument
									}
								}
								if(!foundArgument)	//if there is no read-only property for this constructor argument, check the default arguments
								{
									for(final Object defaultConstructorArgument:getDefaultConstructorArguments())	//for each default constructor argument
									{
										if(parameterType.isAssignableFrom(defaultConstructorArgument.getClass()))	//if this default property argument will work for this parameter
										{
	//										TODO del Debug.trace("matches!");
											arguments[parameterIndex]=defaultConstructorArgument;	//use this default constructor argument in the constructor
											foundArgument=true;	//show that we found an argument
											break;	//stop looking for the argument
										}
									}									
								}
								if(!foundArgument)	//if we couldn't find an argument for this parameter
								{
									foundArguments=false;	//indicate that parameters don't match for this constructor
								}
							}
							if(foundArguments)	//if we found a constructor for which we have arguments
							{
								try
								{
	//								TODO del Debug.trace("found constructor with the following arguments:", ArrayUtilities.toString(arguments));
									final Object object=constructor.newInstance(arguments);	//invoke the constructor with the arguments
									setObjectProperties(object, resource, propertyDescriptionMap);	//initialize the object with the properties
									return object;	//return the constructed and initialized object
								}
								catch(final InstantiationException instantiationException)
								{
									throw new IllegalArgumentException(instantiationException);
								}
								catch(final IllegalAccessException illegalAccessException)
								{
									throw new IllegalArgumentException(illegalAccessException);
								}
							}
						}
					}
				}
				throw new IllegalArgumentException("Value class "+valueClass+" does not have a constructor appropriate for the available read-only properties: "+CollectionUtilities.toString(readOnlyProperties));
			}
			else	//if we don't know the value class, try to create a simple object from the resource
			{
				final Object object=asObject(resource);	//see if we can turn the resource into a simple object
				if(object==null)	//if we couldn't turn the resource into an object
				{
					throw new IllegalArgumentException("Value resource "+resource+" missing type information.");
				}
				return object;	//return the simple object we found
			}
		}
	}

	/**Initializes an object based upon the given description.
	The object being initialized will be stored locally keyed to the resource description for later lookup.
	This implementation also recognizes the {@link URFResource} type and will transfer all non-PLOOP properties when an instance is encountered.
	@param object The object to initialize.
	@param resource The description for the object.
	@exception IllegalArgumentException if a particular value is not an appropriate argument for the corresponding property.
	@exception IllegalStateException If a particular property could not be accessed.
	@exception InvocationTargetException if the given resource indicates a Java class the constructor of which throws an exception.
	@see #setObjectProperties(Object, Map)
	*/
	public void setObjectProperties(final Object object, final URFResource resource) throws InvocationTargetException
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
	@exception IllegalArgumentException if a particular value is not an appropriate argument for the corresponding property.
	@exception IllegalStateException If a particular property could not be accessed.
	@exception InvocationTargetException if the given resource indicates a Java class the constructor of which throws an exception.
	*/
	protected void setObjectProperties(final Object object, final URFResource resource, final Map<URI, PropertyDescription> propertyDescriptionMap) throws InvocationTargetException
	{
//TODO del		for(final Map.Entry<URI, PropertyDescription> propertyDescription:propertyDescriptionMap.entrySet())	//for each property description entry
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
 	@exception IllegalArgumentException if the object has no setter method for the given property.
	@exception IllegalArgumentException if the particular value is not an appropriate argument for the given property.
	@exception IllegalStateException If the given property could not be accessed.
	@exception InvocationTargetException if the given resource indicates a Java class the constructor of which throws an exception.
	*/
	public boolean setObjectProperty(final Object object, final URFResource resource, final String propertyName) throws InvocationTargetException
	{
		final URI propertyURI=getPropertyURI(object, propertyName);	//get the property URI for the given property of the object
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
		throw new IllegalArgumentException("Object "+object+" has no property "+propertyName);
	}

	/**Sets the property of an object using the given setter method and value.
	@param object The object to initialize.
	@param setterMethod The method to use in setting the object property.
	@param value The value to set.
	@exception NullPointerException if the given object and/or setter method is <code>null</code>.
	@exception IllegalArgumentException if the given value is not an appropriate argument for the given property.
	@exception IllegalStateException If the given property could not be accessed.
	@exception InvocationTargetException if the given Java property method throws an exception.
	*/
	public static void setObjectProperty(final Object object, final Method setterMethod, final Object value) throws InvocationTargetException
	{
		try
		{
			setterMethod.invoke(object, value);	//invoke the setter on the object
		}
		catch(final IllegalAccessException illegalAccessException)	//if we can't access this setter
		{
			throw new IllegalStateException(illegalAccessException);
		}
	}
	
	/**Constructs a map of property descriptions for a class based upon a resource description.
	If there are duplicate properties, only one will be stored.
	@param objectClass The class of the object to be constructed.
	@param resource The description fo the object.
	@return A map of property descriptions keyed to property URIs.
	@exception InvocationTargetException if a resource indicates a Java class the constructor of which throws an exception.
	*/
	protected Map<URI, PropertyDescription> getPropertyDescriptionMap(final Class<?> objectClass, final URFResource resource) throws InvocationTargetException
	{
Debug.trace("ready to get property description map");
		final URI namespaceURI=createInfoJavaURI(objectClass.getPackage());	//the URI of the object's class package will be the namespace of the object's class
Debug.trace("namespace URI", namespaceURI);
		final Map<URI, PropertyDescription> propertyDescriptionMap=new HashMap<URI, PropertyDescription>((int)resource.getPropertyCount());	//create a map to hold property descriptions, with a least enough capacity to hold descriptions for all properties
		for(final URFProperty property:resource.getProperties())	//for each resource property
		{
			final URI propertyURI=property.getPropertyURI();	//get the property URI
Debug.trace("property URI", propertyURI, "with namespace", getNamespaceURI(propertyURI));
			if(namespaceURI.equals(getNamespaceURI(propertyURI)))	//if this property is in the class's package namespace
			{
				final String propertyName=getLocalName(property.getPropertyURI());	//get the local name of the property
Debug.trace("property name", propertyName);
				final PropertyDescription propertyDescription=getPropertyDescription(objectClass, propertyName, property.getValue());	//get a description for this property
Debug.trace("got property description:", propertyDescription);
				if(propertyDescription!=null)	//if this was a recognized property
				{
					propertyDescriptionMap.put(propertyURI, propertyDescription);	//store this property description in the map
				}
			}
		}
		return propertyDescriptionMap;	//return the property description map
	}
	
	/**Gets a description of a property of the object based upon the given URF property.
	The returned property description will indicate a method if the property is settable.
	@param objectClass The class of the object to be constructed.
	@param property The URF property pair potentially representing an object property.
	@return A description of the property, or <code>null</code> if the property is not recognized.
	*/
/*TODO del
	protected PropertyDescription getPropertyDescription(final Class<?> objectClass, final URFProperty property)
	{
		final URI propertyURI=property.propertyURI());	//get the URI of the property
//TODO del Debug.trace("looking at property:", propertyURI);
		if(PLOOP_PROPERTY_NAMESPACE_URI.equals(getNamespaceURI(propertyURI)))	//if this is a PLOOP property
		{
			return getPropertyDescription(objectClass, propertyURI, property.getValue());	//return the property description from the property URI and the property value
		}
		return null;	//indicate that we don't recognize this property
	}
*/

	/**Gets a description of a property of the object based upon the given property name and value.
	An URF property is considered to be a PLOOP property the property is in the namespace indicatd by the class package
	and there the class has either a getter or setter for the URF property's local name.
	The returned property description will indicate a setter method if the property is settable.
	@param objectClass The class of the object to be updated.
	@param propertyName The name of the property potentially representing an object property.
//TODO del	@param propertyURI The URI of the URF property potentially representing a Guise object property.
	@param propertyValueResource The value of the URF property potentially representing a object property.
	@return A description of the property, or <code>null</code> if the property is not recognized.
	@exception NullPointerException if the given object class and/or property name is <code>null</code>.
	@exception InvocationTargetException if a resource indicates a Java class the constructor of which throws an exception.
	*/
	protected PropertyDescription getPropertyDescription(final Class<?> objectClass, /*TODO del final URI propertyURI, */final String propertyName, final URFResource propertyValueResource) throws InvocationTargetException
	{
Debug.trace("ready to get property description for property", propertyName, "with resource value", propertyValueResource);
		Object propertyValue=getObject(propertyValueResource);	//get the appropriate value for the property TODO get the type and save it somewhere, because this may return null
		final Class<?> propertyValueType=propertyValue.getClass();	//get the type of the value
//TODO del		final String variableName=getLocalName(propertyURI);	//get the local name of the property
//		TODO del 	Debug.trace("looking at property name:", variableName);
/*TODO fix
			final String setterMethodName=getSetterMethodName(variableName);	//get the setter method name based upon the variable name
//		TODO del Debug.trace("setter: ", setterMethodName);
Debug.trace("setter: ", setterMethodName);
*/
			//try to find a compatible setter method; get the variable name from each supposed setter in case the setter has multiple capital letters, such as setID()
		final Method[] methods=objectClass.getMethods();	//get all the class methods
		for(final Method method:methods)	//for each method
		{
Debug.trace("looking at method:", method.getName());
//TODO del when works				if(method.getName().equals(setterMethodName))	//if this has the setter name
			if(propertyName.equals(getSetterPropertyName(method.getName())))	//if we could consider this method a setter for the variable we have 
			{
Debug.trace("found setter", propertyName);
				final Class<?>[] parameterTypes=method.getParameterTypes();	//get the parameter types for this method
				if(parameterTypes.length==1)	//if this setter has one parameter
				{
Debug.trace("this setter has one param");
					final Class<?> parameterType=parameterTypes[0];	//get the single parameter type
						//TODO don't convert the object if this is a typed literal; instead, accept whatever type was given
					final Object value=convertObject(propertyValue, parameterType);	//convert the object to the correct type
					if(value!=null)	//if we found a parameter to use for this method
					{
Debug.trace("property value has correct type for setter:", parameterType, "property value:", value);
						return new PropertyDescription(parameterType, value, method);	//return a description of this property with the method and parameter
					}
				}
			}
		}
			//if no setter could be found, try to find a getter method to verify this is a property that can be set
			//get the variable name from each supposed getter in case the getter has multiple capital letters, such as getID()
//TODO del when works			final String getterMethodName=getGetterMethodName(variableName);	//get the getter method name based upon the variable name
//TODO del Debug.trace("getter: ", getterMethodName);
Debug.trace("to verify read-only property", propertyName, "look for a getter");
		for(final Method method:methods)	//for each method
		{
//TODO del when works				if(method.getName().equals(getterMethodName) && method.getParameterTypes().length==0)	//if this has the getter name and no parameters
			if(propertyName.equals(getGetterPropertyName(method.getName())))	//if we could consider this method a getter for the variable we have 
			{
				final Class<?> returnType=method.getReturnType();	//get the return type of the getter
Debug.trace("found getter", propertyName, "for class", objectClass, "with return type", returnType);
				final Object value=convertObject(propertyValue, returnType);	//convert the object to the getter return type, if we can
Debug.trace("converted", propertyValue, "to", value);
				if(value!=null)	//if we can convert the property value to the getter return type
				{
//TODO del						Debug.trace("property value has correct type for getter:", returnType, "property value:", value);
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
	@exception IllegalArgumentException if the given object should be able to be converted to the required type but something about its state, format, or contents prevented the conversion.
	@exception NullPointerException if the given object is <code>null</code>.
	*/
	protected Object convertObject(final Object object, final Class<?> requiredType)	//TODO search for a string contructor or a static valueOf() method
	{
		final Class<?> objectType=object.getClass();	//get the type of the object
		if(requiredType.isAssignableFrom(objectType))	//if we expect this type (this algorithm could be improved to first try to find an exact match and then find a convertible match)
		{
//TODO del			Debug.trace("object has correct type:", requiredType);
			return object;	//use the object as-is
		}
		else	//if we expect for another object type
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
				else if(Color.class.isAssignableFrom(requiredType))	//if the required type is Color
				{
					return AbstractModeledColor.valueOf(stringObject);	//compile a color from the string
				}
				else if(Enum.class.isAssignableFrom(requiredType))	//if the required type is an enumeration
				{
//Debug.trace("Creating enum of type", requiredType);
							//TODO document serialized enum form
					return getSerializedEnum((Class<? extends Enum>)requiredType, stringObject);	//get the enum from its serialized form TODO check for an IllegalArgumentException here
				}
/*TODO del if we can
				else if(Class.class.isAssignableFrom(requiredType))	//if the required type is Class
				{
					try
					{
						return Class.forName(stringObject);	//load the given class
					}
					catch(final ClassNotFoundException classNotFoundException)	//if we couldn't find the class
					{
						throw new IllegalArgumentException(classNotFoundException);
					}
				}
*/
/*TODO del
				else if(Locale.class.isAssignableFrom(requiredType))	//if the required type is Locale 
				{
					return createLocale(stringObject);	//construct a Locale from the object, accepting the RFC 1766 syntax as well as the Java syntax
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
				//TODO check for a string-compatible constructor
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

		/**The URI identifying the property.*/
//TODO del		private final URI propertyURI;	//TODO probably remove this

			/**@return The URI identifying the property.*/
//TODO del			public URI getPropertyURI() {return propertyURI;}

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
//TODO del		@param propertyURI The URI identifying the property.
		@param propertyClass The class representing the property type.
		@param value The property value.
		@exception NullPointerException if the given property URI and/or property class is <code>null</code>.
		*/
		public PropertyDescription(/*TODO del final URI propertyURI, */final Class<?> propertyClass, final Object value)
		{
			this(/*TODO del propertyURI, */propertyClass, value, null);	//construct the class with no setter
		}

		/**Setter and value constructor.
		@param propertyURI The URI identifying the property.
		@param setter The setter method to be invoked, or <code>null</code> if no setting method is known.
		@param value The property value.
		@exception NullPointerException if the given property URI and/or property class is <code>null</code>.
		*/
		public PropertyDescription(/*TODO del final URI propertyURI, */final Class<?> propertyClass, final Object value, final Method setter)
		{
//TODO del			this.propertyURI=checkInstance(propertyURI, "Property URI cannot be null.");
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
