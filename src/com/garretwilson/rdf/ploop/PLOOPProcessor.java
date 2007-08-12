package com.garretwilson.rdf.ploop;

import static java.util.Collections.*;
import static com.garretwilson.lang.ClassUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.rdf.RDFUtilities.*;
import static com.garretwilson.rdf.ploop.PLOOPConstants.*;

import java.lang.reflect.*;
import java.net.URI;
import java.util.*;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.regex.Pattern;

import static com.garretwilson.lang.EnumUtilities.*;
import com.garretwilson.net.Resource;
import com.garretwilson.net.URIPath;
import com.garretwilson.rdf.*;
import com.garretwilson.util.*;
import com.guiseframework.style.AbstractModeledColor;
import com.guiseframework.style.Color;

/**Processes PLOOP objects from an RDF data model.
<p>This is a stateful processor and may only be used for one RDF data model instance.</p>
<p>This processor is not thread safe.</p>
<p>This processor can create the following types of objects from strings:</p>
<ul>
	<li><code>boolean</code></li>
	<li>{@link Boolean}</li>
	<li>{@link Class}</li>
	<li><code>double</code></li>
	<li>{@link Double}</li>
	<li>{@link Enum}</li>
	<li><code>float</code></li>
	<li>{@link Float}</li>
	<li><code>int</code></li>
	<li>{@link Integer}</li>
	<li><code>long</code></li>
	<li>{@link Long}</li>
	<li>{@link Pattern}</li>
	<li>{@link Color}</li>
	<li>{@link URI}</li>
	<li>{@link URIPath}</li>
</ul>
<p>This processor recognizes the {@link Resource} type; when this class is present with a single URI parameter constructor, that constructor will take precedence using the <code>rdf:about</code> resource reference URI value.</p>
<p>This processor also recognizes the {@link RDFResource} type and will transfer all non-PLOOP properties when an instance is encountered.</p>
@author Garret Wilson
*/
public class PLOOPProcessor
{
	
		//TODO eventually delete the name property functionality
	
	/**The default arguments that can be used in calling class constructors.*/
	private final Set<Object> defaultConstructorArguments=new CopyOnWriteArraySet<Object>();

		/**@return The default arguments that can be used in calling class constructors.*/
		protected Set<Object> getDefaultConstructorArguments() {return defaultConstructorArguments;}

	/**The class property under which a reference URI fragment should be stored as a name, or <code>null</code> if object naming is not supported.*/
	private String nameProperty=null;

		/**@return The class property under which a reference URI fragment should be stored as a name, or <code>null</code> if object naming is not supported.*/
		public String getNameProperty() {return nameProperty;}

		/**Sets the class property under which a reference URI fragment should be stored as a name.
		@param nameProperty The class property under which a reference URI fragment should be stored as a name, or <code>null</code> if object naming is not supported.
		*/
		public void setNameProperty(final String nameProperty) {this.nameProperty=nameProperty;}

	/**The map of created objects keyed to the resources from which they were created.*/
	protected final Map<RDFResource, Object> resourceObjectMap=new HashMap<RDFResource, Object>();
		
	/**Default arguments constructor.
	@param defaultConstructorArguments The objects that can be used as default arguments in class constructors.
	*/
	public PLOOPProcessor(final Object... defaultConstructorArguments)
	{
		addAll(this.defaultConstructorArguments, defaultConstructorArguments);	//add all the given default constructor arguments to our set of default constructor arguments
	}

	/**Retrieves an object to represent the given RDF object.
	The RDF object must be one of the following:
	<dl>
		<dt>{@link RDFLiteral}</dt> <dd>Currently returns the string version of the literal's lexical form, regardless of the literal type.</dd>
		<dt>{@link RDFListResource}</dt> <dd>Returns a {@link List} recursively containing objects defined by the list's contents.</dd>
		<dt>{@link RDFResource}</dt> <dd>Returns an object based upon the <code>java:</code> type information.
	</dl>
	If the RDF object is a resource for which an object has already been created, the existing object will be returned.
	Otherwise the object is created using {@link #createObject(RDFObject)} and a reference to the created object is stored for later retrieval inside {@link #setObjectProperties(Object, RDFResource, Map)}.
	@param rdfObject The RDF object describing the Java object to be created.
	@return A created and initialized object according to the given RDF description. 
 	@exception IllegalArgumentException if the given RDF object is a resource that does not specify Java type information.
 	@exception IllegalArgumentException if the given RDF object is a Java-typed resource the class of which cannot be found.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class that has no appropriate constructor.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class that is an interface or an abstract class.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if the given RDF object indicates a Java class the constructor of which throws an exception.
	*/
	public Object getObject(final RDFObject rdfObject) throws InvocationTargetException
	{
		Object object=rdfObject instanceof RDFResource ? resourceObjectMap.get((RDFResource)rdfObject) : null;	//look up an existing object if the description is a resource
		if(object==null)	//if we don't have an object already
		{
			object=createObject(rdfObject);	//create the object from the description
		}
		return object;	//return the created object
	}

	/**Retrieves objects representing the given RDF instance.
	Objects for all Java typed resources will be returned. This is a convenient way to ensure all Java classes described by an RDF instance have been created.
	If the RDF object is a resource for which an object has already been created, the existing object will be returned.
	Otherwise the object is created using {@link #createObject(RDFObject)} and a reference to the created object is stored for later retrieval inside {@link #setObjectProperties(Object, RDFResource, Map)}.
	@param rdf The RDF instance describing the Java objects to be created.
	@return A list of created and initialized objects according to the given RDF description. 
 	@exception IllegalArgumentException if the given RDF object is a resource that does not specify Java type information.
 	@exception IllegalArgumentException if the given RDF object is a Java-typed resource the class of which cannot be found.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class that has no appropriate constructor.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class that is an interface or an abstract class.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if the given RDF object indicates a Java class the constructor of which throws an exception.
	*/
	public List<Object> getObjects(final RDF rdf) throws InvocationTargetException
	{
		final List<Object> objects=new ArrayList<Object>();	//create a new list of objects
		for(final RDFResource resource:rdf.getResources())	//for each resource
		{
			boolean hasJavaType=false;	//we'll see if this resource has a Java type
			final Iterator<RDFObject> typeIterator=getTypes(resource).iterator();	//get an iterator to all the resource types
			while(!hasJavaType && typeIterator.hasNext())	//while we haven't found a Java type and there are other types left
			{
				final RDFObject typeResource=typeIterator.next();	//get the next type
				if(typeResource instanceof RDFResource)	//if the type is an RDF resource (it always should be)
				{
					final URI typeURI=((RDFResource)typeResource).getReferenceURI();	//get the type URI
					if(JAVA_SCHEME.equals(typeURI.getScheme()))	//if the type is a Java type
					{
						hasJavaType=true;	//show that this resource has a Java type
					}
				}
			}
			if(hasJavaType)	//if this resource has a Java type
			{
				objects.add(getObject(resource));	//get the instance of this resource
			}
		}
		return objects;
	}

	/**Retrieves an object of the given type or subtype from the given RDF instance.
	All Java classes described by an RDF instance will first be ensured to have been created.
	@param rdf The RDF instance describing the Java objects to be created.
	@param type The type of object to return.
	@return A created and initialized object according to the given RDF description of the first object of the given type, or <code>null</code> if no object of the given type is described in the RDF instance. 
 	@exception IllegalArgumentException if the given RDF object is a resource that does not specify Java type information.
 	@exception IllegalArgumentException if the given RDF object is a Java-typed resource the class of which cannot be found.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class that has no appropriate constructor.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class that is an interface or an abstract class.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class the constructor of which is not accessible.
	@exception InvocationTargetException if the given RDF object indicates a Java class the constructor of which throws an exception.
	*/
	public <T> T getObject(final RDF rdf, final Class<T> type) throws InvocationTargetException
	{
		for(final Object object:getObjects(rdf))	//get all objects and look at each one of them
		{
			if(type.isInstance(object))	//if the object is an instance of the type
			{
				return type.cast(object);	//cast the object to the correct type and return it
			}
		}
		return null;	//indicate that no matching object could be found
	}

	/**Creates and initializes an object to represent the given RDF object.
	The RDF object must be one of the following:
	<dl>
		<dt>{@link RDFLiteral}</dt> <dd>Currently returns the string version of the literal's lexical form, regardless of the literal type.</dd>
		<dt>{@link RDFListResource}</dt> <dd>Returns a {@link List} recursively containing objects defined by the list's contents.</dd>
		<dt>{@link RDFResource}</dt> <dd>Returns an object based upon the <code>java:</code> type information.
	</dl>
	@param rdfObject The RDF object describing the Java object to be created.
	@return A created and initialized object according to the given RDF description. 
 	@exception IllegalArgumentException if the given RDF object is a resource that does not specify Java type information.
 	@exception IllegalArgumentException if the given RDF object is a Java-typed resource the class of which cannot be found.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class that has no appropriate constructor.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class that is an interface or an abstract class.
 	@exception IllegalArgumentException if the given RDF object indicates a Java class the constructor of which is not accessible.
	@exception IllegalStateException If a particular property could not be accessed.
	@exception InvocationTargetException if the given RDF object indicates a Java class the constructor of which throws an exception.
	*/
	protected Object createObject(final RDFObject rdfObject) throws InvocationTargetException
	{
		if(rdfObject instanceof RDFLiteral)	//if the object is a literal
		{
			final RDFLiteral rdfLiteral=(RDFLiteral)rdfObject;	//cast the object to a literal
			final String literalLexicalForm=rdfLiteral.getLexicalForm();	//get the lexical form of the typed literal
			if(rdfLiteral instanceof RDFTypedLiteral)	//if this is a typed literal
			{
				final RDFTypedLiteral<?> rdfTypedLiteral=(RDFTypedLiteral<?>)rdfLiteral;	//cast the literal to a typed literal
				final URI datatypeURI=rdfTypedLiteral.getDatatypeURI();	//get the datatype URI
				if(JAVA_SCHEME.equals(datatypeURI.getScheme()))	//if the datatype is a Java type
				{
					final String datatypeClassName=datatypeURI.getSchemeSpecificPart();	//get the class name part of the type
					try
					{
						final Class<?> datatypeClass=Class.forName(datatypeClassName);	//load the class
						return convertObject(literalLexicalForm, datatypeClass);	//convert the lexical form of the literal to the correct type
					}
					catch(final ClassNotFoundException classNotFoundException)	//if we couldn't find the class
					{
						throw new IllegalArgumentException(classNotFoundException);
					}
				}
				else	//if the datatype is not a Java type TODO add support for automatic conversion from W3C primitive types
				{
					throw new IllegalArgumentException("Unsupported typed literal "+rdfLiteral);
				}
			}
			else	//if this is not a typed literal
			{
				return literalLexicalForm;	//return the lexical form of the typed literal
			}
		}
		else if(rdfObject instanceof RDFListResource)	//if the object is a list
		{
			final RDFListResource rdfListResource=(RDFListResource)rdfObject;	//cast the object to a list
			final List<Object> list=new ArrayList<Object>(rdfListResource.size());	//create a new list of the correct size TODO eventually create a list but later check to see if the setter will accept a collection
			for(final RDFObject rdfListItem:rdfListResource)	//for each RDF resource in the list
			{
				list.add(getObject(rdfListItem));	//get or create an object from this RDF list item and add it to our list
			}
			return list;	//return the list of objects we created
		}
		else if(rdfObject instanceof RDFResource)	//if the object is a resource
		{
			final RDFResource resource=(RDFResource)rdfObject;	//cast the object to a resource
//TODO del Debug.trace("Creating object for RDF object", RDFUtilities.toString(resource));
			String valueClassName=null;	//we'll try to find a Java class name part of the type
//			RDFResource typeResource=null;	//we'll try to find a java: type
			int typeCount=0;	//keep track of the number of types
			final Iterator<RDFObject> typeIterator=getTypes(resource).iterator();	//get an iterator to all the resource types
			while(valueClassName==null && typeIterator.hasNext())	//while we haven't found a type URI and there are other types left
			{
				++typeCount;	//indicate that we found another type
				final RDFObject typeResource=typeIterator.next();	//get the next type
				if(typeResource instanceof RDFResource)	//if the type is an RDF resource (it always should be)
				{
					final URI typeURI=((RDFResource)typeResource).getReferenceURI();	//get the type URI
					if(JAVA_SCHEME.equals(typeURI.getScheme()))	//if the type is a Java type
					{
						valueClassName=typeURI.getSchemeSpecificPart();	//get the class name part of the type
					}
				}
			}
			if(valueClassName==null)	//if we don't know the type of the resource
			{
				if(typeCount==0)	//if there were no types indicated, assume this is a proxy for the real value in the rdf:value property TODO document
				{
					final RDFObject valuePropertyObject=getValue(resource);	//get the value of the rdf:value property, if there is one
					if(valuePropertyObject!=null)	//if there is a value specified
					{
						return createObject(valuePropertyObject);	//retrieve a value from the value
					}
				}
				throw new IllegalArgumentException("Value resource "+resource+" missing type information.");
				//TODO this will also happen if an rdf:nodeID has been used that doesn't reference anything, so maybe indicate that possibility in the error message
			}
//		TODO del Debug.trace("Loading class", valueClassName);
//TODO del			final Object value;	//we'll determine the value by invoking the constructor
			try
			{
				final Class<?> valueClass=Class.forName(valueClassName);	//load the class
				final Map<URI, PropertyDescription> propertyDescriptionMap=getPropertyDescriptionMap(valueClass, resource);	//get the property descriptions from the resource description

				final Constructor<?>[] constructors=valueClass.getConstructors();	//get all available constructors
				
					//see if there is an rdf:value
				final RDFObject valuePropertyObject=getValue(resource);	//get the value of the rdf:value property, if there is one
				if(valuePropertyObject instanceof RDFLiteral)	//if there is is a literal property value for rdf:value
				{
//TODO del if not wanted					final String[] valueTokens=valuePropertyLiteral.toString().split("[ ,:'_-]");	//split the value into tokens TODO use a constant
					final String[] valueTokens=((RDFLiteral)valuePropertyObject).getLexicalForm().split(",");	//split the value into tokens TODO use a constant
//TODO del					Constructor<?> constructor=null;	//try to find a constructor
					final int parameterCount=valueTokens.length;	//see how many parameters to expect
					final Class<?>[] strictParameterTypes=ArrayUtilities.createArray(String.class.getClass(), String.class);	//create an array of string classes of the appropriate type so that we can check for a strictly-matching constructor TODO document that string constructors take precedent
					final Constructor<?> strictConstructor=getCompatibleConstructor(valueClass, strictParameterTypes);	//try to find a constructor that strictly matches
					if(strictConstructor!=null)	//if we found a strictly-matching constructor
					{
						try
						{
//								TODO del Debug.trace("found constructor with the following arguments:", ArrayUtilities.toString(arguments));
							final Object object=strictConstructor.newInstance((Object[])valueTokens);	//invoke the constructor with the value tokens as-is
							setObjectProperties(object, resource, propertyDescriptionMap);	//initialize the object with the properties
							return object;	//return the constructed and initialized object
						}
						catch(final InstantiationException instantiationException)	//TODO move all of these exception checks
						{
							throw new IllegalArgumentException(instantiationException);
						}
						catch(final IllegalAccessException illegalAccessException)
						{
							throw new IllegalArgumentException(illegalAccessException);
						}						
					}
					else	//if we didn't find a strictly-matching constructor, see if we can do conversions
					{
						for(final Constructor<?> constructor:constructors)	//look at each constructor to find one with the correct number of parameters
						{
							final Class<?>[] parameterTypes=constructor.getParameterTypes();	//get the parameter types for this constructor
							if(parameterTypes.length==parameterCount)	//if this constructor has the correct number of parameters
							{
	//						TODO del Debug.trace("Looking at constructor with parameter count:", parameterCount);
								boolean foundArguments=true;	//start out by assuming the parameters match
								final Object[] arguments=new Object[parameterCount];	//create an array sufficient for the arguments
								for(int parameterIndex=0; parameterIndex<parameterCount && foundArguments; ++parameterIndex)	//for each parameter, as long we we have matching parameters
								{
									final Class<?> parameterType=parameterTypes[parameterIndex];	//get this parameter type
	//							TODO del Debug.trace("Parameter", parameterIndex, "type: ", parameterType);
									final Object argument=convertObject(valueTokens[parameterIndex], parameterType);	//convert the object to the correct type
									if(argument!=null)	//if we successfully converted this constructor argument
									{
										arguments[parameterIndex]=argument;	//store the argument
									}
									else	//if we couldn't convert this constructor argument
									{
										foundArguments=false;	//show that we couldn't convert this argument
										break;	//stop looking at this constructor
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
					throw new IllegalArgumentException("Value class "+valueClassName+" does not have a constructor appropriate for the specified rdf:value: "+valuePropertyObject.toString());
				}
				else	//if there is no rdf:value literal property value
				{
						//see if this is a Resource with a single URI parameter constructor
					if(Resource.class.isAssignableFrom(valueClass))	//if the value class is a Resource, see if we can
					{
						final Constructor<?> constructor=getCompatibleConstructor(valueClass, URI.class);	//see if there is a single URI parameter constructor
						if(constructor!=null)	//if there is a single URI parameter constructor for the Resource
						{
							try
							{
								final Object object=constructor.newInstance(resource.getReferenceURI());	//invoke the constructor with the resource reference URI
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
	//TODO del Debug.trace("to determine read-only properties, looking at property description for", propertyDescription.getPropertyURI());
						if(propertyDescription.getSetter()==null)	//if there is no setter for this property, it is a read-only property; save it in case we can use it for the constructor
						{
	//TODO del Debug.trace("this is a read-only property");
							readOnlyProperties.add(propertyDescription);	//add this property to the list of read-only properties
						}
					}
	//TODO del when works				readOnlyProperties.add(new PropertyDescription(RDFUtilities.createReferenceURI(GUISE_PROPERTY_NAMESPACE_URI, "session"), GuiseSession.class, getSession()));	//artificially populate the read-only property with a session TODO refactor this to allow such variables to be specfified in a general way
					
					
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
	//						TODO del Debug.trace("Looking at constructor with parameter count:", parameterCount);
								boolean foundArguments=true;	//start out by assuming the parameters match
								final Object[] arguments=new Object[parameterCount];	//create an array sufficient for the arguments
								for(int parameterIndex=0; parameterIndex<parameterCount && foundArguments; ++parameterIndex)	//for each parameter, as long we we have matching parameters
								{
									final Class<?> parameterType=parameterTypes[parameterIndex];	//get this parameter type
	//							TODO del Debug.trace("Parameter", parameterIndex, "type: ", parameterType);
									boolean foundArgument=false;	//we'll try to find an argument
									for(final PropertyDescription propertyDescription:readOnlyProperties)	//look at all the properties to find one for this parameter
									{
	//								TODO del Debug.trace("checking read-only property:", propertyDescription.getPropertyClass());
										if(parameterType.isAssignableFrom(propertyDescription.getPropertyClass()))	//if this read-only property will work for this parameter
										{
	//									TODO del Debug.trace("matches!");
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
	/*TODO del; we're now allowing more arguments than just the session
									catch(final IllegalArgumentException illegalArgumentException)	//our Guise session should always work OK---and we shouldn't get this exception for the default constructor
									{
										throw new AssertionError(illegalArgumentException);
									}
	*/
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
					throw new IllegalArgumentException("Value class "+valueClassName+" does not have a constructor appropriate for the available read-only properties.");
				}
			}
			catch(final ClassNotFoundException classNotFoundException)	//if we couldn't find the class
			{
				throw new IllegalArgumentException(classNotFoundException);
			}
		}
		else	//if the object is neither a literal nor a resource
		{
			throw new AssertionError("Unknown RDF object type: "+rdfObject.getClass());
		}
	}

	/**Initializes an object based upon the given description.
	The object being initialized will be stored locally keyed to the resource description for later lookup.
	This implementation also recognizes the {@link RDFResource} type and will transfer all non-PLOOP properties when an instance is encountered.
	@param object The object to initialize.
	@param resource The description for the object.
	@exception IllegalArgumentException if a particular value is not an appropriate argument for the corresponding property.
	@exception IllegalStateException If a particular property could not be accessed.
	@exception InvocationTargetException if the given RDF object indicates a Java class the constructor of which throws an exception.
	@see #setObjectProperties(Object, Map)
	*/
	public void setObjectProperties(final Object object, final RDFResource resource) throws InvocationTargetException
	{
		final Map<URI, PropertyDescription> propertyDescriptionMap=getPropertyDescriptionMap(object.getClass(), resource);	//get property descriptions from the resource description
		setObjectProperties(object, resource, propertyDescriptionMap);	//initialize the object from the property descriptions
	}

	/**Initializes an object based upon the given URI and property descriptions.
	The object being initialized will be stored locally keyed to the resource description for later lookup.
	This implementation also recognizes the {@link RDFResource} type and will transfer all non-PLOOP properties when an instance is encountered.
	@param object The object to initialize.
	@param resource The description for the object.
	@param propertyDescriptionMap The property descriptions for initializing the object.
	@exception IllegalArgumentException if a particular value is not an appropriate argument for the corresponding property.
	@exception IllegalStateException If a particular property could not be accessed.
	@exception InvocationTargetException if the given RDF object indicates a Java class the constructor of which throws an exception.
	*/
	protected void setObjectProperties(final Object object, final RDFResource resource, final Map<URI, PropertyDescription> propertyDescriptionMap) throws InvocationTargetException
	{
		for(final PropertyDescription propertyDescription:propertyDescriptionMap.values())	//for each property description
		{
			final Method setter=propertyDescription.getSetter();	//get the setter method for this property
			if(setter!=null)	//if there is a setter for this property
			{
				setObjectProperty(object, setter, propertyDescription.getValue());	//set the value for the property
			}
		}
		if(object instanceof RDFResource)	//if the object is an RDF resource, add any non-PLOOP properties to the new RDF resource
		{
			for(final RDFPropertyValuePair rdfPropertyValuePair:resource.getProperties())	//for each property of the source resource
			{
				final RDFResource property=rdfPropertyValuePair.getProperty();	//get the property
				final URI propertyURI=property.getReferenceURI();	//get the property URI
				if(!PLOOP_PROPERTY_NAMESPACE_URI.equals(getNamespaceURI(propertyURI)))	//if this isn't a PLOOP property
				{
					((RDFResource)object).addProperty(propertyURI, rdfPropertyValuePair.getValue());	//add this property and value to the created RDF resource TODO see if we can transfer the owner RDF instance when we transfer the value
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
	@exception InvocationTargetException if the given RDF object indicates a Java class the constructor of which throws an exception.
	*/
	public boolean setObjectProperty(final Object object, final RDFResource resource, final String propertyName) throws InvocationTargetException
	{
		final URI propertyURI=createReferenceURI(PLOOP_PROPERTY_NAMESPACE_URI, propertyName);	//create a property URI for the given property
		final RDFObject propertyValueRDFObject=resource.getPropertyValue(propertyURI);	//get the property value
		if(propertyValueRDFObject==null)	//if there is no such value
		{
			return false;	//indicate that there is no such property description
		}
		final PropertyDescription propertyDescription=getPropertyDescription(object.getClass(), propertyURI, propertyValueRDFObject);	//get the property description
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
	@exception InvocationTargetException if the given RDF object indicates a Java class the constructor of which throws an exception.
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
	
	/**Constructs a map of property descriptions for a class.
	If there are duplicate properties, only one will be stored.
	@param objectClass The class of the object to be constructed.
	@param resource The description fo the object.
	@return A map of property descriptions keyed to property URIs.
	@exception InvocationTargetException if the given RDF object indicates a Java class the constructor of which throws an exception.
	*/
	protected Map<URI, PropertyDescription> getPropertyDescriptionMap(final Class<?> objectClass, final RDFResource resource) throws InvocationTargetException
	{
		final Map<URI, PropertyDescription> propertyDescriptionMap=new HashMap<URI, PropertyDescription>(resource.getPropertyCount());	//create a map to hold property descriptions, with a least enough capacity to hold descriptions for all properties
		final Iterator<RDFPropertyValuePair> propertyIterator=resource.getPropertyIterator();	//get an iterator to the resource properties
		while(propertyIterator.hasNext())	//while there are more properties
		{
			final RDFPropertyValuePair property=propertyIterator.next();	//get the next property
//TODO del Debug.trace("filling property description map; looking at property", property.getProperty().getReferenceURI());
			final PropertyDescription propertyDescription=getPropertyDescription(objectClass, property);	//get a description for this property
			if(propertyDescription!=null)	//if this was a recognized property
			{
				propertyDescriptionMap.put(propertyDescription.getPropertyURI(), propertyDescription);	//store this property description in the map
			}
		}
		return propertyDescriptionMap;	//return the property description map
	}
	
	/**Gets a description of a property of the object based upon the given RDF property/value pair.
	The returned property description will indicate a method if the property is settable.
	@param objectClass The class of the object to be constructed.
	@param property The RDF property/value pair potentially representing a Guise object property.
	@return A description of the property, or <code>null</code> if the property is not recognized.
	*/
	protected PropertyDescription getPropertyDescription(final Class<?> objectClass, final RDFPropertyValuePair property) throws InvocationTargetException
	{
//TODO del		final RDFObject propertyValue=property.getValue();	//get the property value
		final URI propertyURI=property.getName().getReferenceURI();	//get the URI of the property
//TODO del Debug.trace("looking at property:", propertyURI);
		if(PLOOP_PROPERTY_NAMESPACE_URI.equals(getNamespaceURI(propertyURI)))	//if this is a PLOOP property
		{
			return getPropertyDescription(objectClass, propertyURI, property.getValue());	//return the property description from the property URI and the property value
		}
		return null;	//indicate that we don't recognize this property
	}

	/**Gets a description of a property of the object based upon the given RDF property/value pair.
	The returned property description will indicate a method if the property is settable.
	@param objectClass The class of the object to be updated.
	@param propertyURI The URI of the RDF property potentially representing a Guise object property.
	@param propertyValueRDFObject The value of the RDF property potentially representing a Guise object property.
	@return A description of the property, or <code>null</code> if the property is not recognized.
	*/
	protected PropertyDescription getPropertyDescription(final Class<?> objectClass, final URI propertyURI, final RDFObject propertyValueRDFObject) throws InvocationTargetException
	{
		final Object propertyValue=getObject(propertyValueRDFObject);	//get the appropriate value for the property TODO get the type and save it somewhere, because this may return null
		final Class<?> propertyValueType=propertyValue.getClass();	//get the type of the value
		final String variableName=getLocalName(propertyURI);	//get the local name of the property
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
//TODO del Debug.trace("looking at method:", method.getName());
//TODO del when works				if(method.getName().equals(setterMethodName))	//if this has the setter name
			if(variableName.equals(getSetterPropertyName(method.getName())))	//if we could consider this method a setter for the variable we have 
			{
//Debug.trace("found setter", variableName);
				final Class<?>[] parameterTypes=method.getParameterTypes();	//get the parameter types for this method
				if(parameterTypes.length==1)	//if this setter has one parameter
				{
//					TODO del Debug.trace("this setter has one param");
					final Class<?> parameterType=parameterTypes[0];	//get the single parameter type
						//TODO don't convert the object if this is a typed literal; instead, accept whatever type was given
					final Object value=convertObject(propertyValue, parameterType);	//convert the object to the correct type
					if(value!=null)	//if we found a parameter to use for this method
					{
//TODO del							Debug.trace("property value has correct type for setter:", parameterType, "property value:", value);
						return new PropertyDescription(propertyURI, parameterType, value, method);	//return a description of this property with the method and parameter
					}
				}
			}
		}
			//if no setter could be found, try to find a getter method to verify this is a property that can be set
			//get the variable name from each supposed getter in case the getter has multiple capital letters, such as getID()
//TODO del when works			final String getterMethodName=getGetterMethodName(variableName);	//get the getter method name based upon the variable name
//TODO del Debug.trace("getter: ", getterMethodName);
		for(final Method method:methods)	//for each method
		{
//TODO del when works				if(method.getName().equals(getterMethodName) && method.getParameterTypes().length==0)	//if this has the getter name and no parameters
			if(variableName.equals(getGetterPropertyName(method.getName())))	//if we could consider this method a getter for the variable we have 
			{
				final Class<?> returnType=method.getReturnType();	//get the return type of the getter
//				TODO del Debug.trace("found getter", getterMethodName, "for class", objectClass, "with return type", returnType);
				final Object value=convertObject(propertyValue, returnType);	//convert the object to the getter return type, if we can
				if(value!=null)	//if we can convert the property value to the getter return type
				{
//TODO del						Debug.trace("property value has correct type for getter:", returnType, "property value:", value);
					return new PropertyDescription(propertyURI, value!=null ? value.getClass() : returnType, value);	//return a description of this property with just the value TODO see why covariant return types aren't working correctly; for now, we'll just get the value type directly
				}
			}
		}
		return null;	//indicate that we don't recognize this property
	}

	/**Converts an object to the correct type.
	If the object is already of the correct type, no action occurs.
	Strings can be converted to the following types of objects:
	<ul>
		<li><code>char[]</code></li>
		<li><code>boolean</code></li>
		<li>{@link Boolean}</li>
		<li>{@link Class}</li>
		<li><code>double</code></li>
		<li>{@link Double}</li>
		<li>{@link Enum}</li>
		<li><code>float</code></li>
		<li>{@link Float}</li>
		<li><code>int</code></li>
		<li>{@link Integer}</li>
		<li><code>long</code></li>
		<li>{@link Long}</li>
		<li>{@link Pattern}</li>
		<li>{@link Color}</li>
		<li>{@link URI}</li>
		<li>{@link URIPath}</li>
	</ul>
	@param object The object to convert.
	@param requiredType The required type of the object.
	@return The object as the required type, or <code>null</code> if the object cannot be converted to the required type.
	@exception IllegalArgumentException if the given object should be able to be converted to the required type but something about its state, format, or contents prevented the conversion.
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
//TODO del			try
			{
				if(object instanceof String)	//if the object is a string, see if we can convert it to the correct type
				{
					final String stringObject=(String)object;	//cast the value to a string
					if(requiredType.isArray() && Character.TYPE.equals(requiredType.getComponentType()))	//if the required type is a character array
					{
						return stringObject.toCharArray();	//return the string as a character array
					}
					else if(Enum.class.isAssignableFrom(requiredType))	//if the required type is an enumeration
					{
	//TODO del Debug.trace("Creating enum of type", requiredType);
								//TODO document serialized enum form
						return getSerializedEnum((Class<? extends Enum>)requiredType, stringObject);	//get the enum from its serialized form TODO check for an IllegalArgumentException here
					}
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
					else if(Boolean.TYPE.equals(requiredType) || Boolean.class.isAssignableFrom(requiredType))	//if the required type is boolean or Boolean
					{
						return Boolean.valueOf(stringObject);	//create a Boolean from the object
					}
					else if(Double.TYPE.equals(requiredType) || Double.class.isAssignableFrom(requiredType))	//if the required type is double or Double
					{
						return Double.valueOf(stringObject);	//create a Double from the object
					}
					else if(Integer.TYPE.equals(requiredType) || Integer.class.isAssignableFrom(requiredType))	//if the required type is int or Integer
					{
						return Integer.valueOf(stringObject);	//create an Integer from the object
					}
	/*TODO del
					else if(Locale.class.isAssignableFrom(requiredType))	//if the required type is Locale 
					{
						return createLocale(stringObject);	//construct a Locale from the object, accepting the RFC 1766 syntax as well as the Java syntax
					}
	*/
					else if(Long.TYPE.equals(requiredType) || Long.class.isAssignableFrom(requiredType))	//if the required type is long or Long 
					{
						return Long.valueOf(stringObject);	//create a Long from the object
					}
					else if(Float.TYPE.equals(requiredType) || Float.class.isAssignableFrom(requiredType))	//if the required type is float or Float
					{
						return Float.valueOf(stringObject);	//create a Float from the object
					}
					else if(Pattern.class.isAssignableFrom(requiredType))	//if the required type is Pattern
					{
						return Pattern.compile(stringObject);	//compile a pattern from the string
					}
					else if(Color.class.isAssignableFrom(requiredType))	//if the required type is Color
					{
						return AbstractModeledColor.valueOf(stringObject);	//compile a color from the string
					}
					else if(URI.class.isAssignableFrom(requiredType))	//if the required type is URI TODO maybe change to using the string constructor
					{
						return URI.create(stringObject);	//create a URI from the string
					}
					else if(URIPath.class.isAssignableFrom(requiredType))	//if the required type is URIPath
					{
						return new URIPath(stringObject);	//create a URI path from the string
					}
					//TODO check for a string-compatible constructor
				}
			}
/*TODO bring back if we decide to use; the problem with this is that it lets actual mistakes go unnoticed
			catch(final IllegalArgumentException illegalArgumentException)	//if we couldn't convert the object, ignore the error and return null to indicate that we couldn't do a conversion
			{
			}
*/
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
		private final URI propertyURI;	//TODO probably remove this

			/**@return The URI identifying the property.*/
			public URI getPropertyURI() {return propertyURI;}

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
		@param propertyURI The URI identifying the property.
		@param propertyClass The class representing the property type.
		@param value The property value.
		@exception NullPointerException if the given property URI and/or property class is <code>null</code>.
		*/
		public PropertyDescription(final URI propertyURI, final Class<?> propertyClass, final Object value)
		{
			this(propertyURI, propertyClass, value, null);	//construct the class with no setter
		}

		/**Setter and value constructor.
		@param propertyURI The URI identifying the property.
		@param setter The setter method to be invoked, or <code>null</code> if no setting method is known.
		@param value The property value.
		@exception NullPointerException if the given property URI and/or property class is <code>null</code>.
		*/
		public PropertyDescription(final URI propertyURI, final Class<?> propertyClass, final Object value, final Method setter)
		{
			this.propertyURI=checkInstance(propertyURI, "Property URI cannot be null.");
			this.propertyClass=checkInstance(propertyClass, "Property class cannot be null.");
			this.setter=setter;
			this.value=value;
		}
		
	}

}
