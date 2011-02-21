/*
 * Copyright © 2007-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package org.ploop;

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
import com.globalmentor.config.ConfigurationException;
import com.globalmentor.java.Classes;
import com.globalmentor.java.Objects;
import com.globalmentor.net.*;
import com.globalmentor.urf.*;
import com.globalmentor.util.*;

/*TODO fix for Guise
import com.guiseframework.style.AbstractModeledColor;
import com.guiseframework.style.Color;
*/

/**
 * Processes PLOOP objects from an {@link URFAssertionSource}.
 * <p>
 * This processor is not thread safe.
 * </p>
 * <p>
 * This processor recognizes the {@link Resource} type; when this class is present with a single URI parameter constructor, that constructor will take
 * precedence using the resource URI value.
 * </p>
 * <p>
 * This processor also recognizes the {@link URFResource} type and will transfer all non-PLOOP properties when an instance is encountered.
 * </p>
 * @author Garret Wilson
 */
public class PLOOPURFAssertionProcessor
{

	/** The source of cached PLOOP resources. */
	private final PLOOPSource ploopSource;

	/** @return The source of cached PLOOP resources. */
	public PLOOPSource getPLOOPSource()
	{
		return ploopSource;
	}

	/** The source of assertions to be processed. */
	private final URFAssertionSource assertionSource;

	/** @return The source of assertions to be processed. */
	public URFAssertionSource getAssertionSource()
	{
		return assertionSource;
	}

	/** The default arguments that can be used in calling class constructors. */
	private final Set<Object> defaultConstructorArguments = new CopyOnWriteArraySet<Object>();

	/** @return The default arguments that can be used in calling class constructors. */
	protected Set<Object> getDefaultConstructorArguments()
	{
		return defaultConstructorArguments;
	}

	/**
	 * Assertion source, and default arguments constructor.
	 * <p>
	 * This constructor creates an internal PLOOP source that caches resources locally and never releases them for the life of the assertion processor.
	 * @param assertionSource The source of assertions to be processed.
	 * @param defaultConstructorArguments The objects that can be used as default arguments in class constructors.
	 * @throws NullPointerException if the given session is <code>null</code>.
	 */
	public PLOOPURFAssertionProcessor(final URFAssertionSource assertionSource, final Object... defaultConstructorArguments)
	{
		this.ploopSource = new AbstractPLOOPSource(new HashMap<URI, Object>()) //use a PLOOP source that uses this assertion processor
		{
			@Override
			protected PLOOPURFAssertionProcessor getAssertionProcessor()
			{
				return PLOOPURFAssertionProcessor.this;
			}
		};
		this.assertionSource = Objects.checkInstance(assertionSource, "Assertion source cannot be null.");
		addAll(this.defaultConstructorArguments, defaultConstructorArguments); //add all the given default constructor arguments to our set of default constructor arguments
	}

	/**
	 * PLOOP source, assertion source, and default arguments constructor.
	 * @param ploopSource The source of cached PLOOP resources.
	 * @param assertionSource The source of assertions to be processed.
	 * @param defaultConstructorArguments The objects that can be used as default arguments in class constructors.
	 * @throws NullPointerException if the given session is <code>null</code>.
	 */
	public PLOOPURFAssertionProcessor(final PLOOPSource ploopSource, final URFAssertionSource assertionSource, final Object... defaultConstructorArguments)
	{
		this.ploopSource = Objects.checkInstance(ploopSource, "PLOOP source cannot be null.");
		this.assertionSource = Objects.checkInstance(assertionSource, "Assertion source cannot be null.");
		addAll(this.defaultConstructorArguments, defaultConstructorArguments); //add all the given default constructor arguments to our set of default constructor arguments
	}

	/**
	 * Processes and existing object, retrieving a resource if necessary.
	 * <p>
	 * If the given object is a {@link ReferenceResource}, the resource is retrieved using {@link PLOOPSource#findByURI(URI)}. Otherwise, the object is returned
	 * as-is.
	 * @param object The object to process.
	 * @return The resulting object, or <code>null</code> if the given object was a {@link ReferenceResource} that referenced a resource that no longer exists.
	 * @throws ConfigurationException if a resource is a Java-typed resource the class of which cannot be found.
	 * @throws ConfigurationException if a resource indicates a Java class that has no appropriate constructor.
	 * @throws ConfigurationException if a resource indicates a Java class that is an interface or an abstract class.
	 * @throws ConfigurationException if a resource indicates a Java class the constructor of which is not accessible.
	 * @throws IllegalStateException if a resource indicates a Java class the constructor of which throws an exception.
	 * @throws DataException if a resource does not specify Java type information.
	 * @throws DataException if the given object references a resource that does not exist.
	 * @throws DataException if some value could not be converted.
	 */
	public Object processObject(final Object object) throws ConfigurationException, IllegalStateException, DataException
	{
		if(object instanceof ReferenceResource) //if the object refers to a resource, find that resource
		{
			final URI resourceURI = ((ReferenceResource) object).getURI();
			final Object referencedObject = getPLOOPSource().findByURI(resourceURI); //look up the resource TODO change to referencedResource
			if(referencedObject == null)
			{
				throw new DataException("Referenced resource " + resourceURI + " does not exist.");
			}
			return referencedObject;
		}
		else
		//otherwise, return the object as-is
		{
			return object;
		}
	}

	/**
	 * Creates and initializes an object to represent the identified resource using the assertions in the given source. TODO describe process
	 * @param referenceURI The URI identifying the resource for which a Java object should be created.
	 * @return A created and initialized object according to the given resource description.
	 * @throws NullPointerException if the given resource URI is <code>null</code>.
	 * @throws ConfigurationException if a resource is a Java-typed resource the class of which cannot be found.
	 * @throws ConfigurationException if a resource indicates a Java class that has no appropriate constructor.
	 * @throws ConfigurationException if a resource indicates a Java class that is an interface or an abstract class.
	 * @throws ConfigurationException if a resource indicates a Java class the constructor of which is not accessible.
	 * @throws IllegalStateException if a resource indicates a Java class the constructor of which throws an exception.
	 * @throws DataException if a resource does not specify Java type information.
	 * @throws DataException if an assertion references a resource that does not exist.
	 * @throws DataException if some value could not be converted.
	 * @see #processObject(Object)
	 * @see #convertObject(Object, Class)
	 */
	public Object createObject(final URI resourceURI) throws ConfigurationException, IllegalStateException, DataException
	{
		final URFAssertionSource assertionSource = getAssertionSource();
		Class<?> valueClass = null; //we'll try to find a Java class from one of the types
		for(final URFAssertion typeAssertion : assertionSource.getAssertionsBySubjectAndPredicate(resourceURI, TYPE_PROPERTY_URI)) //look at each type
		{
			final Resource type = Objects.asInstance(typeAssertion.getObject(), Resource.class); //the type value should be a resource indicating the type
			if(type != null) //ignore type values we don't understand
			{
				final URI typeURI = type.getURI(); //get the type URI
				//TODO make sure type is not null; this would be a data error
				if(JAVA_URI_SCHEME.equals(typeURI.getScheme())) //if this is an a java: URI
				{
					try
					{
						valueClass = Classes.asClass(type.getURI()); //try to get a class from the type URI
					}
					catch(final ClassNotFoundException classNotFoundException)
					{
						throw new DataException(classNotFoundException);
					}
				}
				else if(SET_CLASS_URI.equals(typeURI)) //if this is an URF set
				{
					final Set<Object> set = new HashSet<Object>(); //create a new set
					for(final URFAssertion elementAssertion : assertionSource.getAssertionsBySubjectAndPredicate(resourceURI, ELEMENT_PROPERTY_URI)) //look at each set element
					{
						final Object element = processObject(elementAssertion.getObject()); //transform the object into an element, creating a resource if necessary
						set.add(element); //add this element to the set
					}
					return set; //return the new set
				}
			}
			if(valueClass != null) //if we know the value class, try to construct a Java object
			{
				final Map<URI, PropertyDescription> propertyDescriptionMap = getPropertyDescriptionMap(valueClass, resourceURI); //get the property descriptions for this resource
				Constructor<?> constructor = null; //we'll store an appropriate constructor here
				Object[] arguments = null; //we'll keep track of the arguments here
				final Constructor<?>[] constructors = valueClass.getConstructors(); //get all available constructors
				/*TODO fix
									final URFListResource<?> selector = asListInstance(typeProperty.getScope().getPropertyValue(SELECTOR_PROPERTY_URI)); //get the selector list, if any
									if(selector != null) //if a selector was specified
									{
										final int argumentCount = selector.size(); //see how many selector arguments there are
										for(final Constructor<?> candidateConstructor : constructors) //look at each constructor to find one with the correct number of parameters
										{
											final Class<?>[] parameterTypes = candidateConstructor.getParameterTypes(); //get the parameter types for this constructor
											if(parameterTypes.length == argumentCount) //if this constructor has the correct number of parameters
											{
												arguments = new Object[argumentCount]; //create an array sufficient for the arguments
												for(int parameterIndex = 0; parameterIndex < argumentCount; ++parameterIndex) //for each parameter, as long we we have matching parameters
												{
													final Class<?> parameterType = parameterTypes[parameterIndex]; //get this parameter type
													final Object argument = convertObject(getObject(selector.get(parameterIndex)), parameterType); //get an object from the selector resource and covert it to the correct type
													if(argument != null) //if we successfully converted this constructor argument
													{
														arguments[parameterIndex] = argument; //store the argument
													}
													else
													//if we couldn't convert this constructor argument
													{
														arguments = null; //these arguments won't work
														break; //stop looking at this constructor
													}
												}
											}
											if(arguments != null) //if these arguments worked
											{
												constructor = candidateConstructor; //use this constructor
												break; //stop looking for a matching constructor
											}
										}
										if(constructor == null) //if we didn't find an appropriate constructor
										{
											throw new DataException("Value class " + valueClass + " does not have a constructor appropriate for the specified selector parameters: "
													+ Collections.toString(selector));
										}
									}
									else
				*/
				//if there is no type selector
				{
					if(Resource.class.isAssignableFrom(valueClass)) //if the value class is a Resource, see if we can create it with a single URI
					{
						constructor = getCompatibleConstructor(valueClass, URI.class); //see if there is a single URI parameter constructor
						if(constructor != null) //if there is a single URI parameter constructor for the Resource
						{
							arguments = new Object[] { resourceURI }; //the resource URI will be constructor argument
						}
					}
					if(constructor == null) //if we still don't have a constructor
					{
						constructor = getPublicDefaultConstructor(valueClass); //see if there's a default constructor
						if(constructor != null) //if there is a default constructor
						{
							arguments = EMPTY_OBJECT_ARRAY; //the default constructor has no arguments
						}
					}
				}
				if(constructor != null) //if we found a constructor
				{
					assert arguments != null : "Found constructor but missing arguments.";
					try
					{
						final Object object = constructor.newInstance(arguments); //invoke the constructor with the arguments
						setObjectProperties(object, propertyDescriptionMap); //initialize the object with the properties
						return object; //return the constructed and initialized object
					}
					catch(final InstantiationException instantiationException)
					{
						throw new DataException(instantiationException);
					}
					catch(final IllegalAccessException illegalAccessException)
					{
						throw new DataException(illegalAccessException);
					}
					catch(final IllegalArgumentException illegalArgumentException)
					{
						throw new IllegalStateException(illegalArgumentException);
					}
					catch(final InvocationTargetException invocationTargetException)
					{
						throw new IllegalStateException(invocationTargetException);
					}
				}
				else
				//if we didn't find a constructor
				{
					throw new DataException("Value class " + valueClass + " does not have an appropriate constructor.");
				}
			}
		}
		throw new IllegalArgumentException("Insufficient data to create object for resource " + resourceURI);
		//TODO del			return resource; //return the resource itself
		//TODO del		}
	}

	/**
	 * Initializes an object based upon the given URI and property descriptions. TODO This implementation also recognizes the {@link URFResource} type and will
	 * transfer all non-PLOOP properties to the object when an instance is encountered.
	 * @param object The object to initialize.
	 * @param propertyDescriptionMap The property descriptions for initializing the object.
	 * @throws NullPointerException if the given object and/or property description map is <code>null</code>.
	 * @throws ConfigurationException if a resource is a Java-typed resource the class of which cannot be found.
	 * @throws ConfigurationException if a particular value is not an appropriate argument for the corresponding property.
	 * @throws ConfigurationException If a particular property could not be accessed.
	 * @throws IllegalStateException if a resource indicates a Java class the constructor of which throws an exception.
	 */
	protected void setObjectProperties(final Object object, final Map<URI, PropertyDescription> propertyDescriptionMap) throws ConfigurationException,
			IllegalStateException
	{
		for(final PropertyDescription propertyDescription : propertyDescriptionMap.values()) //for each property description
		{
			final Method setter = propertyDescription.getSetter(); //get the setter method for this property
			if(setter != null) //if there is a setter for this property
			{
				setObjectProperty(object, setter, propertyDescription.getValue()); //set the value for the property
			}
		}
		/*TODO del if not needed
				if(object instanceof URFResource) //if the object is an URF resource, add any non-PLOOP properties to the new URF resource
				{
					for(final URFProperty property : resource.getProperties()) //for each property of the source resource
					{
						final URI propertyURI = property.getPropertyURI(); //get the property URI
						if(!propertyDescriptionMap.containsKey(propertyURI)) //if this was not a PLOOP property
						{
							((URFResource) object).addPropertyValue(propertyURI, property.getValue()); //add this property and value to the created URF resource
						}
					}
				}
		*/
		//TODO del		resourceObjectMap.put(resourceURI, object); //associate the initialized object with its resource description
	}

	/**
	 * Sets the property of an object using the given setter method and value.
	 * @param object The object to initialize.
	 * @param setterMethod The method to use in setting the object property.
	 * @param value The value to set.
	 * @throws NullPointerException if the given object and/or setter method is <code>null</code>.
	 * @throws ConfigurationException if the given value is not an appropriate argument for the given property.
	 * @throws ConfigurationException If the given property could not be accessed.
	 * @throws IllegalStateException if the given Java property method throws an exception.
	 */
	public static void setObjectProperty(final Object object, final Method setterMethod, final Object value) throws ConfigurationException, IllegalStateException
	{
		try
		{
			setterMethod.invoke(object, value); //invoke the setter on the object
		}
		catch(final IllegalAccessException illegalAccessException) //if we can't access this setter
		{
			throw new ConfigurationException(illegalAccessException);
		}
		catch(final IllegalArgumentException illegalArgumentException)
		{
			throw new ConfigurationException(illegalArgumentException);
		}
		catch(final InvocationTargetException invocationTargetException)
		{
			throw new IllegalStateException(invocationTargetException);
		}
	}

	/**
	 * Constructs a map of property descriptions for a class based upon a resource description. If there are duplicate properties, only one will be stored.
	 * @param objectClass The class of the object to be constructed.
	 * @param resourceURI The URI of the resource.
	 * @return A map of property descriptions keyed to property URIs.
	 * @throws NullPointerException if the given object class, and/or resource URI is <code>null</code>.
	 * @throws ConfigurationException if a resource is a Java-typed resource the class of which cannot be found.
	 * @throws ConfigurationException if a resource indicates a Java class that has no appropriate constructor.
	 * @throws ConfigurationException if a resource indicates a Java class that is an interface or an abstract class.
	 * @throws ConfigurationException if a resource indicates a Java class the constructor of which is not accessible.
	 * @throws IllegalStateException if a resource indicates a Java class the constructor of which throws an exception.
	 * @throws DataException if a resource does not specify Java type information.
	 * @throws DataException if an assertion references a resource that does not exist.
	 * @throws DataException if some value could not be converted.
	 */
	protected Map<URI, PropertyDescription> getPropertyDescriptionMap(final Class<?> objectClass, final URI resourceURI) throws ConfigurationException,
			IllegalStateException, DataException
	{
		final URFAssertionSource assertionSource = getAssertionSource();
		final Set<URFAssertion> assertions = assertionSource.getAssertionsBySubject(resourceURI); //get the assertions describing this resource
		final Map<URI, PropertyDescription> propertyDescriptionMap = new HashMap<URI, PropertyDescription>(assertions.size()); //create a map to hold property descriptions, with a least enough capacity to hold descriptions for all properties
		for(final URFAssertion assertion : assertionSource.getAssertionsBySubject(resourceURI)) //for each resource property
		{
			final URI propertyURI = assertion.getPredicateURI(); //get the property URI
			if(DEFAULT_NAMESPACE_URI.equals(getNamespaceURI(propertyURI))) //if this property is in the default namespace
			{
				final String propertyName = getLocalName(propertyURI); //get the local name of the property
				final Object propertyValue = processObject(assertion.getObject()); //process the object so that it can be set as a value
				final PropertyDescription propertyDescription = getPropertyDescription(objectClass, propertyName, propertyValue); //get a description for this property
				if(propertyDescription != null) //if this was a recognized property
				{
					propertyDescriptionMap.put(propertyURI, propertyDescription); //store this property description in the map
				}
			}
		}
		return propertyDescriptionMap; //return the property description map
	}

	/**
	 * Gets a description of a property of the object based upon the given property name and value. The returned property description will indicate a setter
	 * method if the property is settable.
	 * @param objectClass The class of the object to be updated.
	 * @param propertyName The name of the property potentially representing an object property.
	 * @param propertyValue The value potentially representing an object property.
	 * @return A description of the property, or <code>null</code> if the property is not recognized.
	 * @throws NullPointerException if the given object class, property name, and/or property value is <code>null</code>.
	 * @throws DataException if a value could not be converted.
	 */
	protected PropertyDescription getPropertyDescription(final Class<?> objectClass, final String propertyName, final Object propertyValue) throws DataException
	{
		//TODO del		final Class<?> propertyValueType = propertyValue.getClass(); //get the type of the value
		//try to find a compatible setter method; get the variable name from each supposed setter in case the setter has multiple capital letters, such as setID()
		final Method[] methods = objectClass.getMethods(); //get all the class methods
		for(final Method method : methods) //for each method
		{
			if(propertyName.equals(getSetterPropertyName(method.getName()))) //if we could consider this method a setter for the variable we have 
			{
				final Class<?>[] parameterTypes = method.getParameterTypes(); //get the parameter types for this method
				if(parameterTypes.length == 1) //if this setter has one parameter
				{
					final Class<?> parameterType = parameterTypes[0]; //get the single parameter type
					final Object value = convertObject(propertyValue, parameterType); //convert the object to the correct type
					if(value != null) //if we found a parameter to use for this method
					{
						return new PropertyDescription(parameterType, value, method); //return a description of this property with the method and parameter
					}
				}
			}
		}
		//if no setter could be found, try to find a getter method to verify this is a property that can be set
		//get the variable name from each supposed getter in case the getter has multiple capital letters, such as getID()
		for(final Method method : methods) //for each method
		{
			if(propertyName.equals(getGetterPropertyName(method.getName()))) //if we could consider this method a getter for the variable we have 
			{
				final Class<?> returnType = method.getReturnType(); //get the return type of the getter
				final Object value = convertObject(propertyValue, returnType); //convert the object to the getter return type, if we can
				if(value != null) //if we can convert the property value to the getter return type
				{
					return new PropertyDescription(value != null ? value.getClass() : returnType, value); //return a description of this property with just the value TODO see why covariant return types aren't working correctly; for now, we'll just get the value type directly
				}
			}
		}
		return null; //indicate that we don't recognize this property
	}

	/**
	 * Converts an object to the correct Java type if possible. If the object is already of the correct type, no action occurs. The following types can be
	 * converted to the listed types:
	 * <dl>
	 * <dt>{@link Boolean}</dt>
	 * <dd><code>boolean</code></dd>
	 * <dt>{@link Character}</dt>
	 * <dd><code>char</code></dd>
	 * <dt>{@link Number}</dt>
	 * <dd>{@link Long}, <code>long</code>, {@link Integer}, <code>int</code>, {@link Double}, <code>double</code>, {@link Float}, <code>float</code></dd>
	 * <dt>{@link String}</dt>
	 * <dd><code>char[]</code>, {@link Color}, {@link Enum}, {@link Pattern}, {@link URIPath}</dd>
	 * </dl>
	 * @param object The object to convert.
	 * @param requiredType The required type of the object.
	 * @return The object as the required type, or <code>null</code> if the object cannot be converted to the required type.
	 * @throws NullPointerException if the given object is <code>null</code>.
	 * @throws DataException if the given object should be able to be converted to the required type but something about its state, format, or contents prevented
	 *           the conversion.
	 */
	protected Object convertObject(final Object object, final Class<?> requiredType) throws DataException //TODO search for a string constructor or a static valueOf() method
	{
		final Class<?> objectType = object.getClass(); //get the type of the object
		if(requiredType.isAssignableFrom(objectType)) //if we expect this type (this algorithm could be improved to first try to find an exact match and then find a convertible match)
		{
			return object; //use the object as-is
		}
		else
		//if we expect for another object type
		{
			try
			{
				if(object instanceof Boolean) //if the object is a Boolean
				{
					if(Boolean.TYPE.equals(requiredType)) //if the required type is boolean (we already checked for Boolean when we checked to see if the types were the same)
					{
						return object; //return the Boolean object
					}
				}
				else if(object instanceof Character) //if the object is a Character
				{
					if(Character.TYPE.equals(requiredType)) //if the required type is char (we already checked for Character when we checked to see if the types were the same)
					{
						return object; //return the Character object
					}
				}
				else if(object instanceof Number) //if the object is a Number
				{
					if(Long.TYPE.equals(requiredType) || Long.class.isAssignableFrom(requiredType)) //if the required type is long or Long 
					{
						return object instanceof Long ? object : Long.valueOf(((Number) object).longValue()); //return a Long version of the object
					}
					else if(Integer.TYPE.equals(requiredType) || Integer.class.isAssignableFrom(requiredType)) //if the required type is integer or integer 
					{
						return object instanceof Integer ? object : Integer.valueOf(((Number) object).intValue()); //return an Integer version of the object
					}
					else if(Double.TYPE.equals(requiredType) || Double.class.isAssignableFrom(requiredType)) //if the required type is double or Double
					{
						return object instanceof Double ? object : Double.valueOf(((Number) object).doubleValue()); //return a Double version of the object
					}
					else if(Float.TYPE.equals(requiredType) || Float.class.isAssignableFrom(requiredType)) //if the required type is float or Float
					{
						return object instanceof Double ? object : Float.valueOf(((Number) object).floatValue()); //return a Float version of the object
					}
					//TODO add BigInteger and BigDecimal types
				}
				else if(object instanceof String) //if the object is a string, see if we can convert it to the correct type
				{
					final String stringObject = (String) object; //cast the value to a String
					if(requiredType.isArray() && Character.TYPE.equals(requiredType.getComponentType())) //if the required type is a character array
					{
						return stringObject.toCharArray(); //return the string as a character array
					}
					/*TODO fix for Guise
										else if(Color.class.isAssignableFrom(requiredType))	//if the required type is Color
										{
											return AbstractModeledColor.valueOf(stringObject);	//compile a color from the string
										}
					*/
					else if(Pattern.class.isAssignableFrom(requiredType)) //if the required type is Pattern
					{
						return Pattern.compile(stringObject); //compile a pattern from the string
					}
					else if(URIPath.class.isAssignableFrom(requiredType)) //if the required type is URIPath
					{
						return new URIPath(stringObject); //create a URI path from the string
					}
				}
			}
			catch(final IllegalArgumentException illegalArgumentException) //if there was a conversion error
			{
				throw new DataException(illegalArgumentException); //throw a data error
			}
		}
		return null; //indicate we couldn't get an object of the correct type
	}

	/**
	 * Property information for an object's property. The information indicates the value of a property, and may also indicate a method for setting the given
	 * property.
	 * @author Garret Wilson
	 */
	protected static class PropertyDescription
	{

		/** The class representing the property type. */
		private final Class<?> propertyClass;

		/** @return The class representing the property type. */
		public Class<?> getPropertyClass()
		{
			return propertyClass;
		}

		/** The property value. */
		private final Object value;

		/** @return The property value. */
		public Object getValue()
		{
			return value;
		}

		/** The setter method to be invoked, or <code>null</code> if no setting method is known. */
		private final Method setter;

		/** @return The setter method to be invoked, or <code>null</code> if no setting method is known. */
		public Method getSetter()
		{
			return setter;
		}

		/**
		 * Value constructor.
		 * @param propertyClass The class representing the property type.
		 * @param value The property value.
		 * @throws NullPointerException if the given property URI and/or property class is <code>null</code>.
		 */
		public PropertyDescription(final Class<?> propertyClass, final Object value)
		{
			this(propertyClass, value, null); //construct the class with no setter
		}

		/**
		 * Setter and value constructor.
		 * @param propertyURI The URI identifying the property.
		 * @param setter The setter method to be invoked, or <code>null</code> if no setting method is known.
		 * @param value The property value.
		 * @throws NullPointerException if the given property URI and/or property class is <code>null</code>.
		 */
		public PropertyDescription(final Class<?> propertyClass, final Object value, final Method setter)
		{
			this.propertyClass = checkInstance(propertyClass, "Property class cannot be null.");
			this.setter = setter;
			this.value = value;
		}

		/** @return A string representation of this property description. */
		public String toString()
		{
			final StringBuilder stringBuilder = new StringBuilder(); //create a string builder for constructing the string
			final Object value = getValue(); //get this property's value
			if(value != null) //if there is a value
			{
				stringBuilder.append(value).append(' '); //show a string representation of the value
			}
			stringBuilder.append('(').append(getPropertyClass()).append(')'); //(propertyClass)
			return stringBuilder.toString(); //return the string we constructed
		}
	}

}
