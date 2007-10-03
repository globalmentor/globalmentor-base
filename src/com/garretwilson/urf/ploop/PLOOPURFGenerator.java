package com.garretwilson.urf.ploop;

import java.lang.reflect.*;
import java.net.URI;
import java.util.*;

import static com.garretwilson.lang.ClassUtilities.*;
import static com.garretwilson.lang.EnumUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.lang.EnumUtilities;
import com.garretwilson.net.*;
import com.garretwilson.urf.*;
import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.ploop.PLOOP.*;

import com.guiseframework.style.Color;
import com.guiseframework.style.RGBColor;

/**Class that generates an RDF instance from a Java object.
This PLOOP processor recognizes {@link Resource} objects and writes their reference URIs.
This PLOOP processor also recognizes {@link RDFResource} objects and transfers any non-PLOOP properties when an instance is encountered.
@author Garret Wilson
*/
public class PLOOPURFGenerator
{

	/**The URF instance to use in generating URF.*/
	private final URF urf;

		/**@return The URF instance to use in generating URF.*/
		public URF getURF() {return urf;}

	/**Default constructor with a default URF instance.*/
	public PLOOPURFGenerator()
	{
		this(new URF());	//create the generator with a default URF instance
	}

	/**URF instance constructor.
	@param urf The URF instance to use in generating URF.
	@exception NullPointerException if the given URF instance is <code>null</code>.
	*/
	public PLOOPURFGenerator(final URF urf)
	{
		this.urf=checkInstance(urf, "URF instance cannot be null.");
	}

	/**Generates a PLOOP URF resource to represent the given Java object.
	If a resource already exists for the given objects it will be returned.
	The RDF resource is added to the RDF instance.
	Otherwise, the URF resource generated for the object is added to the URF instance.
	If the given object is a {@link Resource}, its reference URI will be used as the reference URI of the generated RDF resource.
	The preferred type is only used to indicate when special resource types should be generated in ambiguous special cases,
	such as whether an object that implements the {@link List} interface should be generated as its concrete type or as a list.
	Special resources with URIs in the indicated namespaces will be produced for objects of the following types:
	<dl>
		<dt><code>char[]</code></dt> <dd>{@value URF#STRING_NAMESPACE_URI} using {@link String#String(char[])}</dd>
		<dt>{@link Color}</dt> <dd>{@value URF#STRING_NAMESPACE_URI} using {@link Color#asRGB()} {@link RGBColor#toString()}</dd>
		<dt>{@link Enum}</dt> <dd>{@value URF#STRING_NAMESPACE_URI} using {@link EnumUtilities#getSerializationName(Enum)}</dd>
		<dt>{@link URIPath}</dt> <dd>{@value URF#STRING_NAMESPACE_URI} using {@link URIPath#toString()}</dd>
	</dl>
//TODO fix and change to ploop:null	For <code>null</code>, a resource with the reference URI {@link RDFConstants#NIL_RESOURCE_URI} is returned.
	@param object The object for which an URF resource should be generated.
	@param preferredType The type of object that should be generated.
	@return The generated URF resouce to represent the provided object.
	@exception NullPointerException if the given preferred type is <code>null</code>.
 	@exception IllegalArgumentException if one of the properties of the given object is not accessible.
	@exception InvocationTargetException if one of the properties of the given object throws an exception.
	*/
	public URFResource generateURFResource(final Object object) throws InvocationTargetException
	{
		if(object!=null)
		{
			return generateURFResource(object, object.getClass());	//generate the resource using its own class
		}
		else	//if the object is null
		{
			throw new NullPointerException("null values not yet supported in PLOOP URF generator");
		}
	}

	/**Generates a PLOOP URF resource to represent the given Java object with an explicit preferred type.
	If a resource already exists for the given objects it will be returned.
	The RDF resource is added to the RDF instance.
	Otherwise, the URF resource generated for the object is added to the URF instance.
	If the given object is a {@link Resource}, its reference URI will be used as the reference URI of the generated RDF resource.
	The preferred type is only used to indicate when special resource types should be generated in ambiguous special cases,
	such as whether an object that implements the {@link List} interface should be generated as its concrete type or as a list.
	Special resources with URIs in the indicated namespaces will be produced for objects of the following types:
	<dl>
		<dt><code>char[]</code></dt> <dd>{@value URF#STRING_NAMESPACE_URI} using {@link String#String(char[])}</dd>
		<dt>{@link Color}</dt> <dd>{@value URF#STRING_NAMESPACE_URI} using {@link Color#asRGB()} {@link RGBColor#toString()}</dd>
		<dt>{@link Enum}</dt> <dd>{@value URF#STRING_NAMESPACE_URI} using {@link EnumUtilities#getSerializationName(Enum)}</dd>
		<dt>{@link URIPath}</dt> <dd>{@value URF#STRING_NAMESPACE_URI} using {@link URIPath#toString()}</dd>
	</dl>
//TODO fix and change to ploop:null	For <code>null</code>, a resource with the reference URI {@link RDFConstants#NIL_RESOURCE_URI} is returned.
	@param object The object for which an URF resource should be generated.
	@param preferredType The type of object that should be generated.
	@return The generated URF resouce to represent the provided object.
	@exception NullPointerException if the given preferred type is <code>null</code>.
 	@exception IllegalArgumentException if one of the properties of the given object is not accessible.
	@exception InvocationTargetException if one of the properties of the given object throws an exception.
	*/
	public URFResource generateURFResource(Object object, final Class<?> preferredType) throws InvocationTargetException
	{
		final URFResource resource;	//we'll store the resource here
		final URF urf=getURF();	//get the RDF instance for creating the resource
		if(object!=null)	//if the object is not null
		{
			if(object instanceof List && List.class.isAssignableFrom(preferredType))	//if a list was requested TODO improve for false positives, if needed
			{
				final URFListResource<URFResource> listResource=new URFListResource<URFResource>();	//create a new URF list resource
				for(final Object listItem:(List<?>)object)	//for each item in the list
				{
					final URFResource listItemResource=generateURFResource(listItem);	//generate an URF resource from the property value
					listResource.add(listItemResource);	//add the list item URF resource to the list URF resource
				}
				return listResource;	//return the list resource
			}
			else if(object instanceof Set && Set.class.isAssignableFrom(preferredType))	//if a set was requested TODO improve for false positives, if needed
			{
				final URFSetResource<URFResource> setResource=new URFSetResource<URFResource>();	//create a new URF set resource
				for(final Object setElement:(Set<?>)object)	//for each element in the set
				{
					final URFResource setElementResource=generateURFResource(setElement);	//generate an URF resource from the set element
					setResource.add(setElementResource);	//add the set element URF resource to the set URF resource
				}
				return setResource;	//return the list resource
			}
			else if(object instanceof char[])	//if the object is a character array
			{
				object=new String((char[])object);	//use the string version of the character array
			}
			else if(object instanceof Color)	//if the object is a color
			{
				object=((Color)object).asRGB().toString();	//use the RGB string representation of the color
			}
			else if(object instanceof Enum)	//if the object is an enum, serialize it using special serialization rules TODO document serialization rules
			{
				object=getSerializationName((Enum)object);	//use the serialization name of the object
			}
			else if(object instanceof URIPath)	//if the required type is URIPath
			{
				object=object.toString();	//use the string form of the URI path
			}
/*TODO del			
				//TODO confirm that order of isAssignableFrom() is correct---this may not be right as currently implemented
			
			if(requiredType.isArray() && Character.TYPE.equals(requiredType.getComponentType()))	//if the required type is a character array, treat it specially, as we can't perform char[].toString()
			{
				object=new String((char[])object);	//use the string version of the character array
			}
			else if(Color.class.isAssignableFrom(requiredType))	//if the required type is Color
			{
				object=((Color)object).asRGB().toString();	//use the RGB string representation of the color
			}
			else if(Enum.class.isAssignableFrom(requiredType))	//if the required type is an enum, serialize it using special serialization rules TODO document serialization rules
			{
				object=getSerializationName((Enum)object);	//use the serialization name of the object
			}
			else if(URIPath.class.isAssignableFrom(requiredType))	//if the required type is URIPath
			{
				object=object.toString();	//use the string form of the URI path
			}
*/
			URI resourceURI=asResourceURI(object);	//see if we know how to create a resource URI from this object
			if(resourceURI!=null)	//if we already have a URI for a resource
			{
				return urf.locateResource(resourceURI);	//locate and return a resource for this resource URI
			}
			final boolean isObjectResource=object instanceof Resource;	//see if the object is a resource
			resourceURI=isObjectResource ? ((Resource)object).getURI() : null;	//get the URI of the object, if it is a resource
			final Class<?> objectClass=object.getClass();	//get the object type
			final URI typeURI=createInfoJavaURI(objectClass);	//get a URI to represent the class
			resource=urf.locateResource(resourceURI, typeURI);	//locate and create if needed a new resource
			final Method[] methods=objectClass.getMethods();	//get all the methods of the object
			for(final Method method:methods)	//look at each method; TODO fix; change to look through all the setters first; we'll use all the getters, because a read-only property still may be used for constructing the object
			{
				final Class<?> propertyType=method.getReturnType();	//get the method return type
				if(propertyType!=null && method.getParameterTypes().length==0)	//if this method returns a value and has no arguments
				{
					final String getterPropertyName=getGetterPropertyName(method.getName());	//see if this method name indicates it is a getter; if so, get the property name
					if(getterPropertyName!=null && (!isObjectResource || !"uri".equals(getterPropertyName)))	//if this is a getter method (ignoring "uri" for resources, as we'll put that resulting URI elsewhere) TODO use a constant
					{						
						final Method setterMethod=getCompatibleSetterMethod(objectClass, getterPropertyName, propertyType);	//get a corresponding setter method
						boolean useProperty=setterMethod!=null;	//start off assuming we won't use this property unless there is a corresponding setter
						if(!useProperty)	//if we think we shouldn't use the property, check to see if the property is needed for construction
						{
							if(getPublicDefaultConstructor(objectClass)==null)	//if the class doesn't have a public default constructor
							{
								for(final Constructor<?> constructor:objectClass.getConstructors())	//look at all the constructors
								{
									if(Modifier.isPublic(constructor.getModifiers()));	//if this is a public constructor
									{
										for(final Class<?> constructorParameterType:constructor.getParameterTypes())	//look at each parameter type in the constructor
										{
											if(constructorParameterType.isAssignableFrom(propertyType))	//if this property type might be used in this constructor
											{
												useProperty=true;	//we'll use the property, just in case
												break;	//stop looking for reasons to include the property
											}
										}
										if(useProperty)	//if we decided from this constructor that we should use the property
										{
											break;	//stop looking for reasons to include the property											
										}
									}
								}
							}
						}
//TODO add feature, and add to PLOOPProcessor						if(setterMethod==null)	//if there is no compatible setter method, see if there is an get-iterable/set-collection correspondence
						
//TODO fix						if(setterMethod!=null)	//if there is an equivalent setter method
						if(useProperty)	//if we decided that we should use this property
						{
							setURFResourceProperty(resource, object, getterPropertyName, method);	//set this property of the resource
						}
					}
				}
			}
/*TODO fix
			if(object instanceof RDFResource)	//if the object is an RDF resource, add any non-PLOOP properties to the new RDF resource
			{
				for(final RDFPropertyValuePair rdfPropertyValuePair:((RDFResource)object).getProperties())	//for each property
				{
					final RDFResource property=rdfPropertyValuePair.getProperty();	//get the property
					final URI propertyURI=property.getURI();	//get the property URI
					if(!PLOOP_PROPERTY_NAMESPACE_URI.equals(getNamespaceURI(propertyURI)))	//if this isn't a PLOOP property
					{
						resource.addProperty(propertyURI, rdfPropertyValuePair.getValue());	//add this property and value to the generated RDF resource TODO see if we can transfer the owner RDF instance when we transfer the value
					}
				}
			}
*/
		}
		else	//if the object is null
		{
			throw new NullPointerException("null values not yet supported in PLOOP URF generator");
		}
		return resource;	//configure the resource
	}
	
	/**Sets a PLOOP URF resource property to represent the given Java object property.
//TODO fix and change to ploop:null	For <code>null</code>, a resource with the reference URI {@link RDFConstants#NIL_RESOURCE_URI} is returned.
	@param resource The URF resource representing the given object.
	@param object The object for which a property should be stored in the RDF resource.
	@param propertyName The name of the property to set.
	@exception NullPointerException if the given resource, object, and/or property name is <code>null</code>.
 	@exception IllegalArgumentException if the object has no getter method for the given property.
 	@exception IllegalArgumentException if one of the properties of the given object is not accessible.
	@exception InvocationTargetException if one of the properties of the given object throws an exception.
	*/
	public void setURFResourceProperty(final URFResource resource, final Object object, final String propertyName) throws InvocationTargetException
	{
		final Method getterMethod=getGetterMethod(object.getClass(), propertyName);	//get the object's getter method for this property
		if(getterMethod==null)	//if there is no such getter method
		{
			throw new IllegalArgumentException("Object "+object+" has no property "+propertyName);
		}
		setURFResourceProperty(resource, object, propertyName, getterMethod);	//set the property using the getter method
	}

	/**Sets a PLOOP URF resource property to represent the given Java object property when the property's getter method is known.
//TODO fix and change to ploop:null	For <code>null</code>, a resource with the reference URI {@link RDFConstants#NIL_RESOURCE_URI} is returned.
	@param resource The URF resource representing the given object.
	@param object The object for which a property should be stored in the URF resource.
	@param propertyName The name of the property to set.
	@param getterMethod The method to invoke to retrieve the property value from the object.
	@return The URF property value that was added.
	@exception NullPointerException if the given resource, object, property name, and/or getter method is <code>null</code>.
 	@exception IllegalArgumentException if one of the properties of the given object is not accessible.
	@exception InvocationTargetException if one of the properties of the given RDF throws an exception.
	*/
	public URFResource setURFResourceProperty(final URFResource resource, final Object object, final String propertyName, final Method getterMethod) throws InvocationTargetException
	{
		final Class<?> propertyType=getterMethod.getReturnType();	//get the property type from the method
		final Object propertyValueObject;
		try
		{
			propertyValueObject=getterMethod.invoke(object);	//get the property value by invoking this getter method
		}
		catch(final IllegalAccessException illegalAccessException)
		{
			throw new IllegalArgumentException(illegalAccessException);
		}
		final URFResource propertyValueResource=generateURFResource(propertyValueObject, propertyType);	//generate an URF object from the property value
		final URI propertyURI=getPropertyURI(object, propertyName);	//get the property URI for the given property of the object
		return resource.setPropertyValue(propertyURI, propertyValueResource);	//set this property of the URF resource
	}

}
