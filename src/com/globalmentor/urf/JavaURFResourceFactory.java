package com.globalmentor.urf;

import java.lang.reflect.*;
import java.net.URI;

import static com.garretwilson.lang.JavaConstants.*;
import static com.garretwilson.lang.Objects.*;
import static com.globalmentor.urf.URF.*;

/**An URF resource factory that can create Java classes within a certain package based upon the type local name.
The class of the resource to be created should have a URI constructor accepting <code>null</code>.
The class of the resource to be created must have a URI constructor or a default constructor.
This class is thread safe.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class JavaURFResourceFactory extends DefaultURFResourceFactory
{

	/**The name of the package, with no '.' suffix, from which Java classes for this namespace will be created.*/
	private final String packageName;

		/**@return The name of the package, with no '.' suffix, from which Java classes for this namespace will be created.*/
		public String getPackageName() {return packageName;}

	/**Package constructor.
	@param namespacePackage The package for which Java classes for this namespace will be created.
	@exception NullPointerException if the given package is <code>null</code>.
	*/
	public JavaURFResourceFactory(final Package namespacePackage)
	{
		this(namespacePackage.getName());	//construct the class with the package name
	}

	/**Package name constructor.
	@param packageName The name of the package, with no '.' suffix, from which Java classes for this namespace will be created.
	@exception NullPointerException if the given package name is <code>null</code>.
	*/
	public JavaURFResourceFactory(final String packageName)
	{
		this.packageName=checkInstance(packageName, "Package name cannot be null.");
	}

	/**Creates a resource with the provided URI based upon the type URI, if any.
	If a type URI is provided, a corresponding type property value may be added to the resource before it is returned.
	This implementation creates a class name by combining the package name with the type local name, separated by a '.'.
	If the resulting class has a URI constructor, it will be used; otherwise, its default constructor will be used.
	If there is no type URI or the type URI has no local name, a default resource will be created.
	This implementation ignores the type namespace URI.
	@param resourceURI The URI of the resource to create, or <code>null</code> if the resource created should be anonymous.
	@param typeURI The URI of the resource type, or <code>null</code> if the type is not known.
	@return The resource created with this URI.
	@exception IllegalArgumentException if the Java class indicated by the given type URI is not a subclass of {@link URFResource}.
	@exception IllegalArgumentException if the corresponding Java class could not be found.
	@exception IllegalStateException if there was an error instantiating the corresponding Java class.
	*/
	public URFResource createResource(final URI resourceURI, final URI typeURI)
	{
		if(typeURI!=null)	//if a type URI was provided
		{
			final String typeLocalName=getLocalName(typeURI);	//get the local name of the type URI
			if(typeLocalName!=null)	//if the type URI has a local name
			{
				final String packageName=getPackageName();	//get the package name to use
				final String className=packageName+PACKAGE_SEPARATOR+typeLocalName;	//construct a class name from the package
				try
				{
					final Class<?> resourceClass=Class.forName(className);	//get the class for the resource
					if(!URFResource.class.isAssignableFrom(resourceClass))	//if this is not an URF resource class
					{
						throw new IllegalArgumentException("The type URI "+typeURI+" does not indicate an URFResource subclass in package "+packageName);
					}
					Object resource;	//we'll create the resource and store it here
					try
					{
						try
						{
							final Constructor<?> constructor=resourceClass.getConstructor(URI.class);	//try to get a URI constructor
							resource=constructor.newInstance(resourceURI);	//create the resource from the URI constructor
						}
						catch(final NoSuchMethodException noSuchMethodException)	//if there is no URI constructor
						{
							resource=resourceClass.newInstance();	//create the resource using its default constructor
						}
					}
					catch(final IllegalArgumentException illegalArgumentException)
					{
						throw new IllegalStateException(illegalArgumentException);
					}
					catch(final InstantiationException instantiationException)
					{
						throw new IllegalStateException(instantiationException);
					}
					catch(final IllegalAccessException illegalAccessException)
					{
						throw new IllegalStateException(illegalAccessException);
					}
					catch(final InvocationTargetException invocationTargetException)	//ignore errors and return null
					{
						throw new IllegalStateException(invocationTargetException);
					}
					return URFResource.class.cast(resource);	//cast and return the resource we created
				}
				catch(final ClassNotFoundException classNotFoundException)
				{
					throw new IllegalArgumentException(classNotFoundException);
				}
			}
		}
		return super.createResource(resourceURI, typeURI);	//if we couldn't create a Java class, create and return a default resource
	}

}