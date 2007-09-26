package com.garretwilson.urf;

import java.io.*;
import java.net.*;

import static com.garretwilson.lang.ClassUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.urf.URF.*;

/**Class for loading and saving a particular type of URF resource.
Custom classes can be added using {@link #registerResourceFactory(URI, URFResourceFactory)}.
@param <T> The type to read and write.
@author Garret Wilson
*/
public class TypedURFResourceTURFIO<T extends URFResource> extends AbstractTURFIO<T>
{

	/**The namespace of the URF resource supported.*/
	private final URI resourceNamespaceURI;

		/**@return The namespace of the URF resource supported.*/
		public URI getResourceNamespaceURI() {return resourceNamespaceURI;}

	/**The class name of the URF resource supported.*/
	private final String resourceClassName;

		/**@return The class name of the URF resource supported.*/
		public String getResourceClassName() {return resourceClassName;}

	/**The type URI of the URF resource supported.*/
	private final URI resourceTypeURI;

		/**The type URI of the URF resource supported.*/
		public URI getResourceTypeURI() {return resourceTypeURI;}

	/**Class and namespace constructor.
	A {@link JavaURFResourceFactory} will be registered with the given namespace, configured to create Java instances from the package of the resource class.
	The class name will be set to the local name of the given resource class.
	@param resourceClass The class representing the type of resource expected from the URF instance.
	@param resourceNamespaceURI The namespace of the URF resource supported.
	@exception NullPointerException if the given class and/or namespace URI is <code>null</code>.
	*/
	public TypedURFResourceTURFIO(final Class<T> resourceClass, final URI resourceNamespaceURI)
	{
		this(resourceClass, resourceNamespaceURI, getLocalName(resourceClass), new JavaURFResourceFactory(resourceClass.getPackage()));	//construct the class with a default resource factory
	}

	/**Class, namespace, and class name constructor.
	@param resourceClass The class representing the type of resource expected from the URF instance.
	@param resourceNamespaceURI The namespace of the URF resource supported.
	@param resourceClassName The class name of the URF resource supported.
	@exception NullPointerException if the given class, namespace URI, and/or class name is <code>null</code>.
	*/
	public TypedURFResourceTURFIO(final Class<T> resourceClass, final URI resourceNamespaceURI, final String resourceClassName)
	{
		super(resourceClass);	//construct the parent class
		this.resourceNamespaceURI=checkInstance(resourceNamespaceURI, "Resource namespace URI must be provided.");
		this.resourceClassName=checkInstance(resourceClassName, "Resource class name must be provided.");
		this.resourceTypeURI=createResourceURI(resourceNamespaceURI, resourceClassName);	//determine the type URI
	}

	/**Type constructor with resource factory.
	@param resourceClass The class representing the type of resource expected from the URF instance.
	@param resourceNamespaceURI The namespace of the URF resource supported.
	@param resourceClassName The class name of the URF resource supported.
	@param resouceFactory The resource factory to register with the given namespace.
	@exception NullPointerException if the given class, namespace URI, class name, and/or resource factory is <code>null</code>.
	*/
	public TypedURFResourceTURFIO(final Class<T> resourceClass, final URI resourceNamespaceURI, final String resourceClassName, final URFResourceFactory resourceFactory)
	{
		this(resourceClass, resourceNamespaceURI, resourceClassName);	//construct the class
		registerResourceFactory(resourceNamespaceURI, checkInstance(resourceFactory, "Resource factory cannot be null."));  //register the factory for the given namespace
	}

	/**Reads a resource from an input stream using an existing URF instance.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given URF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public T read(final URF urf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		readURF(urf, inputStream, baseURI);	//read URF from the input stream
		final URFResource resource=urf.getResourceByType(getResourceTypeURI());	//load the correct resource
		if(resource==null)	//if there is no resource
		{
			throw new IOException("No resource found in namespace "+getResourceNamespaceURI()+" with class name "+getResourceClassName()+".");	//G***i18n
		}
		return getObjectClass().cast(resource);	//cast the resource to the correct type and return it
	}

	/**Writes a resource to an output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param resource The resource to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public void write(final OutputStream outputStream, final URI baseURI, final T resource) throws IOException
	{
		writeURFResource(outputStream, baseURI, resource);	//write the URF resource as-is
	}

}
