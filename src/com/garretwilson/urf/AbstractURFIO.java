package com.garretwilson.urf;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import static com.garretwilson.lang.Objects.*;

/**Base functionality for loading and saving URF information.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@param <T> The type to read and write.
@author Garret Wilson
*/
public abstract class AbstractURFIO<T> extends TURFNamespaceLabelManager implements URFIO<T>
{

	/**The class representing the type of object being loaded and saved.*/
	private final Class<T> objectClass;

		/**@return The class representing the type of object being loaded and saved.*/
		public Class<T> getObjectClass() {return objectClass;}

	/**A map of resource factories, keyed to namespace URI.*/
	private final Map<URI, URFResourceFactory> resourceFactoryMap=new ConcurrentHashMap<URI, URFResourceFactory>();

		/**Registers a resource factory to be used to create resources with a type from the specified namespace. If a resource factory is already registered for this namespace, it will be replaced.
		@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
		@param factory The resource factory that will be used to create resources of types from this namespace.
		*/
		public void registerResourceFactory(final URI typeNamespaceURI, final URFResourceFactory factory)
		{
			resourceFactoryMap.put(typeNamespaceURI, factory);
		}

		/**Removes the resource factory being used to create resources with a type from the specified namespace. If there is no resource factory registered for this namespace, no action will be taken.
		@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
		*/
		public void unregisterResourceFactory(final URI typeNamespaceURI)
		{
			resourceFactoryMap.remove(typeNamespaceURI);
		}

	/**The map of XML serialization prefixes, keyed by namespace URIs.*/
	private final Map<URI, String> namespaceURIPrefixMap=new HashMap<URI, String>();

		/**Registers the given XML serialization prefix to be used with the given namespace URI.
		If a prefix is already registered with the given namespace, it is replaced with this prefix.
		@param namespaceURI The namespace URI.
		@param prefix The XML serialization prefix to use with the given namespace.
		*/
		public void registerNamespacePrefix(final URI namespaceURI, final String prefix)
		{
			namespaceURIPrefixMap.put(namespaceURI, prefix);	//store the prefix in the map, keyed to the URI
		}

		/**Unregisters the XML serialization prefix for the given namespace URI.
		If no prefix is registered for the given namespace, no action occurs.
		@param namespaceURI The namespace URI.
		*/
		public void unregisterNamespacePrefix(final String namespaceURI, final String prefix)
		{
			namespaceURIPrefixMap.remove(namespaceURI);	//remove whatever prefix is registered with this namespace, if any
		}

	/**Class constructor.
	@param objectClass The class representing the type of object being loaded and saved.
	@exception NullPointerException if the given class is <code>null</code>.
	*/
	public AbstractURFIO(final Class<T> objectClass)
	{
		this.objectClass=checkInstance(objectClass, "Object class must be provided.");
	}

	/**Creates an URF instance for use in reading URF data.
	This version creates a default URF instance and then registers known resource factories.
	@return An URF instance appropriate for populating with data read from some source.
	*/
	protected URF createURF()
	{
		final URF urf=new URF();  //create a new URF data model
		for(final Map.Entry<URI, URFResourceFactory> resourceFactoryEntry:resourceFactoryMap.entrySet())	//for each registered resource factory
		{
			urf.registerResourceFactory(resourceFactoryEntry.getKey(), resourceFactoryEntry.getValue());  //register the factory with the URF data model				
		}
		return urf;	//return the URF with registered resource factories
	}

	/**Reads a resource from an input stream.
	This version delegates to {@link #read(URF, InputStream, URI)} using {@link #createURF()} to create a new URF instance.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public final T read(final InputStream inputStream, final URI baseURI) throws IOException
	{
		return read(createURF(), inputStream, baseURI);	//create a new URF data model, showing the base URI, and read and return the object
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
	public abstract T read(final URF urf, final InputStream inputStream, final URI baseURI) throws IOException;

	/**Reads URF data from an input stream.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if there is an error reading the data.
	*/ 
	protected abstract URF readURF(final URF urf, InputStream inputStream, final URI baseURI) throws IOException;

	/**Writes an URF resource to an output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param resource The resource to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	*/
	protected abstract void writeURFResource(final OutputStream outputStream, final URI baseURI, final URFResource resource) throws IOException;

}
