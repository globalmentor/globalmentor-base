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

package com.globalmentor.rdf;

import java.io.*;
import java.net.*;

import static com.globalmentor.java.Classes.*;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.rdf.RDFUtilities.*;

/**Class for loading and saving a particular type of RDF resource.
Custom classes can be added using {@link #registerResourceFactory(URI, RDFResourceFactory)}.
@param <T> The type to read and write.
@author Garret Wilson
*/
public class TypedRDFResourceIO<T extends RDFResource> extends AbstractRDFXMLIO<T>
{

	/**The namespace of the RDF resource supported.*/
	private final URI resourceNamespaceURI;

		/**@return The namespace of the RDF resource supported.*/
		public URI getResourceNamespaceURI() {return resourceNamespaceURI;}

	/**The class name of the RDF resource supported.*/
	private final String resourceClassName;

		/**@return The class name of the RDF resource supported.*/
		public String getResourceClassName() {return resourceClassName;}

	/**Class and namespace constructor.
	A {@link DefaultRDFResourceFactory} will be registered with the given namespace, configured to create Java instances from the package of the resource class.
	The class name will be set to the local name of the given resource class.
	@param resourceClass The class representing the type of resource expected from the RDF instance.
	@param resourceNamespaceURI The namespace of the RDF resource supported.
	@exception NullPointerException if the given class and/or namespace URI is <code>null</code>.
	*/
	public TypedRDFResourceIO(final Class<T> resourceClass, final URI resourceNamespaceURI)
	{
		this(resourceClass, resourceNamespaceURI, getLocalName(resourceClass), new DefaultRDFResourceFactory(resourceClass.getPackage()));	//construct the class with a default resource factory
	}

	/**Class, namespace, and class name constructor.
	@param resourceClass The class representing the type of resource expected from the RDF instance.
	@param resourceNamespaceURI The namespace of the RDF resource supported.
	@param resourceClassName The class name of the RDF resource supported.
	@exception NullPointerException if the given class, namespace URI, and/or class name is <code>null</code>.
	*/
	public TypedRDFResourceIO(final Class<T> resourceClass, final URI resourceNamespaceURI, final String resourceClassName)
	{
		super(resourceClass);	//construct the parent class
		this.resourceNamespaceURI=checkInstance(resourceNamespaceURI, "Resource namespace URI must be provided.");
		this.resourceClassName=checkInstance(resourceClassName, "Resource class name must be provided.");
	}

	/**Type constructor with resource factory.
	@param resourceClass The class representing the type of resource expected from the RDF instance.
	@param resourceNamespaceURI The namespace of the RDF resource supported.
	@param resourceClassName The class name of the RDF resource supported.
	@param resouceFactory The resource factory to register with the given namespace.
	@exception NullPointerException if the given class, namespace URI, class name, and/or resource factory is <code>null</code>.
	*/
	public TypedRDFResourceIO(final Class<T> resourceClass, final URI resourceNamespaceURI, final String resourceClassName, final RDFResourceFactory resourceFactory)
	{
		this(resourceClass, resourceNamespaceURI, resourceClassName);	//construct the class
		registerResourceFactory(resourceNamespaceURI, checkInstance(resourceFactory, "Resource factory cannot be null."));  //register the factory for the given namespace
	}

	/**Reads a resource from an input stream using an existing RDF instance.
	@param rdf The RDF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given RDF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public T read(final RDF rdf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		readRDF(rdf, inputStream, baseURI);	//read RDF from the input stream
		final RDFResource resource=getResourceByType(rdf, getResourceNamespaceURI(), getResourceClassName());	//load the correct resource
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
		writeRDFResource(outputStream, baseURI, resource);	//write the RDF resource as-is
	}

}
