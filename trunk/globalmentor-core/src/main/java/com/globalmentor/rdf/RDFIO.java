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

import com.globalmentor.io.IO;

/**Support for reading or writing a particular type stored in RDF.
@param <T> The type to read and write.
@author Garret Wilson
*/
public interface RDFIO<T> extends IO<T>
{

	/**Reads a resource from an input stream using an existing RDF instance.
	@param rdf The RDF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given RDF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	*/ 
	public T read(final RDF rdf, final InputStream inputStream, final URI baseURI) throws IOException;

	/**Registers a resource factory to be used to create resources with a type from the specified namespace. If a resource factory is already registered for this namespace, it will be replaced.
	@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
	@param factory The resource factory that will be used to create resources of types from this namespace.
	*/
	public void registerResourceFactory(final URI typeNamespaceURI, final RDFResourceFactory factory);

	/**Removes the resource factory being used to create resources with a type from the specified namespace. If there is no resource factory registered for this namespace, no action will be taken.
	@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
	*/
	public void unregisterResourceFactory(final URI typeNamespaceURI);

}
