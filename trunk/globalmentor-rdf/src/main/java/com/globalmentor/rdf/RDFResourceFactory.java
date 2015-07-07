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

import java.net.URI;

/**
 * A factory to create resources.
 * @author Garret Wilson
 * @see RDF
 */
public interface RDFResourceFactory {

	/**
	 * Creates a resource with the provided reference URI based upon the type reference URI composed of the given XML serialization type namespace and local name.
	 * A type property derived from the specified type namespace URI and local name will be added to the resource.
	 * @param referenceURI The reference URI of the resource to create, or <code>null</code> if the resource created should be represented by a blank node.
	 * @param typeNamespaceURI The XML namespace used in the serialization of the type URI, or <code>null</code> if the type is not known.
	 * @param typeLocalName The XML local name used in the serialization of the type URI, or <code>null</code> if the type is not known.
	 * @return The resource created with this reference URI, with the given type added if a type was given, or <code>null</code> if no suitable resource can be
	 *         created.
	 */
	public RDFResource createResource(final URI referenceURI, final URI typeNamespaceURI, final String typeLocalName);

}