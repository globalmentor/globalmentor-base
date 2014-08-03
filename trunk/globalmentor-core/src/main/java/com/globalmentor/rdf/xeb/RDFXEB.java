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

package com.globalmentor.rdf.xeb;

import java.net.URI;

import com.globalmentor.rdf.*;

/**
 * Constants and utilities for working with an XEBook (XEB) in RDF.
 * <p>
 * This class also serves as a resource factory that knows how to create RDF resources for XEbook resource descriptions.
 * </p>
 * @author Garret
 * @deprecated
 */
public class RDFXEB implements RDFResourceFactory {

	/** The name extension for XEB book files. */
	public final static String XEB_NAME_EXTENSION = "xeb";

	/** The recommended prefix to the XEB namespace. */
	public final static String XEB_NAMESPACE_PREFIX = "xeb";
	/** The URI to the XEB namespace. */
	public final static URI XEB_NAMESPACE_URI = URI.create("http://xebook.org/namespaces/2003/xebook#");

	//XEbook class names
	/** The local name of xeb:Book. */
	public final static String BOOK_CLASS_NAME = "Book";
	/** The local name of xeb:Publication. */
	public final static String PUBLICATION_CLASS_NAME = "Publication";
	/** The local name of xeb:Binding. */
	public final static String BINDING_CLASS_NAME = "Binding";

	//XEbook property names
	/** The spine of a book. The local name of xeb:spine. */
	public final static String SPINE_PROPERTY_NAME = "spine";

	/**
	 * Creates a resource with the provided reference URI based upon the type reference URI composed of the given XML serialization type namespace and local name.
	 * A type property derived from the specified type namespace URI and local name will be added to the resource.
	 * <p>
	 * This implementation creates XEbook-specific resources.
	 * </p>
	 * @param referenceURI The reference URI of the resource to create, or <code>null</code> if the resource created should be represented by a blank node.
	 * @param typeNamespaceURI The XML namespace used in the serialization of the type URI, or <code>null</code> if the type is not known.
	 * @param typeLocalName The XML local name used in the serialization of the type URI, or <code>null</code> if the type is not known.
	 * @return The resource created with this reference URI, with the given type added if a type was given, or <code>null</code> if no suitable resource can be
	 *         created.
	 */
	public RDFResource createResource(final URI referenceURI, final URI typeNamespaceURI, final String typeLocalName) {
		if(RDFXEB.XEB_NAMESPACE_URI.equals(typeNamespaceURI)) { //if this resource is an XEbook resource
			if(RDFXEB.BINDING_CLASS_NAME.equals(typeLocalName)) { //xeb:Binding
				return new Binding(referenceURI); //create and return a new binding
			}
			if(RDFXEB.BOOK_CLASS_NAME.equals(typeLocalName)) { //xeb:Book
				return new Book(referenceURI); //create and return a new book
			}
		}
		return null; //show that we couldn't create a resource
	}
}
