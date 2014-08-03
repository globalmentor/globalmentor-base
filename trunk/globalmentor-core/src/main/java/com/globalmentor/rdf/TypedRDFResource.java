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
 * An RDF resource that by default adds an <code>rdf:type</code> property upon creation.
 * @author Garret Wilson
 */
public abstract class TypedRDFResource extends DefaultRDFResource {

	/** @return The namespace URI of the ontology defining the default type of this resource; used for adding the initial type property. */
	protected abstract URI getDefaultTypeNamespaceURI();

	/** @return The local name of the default type of this resource; used for adding the initial type property. */
	protected abstract String getDefaultTypeName();

	/** Default constructor that creates a resource without a reference URI. */
	public TypedRDFResource() {
		this((URI)null); //create a resource without a reference URI
	}

	/**
	 * Constructs a resource with a reference URI.
	 * @param referenceURI The reference URI for the new resource.
	 */
	public TypedRDFResource(final URI referenceURI) {
		this(null, referenceURI); //construct the class with no data model
	}

	/**
	 * Convenience constructor that constructs a resource using a namespace URI and local name which will be combined to form the reference URI.
	 * @param newNamespaceURI The XML namespace URI used in the serialization.
	 * @param newLocalName The XML local name used in the serialization.
	 */
	public TypedRDFResource(final URI newNamespaceURI, final String newLocalName) {
		this((RDF)null, newNamespaceURI, newLocalName); //do the default construction, combining the namespace URI and the local name for the reference URI
	}

	/**
	 * Convenience constructor that constructs a resource using a namespace URI and local name which will be combined to form the reference URI.
	 * @param rdf The data model with which this resource should be associated.
	 * @param newNamespaceURI The XML namespace URI used in the serialization.
	 * @param newLocalName The XML local name used in the serialization.
	 */
	public TypedRDFResource(final RDF rdf, final URI newNamespaceURI, final String newLocalName) {
		this(rdf, RDFResources.createReferenceURI(newNamespaceURI, newLocalName)); //do the default construction, combining the namespace URI and the local name for the reference URI
	}

	/**
	 * Constructs a resource with a reference URI from a data model.
	 * @param rdf The data model with which this resource should be associated.
	 * @param referenceURI The reference URI for the new resource.
	 */
	public TypedRDFResource(final RDF rdf, final URI referenceURI) {
		super(rdf, referenceURI); //construct the parent class
		RDFResources.addType(this, getDefaultTypeNamespaceURI(), getDefaultTypeName()); //add the default type
	}

}