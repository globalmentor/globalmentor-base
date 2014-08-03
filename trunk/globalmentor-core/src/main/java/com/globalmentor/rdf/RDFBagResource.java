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

import static com.globalmentor.rdf.RDF.*;

/**
 * Represents an RDF bag resource.
 * @author Garret Wilson
 */
public class RDFBagResource extends RDFContainerResource {

	/** @return The namespace URI of the ontology defining the default type of this resource. */
	public URI getDefaultTypeNamespaceURI() {
		return RDF_NAMESPACE_URI;
	}

	/** @return The local name of the default type of this resource. */
	public String getDefaultTypeName() {
		return BAG_CLASS_NAME;
	}

	/**
	 * Constructs an RDF bag resource with a reference URI.
	 * @param newReferenceURI The reference URI for the new resource.
	 */
	public RDFBagResource(final URI newReferenceURI) {
		super(newReferenceURI); //construct the parent class
	}

	/**
	 * Data model and reference URI constructor.
	 * @param rdf The data model associated with the container.
	 * @param newReferenceURI The reference URI for the new resource.
	 */
	RDFBagResource(final RDF rdf, final URI newReferenceURI) {
		super(rdf, newReferenceURI); //construct the parent class
	}

}