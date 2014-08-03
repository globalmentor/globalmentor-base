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

import static com.globalmentor.rdf.RDFResources.*;
import static com.globalmentor.rdf.xeb.RDFXEB.*;

/**
 * A general XEbook binding of content around a spine.
 * @author Garret Wilson
 * @deprecated
 */
public class Binding extends TypedRDFResource {

	/** @return The namespace URI of the ontology defining the default type of this resource. */
	public URI getDefaultTypeNamespaceURI() {
		return XEB_NAMESPACE_URI;
	}

	/** @return The local name of the default type of this resource. */
	public String getDefaultTypeName() {
		return BINDING_CLASS_NAME;
	}

	/** Default constructor. */
	public Binding() {
		super(); //construct the parent class
	}

	/**
	 * Reference URI constructor.
	 * @param referenceURI The reference URI for the new resource.
	 */
	public Binding(final URI referenceURI) {
		super(referenceURI); //construct the parent class
	}

	/**
	 * Set the <code>xeb:spine</code> property of the resource.
	 * @param spineResource The spine resource, an <code>rdf:List</code>.
	 */
	public void setSpine(final RDFListResource spineResource) {
		setProperty(XEB_NAMESPACE_URI, SPINE_PROPERTY_NAME, spineResource); //set the spine of the resource
	}

	/**
	 * Retrieves the spine of the resource. If this resource has more than one property of <code>xeb:spine</code>, it is undefined which of those property values
	 * will be returned.
	 * @return The spine of the resource, or <code>null</code> if no manifest property exists or the manifest is not a list resource.
	 */
	public RDFListResource<RDFResource> getSpine() {
		return (RDFListResource<RDFResource>)asListResource(getPropertyValue(XEB_NAMESPACE_URI, SPINE_PROPERTY_NAME)); //return the spine as a list resource
	}

}
