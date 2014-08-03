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

package com.globalmentor.rdf.crypto;

import java.net.URI;

import com.globalmentor.java.Objects;
import com.globalmentor.rdf.*;

/**
 * Constants and utilities used in Cryptology Ontology (Crypto) processing.
 * <p>
 * This class also serves as a resource factory that knows how to create RDF resources for Crypto resource descriptions.
 * </p>
 * @author Garret Wilson
 */
public class Crypto implements RDFResourceFactory {

	/** The recommended prefix to the Crypto namespace. */
	public final static String CRYPTO_NAMESPACE_PREFIX = "crypto";
	/** The URI to the Crypto namespace. */
	public final static URI CRYPTO_NAMESPACE_URI = URI.create("http://globalmentor.com/namespaces/2003/crypto#");

	//Crypto ontology class names
	/** The local name of crypto:Digest. */
	public final static String DIGEST_CLASS_NAME = "Digest";
	/** The local name of crypto:DigestMethod. */
	public final static String DIGEST_METHOD_CLASS_NAME = "DigestMethod";

	//general Crypto property names
	/** An algorithm. The local name of crypto:algorithm. */
	public final static String ALGORITHM_PROPERTY_NAME = "algorithm";
	/** The digest of a resouce. The local name of crypto:digest. */
	public final static String DIGEST_PROPERTY_NAME = "digest";

	//Crypto digest property names
	/** The digest method of a digest. The local name of crypto:digestMethod. */
	public final static String DIGEST_METHOD_PROPERTY_NAME = "digestMethod";
	/** The digest value of a digest. The local name of crypto:digestValue. */
	public final static String DIGEST_VALUE_PROPERTY_NAME = "digestValue";

	//Crypto algorithms
	/** The URI identifier of the SHA1 message digest algorithm. */
	public final static URI SHA1_ALGORITHM_URI = URI.create("http://www.w3.org/2000/09/xmldsig#sha1"); //TODO create a constant for the XML digital signature base URI and create the other URIs by resolving against that URI

	/**
	 * Sets the <code>crypto:digest</code> property of a resource.
	 * @param resource The resource to which the property should be set.
	 * @param digest A resource representing the message digest.
	 */
	public static void setDigest(final RDFResource resource, final Digest digest) {
		resource.addProperty(CRYPTO_NAMESPACE_URI, DIGEST_PROPERTY_NAME, digest); //add the digest
	}

	/**
	 * Retrieves the digest value of the resource. If this resource has more than one property of <code>crypto:digest</code>, it is undefined which of these
	 * property values will be returned.
	 * @param resource The resource the digest of which will be returned.
	 * @return The digest of the resource, or <code>null</code> if there is no algorithm or the algorithm is not of the correct type.
	 */
	public static Digest getDigest(final RDFResource resource) {
		return Objects.asInstance(resource.getPropertyValue(CRYPTO_NAMESPACE_URI, DIGEST_PROPERTY_NAME), Digest.class); //get the value of the digest property as the correct type
	}

	/**
	 * Creates a resource with the provided reference URI based upon the type reference URI composed of the given XML serialization type namespace and local name.
	 * A type property derived from the specified type namespace URI and local name will be added to the resource.
	 * <p>
	 * This implementation creates Crypto-specific resources.
	 * </p>
	 * @param referenceURI The reference URI of the resource to create, or <code>null</code> if the resource created should be represented by a blank node.
	 * @param typeNamespaceURI The XML namespace used in the serialization of the type URI, or <code>null</code> if the type is not known.
	 * @param typeLocalName The XML local name used in the serialization of the type URI, or <code>null</code> if the type is not known.
	 * @return The resource created with this reference URI, with the given type added if a type was given, or <code>null</code> if no suitable resource can be
	 *         created.
	 */
	public RDFResource createResource(final URI referenceURI, final URI typeNamespaceURI, final String typeLocalName) {
		if(CRYPTO_NAMESPACE_URI.equals(typeNamespaceURI)) { //if this resource is a Crypto resource
			if(DIGEST_CLASS_NAME.equals(typeLocalName)) { //crypto:Digest
				return new Digest(referenceURI); //create and return a new Crypto digest
			} else if(DIGEST_METHOD_CLASS_NAME.equals(typeLocalName)) { //crypto:DigestMethod
				return new DigestMethod(referenceURI); //create and return a new Crypto digest method
			}
		}
		return null; //show that we couldn't create a resource
	}

}