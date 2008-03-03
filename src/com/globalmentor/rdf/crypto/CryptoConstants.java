package com.globalmentor.rdf.crypto;

import java.net.URI;

/**Constants used in Cryptology Ontology (Crypto) processing.
@author Garret Wilson
*/
public interface CryptoConstants
{

	/**The recommended prefix to the Crypto namespace.*/
	public final static String CRYPTO_NAMESPACE_PREFIX="crypto";
	/**The URI to the Crypto namespace.*/
	public final static URI CRYPTO_NAMESPACE_URI=URI.create("http://globalmentor.com/namespaces/2003/crypto#");


		//Crypto ontology class names
	/**The local name of crypto:Digest.*/
	public final static String DIGEST_CLASS_NAME="Digest";
	/**The local name of crypto:DigestMethod.*/
	public final static String DIGEST_METHOD_CLASS_NAME="DigestMethod";

		//general Crypto property names
	/**An algorithm. The local name of crypto:algorithm.*/
	public final static String ALGORITHM_PROPERTY_NAME="algorithm";
	/**The digest of a resouce. The local name of crypto:digest.*/
	public final static String DIGEST_PROPERTY_NAME="digest";

		//Crypto digest property names
	/**The digest method of a digest. The local name of crypto:digestMethod.*/
	public final static String DIGEST_METHOD_PROPERTY_NAME="digestMethod";
	/**The digest value of a digest. The local name of crypto:digestValue.*/
	public final static String DIGEST_VALUE_PROPERTY_NAME="digestValue";

		//Crypto algorithms
	/**The URI identifier of the SHA1 message digest algorithm.*/
	public final static URI SHA1_ALGORITHM_URI=URI.create("http://www.w3.org/2000/09/xmldsig#sha1");	//TODO create a constant for the XML digital signature base URI and create the other URIs by resolving against that URI
}