package com.garretwilson.rdf.crypto;

import java.net.URI;
import com.garretwilson.lang.ObjectUtilities;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.Base64BinaryLiteral;

/**Crypto digest information.
@author Garret Wilson
*/
public class Digest extends TypedRDFResource implements CryptoConstants
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return CRYPTO_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return DIGEST_CLASS_NAME;}

	/**Default constructor.*/
	public Digest()
	{
		super();	//construct the parent class
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Digest(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Sets the <code>crypto:digestMethod</code> property of the resource.
	@param method The new digest method.
	*/
	public void setDigestMethod(final DigestMethod method)
	{
		setProperty(CRYPTO_NAMESPACE_URI, DIGEST_METHOD_PROPERTY_NAME, method); //add the digest method
	}

	/**Sets the <code>crypto:digestMethod</code> property of the digest to a
	digest method with the given algorithm.
	@param algorithm A resource representing the algorithm.
	@see #setDigestMethod(DigestMethod)
	*/
	public void setDigestMethodAlgorithm(final RDFResource algorithm)
	{
		final DigestMethod digestMethod=new DigestMethod();	//create a new digest method G***can we somehow use the existing RDF data model, if there is one?
		digestMethod.setAlgorithm(algorithm);	//set the algorithm of the digest method
		setDigestMethod(digestMethod);	//set the digest method to the one we created
	}

	/**Sets the <code>crypto:digestMethod</code> property of the digest to a
	digest method with the given algorithm identifier.
	@param algorithmURI A URI representing the algorithm.
	@see #setDigestMethodAlgorithm(RDFResource)
	*/
	public void setDigestMethodAlgorithm(final URI algorithmURI)
	{
		setDigestMethodAlgorithm(new DefaultRDFResource(algorithmURI));	//set the algorithm to a new resource with the correct URI
	}

	/**Retrieves the digest method algorithm of the resource. If this resource has more
		than one property of <code>crypto:digestMethod</code>, it is undefined which
		of these property values will be returned.
	@return The digest method algorithm of the resource, or <code>null</code> if there is no
		digest method or the digest method is not of the correct type, or the digest
		method has no algorithm specified.
	@see #getDigestMethod()
	*/
	public RDFResource getDigestMethodAlgorithm()
	{
		final DigestMethod digestMethod=getDigestMethod();	//get the digest method, if there is one
		return digestMethod!=null ? digestMethod.getAlgorithm() : null;	//return the digest method algorithm, if there is a digest method
	}

	/**Retrieves the digest method of the resource. If this resource has more
		than one property of <code>crypto:digestMethod</code>, it is undefined which
		of these property values will be returned.
	@return The digest method of the resource, or <code>null</code> if there is no
		digest method or the digest method is not of the correct type.
	*/
	public DigestMethod getDigestMethod()
	{
		return ObjectUtilities.asInstance(getPropertyValue(CRYPTO_NAMESPACE_URI, DIGEST_METHOD_PROPERTY_NAME), DigestMethod.class); //get the value of the digest method property as the correct type
	}

	/**Retrieves the digest value of the resource. If this resource has more
		than one property of <code>crypto:digestValue</code>, it is undefined which
		of these property values will be returned.
	@return The digest value of the resource, or <code>null</code> if there is no
		digest value or the digest value is not of the correct type.
	*/
	public byte[] getDigestValue()
	{
		return Base64BinaryLiteral.asBytes(getPropertyValue(CRYPTO_NAMESPACE_URI, DIGEST_VALUE_PROPERTY_NAME)); //get the binary data of the digest value property if such a property exists
	}

	/**Sets the <code>crypto:digestValue</code> property of the resource.
	@param value The new digest value.
	*/
	public void setDigestValue(final byte[] value)
	{
		setProperty(CRYPTO_NAMESPACE_URI, DIGEST_VALUE_PROPERTY_NAME, new Base64BinaryLiteral(value)); //add the digest value
	}

}
