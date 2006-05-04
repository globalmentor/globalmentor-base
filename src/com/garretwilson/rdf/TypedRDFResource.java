package com.garretwilson.rdf;

import java.net.URI;

/**An RDF resource that by default adds an <code>rdf:type</code> property upon creation. 
@author Garret Wilson
*/
public abstract class TypedRDFResource extends DefaultRDFResource
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public abstract URI getDefaultTypeNamespaceURI();

	/**@return The local name of the default type of this resource.*/
	public abstract String getDefaultTypeName();

	/**Default constructor that creates a resource without a reference URI.*/
	public TypedRDFResource()
	{
		this((URI)null);	//create a resource without a reference URI
	}

	/**Constructs a resource with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public TypedRDFResource(final URI referenceURI)
	{
		this(null, referenceURI);	//construct the class with no data model
	}

	/**Constructs a resource with a reference URI from a data model.
	@param rdf The data model with which this resource should be associated.
	@param referenceURI The reference URI for the new resource.
	*/
	public TypedRDFResource(final RDF rdf, final URI referenceURI)
	{
		this(rdf, referenceURI, null, null);	//construct the class with no namespace URI or local name
	}

	/**Convenience constructor that constructs a resource using a namespace URI
		and local name which will be combined to form the reference URI.
	@param newNamespaceURI The XML namespace URI used in the serialization.
	@param newLocalName The XML local name used in the serialization.
	*/
	public TypedRDFResource(final URI newNamespaceURI, final String newLocalName)
	{
		this((RDF)null, newNamespaceURI, newLocalName);  //do the default construction, combining the namespace URI and the local name for the reference URI
	}

	/**Convenience constructor that constructs a resource using a namespace URI
		and local name which will be combined to form the reference URI.
	@param rdf The data model with which this resource should be associated.
	@param newNamespaceURI The XML namespace URI used in the serialization.
	@param newLocalName The XML local name used in the serialization.
	*/
	public TypedRDFResource(final RDF rdf, final URI newNamespaceURI, final String newLocalName)
	{
		this(rdf, RDFUtilities.createReferenceURI(newNamespaceURI, newLocalName), newNamespaceURI, newLocalName);  //do the default construction, combining the namespace URI and the local name for the reference URI
	}

	/**Constructs a resource with a reference URI and separate namespace URI and
		local name.
	@param referenceURI The reference URI for the new resource.
	@param newNamespaceURI The XML namespace URI used in the serialization, or
		<code>null</code> if the namespace URI is not known.
	@param newLocalName The XML local name used in the serialization, or
		<code>null</code> if the local name is not known.
	*/
	TypedRDFResource(final URI referenceURI, final URI newNamespaceURI, final String newLocalName)
	{
		this(null, referenceURI, newNamespaceURI, newLocalName);	//construct the resource with no known data model
	}

	/**Constructs a resource with a reference URI and separate namespace URI and
		local name from a data model.
	@param rdf The data model with which this resource should be associated.
	@param referenceURI The reference URI for the new resource.
	@param newNamespaceURI The XML namespace URI used in the serialization, or
		<code>null</code> if the namespace URI is not known.
	@param newLocalName The XML local name used in the serialization, or
		<code>null</code> if the local name is not known.
	@see RDF#createResource
	*/
	TypedRDFResource(final RDF rdf, final URI referenceURI, final URI newNamespaceURI, final String newLocalName)
	{
		super(rdf, referenceURI, newNamespaceURI, newLocalName);  //construct the parent class
		RDFUtilities.addType(this, getDefaultTypeNamespaceURI(), getDefaultTypeName());	//add the default type
	}

}