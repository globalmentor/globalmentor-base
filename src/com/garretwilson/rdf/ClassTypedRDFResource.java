package com.garretwilson.rdf;

import java.net.URI;

import com.garretwilson.lang.ClassUtilities;
import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.rdf.RDFUtilities.*;

/**An RDF resource that by default adds an <code>rdf:type</code> property upon creation.
By default the type name will be set to the local name of the class. 
@author Garret Wilson
*/
public abstract class ClassTypedRDFResource extends TypedRDFResource
{

	/**The namespace URI of the ontology defining the default type of this resource; used for adding the initial type property.*/
	private final URI defaultTypeNamespaceURI;

		/**@return The namespace URI of the ontology defining the default type of this resource; used for adding the initial type property.*/
		protected URI getDefaultTypeNamespaceURI() {return defaultTypeNamespaceURI;}

	/**Returns the local name used to add the initial type property.
	This version returns the local name of the class.
	@return The local name of the default type of this resource; used for adding the initial type property.
	*/
	protected String getDefaultTypeName() {return ClassUtilities.getLocalName(getClass());}

	/**Constructs a resource with a reference URI.
	@param referenceURI The reference URI for the new resource.
	@param typeNamespaceURI The namespace of the type URI to be added after construction of the object.
	@exception NullPointerException if the given type namespace URI is <code>null</code>.
	*/
	public ClassTypedRDFResource(final URI referenceURI, final URI typeNamespaceURI)
	{
		this(null, referenceURI, typeNamespaceURI);	//construct the class with no data model
	}

	/**Convenience constructor that constructs a resource using a namespace URI and local name which will be combined to form the reference URI.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@param typeNamespaceURI The namespace of the type URI to be added after construction of the object.
	@exception NullPointerException if the given type namespace URI is <code>null</code>.
	*/
	public ClassTypedRDFResource(final URI namespaceURI, final String localName, final URI typeNamespaceURI)
	{
		this((RDF)null, namespaceURI, localName, typeNamespaceURI);  //do the default construction, combining the namespace URI and the local name for the reference URI
	}

	/**Convenience constructor that constructs a resource using a namespace URI and local name which will be combined to form the reference URI.
	@param rdf The data model with which this resource should be associated.
	@param namespaceURI The XML namespace URI used in the serialization.
	@param localName The XML local name used in the serialization.
	@param typeNamespaceURI The namespace of the type URI to be added after construction of the object.
	@exception NullPointerException if the given type namespace URI is <code>null</code>.
	*/
	public ClassTypedRDFResource(final RDF rdf, final URI namespaceURI, final String localName, final URI typeNamespaceURI)
	{
		this(rdf, createReferenceURI(namespaceURI, localName), typeNamespaceURI);  //do the default construction, combining the namespace URI and the local name for the reference URI
	}

	/**Constructs a resource with a reference URI from a data model.
	@param rdf The data model with which this resource should be associated.
	@param referenceURI The reference URI for the new resource.
	@param typeNamespaceURI The namespace of the type URI to be added after construction of the object.
	@exception NullPointerException if the given type namespace URI is <code>null</code>.
	*/
	public ClassTypedRDFResource(final RDF rdf, final URI referenceURI, final URI typeNamespaceURI)
	{
		super(rdf, referenceURI);  //construct the parent class
		this.defaultTypeNamespaceURI=checkInstance(typeNamespaceURI, "Type namespace URI cannot be null.");
	}

}