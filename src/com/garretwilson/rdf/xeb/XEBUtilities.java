package com.garretwilson.rdf.xeb;

import java.net.URI;
import com.garretwilson.rdf.*;
import static com.garretwilson.rdf.RDFUtilities.*;
import static com.garretwilson.rdf.xeb.XEBConstants.*;

/**A set of utilities for working with XEbook resources.
<p>This class also serves as a resource factory that knows how to 
	create RDF resources for XEbook resource descriptions.</p>
@author Garret
*/
public class XEBUtilities implements RDFResourceFactory
{

	/**Set the <code>xeb:spine</code> property of the resource.
	@param resource The resource for which a property should be set.
	@param spineResource The spine resource, an <code>&lt;rdf:List&gt;</code>.
	*/
	public static void setSpine(final RDFResource resource, final RDFListResource spineResource)
	{
		resource.setProperty(XEB_NAMESPACE_URI, SPINE_PROPERTY_NAME, spineResource);	//set the spine of the resource
	}

	/**Retrieves the spine of the resource. If this resource has more than one
		property of <code>xeb:spine</code>, it is undefined which of those
		property values will be returned.
	@param resource The resource the spine of which will be returned.
	@return The spine of the resource, or <code>null</code> if no manifest
		property exists or the manifest is not a list resource.
	*/
	public static RDFListResource getSpine(final RDFResource resource)
	{
		return asListResource(resource.getPropertyValue(XEB_NAMESPACE_URI, SPINE_PROPERTY_NAME)); //return the spine as a list resource
	}

	/**Creates a resource with the provided reference URI based upon the
		type reference URI composed of the given XML serialization type namespace
		and local name
	<p>This method creates XEbook-specific resources.</p>
	@param referenceURI The non-<code>null</code> reference URI of the resource
		to create.
	@param typeNamespaceURI The XML namespace used in the serialization of the
		type URI, or <code>null</code> if the type is not known.
	@param typeLocalName The XML local name used in the serialization of the type
		URI, or <code>null</code> if the type is not known.
	@return The resource created with this reference URI, with the given type
		added if a type was given.
	*/
	public RDFResource createResource(final URI referenceURI, final URI typeNamespaceURI, final String typeLocalName)
	{
		if(XEB_NAMESPACE_URI.equals(typeNamespaceURI)) //if this resource is an XEbook resource
		{
			if(BOOK_CLASS_NAME.equals(typeLocalName)) //xeb:Book
			{
				return new Book(referenceURI);  //create and return a new book
			}
		}
		return null;  //show that we couldn't create a resource
	}
}
