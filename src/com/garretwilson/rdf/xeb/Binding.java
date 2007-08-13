package com.garretwilson.rdf.xeb;

import java.net.URI;
import com.garretwilson.rdf.*;

import static com.garretwilson.rdf.RDFUtilities.asListResource;
import static com.garretwilson.rdf.xeb.XEBConstants.*;

/**A general XEbook binding of content around a spine.
@author Garret Wilson
*/
public class Binding extends TypedRDFResource
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return XEB_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return BINDING_CLASS_NAME;}

	/**Default constructor.*/
	public Binding()
	{
		super();	//construct the parent class
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Binding(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Set the <code>xeb:spine</code> property of the resource.
	@param spineResource The spine resource, an <code>rdf:List</code>.
	*/
	public void setSpine(final RDFListResource spineResource)
	{
		setProperty(XEB_NAMESPACE_URI, SPINE_PROPERTY_NAME, spineResource);	//set the spine of the resource
	}

	/**Retrieves the spine of the resource. If this resource has more than one
		property of <code>xeb:spine</code>, it is undefined which of those
		property values will be returned.
	@return The spine of the resource, or <code>null</code> if no manifest
		property exists or the manifest is not a list resource.
	*/
	public RDFListResource<RDFResource> getSpine()
	{
		return (RDFListResource<RDFResource>)asListResource(getPropertyValue(XEB_NAMESPACE_URI, SPINE_PROPERTY_NAME)); //return the spine as a list resource
	}

}
