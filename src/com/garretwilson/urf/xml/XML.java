package com.garretwilson.urf.xml;

import java.net.URI;

import com.garretwilson.urf.*;
import static com.garretwilson.urf.URF.*;

/**The URF XML ontology.
@author Garret Wilson
*/
public class XML
{

	/**The recommended prefix to the URF XML namespace.*/
	public final static String XML_NAMESPACE_PREFIX="xml";
	/**The URI to the URF XML namespace.*/
	public final static URI XML_NAMESPACE_URI=URI.create("http://urf.name/xml");

		//properties
	/**The DTD of a resource.*/
	public final static URI DTD_PROPERTY_URI=createResourceURI(XML_NAMESPACE_URI, "dtd");
	/**The namespace of a resource.*/
	public final static URI NAMESPACE_PROPERTY_URI=createResourceURI(XML_NAMESPACE_URI, "namespace");
	/**The style of a resource.*/
	public final static URI STYLE_PROPERTY_URI=createResourceURI(XML_NAMESPACE_URI, "style");
	/**The transformation stylesheet of a resource.*/
	public final static URI TRANSFORM_PROPERTY_URI=createResourceURI(XML_NAMESPACE_URI, "transform");

	/**Retrieves an array of XML styles.
	@param resource The resource the styles of which will be returned.
	@return The styles of the resource, if any.
	*/
	public static Iterable<URFResource> getStyles(final URFResource resource)
	{
		return resource.getPropertyValues(STYLE_PROPERTY_URI); //return the styles as URIs
	}

	/**Retrieves an iterable to the XML namespace URIs.
	@param resource The resource the namespaces of which will be returned.
	@return The namespaces of the resource, if any are URIs.
	*/
	public static URI[] getNamespaces(final URFResource resource)
	{
		return asURIs(resource.getPropertyValues(NAMESPACE_PROPERTY_URI)); //return the namespaces as URIs
	}

}
