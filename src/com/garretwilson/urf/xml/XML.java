package com.garretwilson.urf.xml;

import java.net.URI;

import com.garretwilson.urf.*;
import static com.garretwilson.urf.URF.*;

/**The URF XML ontology.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class XML
{

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
