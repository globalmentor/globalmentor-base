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

package com.globalmentor.rdf.xpackage;

import java.net.*;
import java.util.List;

import com.globalmentor.net.*;
import com.globalmentor.rdf.*;
import static com.globalmentor.rdf.RDF.*;
import static com.globalmentor.rdf.RDFResources.*;
import com.globalmentor.rdf.xmlschema.URILiteral;
import com.globalmentor.text.xml.XML;
import com.globalmentor.text.xml.xlink.XLink;
import com.globalmentor.util.Debug;

import org.w3c.dom.*;

/**Utilities for working with XPackage RDF.
@author Garret Wilson
@deprecated
*/
public class XPackage
{

	/**The recommended prefix to the XPackage ontology namespace.*/
	public final static String XPACKAGE_NAMESPACE_PREFIX="xpackage";
	/**The URI to the XPackage namespace.*/
	public final static URI XPACKAGE_NAMESPACE_URI=URI.create("http://xpackage.org/namespaces/xpackage#");

		//XPackage package description document names
	/**The local name of the XPackage xpackage:description element.*/
	public final static String ELEMENT_DESCRIPTION="description";

		//XPackage ontology property names
	/**The alternate of a resource. The local name of xpackage:alternate.*/
	public final static String ALTERNATE_PROPERTY_NAME="alternate";
	/**The location of a resource. The local name of xpackage:location.*/
	public final static String LOCATION_PROPERTY_NAME="location";
	/**The icon of a resource. The local name of xpackage:icon.*/
	public final static String ICON_PROPERTY_NAME="icon";

	/**Adds an alternate to a resource.
	@param resource The resource to which the property should be added.
	@param alternate The alternate to add.
	*/
	public static void addAlternate(final RDFResource resource, final RDFObject alternate)
	{
		resource.addProperty(XPACKAGE_NAMESPACE_URI, ALTERNATE_PROPERTY_NAME, alternate);	//add the alternate to the resource
	}

	/**@return An iterable to alternates, if any, of a resource.
	@param resource The resource for which properties should be iterated.
	*/
	public static Iterable<RDFObject> getAlternateIterator(final RDFResource resource)
	{
		return resource.getPropertyValues(XPACKAGE_NAMESPACE_URI, ALTERNATE_PROPERTY_NAME); //return an iterator to the alternate properties
	}

	/**Adds an <code>&lt;xpackage:location&gt;</code> property to the resource.
	The link will be set to <code>xlink:type="simple"</code>.
	@param resource The resource to which a property should be added.
	@param href The location of the resource, to become an <code>xlink:href</code>
		property of the location property.
	@return The new location resource.
	*/
	public static RDFResource addLocation(final RDFResource resource, final String href)
	{
		final RDFResource locationResource=RDFResources.locateResource(resource, null); //create an anonymous location resource
			//add the xlink:href property to the location
		locationResource.addProperty(XLink.XLINK_NAMESPACE_URI, XLink.ATTRIBUTE_HREF, href);
			//add the xlink:type="simple" property to the location
		locationResource.addProperty(XLink.XLINK_NAMESPACE_URI, XLink.ATTRIBUTE_TYPE, XLink.SIMPLE_TYPE);
			//add the location property to the resource
		resource.addProperty(XPACKAGE_NAMESPACE_URI, LOCATION_PROPERTY_NAME, locationResource);
		return locationResource;  //return the location resource we created
	}

	/**Sets the <code>xpackage:location</code> property to the resource.
	The link will be set to <code>xlink:type="simple"</code>.
	@param resource The resource to which a property should be added.
	@param href The location of the resource, to become an <code>xlink:href</code>
		property of the location property.
	@return The new location resource.
	*/
	public static RDFResource setLocation(final RDFResource resource, final String href)
	{
			//remove all location properties from the resource
		resource.removeProperties(XPACKAGE_NAMESPACE_URI, LOCATION_PROPERTY_NAME);
		return addLocation(resource, href);	//add the location to the resource
	}

	/**Creates a default XPackage package description document.
	@param domImplementation The DOM implementation to use.
	@return A newly created default XPackage package description document with an
		RDF section.
	*/
	public static Document createDefaultXPackageDocument(final DOMImplementation domImplementation)
	{
		  //create an XPackage package description document
		final Document document=domImplementation.createDocument(XPACKAGE_NAMESPACE_URI.toString(), XML.createQualifiedName(XPACKAGE_NAMESPACE_PREFIX, ELEMENT_DESCRIPTION), null);
		  //get the xpackage:description element
		final Element descriptionElement=document.getDocumentElement();
		  //add the RDF namespace declaration prefix, xmlns:rdf
		descriptionElement.setAttributeNS(XML.XMLNS_NAMESPACE_URI.toString(), XML.createQualifiedName(XML.XMLNS_NAMESPACE_PREFIX, RDF_NAMESPACE_PREFIX), RDF_NAMESPACE_URI.toString());
		  //add the XPackage namespace declaration prefix, xmlns:xpackage
		descriptionElement.setAttributeNS(XML.XMLNS_NAMESPACE_URI.toString(), XML.createQualifiedName(XML.XMLNS_NAMESPACE_PREFIX, XPACKAGE_NAMESPACE_PREFIX), XPACKAGE_NAMESPACE_URI.toString());
		  //add the XLink namespace declaration prefix, xmlns:xlink
		descriptionElement.setAttributeNS(XML.XMLNS_NAMESPACE_URI.toString(), XML.createQualifiedName(XML.XMLNS_NAMESPACE_PREFIX, XLink.XLINK_NAMESPACE_PREFIX), XLink.XLINK_NAMESPACE_URI.toString());
		final Element rdfElement=RDFXMLGenerator.createRDFElement(document);  //create an <rdf:RDF> element
		descriptionElement.appendChild(rdfElement);	//add the RDF element to the document
		return document;  //return the document we created
	}

	/**Creates an object that will appropriately generate an XPackage-compliant
		XML representation of RDF data models.
		The created <code>RDFXMLifier</code> will be initialized to encode
		properties from the XLink namespace as XML attributes, for example.
	@return An object for creating an XML tree from RDF data.
	*/
	public static RDFXMLGenerator createRDFXMLifier()
	{
		final RDFXMLGenerator rdfXMLifier=new RDFXMLGenerator(false);  //create a new RDF XMLifier that isn't compact
		  //show that XLink properties should be serialized as attributes
		rdfXMLifier.addLiteralAttributeSerializationNamespaceURI(XLink.XLINK_NAMESPACE_URI);
		return rdfXMLifier; //return the XMLifier we constructed
	}

	/**Retrieves the icon URI of the resource. If this resource has more than one property of <code>xpackage:icon</code>, it is undefined which of those property values will be returned.
	@param resource The resource the icon of which will be returned.
	@return The icon URI of the resource, or <code>null</code> if no valid icon property exists.
	*/
	public static URI getIcon(final RDFResource resource)
	{
		return asURI(resource.getPropertyValue(XPACKAGE_NAMESPACE_URI, ICON_PROPERTY_NAME)); //get the icon URI, if any, and return it
	}

	/**Sets the <code>xpackage:icon</code> property to the resource.
	@param resource The resource on which the property should be set.
	@param iconURI The URI of the icon, or <code>null</code> if there should be no icon property.
	*/
	public static void setIcon(final RDFResource resource, final URI iconURI)
	{
		resource.setProperty(XPACKAGE_NAMESPACE_URI, ICON_PROPERTY_NAME, iconURI!=null ? new URILiteral(iconURI) : null);	//set the icon property
	}	
	
	/**Returns an item resource in the given RDF list that has a matching
		xpackage:location with an xlink:href that matches the requested href,
		relative to the given URI.
	@param list The list of RDF resources.
	@param baseURI The base URI of the package, used to construct absolute URIs
		from relative URIs.
	@param href The relative or absolute reference to the item, which will be
		converted to an absolute URI in order to compare it with each item's fully
		qualified URI.
	@return The item whose fully qualified URI matches the fully qualified
		version of the specified href, or <code>null</code> if there is no match.
	@see #getLocationHRef
	*/
	public static RDFResource getItemByLocationHRef(final List<RDFObject> list, final URI baseURI, final String href)	//TODO should we put this in RDFListResource?
	{
		try
		{
		  final URI absoluteURI=URIs.createURI(baseURI, href);	//create a URI based upon the base URI and the given file location
			return getItemByLocationHRef(list, baseURI, absoluteURI);	//look up the item based upon the URI we formed
		}
		catch(URISyntaxException uriSyntaxException)	//if there is an error with the URI
		{
//TODO fix			Debug.error(uriSyntaxException);	//log the error
			return null;	//that simply means we can't find the item
		}
	}

	/**Returns an item resource in the given RDF list that has a matching
		xpackage:location with an xlink:href that matches the requested URI,
		relative to the given base URI.
	@param list The list of RDF resources.
	@param baseURI The base URI of the package, used to construct absolute URIs
		from relative URIs.
	@param uri The absolute reference to the item, which will be compare with
		each item's fully qualified URI.
	@return The item whose fully qualified URI matches the given URI, or
		<code>null</code> if there is no match.
	@see #getLocationHRef
	*/
	public static RDFResource getItemByLocationHRef(final List<RDFObject> list, final URI baseURI, final URI uri)	//TODO should we put this in RDFListResource?
	{
		for(final RDFObject item:list)	//look at each resource in the list
		{
			if(item instanceof RDFResource)	//if this is a resource
			{
				final RDFResource resource=(RDFResource)item;	//get the resource
			  final String itemHRef=getLocationHRef(resource);  //get the item's href TODO later add something that can look at all the locations rather than just the first one
				if(itemHRef!=null)  //if there is an href
				{
					try
					{
						final URI itemURI=URIs.createURI(baseURI, itemHRef);	//create a URI based upon the base URI and the item's location
						if(uri.equals(itemURI)) //if the URLs match
							return resource;  //return the item
					}
					catch(URISyntaxException uriSyntaxException)	//if there is an error creating the URI
					{
						Debug.warn(uriSyntaxException);	//warn about the error, but keep searching
					}
				}
			}
		}
		return null;  //show that no location matched
	}

	/**Returns an item resource in the manifest of the given resource, if present,
		that has a matching xpackage:location with an xlink:href that matches the
		requested href, relative to the given URL.
	@param resource The resource the manifest of which should be searched.
	@param baseURI The base URI of the package, used to construct absolute URIs
		from relative URIs.
	@param href The relative or absolute reference to the item, which will be
		converted to an absolute URI in order to compare it with each item's fully
		qualified URI.
	@return The item whose fully qualified URI matches the fully qualified
		version of the specified href, or <code>null</code> if there is no match.
	@see #getLocationHRef
	*/
/*TODO del
	public static RDFResource getManifestItemByLocationHRef(final RDFResource resource, final URI baseURI, final String href)
	{
		final RDFListResource manifest=Marmot.getContents(resource);  //get the manifest of this resource
		if(manifest!=null)  //if this resource has a manifest
		{
			return getItemByLocationHRef(manifest, baseURI, href);  //get an item in the manifest that matches the given location
		}
		else  //if there is no manifest
		{
			return null;  //show that we couldn't find a matching item resource
		}
	}
*/

	/**Returns an item resource in the manifest of the given resource, if present,
		that has a matching xpackage:location with an xlink:href that matches the requested URI,
		relative to the given base URI.
	@param resource The resource the manifest of which should be searched.
	@param baseURI The base URI of the package, used to construct absolute URIs
		from relative URIs.
	@param uri The absolute reference to the item, which will be compare with
		each item's fully qualified URI.
	@return The item whose fully qualified URI matches the given URI, or
		<code>null</code> if there is no match.
	@see #getLocationHRef
	*/
/*TODO del
	public static RDFResource getManifestItemByLocationHRef(final RDFResource resource, final URI baseURI, final URI uri)
	{
		final RDFListResource manifest=Marmot.getContents(resource);  //get the manifest of this resource
		if(manifest!=null)  //if this resource has a manifest
		{
			return getItemByLocationHRef(manifest, baseURI, uri);  //get an item in the manifest that matches the given location
		}
		else  //if there is no manifest
		{
			return null;  //show that we couldn't find a matching item resource
		}
	}
*/

	/**Returns an item resource in the given RDF container that has a matching
		xpackage:location with an xlink:href that matches the requested URI,
		relative to the given base URI.
	@param rdfContainer The container that hold RDF resources.
	@param baseURI The base URI of the package, used to construct absolute URIs
		from relative URIs.
	@param uri The absolute reference to the item, which will be compare with
		each item's fully qualified URI.
	@return The item whose fully qualified URI matches the given URI, or
		<code>null</code> if there is no match.
	@see #getLocationHRef
	*/
/*TODO del if not needed
	public static RDFResource getItemByLocationHRef(final RDFContainerResource rdfContainer, final URI baseURI, final URI uri)
	{
		final Iterator itemIterator=rdfContainer.getItemIterator(); //get an iterator to the items in this container
		while(itemIterator.hasNext()) //while there are more items in this container
		{
			final RDFResource item=(RDFResource)itemIterator.next(); //get the next item
		  final String itemHRef=getLocationHRef(item);  //get the item's href TODO later add something that can look at all the locations rather than just the first one
			if(itemHRef!=null)  //if there is an href
			{
				try
				{
					final URI itemURI=URIUtilities.createURI(baseURI, itemHRef);	//create a URI based upon the base URI and the item's location
					if(uri.equals(itemURI)) //if the URLs match
						return item;  //return the item
				}
				catch(URISyntaxException uriSyntaxException)	//if there is an error creating the URI
				{
					Debug.warn(uriSyntaxException);	//warn about the error, but keep searching
				}
			}
		}
		return null;  //show that no location matched
	}
*/

	/**Retrieves the location resource of the resource. If this resource has more
		than one property of <code>xpackage:location</code>, it is undefined which
		of those property values will be returned.
	@param resource The resource the manifest of which will be returned.
	@return The location of the resource, or <code>null</code> if no location
		property exists.
	@exception ClassCastException Thrown if the location is not a resource (an
		XPackage location should never be a literal).
	*/
	public static RDFResource getLocation(final RDFResource resource) throws ClassCastException
	{
		return (RDFResource)resource.getPropertyValue(XPACKAGE_NAMESPACE_URI, LOCATION_PROPERTY_NAME); //return the location, cast to a resource
	}

	/**Retrieves the location href of the resource, which is the literal href
		property of the location property resource.
		If this resource has more than one property of
		<code>xpackage:location</code>, it is undefined which of those properties
		will be used.
	@param resource The resource the manifest of which will be returned.
	@return The location href of the resource, or <code>null</code> if no location
		property exists, or if the location has no href.
	@exception ClassCastException Thrown if the location is not a resource (an
		XPackage location should never be a literal) or if the href is not a literal.
	*/
	public static String getLocationHRef(final RDFResource resource) throws ClassCastException
	{
		final RDFResource locationResource=getLocation(resource);  //get the location resource
		if(locationResource!=null)  //if there this resource has a location
		{
			final RDFLiteral hrefLiteral=(RDFLiteral)locationResource.getPropertyValue(XLink.XLINK_NAMESPACE_URI, XLink.ATTRIBUTE_HREF);  //get the XLink href value
	  	return hrefLiteral!=null ? hrefLiteral.getLexicalForm() : null;  //return the href value or null if there is no href
		}
		else  //if there is no location
			return null;  //show that there is no location
	}

}