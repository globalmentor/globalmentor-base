package com.garretwilson.rdf.xpackage;

import java.io.File;
import java.net.*;
import java.util.Iterator;
import com.garretwilson.io.*;
import com.garretwilson.net.*;
//G***del import com.garretwilson.rdf.dublincore.DCConstants;
import com.garretwilson.text.xml.XMLConstants;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.xlink.XLinkConstants;
import com.garretwilson.rdf.*;
import com.garretwilson.util.Debug;
import org.w3c.dom.*;

import java.net.URI;	//G***del when other URI is removed

/**Utilities for working woth XPackage RDF.
@author Garret Wilson
*/
public class XPackageUtilities implements XPackageConstants
{

	/**Sets the <code>&lt;xpackage:children&gt;</code> property of the resource.
	@param rdf The RDF data model to provide to look up property resources.
	@param resource The resource to which a property should be added.
	@param childrenList The list of children.
	@return The added list of children.
	*/
	public static RDFListResource setChildren(final RDF rdf, final RDFResource resource, final RDFListResource childrenList)
	{
		RDFUtilities.setProperty(rdf, resource, XPACKAGE_NAMESPACE_URI, CHILDREN_PROPERTY_NAME, childrenList);	//add the children property to the resource
		return childrenList;  //return the list of children we added
	}

	/**Adds an <code>&lt;xpackage:location&gt;</code> property to the resource.
	@param rdf The RDF data model to provide to look up property resources.
	@param resource The resource to which a property should be added.
	@param href The location of the resource, to become an <code>xlink:href</code>
		property of the location property.
	@return The new location resource.
	*/
	public static RDFResource addLocation(final RDF rdf, final RDFResource resource, final String href)
	{
		final RDFResource locationResource=rdf.createResource(); //create an anonymous location resource
//G***del; the location doesn't have a type		RDFUtilities.addType(rdf, locationResource, XPACKAGE_NAMESPACE_URI, LOCATION);  //set the location type to xpackage:location
			//add the XLink:href to the location
		RDFUtilities.addProperty(rdf, locationResource, XLinkConstants.XLINK_NAMESPACE_URI, XLinkConstants.HREF, href);
			//add the location property to the resource
		RDFUtilities.addProperty(rdf, resource, XPACKAGE_NAMESPACE_URI, LOCATION_PROPERTY_NAME, locationResource);
		return locationResource;  //return the location resource we created
	}

	/**Adds an <code>&lt;xpackage:manifest&gt;</code> property to the resource.
	@param rdf The RDF data model to provide to look up property resources.
	@param resource The resource to which a property should be added.
	@return The new manifest resource, an <code>&lt;rdf:Bag&gt;</code>.
	*/
	public static RDFBagResource addManifest(final RDF rdf, final RDFResource resource)
	{
		  //create an anonymous manifest resource from the data model
		final RDFBagResource manifestResource=(RDFBagResource)rdf.createResource(null, RDFConstants.RDF_NAMESPACE_URI, RDFConstants.BAG_TYPE_NAME);	//G***maybe creaate a utility method for this
			//add the manifest property to the resource
		RDFUtilities.addProperty(rdf, resource, XPACKAGE_NAMESPACE_URI, MANIFEST_PROPERTY_NAME, manifestResource);
		return manifestResource;  //return the manifest resource we created
	}

	/**Adds an <code>&lt;xpackage:organization&gt;</code> property to the resource.
	@param rdf The RDF data model to provide to look up property resources.
	@param resource The resource to which a property should be added.
	@return The new manifest resource, an <code>&lt;rdf:Seq&gt;</code>.
	*/
	public static RDFSequenceResource addOrganization(final RDF rdf, final RDFResource resource)
	{
		  //create an anonymous organization resource from the data model
		final RDFSequenceResource organizationResource=(RDFSequenceResource)rdf.createResource(null, RDFConstants.RDF_NAMESPACE_URI, RDFConstants.SEQ_TYPE_NAME);	//G***maybe create a utility method for this
			//add the organization property to the resource
		RDFUtilities.addProperty(rdf, resource, XPACKAGE_NAMESPACE_URI, ORGANIZATION_PROPERTY_NAME, organizationResource);
		return organizationResource;  //return the organization resource we created
	}

	/**Creates a default XPackage package description document.
	@param domImplementation The DOM implementation to use.
	@return A newly created default XPackage package description document with an
		RDF section.
	*/
	public static Document createDefaultXPackageDocument(final DOMImplementation domImplementation)
	{
//G***del		final XMLDOMImplementation domImplementation=new XMLDOMImplementation();	//create a new DOM implementation G***later use some Java-specific stuff
//G***del if not needed		final DocumentType documentType=domImplementation.createDocumentType(ELEMENT_DESCRIPTION, OEB101_DOCUMENT_PUBLIC_ID, OEB101_DOCUMENT_SYSTEM_ID);	//create an OEB document type
//G***del		final Document document=domImplementation.createDocument(OEB1_DOCUMENT_NAMESPACE_URI, ELEMENT_HTML, documentType);	//create an OEB XML document
		  //create an XPackage package description document
		final Document document=domImplementation.createDocument(XPACKAGE_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(XPACKAGE_NAMESPACE_PREFIX, ELEMENT_DESCRIPTION), null);
		  //get the xpackage:description element
		final Element descriptionElement=document.getDocumentElement();
		  //add the RDF namespace declaration prefix, xmlns:rdf
		descriptionElement.setAttributeNS(XMLConstants.XMLNS_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(XMLConstants.XMLNS_NAMESPACE_PREFIX, RDFConstants.RDF_NAMESPACE_PREFIX), RDFConstants.RDF_NAMESPACE_URI.toString());
		  //add the XPackage namespace declaration prefix, xmlns:xpackage
		descriptionElement.setAttributeNS(XMLConstants.XMLNS_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(XMLConstants.XMLNS_NAMESPACE_PREFIX, XPACKAGE_NAMESPACE_PREFIX), XPACKAGE_NAMESPACE_URI.toString());
		  //add the XLink namespace declaration prefix, xmlns:xlink
		descriptionElement.setAttributeNS(XMLConstants.XMLNS_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(XMLConstants.XMLNS_NAMESPACE_PREFIX, XLinkConstants.XLINK_NAMESPACE_PREFIX), XLinkConstants.XLINK_NAMESPACE_URI.toString());
		final Element rdfElement=RDFXMLifier.createRDFElement(document);  //create an <rdf:RDF> element
		descriptionElement.appendChild(rdfElement);	//add the RDF element to the document
		return document;  //return the document we created
	}

	/**Creates an object that will appropriately generate an XPackage-compliant
		XML representation of RDF data models.
		The created <code>RDFXMLifier</code> will be initialized to encode
		properties from the XLink namespace as XML attributes, for example.
	@return An object for creating an XML tree from RDF data.
	*/
	public static RDFXMLifier createRDFXMLifier()
	{
		final RDFXMLifier rdfXMLifier=new RDFXMLifier(false);  //create a new RDF XMLifier that isn't compact
		  //show that XLink properties should be serialized as attributes
		rdfXMLifier.addLiteralAttributeSerializationNamespaceURI(XLinkConstants.XLINK_NAMESPACE_URI);
		return rdfXMLifier; //return the XMLifier we constructed
	}

	/**Creates an XPackage resource that has the specified reference URI
		within the given RDF data model.
	@param rdf The RDF data model in which the resource should be created.
	@param referenceURI The reference URI to give to the resource.
	@return A new OEB xpackage:resource object with the given reference URI.
	*/
	public static RDFResource createXPackageResource(final RDF rdf, final URI referenceURI)
	{
		return rdf.createResource(referenceURI);
//G***del when not needed		return rdf.createResource(referenceURI, XPACKAGE_NAMESPACE_URI, RESOURCE_TYPE_NAME);
	}

	/**Returns an item resource in the manifest of the given resource, if present,
		that has a reference URI.
	@param resource The resource the manifest of which should be searched.
	@param referenceURI The reference URI of the item to retrieve.
	@return The manifest item with the given reference URI, or <code>null</code>
		if there is no match.
	*/
	public static RDFResource getManifestItem(final RDFResource resource, final URI referenceURI)
	{
		final RDFBagResource manifest=getManifest(resource);  //get the manifest of this resource
		if(manifest!=null)  //if this resource has a manifest
		{
			return manifest.getItem(referenceURI);  //return the item by its reference URI
		}
		else  //if there is no manifest
		{
			return null;  //show that we couldn't find a matching item resource
		}
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
	public static RDFResource getManifestItemByLocationHRef(final RDFResource resource, final URI baseURI, final String href)
	{
		final RDFBagResource manifest=getManifest(resource);  //get the manifest of this resource
		if(manifest!=null)  //if this resource has a manifest
		{
			return getItemByLocationHRef(manifest, baseURI, href);  //get an item in the manifest that matches the given location
		}
		else  //if there is no manifest
		{
			return null;  //show that we couldn't find a matching item resource
		}
	}

	/**Returns an item resource in the given RDF container that has a matching
		xpackage:location with an xlink:href that matches the requested href,
		relative to the given URI.
	@param rdfContainer The container that hold RDF resources.
	@param baseURI The base URI of the package, used to construct absolute URIs
		from relative URIs.
	@param href The relative or absolute reference to the item, which will be
		converted to an absolute URI in order to compare it with each item's fully
		qualified URI.
	@return The item whose fully qualified URI matches the fully qualified
		version of the specified href, or <code>null</code> if there is no match.
	@see #getLocationHRef
	*/
	public static RDFResource getItemByLocationHRef(final RDFContainerResource rdfContainer, final URI baseURI, final String href)
	{
//G***del Debug.trace("Inside OEBPublication.getManifestItemByHRef() for "+href);	//G***del
		try
		{
		  final URI absoluteURI=URIUtilities.createURI(baseURI, href);	//create a URI based upon the base URI and the given file location
//G***del Debug.trace("Getting manifest item by URL: ", absoluteURL);
			return getItemByLocationHRef(rdfContainer, baseURI, absoluteURI);	//look up the item based upon the URI we formed
		}
		catch(URISyntaxException uriSyntaxException)	//if there is an error with the URI
		{
//G***fix			Debug.error(uriSyntaxException);	//log the error
			return null;	//that simply means we can't find the item
		}
/*G***del when works
		catch(MalformedURLException e)	//if there is an error with the URL
		{
			Debug.error(e);	//log the error
			return null;	//that simply means we can't find the item
		}
*/
	}

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
	public static RDFResource getItemByLocationHRef(final RDFContainerResource rdfContainer, final URI baseURI, final URI uri)
	{
//G***del Debug.trace("looking for resource that matches URL: ", url);  //G***del
		final Iterator itemIterator=rdfContainer.getItemIterator(); //get an iterator to the items in this container
		while(itemIterator.hasNext()) //while there are more items in this container
		{
			final RDFResource item=(RDFResource)itemIterator.next(); //get the next item
//G***del Debug.trace("looking at resource: ", item); //G***del
		  final String itemHRef=getLocationHRef(item);  //get the item's href G***later add something that can look at all the locations rather than just the first one
			if(itemHRef!=null)  //if there is an href
			{
				try
				{
					final URI itemURI=URIUtilities.createURI(baseURI, itemHRef);	//create a URI based upon the base URI and the item's location
//G***del	Debug.trace("comparing with URL: ", itemURL); //G***del
					if(uri.equals(itemURI)) //if the URLs match
						return item;  //return the item
				}
				catch(URISyntaxException uriSyntaxException)	//if there is an error creating the URI
				{
					Debug.warn(uriSyntaxException);	//warn about the error, but keep searching
				}
/*G***del when works
				catch(MalformedURLException e)	//if there is an error creating the URL
				{
					Debug.warn(e);	//warn about the error, but keep searching
				}
*/
			}
		}
		return null;  //show that no location matched
	}

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
//G***del Debug.trace("getting location href in routine");
		final RDFResource locationResource=getLocation(resource);  //get the location resource
//G***del Debug.trace("found location resource: ", locationResource);
		if(locationResource!=null)  //if there this resource has a location
		{
			final RDFLiteral hrefLiteral=(RDFLiteral)locationResource.getPropertyValue(XLinkConstants.XLINK_NAMESPACE_URI, XLinkConstants.HREF);  //get the XLink href value
	  	return hrefLiteral!=null ? hrefLiteral.getLexicalForm() : null;  //return the href value or null if there is no href
		}
		else  //if there is no location
			return null;  //show that there is no location
	}

	/**Retrieves the manifest of the resource. If this resource has more than one
		property of <code>xpackage:manifest</code>, it is undefined which of those
		property values will be returned.
	@param resource The resource the manifest of which will be returned.
	@return The manifest of the resource, or <code>null</code> if no manifest
		property exists.
	@exception ClassCastException Thrown if the manifest is not an
		<code>rdf:Bag</code>.
	*/
	public static RDFBagResource getManifest(final RDFResource resource) throws ClassCastException
	{
		return (RDFBagResource)resource.getPropertyValue(XPACKAGE_NAMESPACE_URI, MANIFEST_PROPERTY_NAME); //return the manifest, cast to an RDF bag resource
	}

	/**Retrieves the organization of the resource. If this resource has more than
		one property of <code>xpackage:organization</code>, it is undefined which
		of those property values will be returned.
	@param resource The resource the organization of which will be returned.
	@return The organization of the resource, or <code>null</code> if no
		organization property exists.
	@exception ClassCastException Thrown if the organization is not an
		<code>rdf:Seq</code>.
	*/
	public static RDFSequenceResource getOrganization(final RDFResource resource) throws ClassCastException
	{
		return (RDFSequenceResource)resource.getPropertyValue(XPACKAGE_NAMESPACE_URI, ORGANIZATION_PROPERTY_NAME); //return the organization, cast to an RDF sequence resource
	}

}