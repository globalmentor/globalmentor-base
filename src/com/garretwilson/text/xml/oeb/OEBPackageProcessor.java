package com.garretwilson.text.xml.oeb;

import java.io.*;
import java.net.*;
import java.net.URI;	//G**del when old URI is gone

import com.garretwilson.io.MediaType;
import com.garretwilson.net.*;
import com.garretwilson.rdf.dublincore.DCConstants;
//G***del import com.garretwilson.text.xml.XMLDOMImplementation;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.XMLProcessor;
import com.garretwilson.text.xml.XPath;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xpackage.*;
import com.garretwilson.util.*;
import org.w3c.dom.*;

//G***del all the XMLUndefinedEntityReferenceException throws when we don't need them anymore, in favor of XMLWellFormednessException

/**Class which parses an input stream containing an OEB publication.
@see XMLProcessor
*/
public class OEBPackageProcessor implements OEBConstants, OEB2Constants, DCConstants, RDFResourceFactory
{

	/**The XML processor for parsing an OEB package description document.*/
	private final XMLProcessor xmlProcessor;

		/**@return The XML processor for parsing an OEB package description document.*/
		protected XMLProcessor getXMLProcessor() {return xmlProcessor;}


	/**Default constructor that creates a new XML processor.*/
	public OEBPackageProcessor()
	{
		this(new XMLProcessor()); //create a new XML processor and finish constructing the class
	}

	/**Constructor that uses an existing XML processor.
	@param processor The XML processor to use for parsing and OEB package description document.
	*/
	public OEBPackageProcessor(final XMLProcessor processor)
	{
		xmlProcessor=processor; //save the XML processor
	}

	/**Reads an OEB publication from a package file and converts it to an
		RDF data model. If the input stream contains an OEB 1.x package document, it
		will be converted.
	@param packageInputStream The input stream containing the package information.
	@return The RDF data model of the package information.
	@exception IOException Thrown if there is an error reading the package
		information from the input stream.
	*/
	//G***add error reporting to this code
	public RDF read(final InputStream packageInputStream) throws IOException
	{
		return read(packageInputStream, null);  //read the package without knowing the URL
	}

	/**Reads an OEB publication from a package file and converts it to an
		RDF data model. If the input stream contains an OEB 1.x package document, it
		will be converted.
	@param packageInputStream The input stream containing the package information.
	@param packageURI The URI to the package information, or <code>null</code> if
		the package URI is not available.
	@return The RDF data model of the package information.
	@exception IOException Thrown if there is an error reading the package
		information from the input stream.
	*/
	//G***add error reporting to this code
	public RDF read(final InputStream packageInputStream, final URI packageURI) throws IOException
	{
		final RDF rdf=new RDF();  //create a new RDF data model
		rdf.registerResourceFactory(OEB1_PACKAGE_NAMESPACE_URI, this);  //register ourselves as a factory for OEB 1.x package resources
		rdf.registerResourceFactory(OEB2_PACKAGE_NAMESPACE_URI, this);  //register ourselves as a factory for OEB 2.x package resources

Debug.trace("reading package from URI: ", packageURI);  //G***del
		final Document document=getXMLProcessor().parseDocument(packageInputStream, packageURI);	//parse the package description document
		document.normalize(); //normalize the package description document
		try
		{
				//check for an OEB 1.x package document
			final DocumentType documentType=document.getDoctype();  //get the document type of this document
			if(documentType!=null)  //if there is a document type
			{
				final String packagePublicID=documentType.getPublicId();  //get the public ID of this package document, if there is one
					//if this is an OEB 1.0 or OEB 1.0.1 package document
				if(OEB10_PACKAGE_PUBLIC_ID.equals(packagePublicID) || OEB101_PACKAGE_PUBLIC_ID.equals(packagePublicID))
			  {
						return convertOEB1Package(rdf, document);	//convert the OEB 1.x package document to RDF
			  }
			}
			final RDFXMLProcessor rdfProcessor=new RDFXMLProcessor(rdf);	//create a new RDF processor using the RDF data model we already created
			return rdfProcessor.process(document, packageURI);  //parse the RDF from the document
		}
		catch (URISyntaxException e)
		{
			throw new IOException(e.toString());
		} 
	}

	/**Converts an OEB 1.x package document to an OEB publication stored in an
		RDF data model.
	@param rdf The RDF data model to use, which should already have the
		appropriate resource factories registered.
	@param oeb1PackageDocument The XML document containing the OEB 1.x package
		information.
	@return An RDF data model containing the converted publication resource.
	@exception URISyntaxException Thrown if any of the URIs in the document are invalid.
	*/
	protected RDF convertOEB1Package(final RDF rdf, final Document oeb1PackageDocument) throws URISyntaxException
	{
		//<package>
		final Element rootElement=oeb1PackageDocument.getDocumentElement();	//get the root of the package
		URI publicationReferenceURI=new URI(URIConstants.URN_SCHEME, "local:anonymous:publication", null); //we'll try to find a URI for the publication; start by assuming we won't be successful G***use constants here
		//get a list of all dc:Identifier elements
		//XPath: /metadata/dc-metadata/dc:Identifier
		final NodeList dcIdentifierElementList=(NodeList)XPath.evaluateLocationPath(rootElement,
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_METADATA+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_METADATA_DC_METADATA+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_MANIFEST_DC_METADATA_DC_IDENTIFIER);
		for(int i=0; i<dcIdentifierElementList.getLength(); ++i)	//look at each DC metadata element
		{
			final Element dcIdentifierElement=(Element)dcIdentifierElementList.item(i);	//get a reference to this DC identifier element
		  final String dcIdentifierElementText=XMLUtilities.getText(dcIdentifierElement, true); //get the text of the element
				//get the trimmed scheme (in lowercase) being used by this Dublin Core identifier in OEBPS 1.x
			final String scheme=dcIdentifierElement.getAttributeNS(null, PKG_METADATA_DC_METADATA_DC_IDENTIFIER_ATTRIBUTE_SCHEME).trim().toLowerCase();
				//if the scheme indicates that the identifier is a URI, a URL, or a URN,
				//  or if the identifier starts with "urn:", "http:", or "file:"
			if(URIConstants.URI_SCHEME.equals(scheme) /*G***del || URIConstants.URL.equals(scheme)*/ || URIConstants.URN_SCHEME.equals(scheme)
				  || dcIdentifierElementText.startsWith(URIConstants.URN_SCHEME+URIConstants.SCHEME_SEPARATOR)
				  || dcIdentifierElementText.startsWith(URLConstants.HTTP_PROTOCOL+URIConstants.SCHEME_SEPARATOR)
				  || dcIdentifierElementText.startsWith(URLConstants.FILE_PROTOCOL+URIConstants.SCHEME_SEPARATOR))
			{
				publicationReferenceURI=new URI(dcIdentifierElementText);  //use the identifier as the publication resource reference URI
				break;  //we found a URI; stop looking for another
			}
		  else if(scheme.length()==0)  //if there is no scheme
			{
				//create a URI in the form "urn:oeb:identifier"
				publicationReferenceURI=new URI(URIConstants.URN_SCHEME, "oeb"+URIConstants.SCHEME_SEPARATOR+dcIdentifierElementText, null);
//G***del when works				publicationReferenceURI=URIConstants.URN+URIConstants.SCHEME_SEPARATOR+"oeb"+URIConstants.SCHEME_SEPARATOR+dcIdentifierElementText;
			}
			else  //if this isn't a URI scheme, we'll create a URI from this identifier but keep looking for something better
			{
				if((dcIdentifierElementText.toLowerCase().startsWith(scheme)))  //if the identifier starts with the scheme
				{
						//if the scheme is followed by a ":"
					if(dcIdentifierElementText.length()>scheme.length() && dcIdentifierElementText.charAt(scheme.length())==URIConstants.SCHEME_SEPARATOR)
					{
						//use the identifier text as-is, after trimming it, and preface it with "urn:"
						publicationReferenceURI=new URI(URIConstants.URN_SCHEME, dcIdentifierElementText.trim(), null);
//G***del when works						publicationReferenceURI=URIConstants.URN+URIConstants.SCHEME_SEPARATOR+dcIdentifierElementText.trim();
					}
					else  //if the scheme prefaces the identifier, but isn't in URI form
					{
						//remove the scheme from the beginning of the identifier
						final String identifierText=dcIdentifierElementText.substring(scheme.length()).trim();
						//create a URI in the form "urn:scheme:identifier"
						publicationReferenceURI=new URI(URIConstants.URN_SCHEME, scheme+URIConstants.SCHEME_SEPARATOR+identifierText, null);
//G***del when works						publicationReferenceURI=URIConstants.URN+URIConstants.SCHEME_SEPARATOR+scheme+URIConstants.SCHEME_SEPARATOR+identifierText;
					}
				}
				else  //if the identifier doesn't start the scheme
				{
					//create a URI in the form "urn:scheme:identifier"
					publicationReferenceURI=new URI(URIConstants.URN_SCHEME, scheme+URIConstants.SCHEME_SEPARATOR+dcIdentifierElementText.trim(), null);
//G***del when works					publicationReferenceURI=URIConstants.URN+URIConstants.SCHEME_SEPARATOR+scheme+URIConstants.SCHEME_SEPARATOR+dcIdentifierElementText.trim();
				}
			}
		}
		final OEBPublication publicationResource=OEBUtilities.createOEBPublication(rdf, publicationReferenceURI);
Debug.trace("converting OEB package, created publication resource: ", publicationResource.getClass().getName());  //G***del
		//XPath: /metadata/dc-metadata/*
		final NodeList dcMetadataElementList=(NodeList)XPath.evaluateLocationPath(rootElement,
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_METADATA+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_METADATA_DC_METADATA+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+XPath.WILDCARD_CHAR);
		for(int i=0; i<dcMetadataElementList.getLength(); ++i)	//look at each DC metadata element
		{
			final Element dcMetadataElement=(Element)dcMetadataElementList.item(i);	//get a reference to this DC metadata element
			final String dcMetadataElementName=dcMetadataElement.getNodeName();	//get the name of this DC metadata element
		  final String dcMetadataElementText=XMLUtilities.getText(dcMetadataElement, true); //get the text of the element
			//<package><metadata><dc-metadata><dc:Title>
			if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TITLE.equals(dcMetadataElementName))
			{
				//store the title as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_TITLE_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Creator>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CREATOR.equals(dcMetadataElementName))
			{
				  //G***currently we ignore the file-as and role attributes
				//store the creator as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_CREATOR_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Subject>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_SUBJECT.equals(dcMetadataElementName))
			{
				//store the subject as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_SUBJECT_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Description>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_DESCRIPTION.equals(dcMetadataElementName))
			{
				//store the description as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_DESCRIPTION_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Publisher>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_PUBLISHER.equals(dcMetadataElementName))
			{
				//store the publisher as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_PUBLISHER_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Contributor>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CONTRIBUTOR.equals(dcMetadataElementName))
			{
				//store the contributor as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_CONTRIBUTOR_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Date>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_DATE.equals(dcMetadataElementName))
			{
				//store the date as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_DATE_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Type>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TYPE.equals(dcMetadataElementName))
			{
				//store the type as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_TYPE_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Format>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_FORMAT.equals(dcMetadataElementName))
			{
				//store the format as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_FORMAT_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Identifier>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_IDENTIFIER.equals(dcMetadataElementName))
			{
				  //get the scheme being used by this Dublin Core identifier in OEBPS 1.x
				final String scheme=dcMetadataElement.getAttributeNS(null, PKG_METADATA_DC_METADATA_DC_IDENTIFIER_ATTRIBUTE_SCHEME);
			  final String identifier;  //we'll determine the identifier format based upon the scheme
				  //if the scheme indicates that the identifier is a URI, a URL, or a URN,
					//  or if the metadata text already begins with the scheme
				if(URIConstants.URI_SCHEME.equals(scheme) /*G***del || URIConstants.URL.equals(scheme)*/ || URIConstants.URN_SCHEME.equals(scheme)
						|| (dcMetadataElementText.toLowerCase().startsWith(scheme)))
				{
				  identifier=dcMetadataElementText;  //we'll store only the identifier as a property of the publication
				}
				else  //for all other schemes, such as "isbn"
				{
				  identifier=scheme+' '+dcMetadataElementText;  //we'll add the scheme to the front of the identifier, as recommended by http://www.dublincore.org/documents/2001/11/28/dcmes-xml/
				}
				//store the identifier as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_IDENTIFIER_PROPERTY_NAME, identifier);
			}
			//<package><metadata><dc-metadata><dc:Source>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_SOURCE.equals(dcMetadataElementName))
			{
				//store the source as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_SOURCE_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Language>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_LANGUAGE.equals(dcMetadataElementName))
			{
				//store the language as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_LANGUAGE_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Relation>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_RELATION.equals(dcMetadataElementName))
			{
				//store the relation as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_RELATION_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Coverage>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_COVERAGE.equals(dcMetadataElementName))
			{
				//store the coverage as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_COVERAGE_PROPERTY_NAME, dcMetadataElementText);
			}
			//<package><metadata><dc-metadata><dc:Rights>
			else if(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_RIGHTS.equals(dcMetadataElementName))
			{
				//store the rights as a property of the publication
				publicationResource.addProperty(DCMI11_ELEMENTS_NAMESPACE_URI, DC_RIGHTS_PROPERTY_NAME, dcMetadataElementText);
			}
		}
//G***fix fallbacks		final Map fallbackMap=new HashMap();  //create a map to be used for storing references to fallbacks
		  //add a manifest to the publication
		final RDFBagResource manifestResource=XPackageUtilities.addManifest(publicationResource);
		//XPath: /manifest/item
		final NodeList manifestElementList=(NodeList)XPath.evaluateLocationPath(rootElement,
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_MANIFEST+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_MANIFEST_ITEM);
		for(int i=0; i<manifestElementList.getLength(); ++i)	//look at each manifest element
		{
			final Element itemElement=(Element)manifestElementList.item(i);	//get a reference to this item in the manifest
		  final String itemID=itemElement.getAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_ID); //get the item ID
		  final String itemHRef=itemElement.getAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_HREF); //get the item href
		  final MediaType itemMediaType=new MediaType(itemElement.getAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_MEDIA_TYPE));  //get the item's media type
				//create an RDF resource for the item with a type of rdf:resource
			final RDFResource itemResource=rdf.createResource(new URI(URIConstants.URN_SCHEME, "local:"+itemID, null)); //G***fix the reference URI
//G***del when not needed		  final RDFResource itemResource=rdf.createResource(new URI(URIConstants.URN_SCHEME, "local:"+itemID, null), XPackageConstants.XPACKAGE_NAMESPACE_URI, XPackageConstants.RESOURCE_TYPE_NAME); //G***fix the reference URI
			MIMEOntologyUtilities.addContentType(itemResource, itemMediaType); //add the item's content type
		  XPackageUtilities.addLocation(itemResource, itemHRef); //add the item's href
		  manifestResource.add(itemResource);  //add the item to the manifest
/*G***fix fallbacks
			if(itemElement.hasAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_FALLBACK)) //if the element has a fallback attribute
				fallbackMap.put(oebItem, itemElement.getAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_FALLBACK)); //put the fallback ID in the map, keyed to the item
*/
		}
/*G***fix fallbacks
			//resolve all the fallbacks
		final Iterator manifestIterator=getManifestMap().values().iterator(); //get an iterator to iterate through the manifest items
		while(manifestIterator.hasNext()) //while there are more items in the manifest
		{
			final OEBItem oebItem=(OEBItem)manifestIterator.next(); //get the next OEB item
			final String fallbackID=(String)fallbackMap.get(oebItem); //get the fallback ID for this OEB item
			if(fallbackID!=null)  //if there is a fallback ID
			{
				final OEBItem fallbackItem=getManifestItemByID(fallbackID); //get the item the fallback ID references
				Debug.assert(fallbackItem!=null, "Invalid fallback ID: "+fallbackID); //G***fix with a real error message
				oebItem.setFallback(fallbackItem);  //set the resolved fallback item
			}
		}
*/

		  //add an organization to the publication
		final RDFSequenceResource organizationResource=XPackageUtilities.addOrganization(publicationResource);

		//XPath: /spine/itemref
		final NodeList spineElementList=(NodeList)XPath.evaluateLocationPath(rootElement,
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_SPINE+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_SPINE_ITEMREF);
		for(int i=0; i<spineElementList.getLength(); ++i)	//look at each spine element
		{
			final Element itemElement=(Element)spineElementList.item(i);	//get a reference to this item in the spine
//G***del Debug.trace("Found spine element: "+itemElement.getAttributeNS(null, PKG_SPINE_ITEMREF_ATTRIBUTE_IDREF));	//G***del
		  final String itemIDRef=itemElement.getAttributeNS(null, PKG_SPINE_ITEMREF_ATTRIBUTE_IDREF);  //get the item's idref value
		  final URI itemReferenceURI=new URI(URIConstants.URN_SCHEME, "local:"+itemIDRef, null);  //G***fix the reference URI
			final RDFResource itemResource=manifestResource.getItem(itemReferenceURI);  //get the referenced item from the manifest G***this is very inefficient; maybe use maps or something
			assert itemResource!=null : "Missing spine element: "+itemIDRef; //TODO fix with a real error message
		  organizationResource.add(itemResource);  //add this item to the organization
		}
//G***fix with new navigation stuff
		//XPath: /guide/reference
		final NodeList guideElementList=(NodeList)XPath.evaluateLocationPath(rootElement,
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_GUIDE+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_GUIDE_REFERENCE);
		for(int i=0; i<guideElementList.getLength(); ++i)	//look at each guide element
		{
			final Element referenceElement=(Element)guideElementList.item(i);	//get a reference to this reference in the guide
		  final String type=referenceElement.getAttributeNS(null, PKG_GUIDE_REFERENCE_ATTRIBUTE_TYPE);  //get the guide type
		  final String title=referenceElement.getAttributeNS(null, PKG_GUIDE_REFERENCE_ATTRIBUTE_TITLE);  //get the guide title
		  final String href=referenceElement.getAttributeNS(null, PKG_GUIDE_REFERENCE_ATTRIBUTE_HREF);  //get the guide href
Debug.trace("found guide type: "+type+" title: "+title+" href: "+href);
			//create a new OEB guide
			final OEBGuide oebGuide=new OEBGuide(type, title, href); //create a new guide
			publicationResource.addGuide(oebGuide); //add this guide to our list
		}
Debug.trace("converted OEB publication RDF: ", RDFUtilities.toString(rdf)); //G***del
		return rdf; //return the RDF data model of the publication we created
	}

	/**Creates a resource with the provided reference URI based upon the
		type reference URI composed of the given XML serialization type namespace
		and local name
		<p>This method creates OEB-specific resource.</p>
	@param referenceURI The non-<code>null</code> reference URI of the resource
		to create.
	@param typeNamespaceURI The XML namespace used in the serialization of the
		type URI, or <code>null</code> if the type is not known.
	@param typeLocalName The XML local name used in the serialization of the type
		URI, or <code>null</code> if the type is not known.
	@return The resource created with this reference URI, with the given type
		added if a type was given.
	*/  //G***change to use OEBUtilities as a factory
	public RDFResource createResource(final URI referenceURI, final URI typeNamespaceURI, final String typeLocalName)
	{
		if(OEB1_PACKAGE_NAMESPACE_URI.equals(typeNamespaceURI) //if this resource is an OEB 1.x package resource
				|| OEB2_PACKAGE_NAMESPACE_URI.equals(typeNamespaceURI)) //or if this resource is an OEB 2.x package resource
		{
			if(PUBLICATION_TYPE_NAME.equals(typeLocalName)) //<oebps:publication>
			{
				return new OEBPublication(referenceURI);  //create and return a new OEB publication
			}
		}
		return null;  //show that we couldn't create a resource
	}

}
