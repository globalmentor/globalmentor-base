package com.garretwilson.text.xml.oeb;

import java.io.*;
import java.net.URI;
import java.util.*;
import javax.mail.internet.ContentType;
import com.garretwilson.rdf.*;
import static com.garretwilson.rdf.RDFUtilities.*;
import com.garretwilson.rdf.dublincore.DCConstants;
import com.garretwilson.rdf.xpackage.XPackageUtilities;
import static com.garretwilson.text.CharacterEncodingConstants.*;

import com.garretwilson.text.CharacterEncoding;
import com.garretwilson.text.xml.XMLConstants;
import com.garretwilson.text.xml.XMLSerializer;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.util.*;
import com.globalmentor.marmot.Marmot;

import org.w3c.dom.*;

/**Serializes an OEB 1.x publication package.
@author Garret Wilson
*/
public class OEB1PackageSerializer implements OEBConstants, DCConstants
{

	/**This class cannot be publicly instantiated.*/
	private OEB1PackageSerializer()
	{
	}

	/**Serializes the package for the specified publication to the given output
		stream using the UTF-8 encoding with the UTF-8 byte order mark.
	@param publication The OEB publication to serialize.
	@param outputStream The stream into which the document should be serialized.
	@exception IOException Thrown if an I/O error occurred.
	@exception UnsupportedEncodingException Thrown if the UTF-8 encoding not recognized.
	*/
	public static void serialize(final OEBPublication publication, final OutputStream outputStream) throws UnsupportedEncodingException, IOException
	{
		serialize(publication, outputStream, UTF_8_ENCODING);	//serialize the publication, defaulting to UTF-8
	}

	/**Serializes the package for the specified publication to the given output
		stream using the specified encoding.
	Any byte order mark specified in the character encoding will be written to the stream.
	@param publication The OEB publication to serialize.
	@param outputStream The stream into which the document should be serialized.
	@param encoding The encoding format to use when serializing.
	@exception IOException Thrown if an I/O error occurred.
	@exception UnsupportedEncodingException Thrown if the specified encoding is not recognized.
	*/
	public static void serialize(final OEBPublication publication, final OutputStream outputStream, final CharacterEncoding encoding) throws IOException, UnsupportedEncodingException
	{
		final Document packageDocument=generatePackage(publication); //generate an XML document representing the publication package
		final Properties serializeOptions=new Properties(); //create properties for the serialization options
		PropertyUtilities.setProperty(serializeOptions, XMLSerializer.FORMAT_OUTPUT_OPTION, true);  //show that we should format the output
		new XMLSerializer(serializeOptions).serialize(packageDocument, outputStream, encoding);	//serialize the publication as a formatted XML document to the output stream G***do we really want to create the serializer each time?
	}

	/**Creates an XML document representing an OEB publication package.
	@param publication The OEB publication.
	@return An XML document representing the OEB publication.
	*/
	public static Document generatePackage(final OEBPublication publication)
	{
		final Document document=OEBUtilities.createDefaultOEB1Package(); //create a package XML document
		//package
		final Element packageElement=XMLUtilities.replaceDocumentElement(document, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_PACKAGE); //create the package element
		  //package/metadata
		final Element metadataElement=XMLUtilities.appendElement(packageElement, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_METADATA); //create the metadata element
			  //package/metadata/dc-metadata
		final Element dcMetadataElement=XMLUtilities.appendElement(metadataElement, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_METADATA_DC_METADATA); //create the dc-metadata element
			  //add the attribute, xmlns:dc="http://purl.org/dc/elements/1.0/"
		dcMetadataElement.setAttributeNS(XMLConstants.XMLNS_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(XMLConstants.XMLNS_NAMESPACE_PREFIX, DCMI_ELEMENTS_NAMESPACE_PREFIX), DCMI10_ELEMENTS_NAMESPACE_URI.toString());
			  //add the attribute, xmlns:oebpackage="http://openebook.org/namespaces/oeb-package/1.0/"
		dcMetadataElement.setAttributeNS(XMLConstants.XMLNS_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(XMLConstants.XMLNS_NAMESPACE_PREFIX, OEB1_PACKAGE_NAMESPACE_PREFIX), OEB1_PACKAGE_NAMESPACE_URI.toString());
		final Iterator propertyIterator=publication.getPropertyIterator();  //get an iterator to the publication's properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final NameValuePair property=(NameValuePair)propertyIterator.next();  //get the property
			final RDFResource propertyResource=(RDFResource)property.getName();  //get the property resource
Debug.trace("looking at publication property: ", propertyResource); //G***del
			final URI propertyNamespaceURI=getNamespaceURI(propertyResource.getURI());	//get the namespace of hte property
				//TODO check for null in both the namespace URI and local name
Debug.trace("property namespace URI: ", propertyNamespaceURI); //G***del
				//if this is a Dublin Core property
			if(DCMI11_ELEMENTS_NAMESPACE_URI.equals(propertyNamespaceURI)
				  || DCMI10_ELEMENTS_NAMESPACE_URI.equals(propertyNamespaceURI))
			{
				final String propertyLocalName=getLocalName(propertyResource.getURI()); //get the  local name of the property
Debug.trace("property local name: ", propertyLocalName); //G***del
				final Object propertyValueObject=property.getValue(); //get the property value
				if(propertyValueObject instanceof RDFLiteral)  //if the value is a literal
				{
Debug.trace("property value is a literal"); //G**8del
					final String propertyValue=((RDFLiteral)propertyValueObject).getLexicalForm(); //get the literal value of the property
Debug.trace("property value: ", propertyValue); //G***del
//G***del					Element dcElement=null; //we'll try to create an element for this Dublin Core property
					//<package><metadata><dc-metadata><dc:Title>
					if(DC_TITLE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TITLE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Creator>
					else if(DC_CREATOR_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CREATOR, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Subject>
					else if(DC_SUBJECT_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_SUBJECT, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Description>
					else if(DC_DESCRIPTION_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_DESCRIPTION, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Publisher>
					else if(DC_PUBLISHER_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_PUBLISHER, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Contributor>
					else if(DC_CONTRIBUTOR_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CONTRIBUTOR, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Date>
					else if(DC_DATE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_DATE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Type>
					else if(DC_TYPE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TYPE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Format>
					else if(DC_FORMAT_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_FORMAT, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Identifier>
					else if(DC_IDENTIFIER_PROPERTY_NAME.equals(propertyLocalName))
					{
						final Element dcIdentifierElement=XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_IDENTIFIER, propertyValue);  //add the OEBPPS 1.x DC metadata element
								//if the package has not yet been assigned a unique identifier attribute
						if(!packageElement.hasAttributeNS(null, PKG_ELEMENT_PACKAGE_ATTRIBUTE_UNIQUE_IDENTIFIER))
						{
								//add the identifier value to the package element
							packageElement.setAttributeNS(null, PKG_ELEMENT_PACKAGE_ATTRIBUTE_UNIQUE_IDENTIFIER, "packageID");  //G***use a constant
						    //add the same identifier value to the dc:Identifier element
							dcIdentifierElement.setAttributeNS(null, PKG_METADATA_DC_METADATA_DC_IDENTIFIER_ATTRIBUTE_ID, "packageID"); //G***use a constant
						}
					}
					//<package><metadata><dc-metadata><dc:Source>
					else if(DC_SOURCE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_SOURCE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Language>
					else if(DC_LANGUAGE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_LANGUAGE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Relation>
					else if(DC_RELATION_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_RELATION, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Coverage>
					else if(DC_COVERAGE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_COVERAGE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Rights>
					else if(DC_RIGHTS_PROPERTY_NAME.equals(propertyLocalName))
					{
						XMLUtilities.appendElement(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_RIGHTS, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
				}
			}
		}
		  //package/manifest
		final RDFListResource manifest=Marmot.getContents(publication); //get the manifest
		if(manifest!=null)  //if the publication has a manifest
		{
			final Element manifestElement=XMLUtilities.appendElement(packageElement, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_MANIFEST); //create the manifest element
		  final Iterator itemIterator=manifest.iterator(); //get an iterator to the manifest items
			while(itemIterator.hasNext()) //while there are more items in the manifest
			{
			  final RDFResource oebItem=(RDFResource)itemIterator.next(); //get the next OEB item
			  manifestElement.appendChild(generateItemElement(document, oebItem, publication.getURI()));  //generate an item element and add it to the manifest element
		  }
		}
		  //package/spine
		final RDFListResource<?> spine=publication.getSpine(); //get the spine
		if(spine!=null)  //if the publication has a spine
		{
			final Element spineElement=XMLUtilities.appendElement(packageElement, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_SPINE); //create the spine element
			for(final RDFObject item:spine)	//for each item in the spine
			{
				spineElement.appendChild(generateItemRefElement(document, (RDFResource)item, publication.getURI()));  //generate an item element and add it to the spine element
			}
		}
		if(publication.getGuideList().size()>0) //if there are guides
		{
				//package/guide
			final Element guideElement=XMLUtilities.appendElement(packageElement, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_GUIDE); //create the guide element
			final Iterator guideIterator=publication.getGuideList().iterator(); //get an iterator to iterate through the guides
			while(guideIterator.hasNext()) //while there are more items in the guide
			{
				final OEBGuide oebGuide=(OEBGuide)guideIterator.next();  //get the next OEB guide
				guideElement.appendChild(generateReferenceElement(document, oebGuide));  //generate a guide reference element and add it to the manifest element
			}
		}
		return document;	//return the document we created
	}

	/**Creates an XML element representing an OEB item.
	@param document The XML document to serve as the element's parent.
	@param oebItem The OEB item to convert to an element.
	@param publicationURI The URI of the publication, or <code>null</code> if the
		publication URI has no bearing on identifiers used.
	@return An XML element representing the OEB item.
	*/
	public static Element generateItemElement(final Document document, final RDFResource oebItem, final URI publicationURI)
	{
		final Element itemElement=document.createElementNS(OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_MANIFEST_ITEM);  //create an OEB item element
		itemElement.setAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_ID, createItemID(oebItem.getURI().toString(), publicationURI));		  //set the ID
		final String href=XPackageUtilities.getLocationHRef(oebItem); //get the item's href
		if(href!=null)  //if the item has an href
		{
		  itemElement.setAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_HREF, href);		  //set the href
//G***maybe assert that there is a media type, here
		}
		final ContentType mediaType=Marmot.getMediaType(oebItem);  //get the item's media type
		if(mediaType!=null) //if the item has a media type
		{
		  itemElement.setAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_MEDIA_TYPE, mediaType.toString());		  //set the media type
		}
/**G***fix
		final OEBItem fallbackItem=oebItem.getFallback(); //get the fallback item
		if(fallbackItem!=null)  //if there is a fallback item
			itemElement.setAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_FALLBACK, fallbackItem.getID()); //set the fallback attribute
*/
		return itemElement; //return the element for the OEB item
	}

	/**Creates an XML element representing an OEB item reference in the spine.
	@param document The XML document to serve as the element's parent.
	@param oebItem The OEB item to convert to an element.
	@param publicationURI The URI of the publication, or <code>null</code> if the
		publication URI has no bearing on identifiers used.
	@return An XML element representing the OEB item.
	*/
	public static Element generateItemRefElement(final Document document, final RDFResource oebItem, final URI publicationURI)
	{
		final Element itemrefElement=document.createElementNS(OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_SPINE_ITEMREF);  //create an OEB itemref element
		itemrefElement.setAttributeNS(null, PKG_SPINE_ITEMREF_ATTRIBUTE_IDREF, createItemID(oebItem.getURI().toString(), publicationURI));		  //set the ID
		return itemrefElement; //return the element for the OEB itemref
	}

	/**Creates an XML element representing an OEB guide reference in the guide list.
	@param document The XML document to serve as the element's parent.
	@param oebGuide The OEB guide to convert to an element.
	@return An XML element representing the OEB guide.
	*/
	public static Element generateReferenceElement(final Document document, final OEBGuide oebGuide)
	{
		final Element referenceElement=document.createElementNS(OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_GUIDE_REFERENCE);  //create an OEB guide reference element
		referenceElement.setAttributeNS(null, PKG_GUIDE_REFERENCE_ATTRIBUTE_TYPE, oebGuide.getType());		  //set the type
		referenceElement.setAttributeNS(null, PKG_GUIDE_REFERENCE_ATTRIBUTE_TITLE, oebGuide.getTitle());		  //set the title
		referenceElement.setAttributeNS(null, PKG_GUIDE_REFERENCE_ATTRIBUTE_HREF, oebGuide.getHRef());		  //set the href
		return referenceElement; //return the element for the OEB guide reference
	}

	/**Creates an ID for an item that conforms to the production of an XML name.
		If a URI is provided for the publication, the local name is separated from
		that URI (for serializing RDF-based models, for instance). Otherwise,
		all XML non-name characters are converted to an underscore ('_'); if the
		first letter of the string is not correct for an XML name, the string will
		be prevised with 'x'.
	@param identifier The identifier from which an ID will be created.
	@param publicationURI The URI of the publication, or <code>null</code> if the
		publication URI has no bearing on identifiers used.
	@return An XML name to be used as an ID for OEB 1 package items.
	*/
	public static String createItemID(final String identifier, final URI publicationURI)
	{
		if(!XMLUtilities.isName(identifier))  //if the identifier is not a name already
		{
			final StringBuffer stringBuffer=new StringBuffer(identifier); //create a string buffer to work with our identifier
				//remove the publication URI, if present
			for(int i=stringBuffer.length()-1; i>=0; --i) //look at each character from right to left
			{
				if(!XMLUtilities.isNameChar(stringBuffer.charAt(i)))  //if this is not a name character
				{
					final String prefix;  //we'll find out the prefix differently, depending on if the divider is a hash symbol
					if(stringBuffer.charAt(i)=='#' && i>0) //if this is a fragment character that isn't the first characterG***use a constant
						prefix=stringBuffer.substring(0, i);  //the prefix is everything before the fragment character
					else  //if this is some other sort of non-name character
						prefix=stringBuffer.substring(0, i+1);  //use everything up to and including the character as a prefix
//G***del Debug.trace("prefix: ", prefix);
//G***del Debug.trace("publication URI: ", publicationURI);

				  if(prefix.equals(publicationURI.toString())) //if the prefix is the publication URI
						stringBuffer.delete(0, i+1);  //delete the entire prefix, including this character
					break;  //stop looking for a non-nam character
				}
			}
				//convert characters to XML name characters
			for(int i=stringBuffer.length()-1; i>=0; --i) //look at each character again
			{
				if(!XMLUtilities.isNameChar(stringBuffer.charAt(i)))  //if this is not a name character
				{
					stringBuffer.setCharAt(i, '_'); //replace the character with an underscore G***use a constant
				}
			}
				//if the first character isn't an XML character
			if(stringBuffer.length()>0 && !XMLUtilities.isNameFirstChar(stringBuffer.charAt(0)))
			{
				stringBuffer.insert(0, 'x');  //prepend the string with 'x' G***use a constant
			}
			return stringBuffer.toString(); //return the string we manipulated
		}
		else  //if the identifier is already a name
			return identifier;  //return the identifier as is
	}

	/**Creates an XML name by replacing every non-name character with an underscore
		('_') character. If the first character of the string cannot begin an XML
		name, it will be replaced with an 'x'. An empty string will receive
		an 'x' as well.
	@param string The string to be changed to an XML name.
	@return The string modified to be an XML name..
	*/
/*G***del
	static public String createName(final String string)
	{
		if(isName(string))  //if the string is already a name (we'll check all the characters, assuming that most of the time the strings will already be valid names, making this more efficient)
		  return string;  //return the string, because it doesn't need to be converted
		else  //if the string isn't a name already (we'll check all the characters, assuming that most of the time the strings will already be valid names, making this more efficient)
		{
			final StringBuffer stringBuffer=new StringBuffer(string); //create a string buffer from the string, so that we can modify it as necessary
			if(stringBuffer.length()==0)  //if the string isn't long enough to be a name
				stringBuffer.append(REPLACEMENT_FIRST_CHAR);  //put an 'x' in the first position
			else if(!isNameFirstChar(stringBuffer.charAt(0))) //if the string does have at least one character, but it's not a valid first character for an XML name
				stringBuffer.setCharAt(0, REPLACEMENT_FIRST_CHAR);  //replace the first character with an 'x'
			for(int i=1; i<string.length(); ++i)  //look at each character in the string, except theh first (which we've already checked)
			{
				if(!isNameChar(stringBuffer.charAt(i)))  //if this character isn't a name character
					stringBuffer.setCharAt(i, REPLACEMENT_CHAR);  //replace the character with an underscore
			}
			return stringBuffer.toString(); //return the string we constructed
		}
	}
*/

}