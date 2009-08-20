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

package com.globalmentor.text.xml.oeb;

import java.io.*;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.*;

import static com.globalmentor.io.Charsets.*;
import static com.globalmentor.rdf.RDFResources.*;

import com.globalmentor.model.NameValuePair;
import com.globalmentor.rdf.*;
import com.globalmentor.rdf.xpackage.XPackage;
import com.globalmentor.text.xml.XMLSerializer;
import com.globalmentor.text.xml.XML;
import static com.globalmentor.text.xml.oeb.OEB.*;
import static com.globalmentor.urf.dcmi.DCMI.*;
import com.globalmentor.util.*;

import org.w3c.dom.*;

/**Serializes an OEB 1.x publication package.
@author Garret Wilson
*/
public class OEB1PackageSerializer
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
		serialize(publication, outputStream, UTF_8_CHARSET);	//serialize the publication, defaulting to UTF-8
	}

	/**Serializes the package for the specified publication to the given output
		stream using the specified encoding.
	Any byte order mark specified in the character encoding will be written to the stream.
	@param publication The OEB publication to serialize.
	@param outputStream The stream into which the document should be serialized.
	@param charset The charset to use when serializing.
	@exception IOException Thrown if an I/O error occurred.
	@exception UnsupportedEncodingException Thrown if the specified encoding is not recognized.
	*/
	public static void serialize(final OEBPublication publication, final OutputStream outputStream, final Charset charset) throws IOException, UnsupportedEncodingException
	{
		final Document packageDocument=generatePackage(publication); //generate an XML document representing the publication package
		final Properties serializeOptions=new Properties(); //create properties for the serialization options
		PropertiesUtilities.setProperty(serializeOptions, XMLSerializer.FORMAT_OUTPUT_OPTION, true);  //show that we should format the output
		new XMLSerializer(serializeOptions).serialize(packageDocument, outputStream, charset);	//serialize the publication as a formatted XML document to the output stream TODO do we really want to create the serializer each time?
	}

	/**Creates an XML document representing an OEB publication package.
	@param publication The OEB publication.
	@return An XML document representing the OEB publication.
	*/
	public static Document generatePackage(final OEBPublication publication)
	{
		final Document document=OEB.createOEB1Package(); //create a package XML document
		//package
		final Element packageElement=XML.replaceDocumentElement(document, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_PACKAGE); //create the package element
		  //package/metadata
		final Element metadataElement=XML.appendElementNS(packageElement, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_METADATA); //create the metadata element
			  //package/metadata/dc-metadata
		final Element dcMetadataElement=XML.appendElementNS(metadataElement, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_METADATA_DC_METADATA); //create the dc-metadata element
			  //add the attribute, xmlns:dc="http://purl.org/dc/elements/1.0/"
		dcMetadataElement.setAttributeNS(XML.XMLNS_NAMESPACE_URI.toString(), XML.createQualifiedName(XML.XMLNS_NAMESPACE_PREFIX, DCMI_ELEMENTS_NAMESPACE_PREFIX), DCMI10_ELEMENTS_NAMESPACE_URI.toString());
			  //add the attribute, xmlns:oebpackage="http://openebook.org/namespaces/oeb-package/1.0/"
		dcMetadataElement.setAttributeNS(XML.XMLNS_NAMESPACE_URI.toString(), XML.createQualifiedName(XML.XMLNS_NAMESPACE_PREFIX, OEB1_PACKAGE_NAMESPACE_PREFIX), OEB1_PACKAGE_NAMESPACE_URI.toString());
		final Iterator propertyIterator=publication.getPropertyIterator();  //get an iterator to the publication's properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final NameValuePair property=(NameValuePair)propertyIterator.next();  //get the property
			final RDFResource propertyResource=(RDFResource)property.getName();  //get the property resource
			final URI propertyNamespaceURI=getNamespaceURI(propertyResource.getURI());	//get the namespace of hte property
				//TODO check for null in both the namespace URI and local name
				//if this is a Dublin Core property
			if(DCMI11_ELEMENTS_NAMESPACE_URI.equals(propertyNamespaceURI)
				  || DCMI10_ELEMENTS_NAMESPACE_URI.equals(propertyNamespaceURI))
			{
				final String propertyLocalName=getLocalName(propertyResource.getURI()); //get the  local name of the property
				final Object propertyValueObject=property.getValue(); //get the property value
				if(propertyValueObject instanceof RDFLiteral)  //if the value is a literal
				{
Debug.trace("property value is a literal"); //G**8del
					final String propertyValue=((RDFLiteral)propertyValueObject).getLexicalForm(); //get the literal value of the property
					//<package><metadata><dc-metadata><dc:Title>
					if(TITLE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TITLE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Creator>
					else if(CREATOR_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CREATOR, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Subject>
					else if(SUBJECT_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_SUBJECT, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Description>
					else if(DESCRIPTION_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_DESCRIPTION, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Publisher>
					else if(PUBLISHER_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_PUBLISHER, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Contributor>
					else if(CONTRIBUTOR_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CONTRIBUTOR, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Date>
					else if(DATE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_DATE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Type>
					else if(TYPE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TYPE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Format>
					else if(FORMAT_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_FORMAT, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Identifier>
					else if(IDENTIFIER_PROPERTY_NAME.equals(propertyLocalName))
					{
						final Element dcIdentifierElement=XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_IDENTIFIER, propertyValue);  //add the OEBPPS 1.x DC metadata element
								//if the package has not yet been assigned a unique identifier attribute
						if(!packageElement.hasAttributeNS(null, PKG_ELEMENT_PACKAGE_ATTRIBUTE_UNIQUE_IDENTIFIER))
						{
								//add the identifier value to the package element
							packageElement.setAttributeNS(null, PKG_ELEMENT_PACKAGE_ATTRIBUTE_UNIQUE_IDENTIFIER, "packageID");  //TODO use a constant
						    //add the same identifier value to the dc:Identifier element
							dcIdentifierElement.setAttributeNS(null, PKG_METADATA_DC_METADATA_DC_IDENTIFIER_ATTRIBUTE_ID, "packageID"); //TODO use a constant
						}
					}
					//<package><metadata><dc-metadata><dc:Source>
					else if(SOURCE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_SOURCE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Language>
					else if(LANGUAGE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_LANGUAGE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Relation>
					else if(RELATION_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_RELATION, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Coverage>
					else if(COVERAGE_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_COVERAGE, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
					//<package><metadata><dc-metadata><dc:Rights>
					else if(RIGHTS_PROPERTY_NAME.equals(propertyLocalName))
					{
						XML.appendElementNS(dcMetadataElement, DCMI10_ELEMENTS_NAMESPACE_URI.toString(),
							PKG_ELEMENT_MANIFEST_DC_METADATA_DC_RIGHTS, propertyValue);  //add the OEBPPS 1.x DC metadata element
					}
				}
			}
		}
		  //package/manifest
/*TODO fix
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
*/
		  //package/spine
		final RDFListResource<?> spine=publication.getSpine(); //get the spine
		if(spine!=null)  //if the publication has a spine
		{
			final Element spineElement=XML.appendElementNS(packageElement, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_SPINE); //create the spine element
			for(final RDFObject item:spine)	//for each item in the spine
			{
				spineElement.appendChild(generateItemRefElement(document, (RDFResource)item, publication.getURI()));  //generate an item element and add it to the spine element
			}
		}
		if(publication.getGuideList().size()>0) //if there are guides
		{
				//package/guide
			final Element guideElement=XML.appendElementNS(packageElement, OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_GUIDE); //create the guide element
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
		final String href=XPackage.getLocationHRef(oebItem); //get the item's href
		if(href!=null)  //if the item has an href
		{
		  itemElement.setAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_HREF, href);		  //set the href
//TODO maybe assert that there is a media type, here
		}
/*TODO fix
		final ContentType mediaType=Marmot.getMediaType(oebItem);  //get the item's media type
		if(mediaType!=null) //if the item has a media type
		{
		  itemElement.setAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_MEDIA_TYPE, mediaType.toString());		  //set the media type
		}
*/
/*TODO fix
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
		if(!XML.isName(identifier))  //if the identifier is not a name already
		{
			final StringBuffer stringBuffer=new StringBuffer(identifier); //create a string buffer to work with our identifier
				//remove the publication URI, if present
			for(int i=stringBuffer.length()-1; i>=0; --i) //look at each character from right to left
			{
				if(!XML.isNameChar(stringBuffer.charAt(i)))  //if this is not a name character
				{
					final String prefix;  //we'll find out the prefix differently, depending on if the divider is a hash symbol
					if(stringBuffer.charAt(i)=='#' && i>0) //if this is a fragment character that isn't the first characterTODO use a constant
						prefix=stringBuffer.substring(0, i);  //the prefix is everything before the fragment character
					else  //if this is some other sort of non-name character
						prefix=stringBuffer.substring(0, i+1);  //use everything up to and including the character as a prefix
				  if(prefix.equals(publicationURI.toString())) //if the prefix is the publication URI
						stringBuffer.delete(0, i+1);  //delete the entire prefix, including this character
					break;  //stop looking for a non-nam character
				}
			}
				//convert characters to XML name characters
			for(int i=stringBuffer.length()-1; i>=0; --i) //look at each character again
			{
				if(!XML.isNameChar(stringBuffer.charAt(i)))  //if this is not a name character
				{
					stringBuffer.setCharAt(i, '_'); //replace the character with an underscore TODO use a constant
				}
			}
				//if the first character isn't an XML character
			if(stringBuffer.length()>0 && !XML.isNameFirstChar(stringBuffer.charAt(0)))
			{
				stringBuffer.insert(0, 'x');  //prepend the string with 'x' TODO use a constant
			}
			return stringBuffer.toString(); //return the string we manipulated
		}
		else  //if the identifier is already a name
			return identifier;  //return the identifier as is
	}

}