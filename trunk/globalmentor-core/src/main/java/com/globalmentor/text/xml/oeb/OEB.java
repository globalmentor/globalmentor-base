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

import java.net.URI;

import javax.xml.parsers.*;

import com.globalmentor.net.*;

import static com.globalmentor.text.xml.oeb.css.OEBCSS.*;
import static com.globalmentor.text.xml.XML.*;
import static com.globalmentor.text.xml.xhtml.XHTML.*;

import org.w3c.dom.*;

/**
 * Constants and utilities for working with the Open eBook (OEB) format.
 * @author Garret Wilson
 */
public class OEB {

	/** The public ID for the OEBPS 1.0 package. */
	public final static String OEB10_PACKAGE_PUBLIC_ID = "+//ISBN 0-9673008-1-9//DTD OEB 1.0 Package//EN";
	/** The default system ID for the OEBPS 1.0 package. */
	public final static String OEB10_PACKAGE_SYSTEM_ID = "http://openebook.org/dtds/oeb-1.0/oebpkg1.dtd";
	/** The name extension for the OEBPS 1.0 package. */
	public final static String OEB1_PACKAGE_NAME_EXTENSION = "opf";

	/** The public ID for the OEBPS 1.0.1 package. */
	public final static String OEB101_PACKAGE_PUBLIC_ID = "+//ISBN 0-9673008-1-9//DTD OEB 1.0.1 Package//EN";
	/** The default system ID for the OEBPS 1.0.1 package. */
	public final static String OEB101_PACKAGE_SYSTEM_ID = "http://openebook.org/dtds/oeb-1.0.1/oebpkg101.dtd";

	/** The recommended prefix to the OEB 1.0 package namespace. */
	public static final String OEB1_PACKAGE_NAMESPACE_PREFIX = "oebpackage";
	/** The URI to the OEB 1.0 package namespace. */
	public static final URI OEB1_PACKAGE_NAMESPACE_URI = URI.create("http://openebook.org/namespaces/oeb-package/1.0/");

	//The OEB 1.0 XML package element names.*/
	public final static String PKG_ELEMENT_PACKAGE = "package";
	public final static String PKG_ELEMENT_PACKAGE_ATTRIBUTE_UNIQUE_IDENTIFIER = "unique-identifier";
	public final static String PKG_ELEMENT_METADATA = "metadata";
	public final static String PKG_ELEMENT_METADATA_DC_METADATA = "dc-metadata";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TITLE = "dc:Title";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CREATOR = "dc:Creator";
	public final static String PKG_METADATA_DC_METADATA_DC_CREATOR_ATTRIBUTE_ROLE = "role";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_SUBJECT = "dc:Subject";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_DESCRIPTION = "dc:Description";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_PUBLISHER = "dc:Publisher";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CONTRIBUTOR = "dc:Contributor";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_DATE = "dc:Date";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TYPE = "dc:Type";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_FORMAT = "dc:Format";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_IDENTIFIER = "dc:Identifier";
	public final static String PKG_METADATA_DC_METADATA_DC_IDENTIFIER_ATTRIBUTE_ID = "id";
	public final static String PKG_METADATA_DC_METADATA_DC_IDENTIFIER_ATTRIBUTE_SCHEME = "scheme";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_SOURCE = "dc:Source";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_LANGUAGE = "dc:Language";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_RELATION = "dc:Relation";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_COVERAGE = "dc:Coverage";
	public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_RIGHTS = "dc:Rights";
	public final static String PKG_ELEMENT_MANIFEST = "manifest";
	public final static String PKG_ELEMENT_MANIFEST_ITEM = "item";
	public final static String PKG_MANIFEST_ITEM_ATTRIBUTE_ID = "id";
	public final static String PKG_MANIFEST_ITEM_ATTRIBUTE_HREF = "href";
	public final static String PKG_MANIFEST_ITEM_ATTRIBUTE_MEDIA_TYPE = "media-type";
	public final static String PKG_MANIFEST_ITEM_ATTRIBUTE_FALLBACK = "fallback";
	public final static String PKG_ELEMENT_SPINE = "spine";
	public final static String PKG_ELEMENT_SPINE_ITEMREF = "itemref";
	public final static String PKG_SPINE_ITEMREF_ATTRIBUTE_IDREF = "idref";
	public final static String PKG_ELEMENT_GUIDE = "guide";
	public final static String PKG_ELEMENT_TOURS = "tours";
	public final static String PKG_ELEMENT_GUIDE_REFERENCE = "reference";
	public final static String PKG_GUIDE_REFERENCE_ATTRIBUTE_TYPE = "type";
	public final static String PKG_GUIDE_REFERENCE_ATTRIBUTE_TITLE = "title";
	public final static String PKG_GUIDE_REFERENCE_ATTRIBUTE_HREF = "href";

	/** An OEB 1.x publication zip file MIME subtype. */
	public final static String X_OEB_PUBLICATION_ZIP_SUBTYPE = ContentType.SUBTYPE_EXTENSION_PREFIX + "oeb-publication"
			+ ContentType.SUBTYPE_SUFFIX_DELIMITER_CHAR + XML_SUBTYPE_SUFFIX + ContentType.SUBTYPE_SUFFIX_DELIMITER_CHAR + ContentTypeConstants.ZIP_SUBTYPE_SUFFIX;
	/** An OEB 1.x publication document MIME subtype. */
	public final static String X_OEB1_DOCUMENT_SUBTYPE = ContentType.SUBTYPE_EXTENSION_PREFIX + "oeb1-document";
	/** An OEB 1.x package MIME subtype. */
	public final static String X_OEB1_PACKAGE_XML_SUBTYPE = ContentType.SUBTYPE_EXTENSION_PREFIX + "oeb1-package" + ContentType.SUBTYPE_SUFFIX_DELIMITER_CHAR
			+ XML_SUBTYPE_SUFFIX;

	/** The media type of an OEB 1.0 package. */
	public final static ContentType OEB10_PACKAGE_MEDIA_TYPE = ContentType.create(ContentType.APPLICATION_PRIMARY_TYPE, X_OEB1_PACKAGE_XML_SUBTYPE);

	/** The media type of an OEB 1.0 document. */
	public final static ContentType OEB10_DOCUMENT_MEDIA_TYPE = ContentType.create(ContentType.TEXT_PRIMARY_TYPE, X_OEB1_DOCUMENT_SUBTYPE);

	/** The media type of an OEB 1.0 CSS document. */
	public final static ContentType OEB10_CSS_MEDIA_TYPE = ContentType.create(ContentType.TEXT_PRIMARY_TYPE, X_OEB1_CSS_SUBTYPE);

	/** The public ID for OEBPS 1.0. */
	public final static String OEB10_DOCUMENT_PUBLIC_ID = "+//ISBN 0-9673008-1-9//DTD OEB 1.0 Document//EN";
	/** The default system ID for OEBPS 1.0. */
	public final static String OEB10_DOCUMENT_SYSTEM_ID = "http://openebook.org/dtds/oeb-1.0/oebdoc1.dtd";

	/** The public ID for OEBPS 1.0.1. */
	public final static String OEB101_DOCUMENT_PUBLIC_ID = "+//ISBN 0-9673008-1-9//DTD OEB 1.0.1 Document//EN";
	/** The default system ID for OEBPS 1.0.1. */
	public final static String OEB101_DOCUMENT_SYSTEM_ID = "http://openebook.org/dtds/oeb-1.0.1/oebdoc101.dtd";

	/** The recommended prefix to the OEB 1.0 namespace. */
	public static final String OEB1_DOCUMENT_NAMESPACE_PREFIX = "oeb1";

	/** The URI to the OEB 1.0 namespace. */
	public static final URI OEB1_DOCUMENT_NAMESPACE_URI = URI.create("http://openebook.org/namespaces/oeb-document/1.0/");

	//Property names for OEB1
	public final static String OEB_CSS_PROP_OEB_COLUMN_NUMBER = "oeb-column-number";

	/**
	 * Creates a default OEB 1 document.
	 * @return A newly created default OEB 1 document with a body.
	 * @throws IllegalStateException if a document builder cannot be created which satisfies the configuration requested.
	 * @throws DOMException if there is an error creating the document.
	 */
	public static Document createDefaultOEB1Document() {
		return createXHTMLDocument("", true, OEB101_DOCUMENT_PUBLIC_ID, OEB101_DOCUMENT_SYSTEM_ID, true); //create a formatted XHTML document with the OEB doctype
	}

	/**
	 * Creates an OEB 1 document with the given fragment as children of the document <code>&lt;body&gt;</code> element.
	 * @param fragment The document fragment to add.
	 * @return A newly created OEB 1 document with the given content.
	 * @throws IllegalStateException if a document builder cannot be created which satisfies the configuration requested.
	 * @throws DOMException if there is an error creating the document.
	 */
	public static Document createOEB1Document(final DocumentFragment fragment) {
		final Document document = createDefaultOEB1Document(); //create a default document
		final Element bodyElement = getBodyElement(document); //get the body element
		final DocumentFragment importedFragment = (DocumentFragment)document.importNode(fragment, true); //import the fragment
		bodyElement.appendChild(importedFragment); //append the children of the new fragment to the document
		return document; //return the document we constructed
	}

	/**
	 * Creates a default OEB 1 package XML document.
	 * @return A newly created default OEB 1 package with a body.
	 * @throws IllegalStateException if a document builder cannot be created which satisfies the configuration requested.
	 * @throws DOMException if there is an error creating the document.
	 */
	public static Document createOEB1Package() {
		final DocumentBuilder documentBuilder;
		documentBuilder = createDocumentBuilder(true); //create a namespace-aware document builder
		final DOMImplementation domImplementation = documentBuilder.getDOMImplementation(); //get the DOM implementation from the document builder
		final DocumentType documentType = domImplementation.createDocumentType(PKG_ELEMENT_PACKAGE, OEB101_PACKAGE_PUBLIC_ID, OEB101_PACKAGE_SYSTEM_ID); //create an XML document type for the publication
		final Document document = domImplementation.createDocument(OEB1_PACKAGE_NAMESPACE_URI.toString(), PKG_ELEMENT_PACKAGE, documentType); //create a package XML document
		return document; //return the document we created
	}

}
