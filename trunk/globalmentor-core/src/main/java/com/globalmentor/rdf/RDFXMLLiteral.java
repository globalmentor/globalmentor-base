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

package com.globalmentor.rdf;

import java.io.*;

import static com.globalmentor.io.Charsets.*;
import static com.globalmentor.rdf.RDF.*;

import com.globalmentor.text.xml.*;

import org.w3c.dom.*;
import org.xml.sax.SAXException;

/**
 * An encapsulation of <code>rdf:XMLLiteral</code> that holds a <code>DocumentFragment</code> containing the literal value.
 * @author Garret Wilson
 * @see DocumentFragment
 */
public class RDFXMLLiteral extends RDFTypedLiteral<DocumentFragment> {

	/**
	 * Returns the lexical form of the literal. This version returns a serialized version of the contents of the stored XML document fragment.
	 * @return The lexical form, a Unicode string in Normal Form C.
	 * @see #getValue()
	 */
	public String getLexicalForm() {
		final XMLSerializer xmlSerializer = new XMLSerializer(false); //create an unformatted XML serializer
		return xmlSerializer.serialize(getValue()); //serialize the document fragment into a string and return that string
	}

	/** @return A locale-aware representation of the literal's lexical form, indicating any locale information available. */
	/*TODO create and return a special LocaleText subclass that has both plain text and an XML fragment 
		public LocaleText toLocaleText()
		{
			return new LocaleText(getLexicalForm());	//by default we don't know any locale 
		}
	*/

	/**
	 * Constructs an XML literal using the datatype <code>http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral</code>.
	 * @param documentFragment The document fragment representing the value of the XML literal.
	 */
	public RDFXMLLiteral(final DocumentFragment documentFragment) {
		super(documentFragment, XML_LITERAL_DATATYPE_URI); //save the document fragment as the value, specifying the XML literal datatype URI
	}

	/**
	 * Constructs an XML literal from the given lexical from of an XML document fragment.
	 * @param lexicalForm The lexical form of the XML document fragment.
	 * @throws IllegalArgumentException If there is a problem parsing the given string as an XML document fragment.
	 */
	public RDFXMLLiteral(final String text) {
		super(createDocumentFragment(text), XML_LITERAL_DATATYPE_URI); //save a document fragment constructed from the text as the value, specifying the XML literal datatype URI
	}

	/**
	 * Constructs an XML literal with the given text as its contents.
	 * @param text The text to be wrapped in a document fragment and stored as the value of the XML literal.
	 */
	/*TODO add as a factory method if needed.
		public RDFXMLLiteral(final String text)
		{
			super(createDocumentFragment(text), XML_LITERAL_DATATYPE_URI);	//save a document fragment constructed from the text as the value, specifying the XML literal datatype URI
		}
	*/

	/**
	 * Creates a document fragment containing the given text.
	 * @param text The text to be wrapped in a document fragment.
	 */
	/*TODO bring back if needed
		protected static DocumentFragment createDocumentFragment(final String text)
		{
			final DOMImplementation domImplementation=new XMLDOMImplementation();	//TODO get a DOMImplementation in an implementation-agnostic way
				//create a document with an document element of <rdf:value>
			final Document document=domImplementation.createDocument(RDF_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, VALUE_PROPERTY_NAME), null);
			final Element documentElement=document.getDocumentElement();	//get a reference to the document element
			XMLUtilities.appendText(documentElement, text);	//append the text to the document element
			return XMLUtilities.extractChildren(documentElement);	//extract the children of the document element to a document fragment and return that fragment
		}
	*/

	/**
	 * Creates a document fragment from the given lexical form.
	 * @param lexicalForm The lexical form of the document fragment.
	 * @throws IllegalArgumentException If there is a problem parsing the given string as an XML document fragment.
	 */
	protected static DocumentFragment createDocumentFragment(final String lexicalForm) {
		final String xmlDocumentLexicalForm = "<dummy>" + lexicalForm + "</dummy>"; //wrap the lexical form in a dummy element
		try {
			final Document document = XML.createDocumentBuilder(true).parse(new ByteArrayInputStream(xmlDocumentLexicalForm.getBytes(UTF_8_CHARSET))); //parse the document, recognizing namespaces
			return XML.extractChildren(document.getDocumentElement()); //extract the children of the document element to a document fragment and return that fragment
		} catch(final IOException ioException) { //there should never be an I/O exception reading from a string
			throw new AssertionError(ioException);
		} catch(final SAXException saxException) {
			throw new IllegalArgumentException(saxException);
		}
	}

}