package com.garretwilson.rdf;

import java.io.*;

import javax.xml.parsers.ParserConfigurationException;

import static com.garretwilson.rdf.RDFConstants.*;

import static com.garretwilson.text.CharacterEncodingConstants.*;
import com.garretwilson.text.xml.*;

import org.w3c.dom.*;
import org.xml.sax.SAXException;

/**An encapsulation of <code>rdf:XMLLiteral</code> that holds a
	<code>DocumentFragment</code> containing the literal value.
@author Garret Wilson
@see DocumentFragment
*/
public class RDFXMLLiteral extends RDFTypedLiteral<DocumentFragment>
{

	/**Returns the lexical form of the literal.
	This version returns a serialized version of the contents of the stored XML document fragment.
	@return The lexical form, a Unicode string in Normal Form C.
	@see #getValue()
	*/
	public String getLexicalForm()
	{
		final XMLSerializer xmlSerializer=new XMLSerializer(false);	//create an unformatted XML serializer
		return xmlSerializer.serialize(getValue());	//serialize the document fragment into a string and return that string
	}

	/**@return A locale-aware representation of the literal's lexical form, indicating any locale information available.*/
/*TODO create and return a special LocaleText subclass that has both plain text and an XML fragment 
	public LocaleText toLocaleText()
	{
		return new LocaleText(getLexicalForm());	//by default we don't know any locale 
	}
*/

	/**Constructs an XML literal using the datatype <code>http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral</code>.
	@param documentFragment The document fragment representing the value of the XML literal.
	*/
	public RDFXMLLiteral(final DocumentFragment documentFragment)
	{
		super(documentFragment, XML_LITERAL_DATATYPE_URI);	//save the document fragment as the value, specifying the XML literal datatype URI
	}

	/**Constructs an XML literal from the given lexical from of an XML document fragment.
	@param lexicalForm The lexical form of the XML document fragment.
	@exception IllegalArgumentException If there is a problem parsing the given string as an XML document fragment.
	*/
	public RDFXMLLiteral(final String text)
	{
		super(createDocumentFragment(text), XML_LITERAL_DATATYPE_URI);	//save a document fragment constructed from the text as the value, specifying the XML literal datatype URI
	}

	/**Constructs an XML literal with the given text as its contents.
	@param text The text to be wrapped in a document fragment and stored as the
		value of the XML literal.
	*/
/**TODO add as a factory method if needed.
	public RDFXMLLiteral(final String text)
	{
		super(createDocumentFragment(text), XML_LITERAL_DATATYPE_URI);	//save a document fragment constructed from the text as the value, specifying the XML literal datatype URI
	}
*/

	/**Creates a document fragment containing the given text.
	@param text The text to be wrapped in a document fragment.
	*/
/**TODO bring back if needed
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

	/**Creates a document fragment from the given lexical form.
	@param lexicalForm The lexical form of the document fragment.
	@exception IllegalArgumentException If there is a problem parsing the given string as an XML document fragment.
	*/
	protected static DocumentFragment createDocumentFragment(final String lexicalForm)
	{
		final String xmlDocumentLexicalForm="<dummy>"+lexicalForm+"</dummy>";	//wrap the lexical form in a dummy element
		try
		{
			final Document document=XMLUtilities.createDocumentBuilder(true).parse(new ByteArrayInputStream(xmlDocumentLexicalForm.getBytes(UTF_8)));	//parse the document, recognizing namespaces
			return XMLUtilities.extractChildren(document.getDocumentElement());	//extract the children of the document element to a document fragment and return that fragment
		}
		catch(final ParserConfigurationException parserConfigurationException)	//we should always be able to configure a namespace-aware XML parser
		{
			throw new AssertionError(parserConfigurationException);
		}
		catch(final IOException ioException)	//there should never be an I/O exception reading from a string
		{
			throw new AssertionError(ioException);
		}
		catch(final SAXException saxException)
		{
			throw new IllegalArgumentException(saxException);
		}
	}

}