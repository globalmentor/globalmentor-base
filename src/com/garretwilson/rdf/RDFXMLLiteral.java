package com.garretwilson.rdf;

import java.io.*;
import com.garretwilson.text.CharacterEncoding;
import com.garretwilson.text.xml.XMLDOMImplementation;
import com.garretwilson.text.xml.XMLSerializer;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.util.Debug;
import com.garretwilson.util.LocaleText;

import org.w3c.dom.*;

/**An encapsulation of <code>rdf:XMLLiteral</code> that holds a
	<code>DocumentFragment</code> containing the literal value.
@author Garret Wilson
@see DocumentFragment
*/
public class RDFXMLLiteral extends RDFTypedLiteral
{

	/**Returns the literal value that the lexical form represents in the form
		of an XML document fragment. Convenience method for <code>getValue()</code>.
	@return The literal value as an XML document fragment.
	@see DocumentFragment
	*/
	public DocumentFragment getDocumentFragment() {return (DocumentFragment)getValue();}

	/**Returns the lexical form of the literal.
	This version returns a serialized version of the contents of the stored XML
		document fragment.
	@return The lexical form, a Unicode string in Normal Form C.
	@see #getValue
	*/
	public String getLexicalForm()
	{
		try
		{
			final XMLSerializer xmlSerializer=new XMLSerializer(false);	//create an unformatted XML serializer
				//create a new byte array output stream in which to serialize the contents of the document fragment
			final ByteArrayOutputStream byteArrayOutputStream=new ByteArrayOutputStream();
				//serialize the document fragment into the stream using UTF-8 encoding
			xmlSerializer.serializeContent(getDocumentFragment(), byteArrayOutputStream, CharacterEncoding.UTF_8);
			return byteArrayOutputStream.toString(CharacterEncoding.UTF_8);	//convert the serialized content to a string and return it
		}
		catch(IOException ioException)	//if there are any IO errors (this should never happen)
		{
			throw new AssertionError(ioException);
		}
	}

	/**@return A locale-aware representation of the literal's lexical form, indicating any locale information available.*/
/*TODO create and return a special LocaleText subclass that has both plain text and an XML fragment 
	public LocaleText toLocaleText()
	{
		return new LocaleText(getLexicalForm());	//by default we don't know any locale 
	}
*/
	/**Constructs an XML literal using the datatype
	<code>http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral</code>.
	@param documentFragment The document fragment representing the value of the
		XML literal.
	*/
	public RDFXMLLiteral(final DocumentFragment documentFragment)
	{
		super(documentFragment, XML_LITERAL_DATATYPE_URI);	//save the document fragment as the value, specifying the XML literal datatype URI
	}

	/**Constructs an XML literal with the given text as its contents.
	@param text The text to be wrapped in a document fragment and stored as the
		value of the XML literal.
	*/
	public RDFXMLLiteral(final String text)
	{
		super(createDocumentFragment(text), XML_LITERAL_DATATYPE_URI);	//save a document fragment constructed from the text as the value, specifying the XML literal datatype URI
	}

	/**Creates a document fragment containing the given text.
	@param text The text to be wrapped in a document fragment.
	*/
	protected static DocumentFragment createDocumentFragment(final String text)
	{
		final DOMImplementation domImplementation=new XMLDOMImplementation();	//TODO get a DOMImplementation in an implementation-agnostic way
			//create a document with an document element of <rdf:value>
		final Document document=domImplementation.createDocument(RDF_NAMESPACE_URI.toString(), XMLUtilities.createQualifiedName(RDF_NAMESPACE_PREFIX, VALUE_PROPERTY_NAME), null);
		final Element documentElement=document.getDocumentElement();	//get a reference to the document element
		XMLUtilities.appendText(documentElement, text);	//append the text to the document element
		return XMLUtilities.extractChildren(documentElement);	//extract the children of the document element to a document fragment and return that fragment
	}
}