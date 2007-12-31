package com.garretwilson.rdf.xmlschema;

import static com.garretwilson.lang.Objects.*;

import java.net.URI;
import java.net.URISyntaxException;

import com.garretwilson.rdf.*;
import static com.garretwilson.text.xml.schema.XMLSchemaConstants.*;

/**An RDF literal that represents an XML Schema URI.
@author Garret Wilson
@see URI
*/
public class URILiteral extends RDFTypedLiteral<URI>
{

	/**Constructs a URI literal using the datatype <code>http://www.w3.org/2001/XMLSchema#anyURI</code>.
	@param object The URI object representing the value of the literal.
	*/
	public URILiteral(final URI object)
	{
		super(object, URI_DATATYPE_URI);	//save the URI object as the value, specifying the XML Schema boolean datatype URI
	}

	/**Constructs a URI literal from a lexical value.
	@param lexicalForm The lexical form of the URI value.
	@exception URISyntaxException if the given string does not represent a valid URI.
	*/
	public URILiteral(final String lexicalForm) throws URISyntaxException
	{
		super(new URI(lexicalForm), URI_DATATYPE_URI);	//create a URI from the lexical form, specifying the XML boolean datatype URI
	}

	/**Determines if the RDF object is a URI literal and, if so, casts the object to a URI literal and returns it.
	@param rdfObject The RDF object in question.
	@return The RDF object as a URI literal, or <code>null</code> if the object is not a URI literal or the object is <code>null</code>.
	*/
	public static URILiteral asURILiteral(final RDFObject rdfObject)
	{
		return asInstance(rdfObject, URILiteral.class);	//cast the object to a boolean literal if we can
	}

}