package com.globalmentor.rdf.xmlschema;


import com.globalmentor.java.Objects;
import static com.globalmentor.text.xml.schema.XMLSchema.*;

import com.globalmentor.rdf.*;
import com.globalmentor.util.Base64;

/**An RDF literal that represents an XML Schema base64-encoded binary value.
@author Garret Wilson
@see Byte
*/
public class Base64BinaryLiteral extends RDFTypedLiteral<byte[]>
{

	/**Constructs an integer literal using the datatype <code>http://www.w3.org/2001/XMLSchema#base64Binary</code>.
	@param value The binary value of the literal.
	*/
	public Base64BinaryLiteral(final byte[] value)
	{
		super(value, BASE64_BINARY_DATATYPE_URI);	//save the binary data as the value, specifying the XML Schema base 64 binary datatype URI
	}

	/**Constructs a base64-encoded binary literal from a lexical value.
	@param lexicalForm The lexical form of the binary value.
	*/
	public Base64BinaryLiteral(final String lexicalForm)
	{
		super(Base64.decode(lexicalForm), BASE64_BINARY_DATATYPE_URI);	//save binary data constructed from the text as the value, specifying the XML base64-encoded binary datatype URI
	}

	/**Returns the lexical form of the literal.
	This version returns the base64-encoded string version of the stored bytes.
	@return The lexical form, the base64-encoded string version of the stored bytes.
	@see #getValue()
	*/
	public String getLexicalForm() {return Base64.encodeBytes(getValue());}

	/**Determines if the RDF object is a base64-encoded binary literal and, if so, 
		casts the object to a base64-encoded binary literal and returns it.
	@param rdfObject The RDF object in question.
	@return The RDF object as a base64-encoded binary literal, or <code>null</code> if the
		object is not a base64-encoded binary literal or the object is <code>null</code>.
	*/
	public static Base64BinaryLiteral asBase64BinaryLiteral(final RDFObject rdfObject)
	{
		return Objects.asInstance(rdfObject, Base64BinaryLiteral.class);	//cast the object to an integer literal if we can
	}

	/**Determines if the RDF object is a base64-encoded binary literal and, if so, 
		casts the object to a base64-encoded binary literal and returns its value;
		otherwise, returns <code>null</code>.
	@param rdfObject The RDF object in question.
	@return The binary data from the base64-encoded data value of the literal, or -1 if
		the object is not a base64-encoded binary literal or the object is <code>null</code>.
	*/
	public static byte[] asBytes(final RDFObject rdfObject)
	{
		final Base64BinaryLiteral base64BinaryLiteral=asBase64BinaryLiteral(rdfObject);	//cast the object to a base64-encoded binary literal, if it is one
		return base64BinaryLiteral!=null ? base64BinaryLiteral.getValue() : null;	//return the binary value of the base64-encoded binary literal, or null if there is no base64-encoded binary literal
	}

}