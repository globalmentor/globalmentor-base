package com.garretwilson.rdf.xmlschema;

import com.garretwilson.rdf.*;
import com.garretwilson.text.xml.schema.XMLSchemaConstants;

/**An RDF literal that represents an XML Schema integer.
@author Garret Wilson
@see Integer
*/
public class IntegerLiteral extends RDFTypedLiteral implements XMLSchemaConstants
{

	/**Returns the literal value that the lexical form represents.
	Convenience method for <code>getValue()</code>.
	@return The literal value as an <code>Integer</code>.
	*/
	public Integer getInteger() {return (Integer)getValue();}

	/**Constructs an integer literal using the datatype
	<code>http://www.w3.org/2001/XMLSchema#integer</code>.
	@param value The boolean value representing the value of the literal.
	*/
	public IntegerLiteral(final int value)
	{
		this(new Integer(value));	//create a new Integer object and construct the class with it
	}

	/**Constructs an integer literal using the datatype
	<code>http://www.w3.org/2001/XMLSchema#integer</code>.
	@param object The <code>Integer</code> object representing the value of the literal.
	*/
	public IntegerLiteral(final Integer object)
	{
		super(object, INTEGER_DATATYPE_URI);	//save the Integer object as the value, specifying the XML Schema integer datatype URI
	}

	/**Constructs an integer literal from a lexical value.
	@param lexicalForm The lexical form of the integer value.
	*/
	public IntegerLiteral(final String lexicalForm)
	{
		super(Integer.valueOf(lexicalForm), INTEGER_DATATYPE_URI);	//save an Integer constructed from the text as the value, specifying the XML boolean datatype URI
	}

}