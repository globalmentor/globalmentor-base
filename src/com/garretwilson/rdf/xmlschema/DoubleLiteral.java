package com.garretwilson.rdf.xmlschema;

import com.garretwilson.rdf.*;
import com.garretwilson.text.xml.schema.XMLSchemaConstants;

/**An RDF literal that represents an XML Schema 64-bit double.
@author Garret Wilson
@see Double
*/
public class DoubleLiteral extends RDFTypedLiteral implements XMLSchemaConstants
{

	/**Returns the literal value that the lexical form represents.
	Convenience method for <code>getValue()</code>.
	@return The literal value as a <code>Double</code>.
	*/
	public Double getDouble() {return (Double)getValue();}

	/**Constructs a double literal using the datatype
	<code>http://www.w3.org/2001/XMLSchema#double</code>.
	@param value The double value representing the value of the literal.
	*/
	public DoubleLiteral(final double value)
	{
		this(new Double(value));	//create a new Double object and construct the class with it
	}

	/**Constructs a double literal using the datatype
	<code>http://www.w3.org/2001/XMLSchema#double</code>.
	@param object The <code>Double</code> object representing the value of the literal.
	*/
	public DoubleLiteral(final Double object)
	{
		super(object, DOUBLE_DATATYPE_URI);	//save the Double object as the value, specifying the XML Schema double datatype URI
	}

	/**Constructs a double literal from a lexical value.
	@param lexicalForm The lexical form of the float value.
	*/
	public DoubleLiteral(final String lexicalForm)
	{
		super(Double.valueOf(lexicalForm), DOUBLE_DATATYPE_URI);	//save a Double constructed from the text as the value, specifying the XML double datatype URI
	}

}