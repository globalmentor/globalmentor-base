package com.garretwilson.rdf.xmlschema;

import com.garretwilson.rdf.*;
import com.garretwilson.text.xml.schema.XMLSchemaConstants;

/**An RDF literal that represents an XML Schema 32-bit float.
@author Garret Wilson
@see Float
*/
public class FloatLiteral extends RDFTypedLiteral implements XMLSchemaConstants
{

	/**Returns the literal value that the lexical form represents.
	Convenience method for <code>getValue()</code>.
	@return The literal value as a <code>Float</code>.
	*/
	public Float getFloat() {return (Float)getValue();}

	/**Constructs a float literal using the datatype
	<code>http://www.w3.org/2001/XMLSchema#float</code>.
	@param value The float value representing the value of the literal.
	*/
	public FloatLiteral(final float value)
	{
		this(new Float(value));	//create a new Float object and construct the class with it
	}

	/**Constructs a float literal using the datatype
	<code>http://www.w3.org/2001/XMLSchema#float</code>.
	@param object The <code>Float</code> object representing the value of the literal.
	*/
	public FloatLiteral(final Float object)
	{
		super(object, FLOAT_DATATYPE_URI);	//save the Float  object as the value, specifying the XML Schema float datatype URI
	}

	/**Constructs a float literal from a lexical value.
	@param lexicalForm The lexical form of the float value.
	*/
	public FloatLiteral(final String lexicalForm)
	{
		super(Float.valueOf(lexicalForm), FLOAT_DATATYPE_URI);	//save a Float constructed from the text as the value, specifying the XML float datatype URI
	}

}