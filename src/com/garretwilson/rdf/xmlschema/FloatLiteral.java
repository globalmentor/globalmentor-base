package com.garretwilson.rdf.xmlschema;

import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.rdf.*;
import static com.garretwilson.text.xml.schema.XMLSchemaConstants.*;

/**An RDF literal that represents an XML Schema 32-bit float.
@author Garret Wilson
@see Float
*/
public class FloatLiteral extends NumberLiteral<Float>
{

	/**Constructs a float literal using the datatype <code>http://www.w3.org/2001/XMLSchema#float</code>.
	@param value The float value representing the value of the literal.
	*/
	public FloatLiteral(final float value)
	{
		this(Float.valueOf(value));	//create a new Float object and construct the class with it
	}

	/**Constructs a float literal using the datatype <code>http://www.w3.org/2001/XMLSchema#float</code>.
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

	/**Determines if the RDF object is a float literal and, if so, casts the 
		object to a float literal and returns it.
	@param rdfObject The RDF object in question.
	@return The RDF object as a float literal, or <code>null</code> if the
		object is not a float literal or the object is <code>null</code>.
	*/
	public static FloatLiteral asFloatLiteral(final RDFObject rdfObject)
	{
		return asInstance(rdfObject, FloatLiteral.class);	//cast the object to a float literal if we can
	}

}