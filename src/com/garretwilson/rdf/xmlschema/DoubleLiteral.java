package com.garretwilson.rdf.xmlschema;

import com.garretwilson.rdf.*;
import static com.garretwilson.text.xml.schema.XMLSchemaConstants.*;
import static com.globalmentor.java.Objects.*;

/**An RDF literal that represents an XML Schema 64-bit double.
@author Garret Wilson
@see Double
*/
public class DoubleLiteral extends NumberLiteral<Double>
{

	/**Constructs a double literal using the datatype <code>http://www.w3.org/2001/XMLSchema#double</code>.
	@param value The double value representing the value of the literal.
	*/
	public DoubleLiteral(final double value)
	{
		this(Double.valueOf(value));	//create a new Double object and construct the class with it
	}

	/**Constructs a double literal using the datatype <code>http://www.w3.org/2001/XMLSchema#double</code>.
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

	/**Determines if the RDF object is a double literal and, if so, casts the 
	object to a float literal and returns it.
	@param rdfObject The RDF object in question.
	@return The RDF object as a float literal, or <code>null</code> if the
		object is not a float literal or the object is <code>null</code>.
	*/
	public static DoubleLiteral asDoubleLiteral(final RDFObject rdfObject)
	{
		return asInstance(rdfObject, DoubleLiteral.class);	//cast the object to a double literal if we can
	}

}