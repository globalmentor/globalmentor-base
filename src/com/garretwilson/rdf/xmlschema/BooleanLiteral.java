package com.garretwilson.rdf.xmlschema;

import com.garretwilson.rdf.*;
import com.garretwilson.text.xml.schema.XMLSchemaConstants;

/**An RDF literal that represents an XML Schema boolean.
@author Garret Wilson
@see Boolean
*/
public class BooleanLiteral extends RDFTypedLiteral implements XMLSchemaConstants
{

	/**Returns the literal value that the lexical form represents.
	Convenience method for <code>getValue()</code>.
	@return The literal value as a <code>Boolean</code>.
	*/
	public Boolean getBoolean() {return (Boolean)getValue();}

	/**Constructs a boolean literal using the datatype
	<code>http://www.w3.org/2001/XMLSchema#boolean</code>.
	@param value The boolean value representing the value of the literal.
	*/
	public BooleanLiteral(final boolean value)
	{
		this(Boolean.valueOf(value));	//get one of the predefined Boolean objects from the given value and construct this object
	}

	/**Constructs a boolean literal using the datatype
	<code>http://www.w3.org/2001/XMLSchema#boolean</code>.
	@param object The <code>Boolean</code> object representing the value of the literal.
	*/
	public BooleanLiteral(final Boolean object)
	{
		super(object, BOOLEAN_DATATYPE_URI);	//save the boolean object as the value, specifying the XML Schema boolean datatype URI
	}

	/**Constructs a boolean literal from a lexical value.
	@param lexicalForm The lexical form of the boolean value.
	*/
	public BooleanLiteral(final String lexicalForm)
	{
		super(Boolean.valueOf(lexicalForm), BOOLEAN_DATATYPE_URI);	//save a Boolean constructed from the text as the value, specifying the XML literal datatype URI
	}

}