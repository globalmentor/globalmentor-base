package com.garretwilson.rdf.xmlschema;

import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.rdf.*;
import static com.garretwilson.text.xml.schema.XMLSchemaConstants.*;

/**An RDF literal that represents an XML Schema boolean.
@author Garret Wilson
@see Boolean
*/
public class BooleanLiteral extends RDFTypedLiteral
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
		super(object, BOOLEAN_DATATYPE_URI);	//save the Boolean  object as the value, specifying the XML Schema boolean datatype URI
	}

	/**Constructs a boolean literal from a lexical value.
	@param lexicalForm The lexical form of the boolean value.
	*/
	public BooleanLiteral(final String lexicalForm)
	{
		super(Boolean.valueOf(lexicalForm), BOOLEAN_DATATYPE_URI);	//save a Boolean constructed from the text as the value, specifying the XML boolean datatype URI
	}

	/**Determines if the RDF object is a boolean literal and, if so, casts the 
		object to a boolean literal and returns it.
	@param rdfObject The RDF object in question.
	@return The RDF object as a boolean literal, or <code>null</code> if the
		object is not a boolean literal or the object is <code>null</code>.
	*/
	public static BooleanLiteral asBooleanLiteral(final RDFObject rdfObject)
	{
		return asInstance(rdfObject, BooleanLiteral.class);	//cast the object to a boolean literal if we can
	}

	/**Determines if the RDF object is a boolean literal and, if so, casts the 
		object to a boolean literal and returns its value; otherwise, returns
		<code>false</code>.
	@param rdfObject The RDF object in question.
	@return The boolean value of the boolean literal, or <code>false</code> if the
		object is not a boolean literal or the object is <code>null</code>.
	*/
	public static boolean asBooleanValue(final RDFObject rdfObject)
	{
		final BooleanLiteral booleanLiteral=asBooleanLiteral(rdfObject);	//cast the object to a boolean literal, if it is one
		return booleanLiteral!=null ? booleanLiteral.getBoolean().booleanValue() : false;	//return the boolean value of the boolean literal, or false if there is no boolean literal
	}
}