package com.garretwilson.rdf.xmlschema;

import com.garretwilson.lang.ObjectUtilities;
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

	/**Determines if the RDF object is an integer literal and, if so, casts the 
		object to an integer literal and returns it.
	@param rdfObject The RDF object in question.
	@return The RDF object as an integer literal, or <code>null</code> if the
		object is not an integer literal or the object is <code>null</code>.
	*/
	public static IntegerLiteral asIntegerLiteral(final RDFObject rdfObject)
	{
		return (IntegerLiteral)ObjectUtilities.asInstance(rdfObject, IntegerLiteral.class);	//cast the object to an integer literal if we can
	}

	/**Determines if the RDF object is an integer literal and, if so, casts the 
		object to an integer literal and returns its value; otherwise, returns
		<code>false</code>.
	@param rdfObject The RDF object in question.
	@return The boolean value of the integer literal, or -1 if
		the object is not an integer literal or the object is <code>null</code>.
	*/
	public static int asIntValue(final RDFObject rdfObject)
	{
		final IntegerLiteral integerLiteral=asIntegerLiteral(rdfObject);	//cast the object to an integer literal, if it is one
		return integerLiteral!=null ? integerLiteral.getInteger().intValue() : -1;	//return the integer value of the integer literal, or -1 if there is no integer literal
	}

}