package com.garretwilson.rdf.xmlschema;

import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.rdf.*;
import static com.garretwilson.text.xml.schema.XMLSchemaConstants.*;

/**An RDF literal that represents an XML Schema integer.
The integer is represented by a Java long data type.
@author Garret Wilson
@see Integer
*/
public class IntegerLiteral extends NumberLiteral
{

	/**Returns the literal value that the lexical form represents.
	Convenience method for <code>getValue()</code>.
	@return The literal value as an <code>Integer</code>.
	*/
//TODO del if not needed; perhaps use BigInteger for long integers	public Integer getInteger() {return (Integer)getValue();}

	/**Returns the literal value that the lexical form represents.
	Convenience method for <code>getValue()</code>.
	@return The literal value as an <code>Long</code>.
	*/
	public Long getLong() {return (Long)getValue();}

	/**Constructs an integer literal using the datatype
	<code>http://www.w3.org/2001/XMLSchema#integer</code>.
	@param value The boolean value representing the value of the literal.
	*/
	public IntegerLiteral(final long value)
	{
		this(Long.valueOf(value));	//create a new Integer object and construct the class with it
	}

	/**Constructs an integer literal using the datatype
	<code>http://www.w3.org/2001/XMLSchema#integer</code>.
	@param object The <code>Integer</code> object representing the value of the literal.
	*/
	public IntegerLiteral(final Long object)
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
		return asInstance(rdfObject, IntegerLiteral.class);	//cast the object to an integer literal if we can
	}

	/**Determines if the RDF object is an integer literal and, if so, casts the 
		object to an integer literal and returns its value; otherwise, returns
		-1.
	@param rdfObject The RDF object in question.
	@return The long value of the integer literal, or -1 if
		the object is not an integer literal or the object is <code>null</code>.
	*/
	public static long asLongValue(final RDFObject rdfObject)
	{
		final IntegerLiteral integerLiteral=asIntegerLiteral(rdfObject);	//cast the object to an integer literal, if it is one
		return integerLiteral!=null ? integerLiteral.getLong().longValue() : -1;	//return the long value of the integer literal, or -1 if there is no integer literal
	}

	/**Determines if the RDF object is an integer literal and, if so, casts the 
		object to an integer literal and returns its value; otherwise, returns
		-1.
	@param rdfObject The RDF object in question.
	@return The integer value of the integer literal, or -1 if
		the object is not an integer literal or the object is <code>null</code>.
	*/
	public static int asIntValue(final RDFObject rdfObject)	//TODO del if not needed or desired
	{
		final IntegerLiteral integerLiteral=asIntegerLiteral(rdfObject);	//cast the object to an integer literal, if it is one
		return integerLiteral!=null ? integerLiteral.getLong().intValue() : -1;	//return the integer value of the integer literal, or -1 if there is no integer literal
	}

}