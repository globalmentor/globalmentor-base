package com.garretwilson.rdf.xmlschema;

import java.net.URI;

import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.rdf.*;

/**An RDF literal that represents an abstract number object.
@author Garret Wilson
@see Number
*/
public abstract class NumberLiteral extends RDFTypedLiteral
{

	/**Returns the literal value that the lexical form represents.
	Convenience method for <code>getValue()</code>.
	@return The literal value as an <code>Integer</code>.
	*/
	public Number getNumber() {return (Number)getValue();}

	/**Constructs a typed number literal.
	@param literalValue The literal value that the lexical form represents.
	@param literalDatatypeURI The reference URI identifying the datatype of this literal.
	*/
	public NumberLiteral(final Number literalValue, final URI literalDatatypeURI)
	{
		super(literalValue, literalDatatypeURI);	//construct the parent class with the number
	}

	/**Determines if the RDF object is a number literal and, if so, casts the 
		object to a number literal and returns it.
	@param rdfObject The RDF object in question.
	@return The RDF object as a number literal, or <code>null</code> if the
		object is not a number literal or the object is <code>null</code>.
	*/
	public static NumberLiteral asNumberLiteral(final RDFObject rdfObject)
	{
		return asInstance(rdfObject, NumberLiteral.class);	//cast the object to a number literal if we can
	}

}