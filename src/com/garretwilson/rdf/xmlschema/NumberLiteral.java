package com.garretwilson.rdf.xmlschema;

import java.net.URI;

import static com.globalmentor.java.Objects.*;

import com.garretwilson.rdf.*;

/**An RDF literal that represents an abstract number object.
@param <T> The type of object the literal represents.
@author Garret Wilson
@see Number
*/
public abstract class NumberLiteral<T extends Number> extends RDFTypedLiteral<T>
{

	/**Constructs a typed number literal.
	@param literalValue The literal value that the lexical form represents.
	@param literalDatatypeURI The reference URI identifying the datatype of this literal.
	@exception NullPointerException if the value and/or the datatype URI is <code>null</code>.
	*/
	public NumberLiteral(final T literalValue, final URI literalDatatypeURI)
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