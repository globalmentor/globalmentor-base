package com.garretwilson.rdf;

import java.net.URI;

/**The base class for all RDF typed literals.
@author Garret Wilson
*/
public class RDFTypedLiteral extends RDFLiteral
{

	/**The literal value that the lexical form represents.*/
	private Object value;

		/**Returns the literal value that the lexical form represents.
		The type of this object will vary depending on the specific
			typed literal in question and the support of the implementation. 
		@return The literal value represented by the lexical form.
		*/
		public Object getValue() {return value;}

	/**The reference URI identifying the datatype of this literal.*/
	private final URI datatypeURI;

		/**The reference URI identifying the datatype of this literal.*/
		public final URI getDatatypeURI() {return datatypeURI;}

	/**Returns the lexical form of the literal.
	By default this version returns the string version of the stored object.
	@return The lexical form, a Unicode string in Normal Form C.
	@see #getValue
	*/
	public String getLexicalForm() {return getValue().toString();}

	/**Constructs a typed literal.
	@param literalValue The literal value that the lexical form represents.
	@param literalDatatypeURI The reference URI identifying the datatype of this literal.
	*/
	public RDFTypedLiteral(final Object literalValue, final URI literalDatatypeURI)
	{
		value=literalValue;	//save the value
		datatypeURI=literalDatatypeURI;	//save the datatype URI
	}

	/**If <code>object</code> is another <code>RDFTypedLiteral</code>, compares
		the datatype URI and value objects; otherwise, compares the
		objects using the superclass functionality.
	@param object The object with which to compare this typed literal; should be
		another typed literal.
	@return <code>true<code> if this literal equals that specified in
		<code>object</code>.
	@see #getValue
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof RDFTypedLiteral)	//if we're being compared with another typed literal
		{
			final RDFTypedLiteral typedLiteral=(RDFTypedLiteral)object;	//cast the object to a typed literal
			return getDatatypeURI().equals(typedLiteral.getDatatypeURI())	//compare datatype URIs
					&& getValue().equals(typedLiteral.getValue()); //compare values
		}
		else	//if we're being compared with anything else
			return super.equals(object);	//use the default compare
	}

}