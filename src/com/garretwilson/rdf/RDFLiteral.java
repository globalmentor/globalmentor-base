package com.garretwilson.rdf;

/**Represents an RDF literal, either plain or typed.
@author Garret Wilson
*/
public abstract class RDFLiteral implements RDFObject
{

	/**@return The lexical form, a Unicode string in Normal Form C.*/
	public abstract String getLexicalForm();

	/**If <code>object</code> is another <code>RDFLiteral</code>, compares the
		literal values. If <code>object</code> is a <code>String</code>
		compares the string with this literal's value. Otherwise, compares the
		objects using the superclass functionality.
	@param object The object with which to compare this literal; should be
		another literal or a Java string.
	@return <code>true<code> if this literal equals that specified in
		<code>object</code>.
	@see #getValue
	*/
	public boolean equals(Object object)	//G***do we really want to compare this with a string?
	{
		if(object instanceof RDFLiteral)	//if we're being compared with another literal
		{
			return getLexicalForm().equals(((RDFLiteral)object).getLexicalForm()); //compare values
		}
		else if(object instanceof String)	//if we're being compared with a string
		{
			return getLexicalForm().equals((String)object); //compare our value with the string
		}
		else	//if we're being compared with anything else
			return super.equals(object);	//use the default compare
	}

	/**@return A hashcode value composed from the value.*/
	public int hashCode()
	{
		return getLexicalForm().hashCode();  //return the hash code of the value
	}

	/**@return A string representation of the literal's lexical form.*/
	public String toString()
	{
		return getLexicalForm();  //return the lexical form
	}
}