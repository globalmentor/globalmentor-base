package com.garretwilson.rdf;

/**Represents an RDF literal, either plain or typed.
@author Garret Wilson
*/
public abstract class RDFLiteral implements RDFObject
{

	/**@return The lexical form, a Unicode string in Normal Form C.*/
	public abstract String getLexicalForm();

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