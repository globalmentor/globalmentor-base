package com.garretwilson.rdf;

/**Represents an RDF literal.
@author Garret Wilson
*/
public class Literal implements RDFObject
{

	/**The literal value.*/
	private final String value;

		/**@return The literal value.*/
		public String getValue() {return value;}

	/**Constructs a literal with a value.
	@param newValue The literal value.
	*/
	public Literal(final String newValue)
	{
		value=newValue; //set the value
	}

	/**If <code>object</code> is another <code>Literal</code>, compares the
		literal values. If <code>object</code> is a <code>String</code>
		compares the string with this literal's value. Otherwise, compares the
		objects using the superclass functionality.
	@param object The object with which to compare this literal; should be
		another literal or a Java string.
	@return <code>true<code> if this literal equals that specified in
		<code>object</code>.
	@see #getValue
	*/
	public boolean equals(Object object)
	{
		if(object instanceof Literal)	//if we're being compared with another literal
		{
			return getValue().equals(((Literal)object).getValue()); //compare values
		}
		else if(object instanceof String)	//if we're being compared with a string
		{
			return getValue().equals((String)object); //compare our value with the string
		}
		else	//if we're being compared with anything else
			return super.equals(object);	//use the default compare
	}

	/**@return A hashcode value composed from the value.*/
	public int hashCode()
	{
		return getValue().hashCode();  //return the hash code of the value
	}

	/**@return A string representation of the literal.*/
	public String toString()
	{
		return getValue();  //return the value
	}
}