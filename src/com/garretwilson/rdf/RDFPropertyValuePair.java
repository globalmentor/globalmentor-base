package com.garretwilson.rdf;

import com.garretwilson.util.NameValuePair;

/**An RDF property and its value.
@author Garret Wilson
 */
public class RDFPropertyValuePair extends NameValuePair	//TODO update all the name/value pairs in the other RDF classes to use this class
{
	
	/**@return A property resource; the predicate of an RDF statement.*/
	public RDFResource getProperty() {return (RDFResource)getName();}
	
	/**@return A property value; the object of an RDF statement.*/
	public RDFObject getPropertyValue() {return (RDFObject)getValue();}

	/**Constructor specifying the RDF property and value.
	@param property A property resource; the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	*/
	public RDFPropertyValuePair(final RDFResource property, final RDFObject value)
	{
		super(property, value);	//construct the parent class
	}

}
