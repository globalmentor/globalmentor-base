package com.garretwilson.rdf;

/**Constants used in RDF XML serializations.
@author Garret Wilson
*/
public interface RDFXMLConstants
{

	//RDF XML elements
	/**The name of the enclosing RDF element.*/
	public final static String ELEMENT_RDF="RDF";
	/**The name of the RDF description element.*/
	public final static String ELEMENT_DESCRIPTION="Description";
	/**The name of the RDF container member element.*/
	public final static String ELEMENT_LI="li";

		//RDF XML attributes
	/**The name of the rdf:about attribute.*/
	public final static String ATTRIBUTE_ABOUT="about";
	/**The name of the rdf:id attribute.*/
	public final static String ATTRIBUTE_ID="ID";
	/**The name of the rdf:parseType attribute.*/
	public final static String ATTRIBUTE_PARSE_TYPE="parseType";
	/**The name of the rdf:resource attribute.*/
	public final static String ATTRIBUTE_RESOURCE="resource";

		//RDF XML attribute values
	/**The parse type for a collection.*/
	public final static String COLLECTION_PARSE_TYPE="Collection";

}