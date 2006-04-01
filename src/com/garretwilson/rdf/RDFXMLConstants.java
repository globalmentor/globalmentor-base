package com.garretwilson.rdf;

/**Constants used in RDF XML serializations.
@author Garret Wilson
*/
public class RDFXMLConstants
{

	//RDF XML elements
	/**The name of the enclosing RDF element.*/
	public final static String ELEMENT_RDF="RDF";
	/**The name of the RDF description element.*/
	public final static String ELEMENT_DESCRIPTION="Description";

		//RDF XML attributes
	/**The name of the rdf:about attribute.*/
	public final static String ATTRIBUTE_ABOUT="about";
	/**The name of the rdf:ID attribute.*/
	public final static String ATTRIBUTE_ID="ID";
	/**The name of the rdf:nodeID attribute.*/
	public final static String ATTRIBUTE_NODE_ID="nodeID";
	/**The name of the rdf:parseType attribute.*/
	public final static String ATTRIBUTE_PARSE_TYPE="parseType";
	/**The name of the rdf:resource attribute.*/
	public final static String ATTRIBUTE_RESOURCE="resource";
	/**The name of the rdf:datatype attribute.*/
	public final static String ATTRIBUTE_DATATYPE="datatype";

		//RDF XML attribute values
	/**The parse type for a collection.*/
	public final static String COLLECTION_PARSE_TYPE="Collection";
	/**The parse type for an XML literal.*/
	public final static String LITERAL_PARSE_TYPE="Literal";
	/**The parse type for a resource.*/
	public final static String RESOURCE_PARSE_TYPE="Resource";

}