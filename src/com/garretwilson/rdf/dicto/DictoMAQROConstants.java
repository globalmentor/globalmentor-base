package com.garretwilson.rdf.dicto;

import java.net.URI;

/**Constants used in Dictionary Ontology (Dicto) MAQRO processing.
@author Garret Wilson
*/
public interface DictoMAQROConstants
{

	/**The recommended prefix to the Dicto MAQRO ontology namespace.*/
	public final static String DICTO_MAQRO_NAMESPACE_PREFIX="dictomaqro";
	/**The URI to the Dicto MAQRO namespace.*/
	public final static URI DICTO_MAQRO_NAMESPACE_URI=URI.create("http://garretwilson.com/namespaces/2004/dictomaqro#");

		//Dicto MAQRO property names
	/**The property to be used when forming MAQRO question queries. The local name of dictomaqro:queryProperty.*/
	public final static String QUERY_PROPERTY_PROPERTY_NAME="queryProperty";
	/**The property to be used when forming MAQRO question choices. The local name of dictomaqro:choiceProperty.*/
	public final static String CHOICE_PROPERTY_PROPERTY_NAME="choiceProperty";

}