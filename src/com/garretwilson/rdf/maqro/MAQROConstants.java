package com.garretwilson.rdf.maqro;

import java.net.URI;

/**Constants used in MAQRO processing.
@author Garret Wilson
*/
public interface MAQROConstants
{

	/**The recommended prefix to the MAQRO ontology namespace.*/
	public final static String MAQRO_NAMESPACE_PREFIX="maqro";
	/**The URI to the MAQRO namespace.*/
	public final static URI MAQRO_NAMESPACE_URI=URI.create("http://maqro.org/namespaces/2003/maqro#");

		//MAQRO ontology type names
	/**The local name of maqro:Activity.*/
	public final static String ACTIVITY_TYPE_NAME="Activity";
	/**The local name of maqro:Question.*/
	public final static String QUESTION_TYPE_NAME="Question";

		//MAQRO ontology property names
	/**The query of a question. The local name of maqro:query.*/
	public final static String QUERY_PROPERTY_NAME="query";
	/**A choice of a question. The local name of maqro:choice.*/
	public final static String CHOICE_PROPERTY_NAME="choice";

}