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
	/**The local name of maqro:MAQROActivity.*/
	public final static String ACTIVITY_TYPE_NAME="Activity";
	/**The local name of maqro:Question.*/
	public final static String QUESTION_TYPE_NAME="Question";

		//MAQRO ontology property names
	/**The query of a question. The local name of maqro:query.*/
	public final static String QUERY_PROPERTY_NAME="query";
	/**A choice of an interaction. The local name of maqro:category.*/
	public final static String CATEGORY_PROPERTY_NAME="category";
	/**A choice of a question. The local name of maqro:choice.*/
	public final static String CHOICE_PROPERTY_NAME="choice";
	/**The number of choices to choose. The local name of maqro:choiceCount.*/
	public final static String CHOICE_COUNT_PROPERTY_NAME="choiceCount";
	/**The type of response a question expects. The local name of maqro:expect.*/
	public final static String EXPECT_PROPERTY_NAME="expect";
	/**The number of interactions to choose. The local name of maqro:interactionCount.*/
	public final static String INTERACTION_COUNT_PROPERTY_NAME="interactionCount";
	/**A interactions of an activity. The local name of maqro:interactions.*/
	public final static String INTERACTIONS_PROPERTY_NAME="interactions";

}