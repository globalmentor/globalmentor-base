package com.garretwilson.rdf.maqro;

import java.net.URI;

import com.garretwilson.rdf.RDFUtilities;

/**Constants used in MAQRO processing.
@author Garret Wilson
*/
public interface MAQROConstants
{

	/**The recommended prefix to the MAQRO ontology namespace.*/
	public final static String MAQRO_NAMESPACE_PREFIX="maqro";
	/**The URI to the MAQRO namespace.*/
	public final static URI MAQRO_NAMESPACE_URI=URI.create("http://maqro.org/namespaces/2003/maqro#");

	/**The recommended prefix to the MAQRO ontology scoring algorithm namespace.*/
	public final static String MAQRO_SCORING_NAMESPACE_PREFIX="maqroScoring";
	/**The URI to the MAQRO scoring namespace.*/
	public final static URI MAQRO_SCORING_NAMESPACE_URI=URI.create("http://maqro.org/namespaces/2003/maqro/algorithms/scoring#");

	/**The recommended prefix to the MAQRO ontology selection algorithm namespace.*/
	public final static String MAQRO_SELECTION_NAMESPACE_PREFIX="maqroSelection";
	/**The URI to the MAQRO selection namespace.*/
	public final static URI MAQRO_SELECTION_NAMESPACE_URI=URI.create("http://maqro.org/namespaces/2003/maqro/algorithms/selection#");

		//predefined selection algorithms
	/**The local name of the <code>maqroSelection:random</code> resource.*/
	public final static String RANDOM_RESOURCE_NAME="random";
	/**The URI of the <code>maqroSelection:random</code> resource.*/
	public final static URI RANDOM_RESOURCE_URI=RDFUtilities.createReferenceURI(MAQRO_SELECTION_NAMESPACE_URI, RANDOM_RESOURCE_NAME);
	/**The local name of the <code>maqroSelection:sequential</code> resource.*/
	public final static String SEQUENTIAL_RESOURCE_NAME="sequential";
	/**The URI of the <code>maqroSelection:sequential</code> resource.*/
	public final static URI SEQUENTIAL_RESOURCE_URI=RDFUtilities.createReferenceURI(MAQRO_SELECTION_NAMESPACE_URI, SEQUENTIAL_RESOURCE_NAME);

		//MAQRO ontology class names
	/**The local name of maqro:Activity.*/
	public final static String ACTIVITY_CLASS_NAME="Activity";
	/**The local name of maqro:Question.*/
	public final static String QUESTION_CLASS_NAME="Question";
	/**The local name of maqro:SelectionCriteria.*/
	public final static String SELECTION_CRITERIA_CLASS_NAME="SelectionCriteria";

		//MAQRO ontology property names
	/**A choice of an interaction. The local name of maqro:category.*/
	public final static String CATEGORY_PROPERTY_NAME="category";
	/**A choice of a question. The local name of maqro:choice.*/
	public final static String CHOICE_PROPERTY_NAME="choice";
	/**The number of choices to select. The local name of maqro:choiceCount.*/
	public final static String CHOICE_COUNT_PROPERTY_NAME="choiceCount";
	/**The type of response a question expects. The local name of maqro:expect.*/
	public final static String EXPECT_PROPERTY_NAME="expect";
	/**The number of interactions to select. The local name of maqro:interactionCount.*/
	public final static String INTERACTION_COUNT_PROPERTY_NAME="interactionCount";
	/**A interactions of an activity. The local name of maqro:interactions.*/
	public final static String INTERACTIONS_PROPERTY_NAME="interactions";
	/**The order in which to present interactions. The local name of maqro:order.*/
	public final static String ORDER_PROPERTY_NAME="order";
	/**The query of a question. The local name of maqro:query.*/
	public final static String QUERY_PROPERTY_NAME="query";
	/**The selection criteria of an activity. The local name of maqro:selection.*/
	public final static String SELECTION_PROPERTY_NAME="selection";

}