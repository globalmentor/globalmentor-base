package com.garretwilson.rdf.maqro;

import java.net.URI;
import com.garretwilson.rdf.*;

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
//G***fix	public final static String MAQRO_SCORING_NAMESPACE_PREFIX="maqroScoring";
	/**The URI to the MAQRO scoring namespace.*/
//G***fix	public final static URI MAQRO_SCORING_NAMESPACE_URI=URI.create("http://maqro.org/namespaces/2003/maqro/algorithms/scoring#");

	/**The recommended prefix to the MAQRO ontology selection algorithm namespace.*/
//G***fix	public final static String MAQRO_SELECTION_NAMESPACE_PREFIX="maqroSelection";
	/**The URI to the MAQRO selection namespace.*/
//G***fix	public final static URI MAQRO_SELECTION_NAMESPACE_URI=URI.create("http://maqro.org/namespaces/2003/maqro/algorithms/selection#");

		//predefined selection algorithms
	/**The local name of the <code>maqroSelection:random</code> resource.*/
//G***fix	public final static String RANDOM_RESOURCE_NAME="random";
	/**The URI of the <code>maqroSelection:random</code> resource.*/
//G***fix	public final static URI RANDOM_RESOURCE_URI=RDFUtilities.createReferenceURI(MAQRO_SELECTION_NAMESPACE_URI, RANDOM_RESOURCE_NAME);
	/**The local name of the <code>maqroSelection:sequential</code> resource.*/
//G***fix	public final static String SEQUENTIAL_RESOURCE_NAME="sequential";
	/**The URI of the <code>maqroSelection:sequential</code> resource.*/
//G***fix	public final static URI SEQUENTIAL_RESOURCE_URI=RDFUtilities.createReferenceURI(MAQRO_SELECTION_NAMESPACE_URI, SEQUENTIAL_RESOURCE_NAME);

		//MAQRO ontology class names
	/**The local name of maqro:Activity.*/
	public final static String ACTIVITY_CLASS_NAME="Activity";
	/**The local name of maqro:Group.*/
	public final static String GROUP_CLASS_NAME="Group";
	/**The local name of maqro:OrderDescription.*/
	public final static String ORDER_DESCRIPTION_CLASS_NAME="OrderDescription";
	/**The local name of maqro:Question.*/
	public final static String QUESTION_CLASS_NAME="Question";
	/**The local name of maqro:SelectDescription.*/
	public final static String SELECT_DESCRIPTION_CLASS_NAME="SelectDescription";

		//MAQRO interaction property names
	/**A choice of an interaction. The local name of maqro:category.*/
	public final static String CATEGORY_PROPERTY_NAME="category";

		//MAQRO interact description property names
	/**Whether real-time results should be displayed. The local name of maqro:showResultsProgress.*/
	public final static String SHOW_RESULTS_PROGRESS_PROPERTY_NAME="showResultsProgress";

		//MAQRO question property names
	/**A choice of a question. The local name of maqro:choice.*/
	public final static String CHOICE_PROPERTY_NAME="choice";
	/**The type of response a question expects. The local name of maqro:expect.*/
	public final static String EXPECT_PROPERTY_NAME="expect";
	/**The minimum number of responses to accept. The local name of maqro:minResponseCount.*/
	public final static String MIN_RESPONSE_COUNT_PROPERTY_NAME="minResponseCount";
	/**The maximum number of responses to accept. The local name of maqro:maxResponseCount.*/
	public final static String MAX_RESPONSE_COUNT_PROPERTY_NAME="maxResponseCount";
	/**The query of a question. The local name of maqro:query.*/
	public final static String QUERY_PROPERTY_NAME="query";

		//MAQRO ontology property names
	/**The number of choices to select. The local name of maqro:choiceCount.*/
	public final static String CHOICE_COUNT_PROPERTY_NAME="choiceCount";
	/**The number of interactions to select. The local name of maqro:questionCount.*/
	public final static String QUESTION_COUNT_PROPERTY_NAME="questionCount";
	/**A interactions of an activity. The local name of maqro:interactions.*/
	public final static String INTERACTIONS_PROPERTY_NAME="interactions";
	/**The order criteria of an activity. The local name of maqro:order.*/
	public final static String ORDER_PROPERTY_NAME="order";
	/**Whether something should be random. The local name of maqro:random.*/
	public final static String RANDOM_PROPERTY_NAME="random";
	/**The selection criteria of an activity. The local name of maqro:select.*/
	public final static String SELECT_PROPERTY_NAME="select";


	/**The constant category that indicates no specified category.*/
	public final static RDFPlainLiteral NO_CATEGORY=new RDFPlainLiteral("No category specified");	//G***i18n 

}