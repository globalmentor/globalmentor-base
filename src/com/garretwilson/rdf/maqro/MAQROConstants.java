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
	/**The local name of maqro:Dialogue.*/
	public final static String DIALOGUE_CLASS_NAME="Dialogue";
	/**The local name of maqro:Group.*/
	public final static String GROUP_CLASS_NAME="Group";
	/**The local name of maqro:OrderDescription.*/
	public final static String ORDER_DESCRIPTION_CLASS_NAME="OrderDescription";
	/**The local name of maqro:Question.*/
	public final static String QUESTION_CLASS_NAME="Question";
	/**The local name of maqro:Result.*/
	public final static String RESULT_CLASS_NAME="Result";
	/**The local name of maqro:SelectDescription.*/
	public final static String SELECT_DESCRIPTION_CLASS_NAME="SelectDescription";

		//MAQRO interaction property names
	/**A choice of an interaction. The local name of maqro:category.*/
	public final static String CATEGORY_PROPERTY_NAME="category";

		//MAQRO activity behavior property names
			//permissions
	/**Whether hints are allowed. The local name of maqro:allowHint.*/
	public final static String ALLOW_HINT_PROPERTY_NAME="allowHint";
	/**Whether navigating to the previous interaction is allowed. The local name of maqro:allowPrevious.*/
	public final static String ALLOW_PREVIOUS_PROPERTY_NAME="allowPrevious";
	/**Whether the activity can be canceled. The local name of maqro:allowCancel.*/
	public final static String ALLOW_CANCEL_PROPERTY_NAME="allowCancel";
			//process
	/**Whether each response commit should be confirmed. The local name of maqro:confirmCommit.*/
	public final static String CONFIRM_COMMIT_PROPERTY_NAME="confirmCommit";
	/**Whether submission of the activity should be confirmed. The local name of maqro:confirmSubmit.*/
	public final static String CONFIRM_SUBMIT_PROPERTY_NAME="confirmSubmit";
	/**Whether a response is required for all applicable interactions. The local name of maqro:requireResponse.*/
	public final static String REQUIRE_RESPONSE_PROPERTY_NAME="requireResponse";
	/**The maximum amount of time for the activity, in milliseconds. The local name of maqro:maxTime.*/
	public final static String MAX_TIME_PROPERTY_NAME="maxTime";
			//feedback
	/**Whether the result of each interaction should be immediately shown. The local name of maqro:showResult.*/
	public final static String SHOW_EACH_RESULT_PROPERTY_NAME="showEachResult";
	/**Whether the final result of the activity should be shown. The local name of maqro:showFinalResult.*/
	public final static String SHOW_FINAL_RESULT_PROPERTY_NAME="showFinalResult";
	/**Whether the current result should continuously be shown. The local name of maqro:showResultProgress.*/
	public final static String SHOW_RESULT_PROGRESS_PROPERTY_NAME="showResultProgress";
	/**Whether the position within the activity should be continuously shown. The local name of maqro:showProgress.*/
	public final static String SHOW_PROGRESS_PROPERTY_NAME="showProgress";
	/**Whether the current time used and/or available should be shown. The local name of maqro:showTime.*/
	public final static String SHOW_TIME_PROPERTY_NAME="showTime";

		//MAQRO question property names
	/**The answer of a question. The local name of maqro:answer.*/
	public final static String ANSWER_PROPERTY_NAME="answer";
	/**A choice of a question. The local name of maqro:choices.*/
	public final static String CHOICES_PROPERTY_NAME="choices";
	/**The type of response a question expects. The local name of maqro:expectation.*/
	public final static String EXPECTATION_PROPERTY_NAME="expectation";
	/**An explanation of a question. The local name of maqro:explanation.*/
	public final static String EXPLANATION_PROPERTY_NAME="explanation";
	/**The minimum number of responses to accept. The local name of maqro:minResponseCount.*/
	public final static String MIN_RESPONSE_COUNT_PROPERTY_NAME="minResponseCount";
	/**The maximum number of responses to accept. The local name of maqro:maxResponseCount.*/
	public final static String MAX_RESPONSE_COUNT_PROPERTY_NAME="maxResponseCount";
	/**The query of a question. The local name of maqro:query.*/
	public final static String QUERY_PROPERTY_NAME="query";

		//MAQRO result property names
	/**Whether the result is correct. The local name of maqro:correct.*/
	public final static String CORRECT_PROPERTY_NAME="correct";
	/**A response to an interaction. The local name of maqro:response.*/
	public final static String RESPONSE_PROPERTY_NAME="response";

		//MAQRO ontology property names
	/**The number of choices to select. The local name of maqro:choiceCount.*/
	public final static String CHOICE_COUNT_PROPERTY_NAME="choiceCount";
	/**The number of interactions to select. The local name of maqro:questionCount.*/
	public final static String QUESTION_COUNT_PROPERTY_NAME="questionCount";
	/**Identifies an interaction. The local name of maqro:interaction.*/
	public final static String INTERACTION_PROPERTY_NAME="interaction";
	/**The interactions of an activity. The local name of maqro:interactions.*/
	public final static String INTERACTIONS_PROPERTY_NAME="interactions";
	/**The order criteria of an activity. The local name of maqro:order.*/
	public final static String ORDER_PROPERTY_NAME="order";
	/**Whether something should be random. The local name of maqro:random.*/
	public final static String RANDOM_PROPERTY_NAME="random";
	/**The selection criteria of an activity. The local name of maqro:select.*/
	public final static String SELECT_PROPERTY_NAME="select";
	/**A supplement to an interaction or part of a question. The local name of maqro:supplement.*/
	public final static String SUPPLEMENT_PROPERTY_NAME="supplement";

	/**The constant category that indicates no specified category.*/
	public final static RDFPlainLiteral NO_CATEGORY=new RDFPlainLiteral("No category specified");	//G***i18n 

}