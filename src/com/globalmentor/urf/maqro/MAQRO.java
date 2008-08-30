/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.urf.maqro;

import static com.globalmentor.java.Java.getVariableName;
import static com.globalmentor.urf.URF.createResourceURI;

import java.net.URI;
import java.util.*;

import com.globalmentor.rdf.*;

/**Constants and utilities used in MAQRO processing.
@author Garret Wilson
*/
public class MAQRO
{

	/**The recommended prefix to the MAQRO ontology namespace.*/
//TODO del	public final static String MAQRO_NAMESPACE_PREFIX="maqro";
	/**The URI to the MAQRO namespace.*/
	public final static URI MAQRO_NAMESPACE_URI=URI.create("http://maqro.org/maqro");

	/**The recommended prefix to the MAQRO ontology scoring algorithm namespace.*/
//TODO fix	public final static String MAQRO_SCORING_NAMESPACE_PREFIX="maqroScoring";
	/**The URI to the MAQRO scoring namespace.*/
//TODO fix	public final static URI MAQRO_SCORING_NAMESPACE_URI=URI.create("http://maqro.org/namespaces/2003/maqro/algorithms/scoring#");

	/**The recommended prefix to the MAQRO ontology selection algorithm namespace.*/
//TODO fix	public final static String MAQRO_SELECTION_NAMESPACE_PREFIX="maqroSelection";
	/**The URI to the MAQRO selection namespace.*/
//TODO fix	public final static URI MAQRO_SELECTION_NAMESPACE_URI=URI.create("http://maqro.org/namespaces/2003/maqro/algorithms/selection#");

		//predefined selection algorithms
	/**The local name of the <code>maqroSelection:random</code> resource.*/
//TODO fix	public final static String RANDOM_RESOURCE_NAME="random";
	/**The URI of the <code>maqroSelection:random</code> resource.*/
//TODO fix	public final static URI RANDOM_RESOURCE_URI=RDFUtilities.createReferenceURI(MAQRO_SELECTION_NAMESPACE_URI, RANDOM_RESOURCE_NAME);
	/**The local name of the <code>maqroSelection:sequential</code> resource.*/
//TODO fix	public final static String SEQUENTIAL_RESOURCE_NAME="sequential";
	/**The URI of the <code>maqroSelection:sequential</code> resource.*/
//TODO fix	public final static URI SEQUENTIAL_RESOURCE_URI=RDFUtilities.createReferenceURI(MAQRO_SELECTION_NAMESPACE_URI, SEQUENTIAL_RESOURCE_NAME);

		//MAQRO ontology class names
	/**The local name of maqro.Activity.*/
	public final static String ACTIVITY_CLASS_NAME="Activity";
	/**The local name of maqro.Dialogue.*/
	public final static String DIALOGUE_CLASS_NAME="Dialogue";
	/**The local name of maqro.FollowupEvaluation.*/
	public final static String FOLLOWUP_EVALUATION_CLASS_NAME="FollowupEvaluation";
	/**The local name of maqro.Group.*/
	public final static String GROUP_CLASS_NAME="Group";
	/**The local name of maqro.Question.*/
	public final static String QUESTION_CLASS_NAME="Question";
	/**The local name of maqro.Outcome.*/
	public final static String OUTCOME_CLASS_NAME="Outcome";
	/**The local name of maqro.Selection.*/
	public final static String SELECTION_CLASS_NAME="Selection";
	/**The local name of maqro.Text.*/
//TODO del	public final static String TEXT_CLASS_NAME="Text";	//TODO add to spec

		//MAQRO interaction property names
	/**A choice of an interaction. The local name of maqro.category.*/
	public final static String CATEGORY_PROPERTY_NAME="category";

		//MAQRO activity behavior property names
			//permissions
	/**Whether hints are allowed. The local name of maqro.allowHint.*/
	public final static String ALLOW_HINT_PROPERTY_NAME="allowHint";
	/**Whether navigating to the previous interaction is allowed. The local name of maqro.allowPrevious.*/
	public final static String ALLOW_PREVIOUS_PROPERTY_NAME="allowPrevious";
	/**Whether the activity can be canceled. The local name of maqro.allowCancel.*/
	public final static String ALLOW_CANCEL_PROPERTY_NAME="allowCancel";
	/**Whether the activity can be submitted. The local name of maqro.allowSubmit.*/
	public final static String ALLOW_SUBMIT_PROPERTY_NAME="allowSubmit";
			//process
	/**Whether each response commit should be confirmed. The local name of maqro.confirmCommit.*/
	public final static String CONFIRM_COMMIT_PROPERTY_NAME="confirmCommit";
	/**Whether submission of the activity should be confirmed. The local name of maqro.confirmSubmit.*/
	public final static String CONFIRM_SUBMIT_PROPERTY_NAME="confirmSubmit";
	/**Whether a response is required for all applicable interactions. The local name of maqro.requireResponse.*/
	public final static String REQUIRE_RESPONSE_PROPERTY_NAME="requireResponse";
	/**The maximum amount of time for the activity, in milliseconds. The local name of maqro.maxTime.*/
	public final static String MAX_TIME_PROPERTY_NAME="maxTime";
			//feedback
	/**Whether the result of each interaction should be immediately shown. The local name of maqro.showResult.*/
	public final static String SHOW_EACH_RESULT_PROPERTY_NAME="showEachResult";
	/**Whether the final result of the activity should be shown. The local name of maqro.showFinalResult.*/
	public final static String SHOW_FINAL_RESULT_PROPERTY_NAME="showFinalResult";
	/**Whether the current result should continuously be shown. The local name of maqro.showResultProgress.*/
	public final static String SHOW_RESULT_PROGRESS_PROPERTY_NAME="showResultProgress";
	/**Whether the position within the activity should be continuously shown. The local name of maqro.showProgress.*/
	public final static String SHOW_PROGRESS_PROPERTY_NAME="showProgress";
	/**Whether the current time used and/or available should be shown. The local name of maqro.showTime.*/
	public final static String SHOW_TIME_PROPERTY_NAME="showTime";

		//MAQRO question property names
	/**The answer of a question. The local name of maqro.answer.*/
	public final static String ANSWER_PROPERTY_NAME="answer";
	/**A choice of a question. The local name of maqro.choice.*/
	public final static String CHOICE_PROPERTY_NAME="choice";
	/**The type of response a question expects. The local name of maqro.expectation.*/
	public final static String EXPECTATION_PROPERTY_NAME="expectation";
	/**An explanation of a question. The local name of maqro.explanation.*/
	public final static String EXPLANATION_PROPERTY_NAME="explanation";
	/**A hint of a question. The local name of maqro.hint.*/
	public final static String HINT_PROPERTY_NAME="hint";
	/**The minimum number of responses to accept. The local name of maqro.minResponseCount.*/
	public final static String MIN_RESPONSE_COUNT_PROPERTY_NAME="minResponseCount";
	/**The maximum number of responses to accept. The local name of maqro.maxResponseCount.*/
	public final static String MAX_RESPONSE_COUNT_PROPERTY_NAME="maxResponseCount";
	/**The query of a question. The local name of maqro.query.*/
	public final static String QUERY_PROPERTY_NAME="query";

		//MAQRO selection class names
	/**The local name of maqro.CategoryFilter.*/
	public final static String CATEGORY_FILTER_CLASS_NAME="CategoryFilter";
	/**The local name of maqro.InteractionTypeFilter.*/
	public final static String INTERACTION_TYPE_FILTER_CLASS_NAME="InteractionTypeFilter";
	/**The local name of maqro.Selector.*/
	public final static String SELECTOR_CLASS_NAME="Selector";
	/**The local name of maqro.RandomOrder.*/
	public final static String RANDOM_ORDER_CLASS_NAME="RandomOrder";
	/**The local name of maqro.RandomSelection.*/
	public final static String RANDOM_SELECTION_CLASS_NAME="RandomSelection";
	/**The local name of maqro.SequentialOrder.*/
	public final static String SEQUENTIAL_ORDER_CLASS_NAME="SequentialOrder";
	/**The local name of maqro.SequentialSelection.*/
	public final static String SEQUENTIAL_SELECTION_CLASS_NAME="SequentialSelection";
		//MAQRO selection property names
	/**The filters of a selector. The local name of maqro.filters.*/
	public final static String FILTERS_PROPERTY_NAME="filters";
	/**The selectors of a selection. The local name of maqro.selectors.*/
	public final static String SELECTORS_PROPERTY_NAME="selectors";
	/**The number of interactions to select. The local name of maqro.count.*/
	public final static String COUNT_PROPERTY_NAME="count";
	/**The order criteria of an activity. The local name of maqro.order.*/
	public final static String ORDER_PROPERTY_NAME="order";

		//MAQRO outcome class names
	/**The local name of maqro.Score.*/
	public final static String SCORE_CLASS_NAME="Score";
		//MAQRO outcome property names
	/**The actual score. The local name of maqro.actual.*/
	public final static String ACTUAL_PROPERTY_NAME="actual";
	/**Whether the result is correct. The local name of maqro.correct.*/
	public final static String CORRECT_PROPERTY_NAME="correct";
	/**The outcomes of a group's interactions. The local name of maqro.outcomes.*/
	public final static String OUTCOMES_PROPERTY_NAME="outcomes";
	/**The possible score. The local name of maqro.possible.*/
	public final static String POSSIBLE_PROPERTY_NAME="possible";
	/**A response to an interaction. The local name of maqro.response.*/
	public final static String RESPONSE_PROPERTY_NAME="response";
	/**A result of an outcome evaluation. The local name of maqro.result.*/
	public final static String RESULT_PROPERTY_NAME="result";

		//MAQRO ontology properties
	/**The number of choices to select. The local name of maqro.choiceCount.*/
//TODO fix or del	public final static String CHOICE_COUNT_PROPERTY_NAME="choiceCount";
	/**The number of interactions to select. The local name of maqro.questionCount.*/
//TODO del if not needed	public final static String QUESTION_COUNT_PROPERTY_NAME="questionCount";
	/**Identifies a SPARQL condition. The local name of maqro.condition.*/
	public final static String CONDITION_PROPERTY_NAME="condition";
	/**Identifies an ordered list of evaluations. The local name of maqro.evaluations.*/
	public final static String EVALUATIONS_PROPERTY_NAME="evaluations";
	/**Identifies a followup interaction. The local name of maqro.followup.*/
	public final static String FOLLOWUP_PROPERTY_NAME="followup";
	/**Identifies an ordered list of followup interactions. The local name of maqro.followups.*/
	public final static String FOLLOWUPS_PROPERTY_NAME="followups";
	/**Identifies an interaction. The local name of maqro.interaction.*/
	public final static String INTERACTION_PROPERTY_NAME="interaction";
	/**The interactions of an activity. The local name of maqro.interactions.*/
	public final static String INTERACTIONS_PROPERTY_NAME="interactions";
	/**The selection criteria of an activity. The local name of maqro.selection.*/
	public final static String SELECTION_PROPERTY_NAME="selection";
	/**A supplement to an interaction or part of a question. The local name of maqro.supplement.*/
	public final static String SUPPLEMENT_PROPERTY_NAME="supplement";

	/**The actual score.*/
	public final static URI ACTUAL_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, ACTUAL_PROPERTY_NAME);
	/**The answer of a question.*/
	public final static URI ANSWER_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, ANSWER_PROPERTY_NAME);
	/**A choice of a question.*/
	public final static URI CHOICE_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, CHOICE_PROPERTY_NAME);
	/**Whether the result is correct.*/
	public final static URI CORRECT_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, CORRECT_PROPERTY_NAME);
	/**The number of interactions to select.*/
	public final static URI COUNT_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, COUNT_PROPERTY_NAME);;
	/**A hint of a question.*/
	public final static URI HINT_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, HINT_PROPERTY_NAME);
	/**Identifies an interaction.*/
	public final static URI INTERACTION_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, INTERACTION_PROPERTY_NAME);
	/**The interactions of a group.*/
	public final static URI INTERACTIONS_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME);
	/**An explanation of a question.*/
	public final static URI EXPLANATION_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, EXPLANATION_PROPERTY_NAME);
	/**The type of response a question expects.*/
	public final static URI EXPECTATION_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, EXPECTATION_PROPERTY_NAME);
	/**A followup interaction.*/
	public final static URI FOLLOWUP_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, FOLLOWUP_PROPERTY_NAME);
	/**The order criteria of an activity. The local name of maqro.order.*/
	public final static URI ORDER_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME);
	/**The outcomes of a group's interactions.*/
	public final static URI OUTCOMES_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, OUTCOMES_PROPERTY_NAME);
	/**The possible score.*/
	public final static URI POSSIBLE_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, POSSIBLE_PROPERTY_NAME);
	/**The query of a question. The local name of maqro.query.*/
	public final static URI QUERY_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, QUERY_PROPERTY_NAME);
	/**A response to an interaction.*/
	public final static URI RESPONSE_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, RESPONSE_PROPERTY_NAME);
	/**A result of an outcome evaluation.*/
	public final static URI RESULT_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, RESULT_PROPERTY_NAME);

	/**The constant category that indicates no specified category.*/
	public final static RDFPlainLiteral NO_CATEGORY=new RDFPlainLiteral("No category specified");	//TODO i18n

	//MAQRO representation names
	/**The name for the yes/no representation boolean. The fragement identifier of <code>http://maqro.org/namespaces/representation#booleanYesNo</code>.*/
//TODO fix	public final static String BASE64_BINARY_DATATYPE_NAME="base64Binary";

	/**The resource that indicates random selection order should be used.
	<p>This class will return this same resource for the correct URI whenever
		this class is used as a resource factory.</p>
	*/
//TODO fix	public final static RDFResource RANDOM_SELECTION_ORDER_RESOURCE=new DefaultRDFResource(MAQRO_SELECTION_NAMESPACE_URI, RANDOM_RESOURCE_NAME);

	/**The resource that indicates sequential selection order should be used.
	<p>This class will return this same resource for the correct URI whenever
		this class is used as a resource factory.</p>
	*/
//TODO fix	public final static RDFResource SEQUENTIAL_SELECTION_ORDER_RESOURCE=new DefaultRDFResource(MAQRO_SELECTION_NAMESPACE_URI, SEQUENTIAL_RESOURCE_NAME);

	/**Adds a <code>maqro.category</code> property to the resource.
	<p>If an equivalent property already exists, no action is taken.</p>
	@param resource The resource to which the category should be added.
	@param categoryLiteral A literal category value.
	*/
/*TODO fix or del
	public static void addCategory(final URFResource resource, final String categoryLiteral)
	{
		resource.addProperty(MAQRO_NAMESPACE_URI, CATEGORY_PROPERTY_NAME, categoryLiteral); //add a literal value to the resource as a category
	}
*/

	/**Adds a <code>maqro.category</code> property to the resource.
	<p>If an equivalent property already exists, no action is taken.</p>
	@param resource The resource to which the category should be added.
	@param category A string category value.
	@param language The language of the category, or <code>null</code> if
		no language should be specified.
	*/
	/*TODO fix or del
	public static void addCategory(final RDFResource resource, final String category, final Locale language)
	{
		resource.addProperty(MAQRO_NAMESPACE_URI, CATEGORY_PROPERTY_NAME, category, language); //add a literal value to the resource as a category
	}
*/

	/**Retrieves the category value of the resource. If this resource has more than
		one property of <code>maqro.category</code>, it is undefined which of these
		property values will be returned.
	@param resource The resource the category of which will be returned.
	@return The category of the resource, or <code>null</code> if there is no
		category or the category is not a literal.
	*/
	/*TODO fix or del
	public static RDFLiteral getCategory(final RDFResource resource)
	{
		return RDFResources.asLiteral(resource.getPropertyValue(MAQRO_NAMESPACE_URI, CATEGORY_PROPERTY_NAME)); //get the value of the category property as a literal
	}
*/

	/**Retrieves an iterable to all the <code>maqro.category</code> property values of the resource.
	@param resource The resource the categories of which will be returned.
	@return An iterable to all the categories of the resource.
	*/
	/*TODO fix or del
	public static Iterable<RDFObject> getCategories(final RDFResource resource)
	{
		return resource.getPropertyValues(MAQRO_NAMESPACE_URI, CATEGORY_PROPERTY_NAME); //return an iterable to the category properties
	}
*/

	/**Replaces all <code>maqro.category</code> properties of the resource with a
		new property with the given value.
	@param resource The resource for which the category properties should be replaced.
	@param category A string category value.
	@param language The language of the category, or <code>null</code> if
		no language should be specified.
	*/
	/*TODO fix or del
	public static void setCategory(final RDFResource resource, final String category, final Locale language)
	{
		resource.setProperty(MAQRO_NAMESPACE_URI, CATEGORY_PROPERTY_NAME, category, language); //replace all category properties with a literal category value
	}
*/

	/**Determines if the given resource has a category in the given category set.
	<p>If the category set contains <code>NO_CATEGORY</code>, this method returns
		<code>true</code> if the given resource has no categories specified.</p>
	@param resource The resource the categories of which should be checked.
	@param categorySet The set of categories, any category of which will allow
		the resource to be selected.
	@return <code>true</code> if the given resource has a category that is
		included in the category set.
	*/
	/*TODO fix or del
	public static boolean hasCategory(final RDFResource resource, final Set categorySet)
	{
		boolean hasCategories=false;	//we'll see if this resource has any categories at all
		final Iterator categoryIterator=getCategories(resource).iterator();	//get an iterator to the interaction's categories
		while(categoryIterator.hasNext())	//while this interaction has more categories
		{
			hasCategories=true;	//show that this resource has at least one category
			if(categorySet.contains(categoryIterator.next()))	//if this category is in the set
			{
				return true;	//show that this resource is in one of the given categories 
			}
		}
		if(!hasCategories && categorySet.contains(NO_CATEGORY))	//if this resource had not categories, and the category set accepts no categories
		{
			return true;	//show that this resource should be accepted, even though it has no categories
		}
		return false;	//show that the category is in none of the given categories
	}
*/

	/**Adds a supplement to a resource.
	@param resource The resource for which the supplement should be added.
	@param supplement A supplementary resource or literal.
	*/
	/*TODO fix or del
	public static void addSupplement(final RDFResource resource, final RDFObject supplement)
	{
		resource.addProperty(MAQRO_NAMESPACE_URI, SUPPLEMENT_PROPERTY_NAME, supplement);	//add the supplement
	}
*/

	/**Returns an iterable to all supplements.
	@param resource The resource for which the an iterable to supplements should be returned..
	@return An iterable to the supplements, if any, of the resource.
	*/
	/*TODO fix or del
	public static Iterable<RDFObject> getSupplements(final RDFResource resource)
	{
		return resource.getPropertyValues(MAQRO_NAMESPACE_URI, SUPPLEMENT_PROPERTY_NAME);	//return an iterable to the supplements 
	}
*/

	/**Removes all supplements from the resource.
	@param resource The resource from which all supplements should be removed.
	*/
	/*TODO fix or del
	public static void removeSupplements(final RDFResource resource)
	{
		resource.removeProperties(MAQRO_NAMESPACE_URI, SUPPLEMENT_PROPERTY_NAME);	//remove all supplements
	}
*/

}