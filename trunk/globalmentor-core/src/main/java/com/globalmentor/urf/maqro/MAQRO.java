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

import java.net.URI;

import com.globalmentor.net.ContentType;

import static com.globalmentor.urf.URF.*;

/**Constants and utilities used in MAQRO processing.
@author Garret Wilson
*/
public class MAQRO
{

	/**The URI to the MAQRO namespace.*/
	public final static URI MAQRO_NAMESPACE_URI=URI.create("http://maqro.org/maqro/");
	/**The URI to the MAQRO interactivity namespace.*/
	public final static URI MAQRO_INTERACT_NAMESPACE_URI=URI.create("http://maqro.org/interact/");

	/**The MIME content type for mentoring activities.*/
	public final static ContentType MENTOR_ACTIVITY_CONTENT_TYPE=ContentType.getInstance(ContentType.APPLICATION_PRIMARY_TYPE, "x-mentor-activity+turf");
	/**An extension for mentoring activity resource names.*/
	public final static String MENTOR_ACTIVITY_NAME_EXTENSION="mact";
	/**The MIME content type for mentoring interactions.*/
	public final static ContentType MENTOR_INTERACTION_CONTENT_TYPE=ContentType.getInstance(ContentType.APPLICATION_PRIMARY_TYPE, "x-mentor-interaction+turf");
	/**An extension for mentoring interaction resource names.*/
	public final static String MENTOR_INTERACTION_NAME_EXTENSION="mint";

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
	/**A category of an interaction. The local name of maqro.category.*/
	public final static String CATEGORY_PROPERTY_NAME="category";

		//MAQRO activity behavior property names
			//permissions
	/**Whether hints are allowed.*/
	public final static URI ALLOW_HINT_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "allowHint");
	/**Whether navigating to the previous interaction is allowed.*/
	public final static URI ALLOW_PREVIOUS_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "allowPrevious");
	/**Whether the activity can be canceled.*/
	public final static URI ALLOW_CANCEL_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "allowCancel");
	/**Whether the activity can be submitted.*/
	public final static URI ALLOW_SUBMIT_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "allowSubmit");
			//process
	/**Whether each response commit should be confirmed.*/
	public final static URI CONFIRM_COMMIT_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "confirmCommit");
	/**Whether submission of the activity should be confirmed.*/
	public final static URI CONFIRM_SUBMIT_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "confirmSubmit");
	/**Whether a response is required for all applicable interactions.*/
	public final static URI REQUIRE_RESPONSE_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "requireResponse");
	/**The maximum amount of time for the activity, in milliseconds.*/
	public final static URI MAX_TIME_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "maxTime");
			//feedback
	/**Whether the result of each interaction should be immediately shown.*/
	public final static URI REPORT_EACH_RESULT_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "reportEachResult");
	/**Whether the final result of the activity should be shown.*/
	public final static URI REPORT_FINAL_RESULT_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "reportFinalResult");
	/**Whether the current result should continuously be shown.*/
	public final static URI REPORT_RESULT_PROGRESS_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "reportResultProgress");
	/**Whether the position within the activity should be continuously shown.*/
	public final static URI REPORT_PROGRESS_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "reportProgress");
	/**Whether the current time used and/or available should be shown.*/
	public final static URI REPORT_TIME_PROPERTY_URI=createResourceURI(MAQRO_INTERACT_NAMESPACE_URI, "reportTime");

		//MAQRO question property names
	/**The answer of a question. The local name of maqro.answer.*/
	public final static String ANSWER_PROPERTY_NAME="answer";
	/**The answer of a question.*/
	public final static URI ANSWER_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, ANSWER_PROPERTY_NAME);
	/**A choice of a question. The local name of maqro.choice.*/
	public final static String CHOICE_PROPERTY_NAME="choice";
	/**A choice of a question.*/
	public final static URI CHOICE_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, CHOICE_PROPERTY_NAME);
	/**The type of response a question expects. The local name of maqro.expectation.*/
	public final static String EXPECTATION_PROPERTY_NAME="expectation";
	/**The type of response a question expects.*/
	public final static URI EXPECTATION_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, EXPECTATION_PROPERTY_NAME);
	/**An explanation of a question. The local name of maqro.explanation.*/
	public final static String EXPLANATION_PROPERTY_NAME="explanation";
	/**An explanation of a question.*/
	public final static URI EXPLANATION_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, EXPLANATION_PROPERTY_NAME);
	/**A hint of a question. The local name of maqro.hint.*/
	public final static String HINT_PROPERTY_NAME="hint";
	/**A hint of a question.*/
	public final static URI HINT_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, HINT_PROPERTY_NAME);
	/**The minimum number of responses to accept. The local name of maqro.minResponseCount.*/
	public final static String MIN_RESPONSE_COUNT_PROPERTY_NAME="minResponseCount";
	/**The maximum number of responses to accept. The local name of maqro.maxResponseCount.*/
	public final static String MAX_RESPONSE_COUNT_PROPERTY_NAME="maxResponseCount";
	/**The maximum number of responses to accept.*/
	public final static URI MAX_RESPONSE_COUNT_URI=createResourceURI(MAQRO_NAMESPACE_URI, MAX_RESPONSE_COUNT_PROPERTY_NAME);
	/**The query of a question. The local name of maqro.query.*/
	public final static String QUERY_PROPERTY_NAME="query";
	/**The query of a question.*/
	public final static URI QUERY_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, QUERY_PROPERTY_NAME);

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
	/**The number of interactions to select.*/
	public final static URI COUNT_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, COUNT_PROPERTY_NAME);;
	/**The order criteria of an activity. The local name of maqro.order.*/
	public final static String ORDER_PROPERTY_NAME="order";
	/**The order criteria of an activity. The local name of maqro.order.*/
	public final static URI ORDER_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, ORDER_PROPERTY_NAME);

		//MAQRO outcome class names
	/**The local name of maqro.Score.*/
	public final static String SCORE_CLASS_NAME="Score";
		//MAQRO outcome property names
	/**The actual score. The local name of maqro.actual.*/
	public final static String ACTUAL_PROPERTY_NAME="actual";
	/**The actual score.*/
	public final static URI ACTUAL_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, ACTUAL_PROPERTY_NAME);
	/**Whether the result is correct. The local name of maqro.correct.*/
	public final static String CORRECT_PROPERTY_NAME="correct";
	/**Whether the result is correct.*/
	public final static URI CORRECT_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, CORRECT_PROPERTY_NAME);
	/**The outcomes of a group's interactions. The local name of maqro.outcomes.*/
	public final static String OUTCOMES_PROPERTY_NAME="outcomes";
	/**The outcomes of a group's interactions.*/
	public final static URI OUTCOMES_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, OUTCOMES_PROPERTY_NAME);
	/**The possible score. The local name of maqro.possible.*/
	public final static String POSSIBLE_PROPERTY_NAME="possible";
	/**The possible score.*/
	public final static URI POSSIBLE_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, POSSIBLE_PROPERTY_NAME);
	/**A response to an interaction. The local name of maqro.response.*/
	public final static String RESPONSE_PROPERTY_NAME="response";
	/**A response to an interaction.*/
	public final static URI RESPONSE_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, RESPONSE_PROPERTY_NAME);
	/**A result of an outcome evaluation. The local name of maqro.result.*/
	public final static String RESULT_PROPERTY_NAME="result";
	/**A result of an outcome evaluation.*/
	public final static URI RESULT_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, RESULT_PROPERTY_NAME);

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
	/**A followup interaction.*/
	public final static URI FOLLOWUP_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, FOLLOWUP_PROPERTY_NAME);
	/**Identifies an ordered list of followup interactions. The local name of maqro.followups.*/
	public final static String FOLLOWUPS_PROPERTY_NAME="followups";
	/**Identifies an interaction. The local name of maqro.interaction.*/
	public final static String INTERACTION_PROPERTY_NAME="interaction";
	/**Identifies an interaction.*/
	public final static URI INTERACTION_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, INTERACTION_PROPERTY_NAME);
	/**The interactions of an activity. The local name of maqro.interactions.*/
	public final static String INTERACTIONS_PROPERTY_NAME="interactions";
	/**The interactions of a group.*/
	public final static URI INTERACTIONS_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, INTERACTIONS_PROPERTY_NAME);
	/**The introduction of an activity.*/
	public final static String INTRODUCTION_PROPERTY_NAME="introduction";
	/**The introduction of an activity.*/
	public final static URI INTRODUCTION_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, INTRODUCTION_PROPERTY_NAME);
	/**The selection criteria of a group. The local name of maqro.selection.*/
	public final static String SELECTION_PROPERTY_NAME="selection";
	/**The selection criteria of a group.*/
	public final static URI SELECTION_PROPERTY_URI=createResourceURI(MAQRO_NAMESPACE_URI, SELECTION_PROPERTY_NAME);
	/**A supplement to an interaction or part of a question. The local name of maqro.supplement.*/
	public final static String SUPPLEMENT_PROPERTY_NAME="supplement";

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