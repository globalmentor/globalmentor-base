package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;

import org.w3c.dom.*;

import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.*;
import com.garretwilson.text.xml.XMLUtilities;
import static com.garretwilson.text.xml.XMLUtilities.*;
import static com.garretwilson.text.xml.xhtml.XHTMLConstants.*;

/**Creates an XHTML tree representing MAQRO resources.
@author Garret Wilson
*/
public class MAQROXHTMLifier
{

	/**The DOM implementation for generating XML.*/
//G***del	private final DOMImplementation domImplementation;

		/**@return The DOM implementation for generating XML.*/
//G***del		protected DOMImplementation getDOMImplementation() {return domImplementation;}

	/**Default constructor.*/
	public MAQROXHTMLifier()
	{
	}

	/**Creates an XHTML element representing the given outcome.
	@param document The document to serve as an element factory.
	@param outcome The outcome to represent in XHTML.
	@return An XHTML element representing the outcome.
	*/
	public Element createElement(final Document document, final Outcome outcome)
	{
		final Element element=document.createElementNS(XHTML_NAMESPACE_URI.toString(), ELEMENT_DIV);	//<div>
		constructElement(element, outcome);	//construc the element
		return element;	//return the element representing the outcome
	}

	/**Constructs an XHTML element representing the given outcome.
	@param element The element to represent the outcome.
	@param outcome The outcome to represent in XHTML.
	@return An XHTML element representing the outcome.
	*/
	protected void constructElement(final Element element, final Outcome outcome)
	{
		final Document document=element.getOwnerDocument();	//get the document to use an an element factory
		final Element dlElement=document.createElementNS(XHTML_NAMESPACE_URI.toString(), ELEMENT_DL);	//<dl>
		element.appendChild(dlElement);	//add the definition list
		final Iterator<RDFObject> resultIterator=outcome.getResultIterator();	//get an iterator to results
		while(resultIterator.hasNext())	//while there are more results
		{
			final RDFObject object=resultIterator.next();	//get the next result
			if(object instanceof Result)	//if this is a result object
			{
				final Result result=(Result)object;	//cast the object to a result
				dlElement.appendChild(XMLUtilities.createElement(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DT, "Result"));	//TODO i18n
				dlElement.appendChild(XMLUtilities.createElement(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DD, getResultString(result)));	//show the result
			}
		}
		final Interaction interaction=outcome.getInteraction();	//get the interaction associated with the outcome
		dlElement.appendChild(XMLUtilities.createElement(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DT, "Interaction"));	//TODO i18n
		final Element interactionElement=document.createElementNS(XHTML_NAMESPACE_URI.toString(), ELEMENT_DD);	//create an element for the interaction
		dlElement.appendChild(interactionElement);	//add the interaction element to the 
		constructElement(interactionElement, outcome, interaction);
		final Iterator<RDFObject> responseIterator=outcome.getResponseIterator();	//get an iterator to responses
		while(responseIterator.hasNext())	//while there are more responses
		{
			final RDFObject object=responseIterator.next();	//get the next response
			if(object instanceof Dialogue)	//if this is dialogue
			{
				final Dialogue dialogue=(Dialogue)object;	//cast the object to dialogue
				dlElement.appendChild(XMLUtilities.createElement(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DT, "Response"));	//TODO i18n
				dlElement.appendChild(XMLUtilities.createElement(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DD, getDialogueString(dialogue)));	//show the result
			}
		}		
	}

	/**Constructs an XHTML element representing the given interaction.
	@param element The element to hold the interaction information
	@param outcome The outcome for this interaction.
	@param interaction The interaction to represent in XHTML.
	*/
	protected void constructElement(final Element element, final Outcome outcome, final Interaction interaction)
	{
		final Document document=element.getOwnerDocument();	//get the document to use an an element factory
		if(interaction instanceof Group)	//if this is a group
		{
				//TODO indicate this is an assessment
			final Element olElement=document.createElementNS(XHTML_NAMESPACE_URI.toString(), ELEMENT_OL);	//<ol>
			element.appendChild(olElement);	//add the list
			final Group group=(Group)interaction;	//get the interaction as a group
			for(final RDFResource resource:group.getInteractions())	//look at all the interactions in the group
			{
				if(resource instanceof Interaction)	//if this is an interaction
				{
					final Interaction subInteraction=(Interaction)resource;	//get the resource as an interaction
					final Outcome subOutcome=outcome.getOutcome(subInteraction);	//get the matching
					if(subOutcome!=null)	//if there is an outcome for this interaction
					{
						final Element liElement=document.createElementNS(XHTML_NAMESPACE_URI.toString(), ELEMENT_LI);	//<li>
						olElement.appendChild(liElement);	//add the list item
						constructElement(liElement, subOutcome);	//construct the element for this outcome
					}
				}				
			}
		}
		else if(interaction instanceof Question)	//if this is a question
		{
				//TODO indicate this is a question
			final Element dlElement=element.getOwnerDocument().createElementNS(XHTML_NAMESPACE_URI.toString(), ELEMENT_DL);	//<dl>
			element.appendChild(dlElement);	//add the definition list
			final Question question=(Question)interaction;	//get the interaction as a question
			final Dialogue query=question.getQuery();	//get the query, if any
			if(query!=null)
			{
				dlElement.appendChild(XMLUtilities.createElement(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DT, "Query"));	//TODO i18n
				dlElement.appendChild(XMLUtilities.createElement(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DD, getDialogueString(query)));	//show the query				
			}
				//TODO fix for AND answers and OR answers
			final Iterator<RDFObject> answerIterator=question.getAnswerIterator();	//get an iterator to answers
			while(answerIterator.hasNext())	//while there are more answers
			{
				final RDFObject object=answerIterator.next();	//get the next answer
				if(object instanceof Dialogue)	//if this is dialogue
				{
					final Dialogue dialogue=(Dialogue)object;	//cast the object to dialogue
					dlElement.appendChild(XMLUtilities.createElement(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DT, "Answer"));	//TODO i18n
					dlElement.appendChild(XMLUtilities.createElement(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DD, getDialogueString(dialogue)));	//show the result
				}
			}		
		}
	}

	/**Creates a string to represent the given result.
	@param result The result to represent.
	@return A string representation of the result.
	*/
	protected String getResultString(final Result result)
	{
		final StringBuilder stringBuilder=new StringBuilder();
		if(result instanceof Score)	//if this is a score
		{
			final Score score=(Score)result;	//get the result as a score
			stringBuilder.append("Score: ");	//TODO i18n
			final NumberLiteral value=score.getValue();	//get the value of the score
			if(value!=null)	//if there is a value
			{
				stringBuilder.append(value.getNumber());	//append the value
			}
			stringBuilder.append('/');	//'/'
			final NumberLiteral possible=score.getPossible();	//get the possible of the score
			if(possible!=null)	//if there is a possible value
			{
				stringBuilder.append(possible.getNumber());	//append the possible value
			}
		}
		return stringBuilder.toString();	//return the string we constructed
	}

	/**Creates a string to represent the given dialogue.
	@param dialogue The dialogue to represent.
	@return A string representation of the dialogue.
	*/
	protected String getDialogueString(final Dialogue dialogue)	//TODO somewhere along the way, literal XML character entities get escaped; fix
	{
		final StringBuilder stringBuilder=new StringBuilder();
		final RDFLiteral literalValue=dialogue.getValue();	//get the literal value, if there is one
		return literalValue!=null ? literalValue.getLexicalForm() : "";	//return the literal value if there is one
	}


}