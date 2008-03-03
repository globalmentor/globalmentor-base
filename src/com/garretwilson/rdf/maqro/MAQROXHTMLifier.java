package com.garretwilson.rdf.maqro;

import org.w3c.dom.*;

import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xmlschema.*;
import com.globalmentor.text.xml.XML;

import static com.globalmentor.text.xml.xhtml.XHTML.*;

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
	The element is not added to the documet.
	@param document The document to serve as an element factory.
	@param outcome The outcome to represent in XHTML.
	@return An XHTML element representing the outcome.
	*/
	public Element createElement(final Document document, final Outcome outcome)
	{
		final Element element=document.createElementNS(XHTML_NAMESPACE_URI.toString(), ELEMENT_DIV);	//<div>
		constructElement(element, outcome);	//construct the element
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
		for(final RDFObject object:outcome.getResults())	//for each result
		{
			if(object instanceof Result)	//if this is a result object
			{
				final Result result=(Result)object;	//cast the object to a result
				dlElement.appendChild(XML.createElementNS(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DT, "Result"));	//TODO i18n
				dlElement.appendChild(XML.createElementNS(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DD, getResultString(result)));	//show the result
			}
		}
		final Interaction interaction=outcome.getInteraction();	//get the interaction associated with the outcome
		dlElement.appendChild(XML.createElementNS(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DT, "Interaction"));	//TODO i18n
		final Element interactionElement=document.createElementNS(XHTML_NAMESPACE_URI.toString(), ELEMENT_DD);	//create an element for the interaction
		dlElement.appendChild(interactionElement);	//add the interaction element to the 
		constructElement(interactionElement, outcome, interaction);
		for(final RDFObject object:outcome.getResponses())	//for each response
		{
			if(object instanceof Dialogue)	//if this is dialogue
			{
				final Dialogue dialogue=(Dialogue)object;	//cast the object to dialogue
				dlElement.appendChild(XML.createElementNS(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DT, "Response"));	//TODO i18n
				final Element ddElement=XML.appendElementNS(dlElement, XHTML_NAMESPACE_URI.toString(), ELEMENT_DD);	//append an element for dialogue
				constructElement(ddElement, dialogue);	//show the result
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
			for(final RDFObject object:group.getInteractions())	//look at all the interactions in the group
			{
				if(object instanceof Interaction)	//if this is an interaction
				{
					final Interaction subInteraction=(Interaction)object;	//get the resource as an interaction
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
				dlElement.appendChild(XML.createElementNS(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DT, "Query"));	//TODO i18n
				final Element ddElement=XML.appendElementNS(dlElement, XHTML_NAMESPACE_URI.toString(), ELEMENT_DD);	//append an element for dialogue
				constructElement(ddElement, query);	//show the query
			}
				//TODO fix for AND answers and OR answers
			for(final RDFObject object:question.getAnswers())	//for each answer
			{
				if(object instanceof Dialogue)	//if this is dialogue
				{
					final Dialogue dialogue=(Dialogue)object;	//cast the object to dialogue
					dlElement.appendChild(XML.createElementNS(document, XHTML_NAMESPACE_URI.toString(), ELEMENT_DT, "Answer"));	//TODO i18n
					final Element ddElement=XML.appendElementNS(dlElement, XHTML_NAMESPACE_URI.toString(), ELEMENT_DD);	//append an element for dialogue
					constructElement(ddElement, dialogue);	//show the result
				}
			}		
		}
	}

	/**Constructs an XHTML element representing dialogue.
	Literal dialogue information will be placed as as text child of the element.
	XML literal dialogue information will be imported into the element hierarchy.
	@param element The element to hold the dialogue information
	@param dialogue The dialogue information to represent in XHTML.
	*/
	protected void constructElement(final Element element, final Dialogue dialogue)
	{
		final RDFLiteral dialogueValue=dialogue.getValue();	//get the value of this dialogue
		if(dialogueValue instanceof RDFPlainLiteral)	//if the dialogue is a plain literal
		{
			XML.appendText(element, dialogueValue.getLexicalForm());	//append the literal text to the element
		}
		else if(dialogueValue instanceof RDFXMLLiteral)	//if the dialogue is an XML literal
		{
			final RDFXMLLiteral xmlLiteralDialogueValue=(RDFXMLLiteral)dialogueValue;
			final DocumentFragment documentFragment=xmlLiteralDialogueValue.getValue();	//get the document fragment of the literal
			XML.appendImportedChildNodes(element, documentFragment, true);	//import the document fragment children into the element
		}
/*TODO fix
		else	//if we don't understand the type of dialogue value given (i.e. it's not a plain literal or an XML literal)
		{
			throw new IllegalArgumentException("Unknown dialogue literal type for "+dialogueValue);
		}
*/	
	}
	
	/**Creates a string to represent the given result.
	@param result The result to represent.
	@return A string representation of the result.
	*/
	protected String getResultString(final Result result)	//TODO refactor and harmonize with the MAQROActivityEngine version
	{
		final StringBuilder stringBuilder=new StringBuilder();
		if(result instanceof Score)	//if this is a score
		{
			final Score score=(Score)result;	//get the result as a score
			stringBuilder.append("Score: ");	//TODO i18n
			final NumberLiteral value=score.getValue();	//get the value of the score
			if(value!=null)	//if there is a value
			{
				stringBuilder.append(value.getValue());	//append the value
			}
			stringBuilder.append('/');	//'/'
			final NumberLiteral possible=score.getPossible();	//get the possible of the score
			if(possible!=null)	//if there is a possible value
			{
				stringBuilder.append(possible.getValue());	//append the possible value
			}
		}
		return stringBuilder.toString();	//return the string we constructed
	}

	/**Creates a string to represent the given dialogue.
	@param dialogue The dialogue to represent.
	@return A string representation of the dialogue.
	*/
/*TODO del when new Dialogue element construction method works
	protected String getDialogueString(final Dialogue dialogue)	//TODO somewhere along the way, literal XML character entities get escaped; fix
	{
		final StringBuilder stringBuilder=new StringBuilder();
		final RDFLiteral literalValue=dialogue.getValue();	//get the literal value, if there is one
		return literalValue!=null ? literalValue.getLexicalForm() : "";	//return the literal value if there is one
	}
*/

}
