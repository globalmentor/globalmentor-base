/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.swing.text.rdf.maqro;

import static com.globalmentor.rdf.RDFResources.*;
import static com.globalmentor.swing.text.rdf.RDFStyles.*;
import static com.globalmentor.urf.maqro.MAQRO.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import javax.swing.text.*;

import com.globalmentor.net.ContentType;
import com.globalmentor.rdf.*;
import com.globalmentor.swing.text.xml.*;
import com.globalmentor.urf.maqro.*;

/**An element kit for creating element specs for MAQRO.
@author Garret Wilson
*/
public class MAQROXMLElementKit implements XMLEditorKit.XMLElementKit
{
	
	/**The default kit for creating element specs from XML elements.*/
	private final XMLEditorKit.DefaultXMLElementKit defaultXMLElementKit;

		/**@return The default kit for creating element specs from XML elements.*/
		protected XMLEditorKit.DefaultXMLElementKit getDefaultXMLElementKit() {return defaultXMLElementKit;}

	/**Constructs a MAQRO element kit with a fallback default XML element kit.
	@param defaultXMLElementKit The default XML element kit to use when this element kit does not know how to create an element spec.
	*/
	public MAQROXMLElementKit(final XMLEditorKit.DefaultXMLElementKit defaultXMLElementKit)
	{
		this.defaultXMLElementKit=defaultXMLElementKit;	//save the default XML element kit
	}

	/**Appends information from an XML element tree into a list of element specs.
	@param elementSpecList The list of element specs to be inserted into the document.
	@param xmlElement The XML element tree.
	@param baseURI The base URI of the document, used for generating full target
		URIs for quick searching.
	@return The attribute set used to represent the element; this attribute set
		can be manipulated after the method returns.
	*/
	public MutableAttributeSet appendElementSpecList(final List<DefaultStyledDocument.ElementSpec> elementSpecList, final org.w3c.dom.Element xmlElement, final URI baseURI)
	{
		final RDF rdf=new RDF();	//create new RDF
//TODO convert to URF		rdf.registerResourceFactory(MAQRO_NAMESPACE_URI, new MAQRO());  //register a factory for MAQRO resource classes TODO make this more efficient and use a common factory
		final RDFXMLProcessor rdfProcessor=new RDFXMLProcessor(rdf); //create a new RDF processor
		try
		{
			final String localName=xmlElement.getLocalName();	//get the local name of the element
			if(ACTIVITY_CLASS_NAME.equals(localName))	//if this is an activity
			{
				final Activity activity=(Activity)rdfProcessor.processRDFResource(xmlElement);	//process the activity				
				return appendElementSpecList(elementSpecList, activity, baseURI);	//append element specs for the activity
			}
			//TODO fix for other MAQRO classes
			else	//if we don't recognize the MAQRO class
			{
				return getDefaultXMLElementKit().appendElementSpecList(elementSpecList, xmlElement, baseURI);	//append a default spec list
			}
		}
		catch(URISyntaxException e)
		{
			throw new AssertionError(e);	//TODO fix with real error handling
		}  
	}

	/**Creates an attribute set for the given RDF resource.
	The attribute set will be given an XML namespace and local name corresponding to the type of the given resource.
	@param resource The RDF resource.
	@return An attribute set reflecting the resource.
	*/
	protected MutableAttributeSet createAttributeSet(final RDFResource resource, final URI baseURI)
	{
		final RDFResource type=getType(resource);	//get the resource type
		final URI namespaceURI=type!=null ? getNamespaceURI(type.getURI()) : null;	//get the namespace of the resource
		final String localName=type!=null ? getLocalName(type.getURI()) : null;	//get the local name of the resource
		final MutableAttributeSet attributeSet=namespaceURI!=null && localName!=null	//if we could find both a type namespace and a type local name
				? getDefaultXMLElementKit().createAttributeSet(namespaceURI, localName)	//create an attribute set with the given namespace and local name
				: new SimpleAttributeSet();	//if type information isn't available, create a new default attribute set for this element
		setRDFResource(attributeSet, resource);	//store the resource in the attribute set
		return attributeSet;	//return the attribute set we created
	}
	
	/**Appends element spec objects from MAQRO activity content data.
	@param elementSpecList The list of element specs to be inserted into the document.
	@param contentData The MAQRO activity content to be inserted into the document.
	@param swingXMLDocument The Swing document into which the content will be set.
	@return The attribute set for the MAQRO activity.
	*/
	protected MutableAttributeSet appendMAQROActivityElementSpecList(final List<DefaultStyledDocument.ElementSpec> elementSpecList, final XMLEditorKit.ContentData<? extends Activity> contentData, final XMLDocument swingXMLDocument)
	{
		final Activity activity=contentData.getObject();	//get a reference to this activity
		final URI baseURI=contentData.getBaseURI(); //get a reference to the base URI
		final ContentType mediaType=contentData.getContentType(); //get a reference to the media type
		return appendElementSpecList(elementSpecList, activity, baseURI);	//append the activity
	}

	/**Appends information from a MAQRO activity to a list of element specs.
	@param elementSpecList The list of element specs to be inserted into the document.
	@param activity The MAQRO activity.
	@param baseURI The base URI of the RDF data model.
	@return The attribute set used to represent the resource.
	@exception BadLocationException for an invalid starting offset
	*/
	public MutableAttributeSet appendElementSpecList(final List<DefaultStyledDocument.ElementSpec> elementSpecList, final Activity activity, final URI baseURI)
	{
		final MutableAttributeSet attributeSet=new SimpleAttributeSet();
/*TODO fix
		final MutableAttributeSet attributeSet=createAttributeSet(activity, baseURI);	//create and fill an attribute set based upon the RDF resource
		elementSpecList.add(new DefaultStyledDocument.ElementSpec(attributeSet, DefaultStyledDocument.ElementSpec.StartTagType));	//create the beginning of a Swing element to model this resource
		final List<RDFObject> interactions=activity.getInteractions();	//get the activity interactions
		if(interactions!=null)	//if there are choices
		{
			for(final RDFObject interaction:interactions)	//for each interaction
			{
				if(interaction instanceof Question)	//if this interaction is a question
				{
					appendElementSpecList(elementSpecList, (Question)interaction, baseURI);	//append element specs for the interaction
				}
				else
				{
					throw new IllegalArgumentException("Non-question interaction not supported");
				}
			}
		}
		else
		{
			throw new IllegalArgumentException("Activity without interactions not supported");
		}
//TODO fix if(!"null".equals(xmlElement.getLocalName()))	//TODO testing
		elementSpecList.add(new DefaultStyledDocument.ElementSpec(attributeSet, DefaultStyledDocument.ElementSpec.EndTagType));	//finish the element we started at the beginning of this function
*/
		return attributeSet;  //return the attribute set used for the element
	}

	/**Appends information from a MAQRO question to a list of element specs.
	The children of the element will be any choices the question may have.
	@param elementSpecList The list of element specs to be inserted into the document.
	@param question The MAQRO question.
	@param baseURI The base URI of the RDF data model.
	@return The attribute set used to represent the resource.
	@exception BadLocationException for an invalid starting offset
	*/
	protected MutableAttributeSet appendElementSpecList(final List<DefaultStyledDocument.ElementSpec> elementSpecList, final Question question, final URI baseURI)
	{
		final MutableAttributeSet attributeSet=new SimpleAttributeSet();
/*TODO fix
		final MutableAttributeSet attributeSet=createAttributeSet(question, baseURI);	//create and fill an attribute set based upon the RDF resource
		elementSpecList.add(new DefaultStyledDocument.ElementSpec(attributeSet, DefaultStyledDocument.ElementSpec.StartTagType));	//create the beginning of a Swing element to model this resource
		final Dialogue query=question.getQuery();	//get the question query
		if(query!=null)	//if the question has a query
		{
			appendElementSpecList(elementSpecList, query, baseURI);	//append element specs for the query dialogue
		}
		final List<RDFObject> choices=question.getChoices();	//get the question choices
		if(choices!=null)	//if there are choices
		{
			final MutableAttributeSet choicesAttributeSet=getDefaultXMLElementKit().createAttributeSet(MAQRO_NAMESPACE_URI, CHOICE_PROPERTY_NAME);	//create and fill an attribute set for the choices property
			elementSpecList.add(new DefaultStyledDocument.ElementSpec(choicesAttributeSet, DefaultStyledDocument.ElementSpec.StartTagType));	//create the beginning of a Swing element to model this resource
			for(final RDFObject choice:choices)	//for each choice
			{
				if(choice instanceof Dialogue)	//if this choice is dialogue
				{
					appendElementSpecList(elementSpecList, (Dialogue)choice, baseURI);	//append element specs for the dialogue
				}
				else
				{
					throw new IllegalArgumentException("Non-dialogue choices not supported");
				}
			}
			elementSpecList.add(new DefaultStyledDocument.ElementSpec(choicesAttributeSet, DefaultStyledDocument.ElementSpec.EndTagType));	//finish the element we started at the beginning of this function
			
		}
*/
/*TODO del; add support for other response expectations
		else
		{
			throw new IllegalArgumentException("Questions without choices not supported");
		}
*/
//TODO fix		elementSpecList.add(new DefaultStyledDocument.ElementSpec(attributeSet, DefaultStyledDocument.ElementSpec.EndTagType));	//finish the element we started at the beginning of this function
		return attributeSet;  //return the attribute set used for the element
	}

	/**Appends information from MAQRO dialogue to a list of element specs.
	The child element will be the literal value of the dialogue.
	@param elementSpecList The list of element specs to be inserted into the document.
	@param dialogue The MAQRO dialogue.
	@param baseURI The base URI of the RDF data model.
	@return The attribute set used to represent the resource.
	@exception BadLocationException for an invalid starting offset
	*/
/*TODO fix
	protected MutableAttributeSet appendElementSpecList(final List<DefaultStyledDocument.ElementSpec> elementSpecList, final Dialogue dialogue, final URI baseURI)
	{
		final MutableAttributeSet attributeSet=createAttributeSet(dialogue, baseURI);	//create and fill an attribute set based upon the RDF resource
		elementSpecList.add(new DefaultStyledDocument.ElementSpec(attributeSet, DefaultStyledDocument.ElementSpec.StartTagType));	//create the beginning of a Swing element to model this resource
		final RDFLiteral dialogueValue=dialogue.getValue();	//get the value of this dialogue
		if(dialogueValue instanceof RDFPlainLiteral)	//if the dialogue is a plain literal
		{
			final String text=dialogueValue!=null ? dialogueValue.getLexicalForm() : "X";	//TODO use an object replacement character TODO fix for empty strings and remove redundant null check
			getDefaultXMLElementKit().appendElementSpecListContent(elementSpecList, null, null, baseURI, text);	//append the dialogue literal text
		}
		else if(dialogueValue instanceof RDFXMLLiteral)	//if the dialogue is an XML literal
		{
			final RDFXMLLiteral xmlLiteralDialogueValue=(RDFXMLLiteral)dialogueValue;
			getDefaultXMLElementKit().appendElementSpecListContent(elementSpecList, xmlLiteralDialogueValue.getValue(), attributeSet, baseURI);	//
		}
		else	//if we don't understand the type of dialogue value given (i.e. it's not a plain literal or an XML literal)
		{
			throw new IllegalArgumentException("Unknown dialogue literal type for "+dialogueValue);
		}
		elementSpecList.add(new DefaultStyledDocument.ElementSpec(attributeSet, DefaultStyledDocument.ElementSpec.EndTagType));	//finish the element we started at the beginning of this function
		return attributeSet;  //return the attribute set used for the element
	}
*/

}
