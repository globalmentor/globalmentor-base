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

package com.globalmentor.rdf.dicto;

import java.net.URI;

import com.globalmentor.rdf.*;
import static com.globalmentor.rdf.dicto.Dicto.*;

/**Class representing a word that can be part of a Dictionary Ontology
	(Dicto) dictionary.
@author Garret Wilson
*/
public class Word extends Entry
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return DICTO_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return WORD_CLASS_NAME;}

	/**Default constructor.*/
	protected Word()
	{
		super();	//construct the parent class
	}
	
	/**Constructs a word with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	protected Word(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The part of speech of the word, or <code>null</code> if there is
		no part of speech.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getSpeechPart()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, SPEECH_PART_PROPERTY_NAME);	//get the part of speech		
	}

	/**@return The gender of the word, or <code>null</code> if there is
		no gender.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getGender()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, GENDER_PROPERTY_NAME);	//get the gender		
	}
}
