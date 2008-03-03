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

/**Class representing an entry that can be part of a Dictionary Ontology
	(Dicto) dictionary.
@author Garret Wilson
*/
public abstract class Entry extends TypedRDFResource
{

	/**Default constructor.*/
	protected Entry()
	{
	}
	
	/**Constructs an entry with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	protected Entry(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The definition of the entry, or <code>null</code> if there is
		no definition.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getDefinition()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, DEFINITION_PROPERTY_NAME);	//get the definition		
	}

	/**@return The orthography of the entry, or <code>null</code> if there is
		no orthography.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getOrthography()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, ORTHOGRAPHY_PROPERTY_NAME);	//get the orthography		
	}

	/**@return The pronunciation of the entry, or <code>null</code> if there is
		no pronunciation.
	*/
	public RDFObject getPronunciation()
	{
		return getPropertyValue(DICTO_NAMESPACE_URI, PRONUNCIATION_PROPERTY_NAME);	//get the pronunciation		
	}

	/**@return An iterable to pronunciations, if any, of the entry.*/
	public Iterable<RDFObject> getPronunciations()
	{
		return getPropertyValues(DICTO_NAMESPACE_URI, PRONUNCIATION_PROPERTY_NAME); //return an iterable to the pronunciation properties
	}

	/**@return The translation of the entry, or <code>null</code> if there is
		no translation.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getTranslation()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, TRANSLATION_PROPERTY_NAME);	//get the translation		
	}

	/**@return The transliteration of the entry, or <code>null</code> if there is
		no transliteration.
	@exception ClassCastException Thrown if the property object is not a plain
		literal.
	*/
	public RDFPlainLiteral getTransliteration()
	{
		return (RDFPlainLiteral)getPropertyValue(DICTO_NAMESPACE_URI, TRANSLITERATION_PROPERTY_NAME);	//get the transliteration		
	}

}
