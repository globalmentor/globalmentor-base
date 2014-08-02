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

package com.globalmentor.rdf;

import java.text.Collator;
import java.util.Locale;

import static com.globalmentor.java.Objects.*;

import com.globalmentor.model.LocaledText;

/**Represents a plain RDF literal.
@author Garret Wilson
*/
public class RDFPlainLiteral extends RDFLiteral
{

	/**The lexical form, a Unicode string in Normal Form C.*/
	private final String lexicalForm;

		/**@return The lexical form, a Unicode string in Normal Form C.*/
		public String getLexicalForm() {return lexicalForm;}

	/**The language of the plain literal, or <code>null</code> if there is no
		language specified.
	*/
	private Locale language;

		/**The language of the plain literal, or <code>null</code> if there is no
			language specified.
		*/
		public Locale getLanguage() {return language;}

	/**@return A locale-aware representation of the literal's lexical form, indicating any locale information available.*/
	public LocaledText toLocaleText()
	{
		return new LocaledText(getLexicalForm(), getLanguage());	//return the text and the language 
	}

	/**Constructs a plain literal with a lexical value.
	@param lexicalValue The lexical form of the literal.
	@throws NullPointerException if the given lexical value is <code>null</code>.
	*/
	public RDFPlainLiteral(final String lexicalValue)
	{
		this(lexicalValue, null);	//construct a plain literal with no language specified
	}

	/**Constructs a plain literal with a lexical value and a language.
	@param lexicalValue The lexical form of the literal.
	@param languageLocale A locale representing the language, or <code>null</code> if no language should be specified.
	@throws NullPointerException if the given lexical value is <code>null</code>.
	*/
	public RDFPlainLiteral(final String lexicalValue, final Locale languageLocale)
	{
		lexicalForm=checkInstance(lexicalValue, "Lexical value cannot be null."); //set the lexical form
		language=languageLocale;	//set the language locale
	}

//TODO update hashCode() to work take into account the language

	/**Returns an initialized collator appropriate for comparing this literal to
		another.
	<p>This version returns a language-specific collator if both literals specify
		languages and both languages are the same. Otherwise, the default collator
		for the default locale is returned. This is done to ensure that all
		non-same-language literals will get sorted the same way, regardless of
		order of comparison.</p>
	@return A collator appropriate for comparing this literal to the given literal.
	@see #initializeCollator(Collator)
	*/  
	protected Collator getCollator(final RDFLiteral literal)
	{
		final Locale language1=getLanguage();	//get our language
		if(language1!=null)	//if we have a language
		{
			if(literal instanceof RDFPlainLiteral)	//if both literals are plain literals
			{
				final RDFPlainLiteral plainLiteral=(RDFPlainLiteral)literal;	//cast the literal to a plain literal
				final Locale language2=plainLiteral.getLanguage();	//get the language of the plain literal
				if(language1.equals(language2))	//if we and the other literal have the same languages, we'll use a collator for that language
				{
					return initializeCollator(Collator.getInstance(language1));	//get a collator using whatever locale we're in and initialize it
				}
			}
		}
		return super.getCollator(literal);	//if the other literal isn't a plain literal, or it has a different language, return the default collator
	}

}