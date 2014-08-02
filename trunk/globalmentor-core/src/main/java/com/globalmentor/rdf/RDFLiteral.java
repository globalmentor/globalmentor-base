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

import com.globalmentor.model.LocaledText;

/**Represents an RDF literal, either plain or typed.
@author Garret Wilson
*/
public abstract class RDFLiteral implements RDFObject, Comparable<RDFLiteral>
{

	/**@return The lexical form, a Unicode string in Normal Form C.*/
	public abstract String getLexicalForm();

	/**@return A hashcode value composed from the value.*/
	public int hashCode()
	{
		return getLexicalForm().hashCode();  //return the hash code of the value
	}

	/**Determines whether the given object is another literal with the same lexical form.
	@param object The object with which to compare this literal.
	@return <code>true<code> if this literal equals that specified in <code>object</code>.
	@see #getLexicalForm()
	*/
	public boolean equals(Object object)
	{
		if(object instanceof RDFLiteral)	//if we're being compared with another literal
		{
			return getLexicalForm().equals(((RDFLiteral)object).getLexicalForm()); //compare values
		}
		else	//if we're being compared with anything else
		{
			return false;	//the objects aren't equal
		}
	}

	/**@return A string representation of the literal's lexical form.*/
	public String toString()
	{
		return getLexicalForm();  //return the lexical form
	}

	/**@return A locale-aware representation of the literal's lexical form, indicating any locale information available.*/
	public LocaledText toLocaleText()
	{
		return new LocaledText(getLexicalForm());	//by default we don't know any locale 
	}

	/**Initializes a collator for comparison.
	@param collator The collator to initialize.
	@return The initialized collator.
	*/
	protected static Collator initializeCollator(final Collator collator)
	{
		collator.setStrength(Collator.PRIMARY);	//sort according to primary differences---ignore accents and case differences
		collator.setDecomposition(Collator.FULL_DECOMPOSITION);	//fully decompose Unicode characters to get the best comparison
		return collator;	//return the initialized collator
	}

	/**Returns an initialized collator appropriate for comparing this literal to
		another.
	<p>This version returns the default collator for the default locale.</p>
	@return A collator appropriate for comparing this literal to the given literal.
	@see #initializeCollator(Collator)
	*/  
	protected Collator getCollator(final RDFLiteral literal)
	{
		return initializeCollator(Collator.getInstance());	//get a collator using whatever locale we're in and initialize it
	}

	/**Compares this object to another object.
	<p>This version determines order based the lexical form of the literal,
		using the appropriate collator returned by
		<code>getCollator(RDFLiteral)</code>.</p>
	@param literal The object with which to compare the object. This must be
		another <code>RDFLiteral</code> object.
	@return A negative integer, zero, or a positive integer if the first resource
		lexical form is less than, equal to, or greater than the lexical form of
		the second literal, respectively.
	@throws ClassCastException Thrown if the specified object's type is not
		<code>RDFLiteral</code>.
	@see #getCollator(RDFLiteral)
	*/
	public int compareTo(final RDFLiteral literal) throws ClassCastException
	{
		final Collator collator=getCollator(literal);	//get a collator with which to compare the literals
		return collator.compare(getLexicalForm(), literal.getLexicalForm());	//compare the lexical forms of the two literals 
	}

}