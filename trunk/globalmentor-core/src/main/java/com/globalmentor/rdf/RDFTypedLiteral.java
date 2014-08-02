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

import java.net.URI;

import static com.globalmentor.java.Objects.*;

/**The base class for all RDF typed literals.
@param <T> The type of object the literal represents.
@author Garret Wilson
*/
public class RDFTypedLiteral<T> extends RDFLiteral
{

	/**The literal value the lexical form represents.*/
	private T value;

		/**@return The literal value represented by the lexical form.*/
		public T getValue() {return value;}

	/**The reference URI identifying the datatype of this literal.*/
	private final URI datatypeURI;

		/**The reference URI identifying the datatype of this literal.*/
		public final URI getDatatypeURI() {return datatypeURI;}

	/**Returns the lexical form of the literal.
	By default this version returns the string version of the stored object.
	@return The lexical form, a Unicode string in Normal Form C.
	@see #getValue()
	*/
	public String getLexicalForm() {return getValue().toString();}

	/**Constructs a typed literal.
	@param literalValue The literal value that the lexical form represents.
	@param literalDatatypeURI The reference URI identifying the datatype of this literal.
	@throws NullPointerException if the value and/or the datatype URI is <code>null</code>.
	*/
	public RDFTypedLiteral(final T literalValue, final URI literalDatatypeURI)
	{
		value=checkInstance(literalValue, "Literal value cannot be null.");	//save the value
		datatypeURI=checkInstance(literalDatatypeURI, "Literal datatype cannot be null.");	//save the datatype URI
	}

	/**If <code>object</code> is another {@link RDFTypedLiteral}, compares the datatype URI and value objects.
	@param object The object with which to compare this typed literal.
	@return <code>true<code> if this literal equals that specified in <code>object</code>.
	@see #getValue()
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof RDFTypedLiteral)	//if we're being compared with another typed literal
		{
			final RDFTypedLiteral<?> typedLiteral=(RDFTypedLiteral<?>)object;	//cast the object to a typed literal
			return getDatatypeURI().equals(typedLiteral.getDatatypeURI())	//compare datatype URIs
					&& getValue().equals(typedLiteral.getValue()); //compare values
		}
		else	//if we're being compared with anything else
		{
			return false;	//the objects aren't equal
		}
	}

}