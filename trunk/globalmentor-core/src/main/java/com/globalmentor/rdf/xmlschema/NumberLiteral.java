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

package com.globalmentor.rdf.xmlschema;

import java.net.URI;

import static com.globalmentor.java.Objects.*;

import com.globalmentor.rdf.*;

/**
 * An RDF literal that represents an abstract number object.
 * @param <T> The type of object the literal represents.
 * @author Garret Wilson
 * @see Number
 */
public abstract class NumberLiteral<T extends Number> extends RDFTypedLiteral<T> {

	/**
	 * Constructs a typed number literal.
	 * @param literalValue The literal value that the lexical form represents.
	 * @param literalDatatypeURI The reference URI identifying the datatype of this literal.
	 * @throws NullPointerException if the value and/or the datatype URI is <code>null</code>.
	 */
	public NumberLiteral(final T literalValue, final URI literalDatatypeURI) {
		super(literalValue, literalDatatypeURI); //construct the parent class with the number
	}

	/**
	 * Determines if the RDF object is a number literal and, if so, casts the object to a number literal and returns it.
	 * @param rdfObject The RDF object in question.
	 * @return The RDF object as a number literal, or <code>null</code> if the object is not a number literal or the object is <code>null</code>.
	 */
	public static NumberLiteral asNumberLiteral(final RDFObject rdfObject) {
		return asInstance(rdfObject, NumberLiteral.class); //cast the object to a number literal if we can
	}

}