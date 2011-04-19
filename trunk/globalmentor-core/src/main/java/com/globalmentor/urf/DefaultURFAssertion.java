/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf;

import static com.globalmentor.collections.Lists.*;
import static com.globalmentor.java.Objects.*;

import java.io.IOException;
import java.net.URI;
import java.util.List;

import com.globalmentor.java.Objects;
import com.globalmentor.model.NameValuePair;
import com.globalmentor.net.ReferenceResource;

/**
 * The default implementation of an URF assertion.
 * 
 * <p>
 * For efficiency the object will be either a value object (such as a primitive class instance, a string, or an immutable object instance) or a
 * {@link ReferenceResource}, the URI of which will indicate the referent object.
 * </p>
 * 
 * @author Garret Wilson
 */
public class DefaultURFAssertion implements URFAssertion	//TODO add specific support for Long, etc. and prevent Integer, etc.
{

	/** The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value. */
	private final List<NameValuePair<URI, Object>> scopeChain;

	/** @return The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value. */
	public List<NameValuePair<URI, Object>> getScopeChain()
	{
		return scopeChain;
	}

	/** The URI of the assertion subject. */
	private URI subjectURI;

	/** @return The URI of the assertion subject. */
	public URI getSubjectURI()
	{
		return subjectURI;
	}

	/** The URI of the assertion predicate. */
	private URI predicateURI;

	/** @return The URI of the assertion predicate. */
	public URI getPredicateURI()
	{
		return predicateURI;
	}

	/** The assertion object. */
	private Object object;

	/** @return The assertion object; either a value object or a {@link ReferenceResource}. */
	public Object getObject()
	{
		return object;
	}

	/**
	 * Creates a new assertion from a subject, predicate, and object.
	 * @param subjectURI The URI of the subject of the assertion.
	 * @param predicateURI The URI of the predicate of the assertion.
	 * @param object The object of the assertion; a value object or a {@link ReferenceResource}.
	 * @param scopeChain The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value.
	 * @exception NullPointerException if the given subject URI, predicate URI, object, and/or scope chain is <code>null</code>.
	 */
	public DefaultURFAssertion(final URI subjectURI, final URI predicateURI, final Object object, final NameValuePair<URI, Object>... scopeChain)
	{
		this.subjectURI = checkInstance(subjectURI, "Subject URI cannot be null.");
		this.predicateURI = checkInstance(predicateURI, "Predicate URI cannot be null.");
		this.object = checkInstance(object, "Object cannot be null.");
		this.scopeChain = immutableListOf(scopeChain);
	}

	/** @return A hash code value for the assertion. */
	public int hashCode()
	{
		return Objects.hashCode(getSubjectURI(), getPredicateURI(), getObject(), getScopeChain()); //hash and return the subject, predicate, object, and scope chain
	}

	/**
	 * Compares assertions based upon subject URI, predicate URI, and object.
	 * @param object The object with which to compare this assertion.
	 * @return <code>true<code> if the other object is an {@link URFAssertion} and the subject URIs, predicate URIs, objects, and scope chains of the two assertions are equal.
	 */
	public boolean equals(final Object object)
	{
		if(object instanceof URFAssertion) //if the other object is a assertion
		{
			final URFAssertion assertion2 = (URFAssertion) object; //cast the object to a assertion
			return getSubjectURI().equals(assertion2.getSubjectURI()) //compare subjects
					&& getPredicateURI().equals(assertion2.getPredicateURI()) //compare predicates
					&& getObject().equals(assertion2.getObject()) //compare objects
					&& getScopeChain().equals(assertion2.getScopeChain()); //compare scope chains
		}
		return false; //show that the assertions do not match
	}

	/**
	 * @return A string representation of the assertion in the form:
	 *         "{<var>subjectURI</var>; <var>scope</var>, <var>scope</var>...; <var>predicateURI</var>; <var>object</var>}".
	 */
	public String toString()
	{
		try
		{
			final StringBuilder stringBuilder = new StringBuilder(); //create a new string builder
			stringBuilder.append('{');
			URFTURFGenerator.appendReference(stringBuilder, getSubjectURI());
			stringBuilder.append(';').append(' '); //{subject;
			stringBuilder.append('('); //begin scopes
			int scopeCount = 0; //keep track of the number of scopes
			for(final NameValuePair<URI, Object> scope : getScopeChain()) //for each scope in the scope chain
			{
				if(scopeCount > 0) //if this isn't the first scope
				{
					stringBuilder.append(',').append(' '); //separate scopes
				}
				URFTURFGenerator.appendReference(stringBuilder, scope.getName());
				stringBuilder.append('=').append(scope.getValue()); //property=value
				++scopeCount; //indicate we appended another scope
			}
			stringBuilder.append(')').append(';').append(' '); //end scopes
			URFTURFGenerator.appendReference(stringBuilder, getPredicateURI());
			stringBuilder.append(';').append(' ').append(getObject());
			stringBuilder.append('}'); //object}
			return stringBuilder.toString(); //return the string we just constructed
		}
		catch(final IOException ioException)
		{
			throw new AssertionError(ioException);
		}
	}

}
