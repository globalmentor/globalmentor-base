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

/**
 * The default implementation of an URF assertion.
 * @author Garret Wilson
 */
public class DefaultURFAssertion
{

	/** The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value. */
	private final List<NameValuePair<URI, URI>> scopeChain;

	/** @return The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value. */
	public List<NameValuePair<URI, URI>> getScopeChain()
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

	/** The URI of the assertion object. */
	private URI objectURI;

	/** @return The URI of the assertion object. */
	public URI getObjectURI()
	{
		return objectURI;
	}

	/**
	 * Creates a new assertion from subject, predicate, and object URIs.
	 * @param subjectURI The URI of the subject of the assertion.
	 * @param predicateURI The URI of the predicate of the assertion.
	 * @param objectURI The URI of the object of the assertion.
	 * @param scopeChain The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value.
	 * @exception NullPointerException if the given subject URI, predicate URI, object URI, and/or scope chain is <code>null</code>.
	 */
	public DefaultURFAssertion(final URI subjectURI, final URI predicateURI, final URI objectURI, final NameValuePair<URI, URI>... scopeChain)
	{
		this.subjectURI = checkInstance(subjectURI, "Subject URI cannot be null.");
		this.predicateURI = checkInstance(predicateURI, "Predicate URI cannot be null.");
		this.objectURI = checkInstance(objectURI, "Object URI cannot be null.");
		this.scopeChain = createReadOnlyList(scopeChain);
	}

	/** @return A hash code value for the assertion. */
	public int hashCode()
	{
		return Objects.hashCode(getSubjectURI(), getPredicateURI(), getObjectURI(), getScopeChain()); //hash and return the subject, predicate, object, and scope chain
	}

	/**
	 * Compares assertions based upon subject URI, predicate URI, and object URI.
	 * @param object The object with which to compare this assertion.
	 * @return <code>true<code> if the other object is an {@link URFAssertion} and the subject URIs, predicate URIs, object URIs, and scope chains of the two assertions are equal.
	 */
	public boolean equals(final Object object)
	{
		if(object instanceof URFAssertion) //if the other object is a assertion
		{
			final URFAssertion assertion2 = (URFAssertion) object; //cast the object to a assertion
			return getSubjectURI().equals(assertion2.getSubjectURI()) //compare subjects
					&& getPredicateURI().equals(assertion2.getPredicateURI()) //compare predicates
					&& getObjectURI().equals(assertion2.getObjectURI()) //compare objects
					&& getScopeChain().equals(assertion2.getScopeChain()); //compare scope chains
		}
		return false; //show that the assertions do not match
	}

	/**
	 * @return A string representation of the assertion in the form:
	 *         "{<var>subjectURI</var>; <var>scope</var>, <var>scope</var>...; <var>predicateURI</var>; <var>objectURI</var>}".
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
			for(final NameValuePair<URI, URI> scope : getScopeChain()) //for each scope in the scope chain
			{
				if(scopeCount > 0) //if this isn't the first scope
				{
					stringBuilder.append(',').append(' '); //separate scopes
				}
				URFTURFGenerator.appendReference(stringBuilder, scope.getName());
				stringBuilder.append('=');
				URFTURFGenerator.appendReference(stringBuilder, scope.getValue()); //property=value
				++scopeCount; //indicate we appended another scope
			}
			stringBuilder.append(')').append(';').append(' '); //end scopes
			URFTURFGenerator.appendReference(stringBuilder, getPredicateURI());
			stringBuilder.append(';').append(' '); //predicate;
			URFTURFGenerator.appendReference(stringBuilder, getObjectURI());
			stringBuilder.append('}'); //object}
			return stringBuilder.toString(); //return the string we just constructed
		}
		catch(final IOException ioException)
		{
			throw new AssertionError(ioException);
		}
	}

}
