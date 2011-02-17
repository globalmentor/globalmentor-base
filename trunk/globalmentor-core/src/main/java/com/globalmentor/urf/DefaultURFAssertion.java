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
import java.util.List;

import com.globalmentor.java.Objects;
import com.globalmentor.model.NameValuePair;
import com.globalmentor.net.Resource;

/**
 * The default implementation of an URF assertion.
 * @author Garret Wilson
 */
public class DefaultURFAssertion
{

	/** The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value. */
	private final List<NameValuePair<Resource, Resource>> scopeChain;

	/** @return The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value. */
	public List<NameValuePair<Resource, Resource>> getScopeChain()
	{
		return scopeChain;
	}

	/** The assertion subject. */
	private Resource subject;

	/** @return The assertion subject. */
	public Resource getSubject()
	{
		return subject;
	}

	/** The assertion predicate. */
	private Resource predicate;

	/** @return The assertion predicate. */
	public Resource getPredicate()
	{
		return predicate;
	}

	/** The assertion object. */
	private Resource object;

	/** @return The assertion object. */
	public Resource getObject()
	{
		return object;
	}

	/**
	 * Creates a new assertion from subject, predicate, and object resources.
	 * @param subject The subject of the assertion.
	 * @param predicate The predicate of the assertion.
	 * @param object The object of the assertion.
	 * @param scopeChain The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value.
	 * @exception NullPointerException if the given subject, predicate, object, and/or scope chain is <code>null</code>.
	 */
	public DefaultURFAssertion(final Resource subject, final Resource predicate, final Resource object, final NameValuePair<Resource, Resource>... scopeChain)
	{
		this.subject = checkInstance(subject, "Subject cannot be null.");
		this.predicate = checkInstance(predicate, "Predicate cannot be null.");
		this.object = checkInstance(object, "Object cannot be null.");
		this.scopeChain = createReadOnlyList(scopeChain);
	}

	/** @return A hash code value for the assertion. */
	public int hashCode()
	{
		return Objects.hashCode(getSubject(), getPredicate(), getObject(), getScopeChain()); //hash and return the subject, predicate, object, and scope chain
	}

	/**
	 * Compares assertions based upon subject, predicate, and object.
	 * @param object The object with which to compare this assertion.
	 * @return <code>true<code> if the other object is an {@link URFAssertion} and the subjects, predicates, objects, and scope chains of the two assertions are equal.
	 */
	public boolean equals(final Object object)
	{
		if(object instanceof URFAssertion) //if the other object is a assertion
		{
			final URFAssertion assertion2 = (URFAssertion) object; //cast the object to a assertion
			return getSubject().equals(assertion2.getSubject()) //compare subjects
					&& getPredicate().equals(assertion2.getPredicate()) //compare predicates
					&& getObject().equals(assertion2.getObject()) //compare objects
					&& getScopeChain().equals(assertion2.getScopeChain()); //compare scope chains
		}
		return false; //show that the assertions do not match
	}

	/**
	 * @return A string representation of the assertion in the form:
	 *         "{<var>subject</var>; <var>scope</var>, <var>scope</var>...; <var>predicate</var>; <var>object</var>}".
	 */
	public String toString()
	{
		try
		{
			final StringBuilder stringBuilder = new StringBuilder(); //create a new string builder
			stringBuilder.append('{');
			URFTURFGenerator.appendReference(stringBuilder, getSubject().getURI());
			stringBuilder.append(';').append(' '); //{subject;
			stringBuilder.append('('); //begin scopes
			int scopeCount = 0; //keep track of the number of scopes
			for(final NameValuePair<Resource, Resource> scope : getScopeChain()) //for each scope in the scope chain
			{
				if(scopeCount > 0) //if this isn't the first scope
				{
					stringBuilder.append(',').append(' '); //separate scopes
				}
				URFTURFGenerator.appendReference(stringBuilder, scope.getName().getURI());
				stringBuilder.append('=');
				URFTURFGenerator.appendReference(stringBuilder, scope.getValue().getURI()); //property=value
				++scopeCount; //indicate we appended another scope
			}
			stringBuilder.append(')').append(';').append(' '); //end scopes
			URFTURFGenerator.appendReference(stringBuilder, getPredicate().getURI());
			stringBuilder.append(';').append(' '); //predicate;
			URFTURFGenerator.appendReference(stringBuilder, getObject().getURI());
			stringBuilder.append('}'); //object}
			return stringBuilder.toString(); //return the string we just constructed
		}
		catch(final IOException ioException)
		{
			throw new AssertionError(ioException);
		}
	}

}
