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

import java.net.URI;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import com.globalmentor.java.Objects;
import com.globalmentor.model.NameValuePair;

/**
 * An implementation of an URF assertion store based upon a set.
 * @author Garret Wilson
 */
public class SetURFAssertionStore extends AbstractURFAssertionSource
{

	/** The store of assertions. */
	private final Set<URFAssertion> assertions;

	/** {@inheritDoc} */
	public Iterator<URFAssertion> iterator()
	{
		return assertions.iterator(); //delegate to our 
	}

	/** Default constructor creating an assertion store backed by a hash set. */
	public SetURFAssertionStore()
	{
		this(new HashSet<URFAssertion>());
	}

	/**
	 * Constructs an assertion store backed by the given set of assertions. The backing is live---the given set will be modified when this assertion store is
	 * modified.
	 * @param assertions The set of assertions to serve as the backing of this assertion store.
	 * @throws NullPointerException if the given set is <code>null</code>.
	 */
	public SetURFAssertionStore(final Set<URFAssertion> assertions)
	{
		this.assertions = Objects.checkInstance(assertions, "Assertion set cannot be null.");
	}

	/**
	 * {@inheritDoc} This implementation only supports assertion queries of type {@link SimpleURFAssertionQuery}.
	 */
	public Set<URFAssertion> getAssertions(final URFAssertionQuery assertionQuery)
	{
		if(!(assertionQuery instanceof SimpleURFAssertionQuery))
		{
			throw new UnsupportedOperationException("URF assertion query of type " + assertionQuery.getClass().getName() + " not supported.");
		}
		final SimpleURFAssertionQuery simpleURFAssertionQuery = (SimpleURFAssertionQuery) assertionQuery;
		final Set<URI> assertionSubjectURIs = simpleURFAssertionQuery.getAssertionSubjectURIs();
		final Set<URI> assertionPredicateURIs = simpleURFAssertionQuery.getAssertionPredicateURIs();
		final Set<NameValuePair<URI, URI>> subjectPredicateValueURIs = simpleURFAssertionQuery.getSubjectPredicateValueURIs();
		final Set<URFAssertion> resultAssertions = new HashSet<URFAssertion>(); //create a new set in which to store assertions
		for(final URFAssertion assertion : this) //iterate through all our assertions
		{
			if(assertionSubjectURIs != null && !assertionSubjectURIs.contains(assertion.getSubject().getURI())) //if this subject is prohibited
			{
				continue; //skip this assertion
			}
			if(assertionPredicateURIs != null && !assertionPredicateURIs.contains(assertion.getPredicate().getURI())) //if this predicate is prohibited
			{
				continue; //skip this assertion
			}
			resultAssertions.add(assertion); //add this assertion to our result set
		}
		//TODO check predicate/values
		return resultAssertions; //return the resulting assertions
	}

}
