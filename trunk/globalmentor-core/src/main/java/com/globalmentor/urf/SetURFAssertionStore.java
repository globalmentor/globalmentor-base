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
import java.util.*;

import com.globalmentor.java.Objects;

/**
 * An implementation of an URF assertion store based upon a set.
 * @author Garret Wilson
 */
public class SetURFAssertionStore extends AbstractURFAssertionSource implements URFAssertionStore
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

	/** {@inheritDoc} */
	public void addAssertion(final URFAssertion assertion)
	{
		this.assertions.add(assertion);
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #addAssertion(URFAssertion)}.
	 */
	public final void addAssertion(final URI subjectURI, final URI predicateURI, final Object object)
	{
		addAssertion(new DefaultURFAssertion(subjectURI, predicateURI, object));
	}

	/** {@inheritDoc} */
	public void addAssertions(final Collection<? extends URFAssertion> assertions)
	{
		this.assertions.addAll(assertions);
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
		final Map<URI, Object> subjectPredicateURIValues = simpleURFAssertionQuery.getSubjectPredicateURIValues();
		final Set<URFAssertion> resultAssertions = new HashSet<URFAssertion>(); //create a new set in which to store assertions
		final Set<URI> retainSubjectsURIs = new HashSet<URI>(); //keep track of which subjects to retain
		for(final URFAssertion assertion : this) //iterate through all our assertions
		{
			if(assertionSubjectURIs != null && !assertionSubjectURIs.contains(assertion.getSubjectURI())) //if this subject is prohibited
			{
				continue; //skip this assertion
			}
			if(assertionPredicateURIs != null && !assertionPredicateURIs.contains(assertion.getPredicateURI())) //if this predicate is prohibited
			{
				continue; //skip this assertion
			}
			resultAssertions.add(assertion); //add this assertion to our result set
			retainSubjectsURIs.add(assertion.getSubjectURI()); //for now, we'll keep this subject
		}
		//recheck the subject URIs to make sure they have the required predicate/value pairs
		if(subjectPredicateURIValues != null) //if we have predicate/value tests
		{
			final Iterator<URI> subjectURIIterator = retainSubjectsURIs.iterator(); //check all the subjects we have so far
			while(subjectURIIterator.hasNext())
			{
				final URI subjectURI = subjectURIIterator.next();
				boolean hasAllPredicateValues = true; //start out assuming that this subject will have all the required predicate values
				for(final Map.Entry<URI, Object> subjectPredicateURIValue : subjectPredicateURIValues.entrySet())
				{
					//if there is no assertion for this subject with the required predicate/value
					if(!resultAssertions.contains(new DefaultURFAssertion(subjectURI, subjectPredicateURIValue.getKey(), subjectPredicateURIValue.getValue())))
					{
						hasAllPredicateValues = false; //this subject doesn't meet the criteria
						break;
					}
				}
				if(!hasAllPredicateValues) //if didn't find all the predicate/values for this subject
				{
					subjectURIIterator.remove(); //we don't want this subject anymore
				}
			}
		}
		//now throw out all results for subjects we don't want
		final Iterator<URFAssertion> resultAssertionIterator = resultAssertions.iterator();
		while(resultAssertionIterator.hasNext())
		{
			final URFAssertion resultAssertion = resultAssertionIterator.next();
			if(!retainSubjectsURIs.contains(resultAssertion.getSubjectURI())) //if this assertion is for a subject we don't want
			{
				resultAssertionIterator.remove(); //throw out this assertion
			}
		}
		return resultAssertions; //return the assertions we wound up with
	}

}
