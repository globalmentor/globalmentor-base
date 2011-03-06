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

import com.globalmentor.collections.*;
import com.globalmentor.java.Objects;
import com.globalmentor.model.NameValuePair;

/**
 * An abstract storage of URF assertions.
 * @author Garret Wilson
 */
public abstract class AbstractURFAssertionStore extends AbstractURFAssertionSource implements URFAssertionStore
{

	/**
	 * {@inheritDoc} This implementation delegates to {@link #addAssertion(URFAssertion)}.
	 */
	public final void addAssertion(final URI subjectURI, final URI predicateURI, final Object object)
	{
		addAssertion(new DefaultURFAssertion(subjectURI, predicateURI, object));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #removeAssertion(URFAssertion)}.
	 */
	public void removeAssertion(final URI subjectURI, final URI predicateURI, final Object object)
	{
		removeAssertion(new DefaultURFAssertion(subjectURI, predicateURI, object));
	}

	/**
	 * {@inheritDoc} This implementation gathers assertions using {@link #getAssertions(URFAssertionQuery)} and then removes them using
	 * {@link #removeAssertions(Collection)}.
	 */
	public void removeAssertions(final URFAssertionQuery assertionQuery)
	{
		removeAssertions(getAssertions(assertionQuery));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #removeAssertions(URFAssertionQuery)};
	 */
	public final void removeAssertionsBySubject(final URI subjectURI)
	{
		removeAssertions(new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(subjectURI), null, null));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #removeAssertions(URFAssertionQuery)};
	 */
	public void removeAssertionsByPredicate(final URI predicateURI)
	{
		removeAssertions(new DefaultSimpleURFAssertionQuery(null, new ObjectSet<URI>(Objects.checkInstance(predicateURI)), null));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #removeAssertions(URFAssertionQuery)};
	 */
	public void removeAssertionsBySubjectAndPredicate(final URI subjectURI, final URI predicateURI)
	{
		removeAssertions(new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(Objects.checkInstance(subjectURI)), new ObjectSet<URI>(
				Objects.checkInstance(predicateURI)), null));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #removeAssertions(URFAssertionQuery)};
	 */
	public void removeAssertionsWithPredicateValue(final URI predicateURI, final Object predicateValue)
	{
		removeAssertions(new DefaultSimpleURFAssertionQuery(null, null, new MapEntryMap<URI, Object>(new NameValuePair<URI, Object>(
				Objects.checkInstance(predicateURI), Objects.checkInstance(predicateValue)))));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #removeAssertions(URFAssertionQuery)};
	 */
	public void removeAssertionsBySubjectWithPredicateValue(final URI subjectURI, final URI predicateURI, final Object predicateValue)
	{
		removeAssertions(new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(Objects.checkInstance(subjectURI)), null, new MapEntryMap<URI, Object>(
				new NameValuePair<URI, Object>(Objects.checkInstance(predicateURI), Objects.checkInstance(predicateValue)))));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #removeAssertions(URFAssertionQuery)};
	 */
	public void removeAssertionsWithPredicateValues(final Map<URI, Object> predicateURIValues)
	{
		removeAssertions(new DefaultSimpleURFAssertionQuery(null, null, Objects.checkInstance(predicateURIValues)));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #removeAssertions(URFAssertionQuery)};
	 */
	public final void removeAssertionsBySubjectWithPredicateValues(final URI subjectURI, final Map<URI, Object> predicateURIValues)
	{
		removeAssertions(new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(Objects.checkInstance(subjectURI)), null, Objects.checkInstance(predicateURIValues)));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #setAssertion(URFAssertion)}.
	 */
	public void setAssertion(final URI subjectURI, final URI predicateURI, final Object object)
	{
		setAssertion(new DefaultURFAssertion(subjectURI, predicateURI, object));
	}

	/**
	 * {@inheritDoc} This implementation removes assertions using {@link #removeAssertionsBySubjectAndPredicate(URI, URI)} and then adds the given assertion using
	 * {@link #addAssertion(URFAssertion)}
	 */
	public void setAssertion(final URFAssertion assertion)
	{
		removeAssertionsBySubjectAndPredicate(assertion.getSubjectURI(), assertion.getPredicateURI());
		addAssertion(assertion);
	}

	/** {@inheritDoc} */
	/*TODO delete if not useful; erroneous implementation that winds up simply removing the given assertions
		public void setAssertions(final Collection<? extends URFAssertion> assertions)
		{
			final Map<URI, Map<URI, Object>> subjectPredicateValues=new HashMap<URI, Map<URI, Object>>();	//create a map of predicate+values for subjects
			for(final URFAssertion assertion:assertions)
			{
				Map<URI, Object> predicateValues=subjectPredicateValues.get(assertion.getSubjectURI());	//see if we already have predicate values for this subject.
				if(predicateValues==null)	//if not, create one
				{
					predicateValues=new HashMap<URI, Object>();
					subjectPredicateValues.put(assertion.getSubjectURI(), predicateValues);	//associate the predicate+values with the subject URI for next time
				}
				predicateValues.put(assertion.getPredicateURI(), assertion.getObject());	//store this predicate+value associated with this subject
			}
			for(final Map.Entry<URI, Map<URI, Object>> subjectPredicateValueEntry:subjectPredicateValues.entrySet())	//each entry now gives us a query for assertions to remove
			{
				removeAssertions(new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(subjectPredicateValueEntry.getKey()), subjectPredicateValueEntry.getValue()));	//remove the predicate/values for this subject
			}
		}
	*/

	/** {@inheritDoc} */
	public void setAssertions(final Collection<? extends URFAssertion> assertions)
	{
		final CollectionMap<URI, URI, Set<URI>> subjectPredicates = new HashSetHashMap<URI, URI>(); //create a map of predicates for subjects
		for(final URFAssertion assertion : assertions)
		{
			subjectPredicates.addItem(assertion.getSubjectURI(), assertion.getPredicateURI()); //associate this predicate with the subject
		}
		for(final Map.Entry<URI, Set<URI>> subjectPredicatesEntry : subjectPredicates.entrySet()) //each entry now gives us a query for assertions to remove
		{
			removeAssertions(new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(subjectPredicatesEntry.getKey()), subjectPredicatesEntry.getValue())); //remove the indicated predicate for this subject
		}
		addAssertions(assertions); //now add the requested assertions
	}

}
