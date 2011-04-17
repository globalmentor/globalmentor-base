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

/**
 * An abstract source of URF assertions.
 * @author Garret Wilson
 */
public abstract class AbstractURFAssertionSource implements URFAssertionSource
{

	/**{@inheritDoc} This implementation delegates to {@link #hasAssertion(URFAssertion)}.*/
	public boolean hasAssertion(final URI subjectURI, final URI predicateURI, final Object object)
	{
		return hasAssertion(new DefaultURFAssertion(subjectURI, predicateURI, object));
	}
	
	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Iterable<URFAssertion> getAssertionsBySubject(final URI subjectURI)
	{
		return getAssertions(DefaultSimpleURFAssertionQuery.bySubject(subjectURI));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Iterable<URFAssertion> getAssertionsByPredicate(final URI predicateURI)
	{
		return getAssertions(DefaultSimpleURFAssertionQuery.byPredicate(predicateURI));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Iterable<URFAssertion> getAssertionsBySubjectAndPredicate(final URI subjectURI, final URI predicateURI)
	{
		return getAssertions(DefaultSimpleURFAssertionQuery.bySubjectAndPredicate(subjectURI, predicateURI));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Iterable<URFAssertion> getAssertionsWithPredicateValue(final URI predicateURI, final Object predicateValue)
	{
		return getAssertions(DefaultSimpleURFAssertionQuery.withPredicateValue(predicateURI, predicateValue));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Iterable<URFAssertion> getAssertionsBySubjectWithPredicateValue(final URI subjectURI, final URI predicateURI, final Object predicateValue)
	{
		return getAssertions(DefaultSimpleURFAssertionQuery.bySubjectWithPredicateValue(subjectURI, predicateURI, predicateValue));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Iterable<URFAssertion> getAssertionsWithPredicateValues(final Map<URI, Object> predicateURIValues)
	{
		return getAssertions(DefaultSimpleURFAssertionQuery.withPredicateValues(predicateURIValues));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Iterable<URFAssertion> getAssertionsBySubjectWithPredicateValues(final URI subjectURI, final Map<URI, Object> predicateURIValues)
	{
		return getAssertions(DefaultSimpleURFAssertionQuery.bySubjectWithPredicateValues(subjectURI, predicateURIValues));
	}

}
