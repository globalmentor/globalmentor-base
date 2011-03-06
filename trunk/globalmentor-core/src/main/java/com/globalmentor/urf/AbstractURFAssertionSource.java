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
	public Set<URFAssertion> getAssertionsBySubject(final URI subjectURI)
	{
		return getAssertions(new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(subjectURI), null, null));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Set<URFAssertion> getAssertionsByPredicate(final URI predicateURI)
	{
		return getAssertions(new DefaultSimpleURFAssertionQuery(null, new ObjectSet<URI>(Objects.checkInstance(predicateURI)), null));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Set<URFAssertion> getAssertionsBySubjectAndPredicate(final URI subjectURI, final URI predicateURI)
	{
		return getAssertions(new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(Objects.checkInstance(subjectURI)), new ObjectSet<URI>(
				Objects.checkInstance(predicateURI)), null));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Set<URFAssertion> getAssertionsWithPredicateValue(final URI predicateURI, final Object predicateValue)
	{
		return getAssertions(new DefaultSimpleURFAssertionQuery(null, null, new MapEntryMap<URI, Object>(new NameValuePair<URI, Object>(
				Objects.checkInstance(predicateURI), Objects.checkInstance(predicateValue)))));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Set<URFAssertion> getAssertionsBySubjectWithPredicateValue(final URI subjectURI, final URI predicateURI, final Object predicateValue)
	{
		return getAssertions(new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(Objects.checkInstance(subjectURI)), null, new MapEntryMap<URI, Object>(
				new NameValuePair<URI, Object>(Objects.checkInstance(predicateURI), Objects.checkInstance(predicateValue)))));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Set<URFAssertion> getAssertionsWithPredicateValues(final Map<URI, Object> predicateURIValues)
	{
		return getAssertions(new DefaultSimpleURFAssertionQuery(null, null, Objects.checkInstance(predicateURIValues)));
	}

	/**
	 * {@inheritDoc} This implementation delegates to {@link #getAssertions(URFAssertionQuery)};
	 */
	public Set<URFAssertion> getAssertionsBySubjectWithPredicateValues(final URI subjectURI, final Map<URI, Object> predicateURIValues)
	{
		return getAssertions(new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(Objects.checkInstance(subjectURI)), null,
				Objects.checkInstance(predicateURIValues)));
	}

}
