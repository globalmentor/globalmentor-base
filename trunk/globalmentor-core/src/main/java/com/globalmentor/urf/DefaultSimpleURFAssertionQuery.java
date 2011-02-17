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
import com.globalmentor.net.ReferenceResource;

/**
 * The default implementation of a simple assertion query.
 * @author Garret Wilson
 */
public class DefaultSimpleURFAssertionQuery implements SimpleURFAssertionQuery
{

	private final Set<URI> assertionSubjectURIs;

	/** {@inheritDoc} */
	public Set<URI> getAssertionSubjectURIs()
	{
		return assertionSubjectURIs;
	}

	private final Set<URI> assertionPredicateURIs;

	/** {@inheritDoc} */
	public Set<URI> getAssertionPredicateURIs()
	{
		return assertionPredicateURIs;
	}

	private final Map<URI, Object> subjectPredicateURIValues;

	/** {@inheritDoc} */
	public Map<URI, Object> getSubjectPredicateURIValues()
	{
		return subjectPredicateURIValues;
	}

	/**
	 * Full constructor.
	 * <p>
	 * The value of each predicate/value pair is a value object or a {@link ReferenceResource}
	 * </p>
	 * @param assertionSubjectURIs The URIs identifying the subjects of the assertions to be returned, or <code>null</code> if there is no restriction of
	 *          subjects.
	 * @param assertionPredicateURIs The URIs identifying the predicates of the assertions to be returned, or <code>null</code> if there is no restriction of
	 *          predicates.
	 * @param subjectPredicateURIValues The the predicate/value pairs identifying which subjects should be returned, or <code>null</code> if there are no required
	 *          predicate values.
	 */
	public DefaultSimpleURFAssertionQuery(final Set<URI> assertionSubjectURIs, final Set<URI> assertionPredicateURIs,
			final Map<URI, Object> subjectPredicateURIValues)
	{
		this.assertionSubjectURIs = assertionSubjectURIs != null ? Sets.toImmutableSet(assertionSubjectURIs) : null;
		this.assertionPredicateURIs = assertionPredicateURIs != null ? Sets.toImmutableSet(assertionPredicateURIs) : null;
		this.subjectPredicateURIValues = subjectPredicateURIValues != null ? Maps.toImmutableMap(subjectPredicateURIValues) : null;
	}

	/**
	 * Creates a query for retrieving all assertions for the identified subject.
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @return The assertions meeting the given criteria.
	 */
/*TODO del
	public static DefaultSimpleURFAssertionQuery getAssertionsBySubject(final URI subjectURI)
	{
		return new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(subjectURI), null, null);
	}
*/
	/**
	 * Creates a query for retrieving all assertions for the identified subject for which there exist all the indicated predicate values. If a subject does not have predicate/value
	 * pairs matching all of those indicated here, no assertions will be returned for that subject.
	 * <p>
	 * The value of each predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @param predicateURIValues The the predicate/value pairs identifying which subjects should be returned, or <code>null</code> if there are no required
	 *          predicate values.
	 * @return The assertions meeting the given criteria.
	 */
/*TODO del
	public static DefaultSimpleURFAssertionQuery forSubjectWithPredicateValues(final URI subjectURI, final Map<URI, Object> predicateURIValues)
	{
		return new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(subjectURI), null, predicateURIValues);
	}
*/
	
}
