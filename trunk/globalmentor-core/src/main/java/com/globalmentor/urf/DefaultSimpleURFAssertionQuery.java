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
	 * Subject URIs and subject predicate URI values constructor.
	 * <p>
	 * The value of each predicate/value pair is a value object or a {@link ReferenceResource}
	 * </p>
	 * @param assertionSubjectURIs The URIs identifying the subjects of the assertions to be returned, or <code>null</code> if there is no restriction of
	 *          subjects.
	 * @param subjectPredicateURIValues The the predicate/value pairs identifying which subjects should be returned, or <code>null</code> if there are no required
	 *          predicate values.
	 */
	public DefaultSimpleURFAssertionQuery(final Set<URI> assertionSubjectURIs, final Map<URI, Object> subjectPredicateURIValues)
	{
		this(assertionSubjectURIs, null, subjectPredicateURIValues);
	}

	/**
	 * Subject URIs and predicate URIs constructor.
	 * @param assertionSubjectURIs The URIs identifying the subjects of the assertions to be returned, or <code>null</code> if there is no restriction of
	 *          subjects.
	 * @param assertionPredicateURIs The URIs identifying the predicates of the assertions to be returned, or <code>null</code> if there is no restriction of
	 *          predicates.
	 */
	public DefaultSimpleURFAssertionQuery(final Set<URI> assertionSubjectURIs, final Set<URI> assertionPredicateURIs)
	{
		this(assertionSubjectURIs, assertionPredicateURIs, null);
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
		this.assertionSubjectURIs = assertionSubjectURIs != null ? Sets.immutableSetOf(assertionSubjectURIs) : null;
		this.assertionPredicateURIs = assertionPredicateURIs != null ? Sets.immutableSetOf(assertionPredicateURIs) : null;
		this.subjectPredicateURIValues = subjectPredicateURIValues != null ? Maps.toImmutableMap(subjectPredicateURIValues) : null;
	}

	/**
	 * Creates a query to retrieve all assertions for the identified subject.
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @return A query to retrieve the assertions meeting the given criteria.
	 * @throws NullPointerException if the given subject URI is <code>null</code>.
	 */
	public static SimpleURFAssertionQuery bySubject(final URI subjectURI)
	{
		return new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(subjectURI), null, null);
	}

	/**
	 * Creates a query to retrieve all assertions for the identified predicate.
	 * @param predicateURI The URI of the predicate resource for which assertions should be returned.
	 * @return A query to retrieve the assertions meeting the given criteria.
	 * @throws NullPointerException if the given predicate URI is <code>null</code>.
	 */
	public static SimpleURFAssertionQuery byPredicate(final URI predicateURI)
	{
		return new DefaultSimpleURFAssertionQuery(null, new ObjectSet<URI>(Objects.checkInstance(predicateURI)), null);
	}

	/**
	 * Creates a query to retrieve all assertions for the identified subject and predicate.
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @param predicateURI The URI of the predicate resource for which assertions should be returned.
	 * @return A query to retrieve the assertions meeting the given criteria.
	 * @throws NullPointerException if the given subject URI and/or predicate URI is <code>null</code>.
	 */
	public static SimpleURFAssertionQuery bySubjectAndPredicate(final URI subjectURI, final URI predicateURI)
	{
		return new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(Objects.checkInstance(subjectURI)), new ObjectSet<URI>(Objects.checkInstance(predicateURI)),
				null);
	}

	/**
	 * Creates a query to retrieve all assertions for which there exists the indicated predicate value. If a subject does not have a predicate/value pair matching
	 * all of the one indicated here, no assertions will be returned for that subject.
	 * <p>
	 * The value of the predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param predicateURI The the predicateURI identifying which subjects should be returned.
	 * @param predicateValue The the value of the predicate identifying which subjects should be returned.
	 * @return A query to retrieve the assertions meeting the given criteria.
	 * @throws NullPointerException if the given predicate URI and/or predicate value is <code>null</code>.
	 */
	public static SimpleURFAssertionQuery withPredicateValue(final URI predicateURI, final Object predicateValue)
	{
		return new DefaultSimpleURFAssertionQuery(null, null, new MapEntryMap<URI, Object>(new NameValuePair<URI, Object>(Objects.checkInstance(predicateURI),
				Objects.checkInstance(predicateValue))));
	}

	/**
	 * Creates a query to retrieve all assertions for the identified subject for which there exists the indicated predicate value. If a subject does not have a
	 * predicate/value pair matching all of the one indicated here, no assertions will be returned for that subject.
	 * <p>
	 * The value of the predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @param predicateURI The the predicateURI identifying which subjects should be returned.
	 * @param predicateValue The the value of the predicate identifying which subjects should be returned.
	 * @return A query to retrieve the assertions meeting the given criteria.
	 * @throws NullPointerException if the given subject URI, predicate URI, and/or predicate value pair is <code>null</code>.
	 */
	public static SimpleURFAssertionQuery bySubjectWithPredicateValue(final URI subjectURI, final URI predicateURI, final Object predicateValue)
	{
		return new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(Objects.checkInstance(subjectURI)), null, new MapEntryMap<URI, Object>(
				new NameValuePair<URI, Object>(Objects.checkInstance(predicateURI), Objects.checkInstance(predicateValue))));
	}

	/**
	 * Creates a query to retrieve all assertions for which there exist all the indicated predicate values. If a subject does not have predicate/value pairs
	 * matching all of those indicated here, no assertions will be returned for that subject.
	 * <p>
	 * The value of each predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param predicateURIValues The the predicate/value pairs identifying which subjects should be returned.
	 * @return A query to retrieve the assertions meeting the given criteria.
	 * @throws NullPointerException if the given and/or predicate/value pairs map is <code>null</code>.
	 */
	public static SimpleURFAssertionQuery withPredicateValues(final Map<URI, Object> predicateURIValues)
	{
		return new DefaultSimpleURFAssertionQuery(null, null, Objects.checkInstance(predicateURIValues));
	}

	/**
	 * Creates a query to retrieve all assertions for the identified subject for which there exist all the indicated predicate values. If a subject does not have
	 * predicate/value pairs matching all of those indicated here, no assertions will be returned for that subject.
	 * <p>
	 * The value of each predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @param predicateURIValues The the predicate/value pairs identifying which subjects should be returned.
	 * @return A query to retrieve the assertions meeting the given criteria.
	 * @throws NullPointerException if the given subject URI and/or predicate/value pairs map is <code>null</code>.
	 */
	public static SimpleURFAssertionQuery bySubjectWithPredicateValues(final URI subjectURI, final Map<URI, Object> predicateURIValues)
	{
		return new DefaultSimpleURFAssertionQuery(new ObjectSet<URI>(Objects.checkInstance(subjectURI)), null, Objects.checkInstance(predicateURIValues));
	}

}
