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

import com.globalmentor.net.ReferenceResource;

/**
 * A source of URF assertions, such as a database or a TURF file.
 * @author Garret Wilson
 */
public interface URFAssertionSource extends Iterable<URFAssertion>
{

	/**
	 * Determines whether this assertion source has the given assertion.
	 * @param assertion The assertion to check for.
	 * @return <code>true</code> if the given assertion exists in the source.
	 * @throws NullPointerException if the given assertion is <code>null</code>.
	 */
	public boolean hasAssertion(final URFAssertion assertion);

	/**
	 * Determines whether this assertion source has the assertion identified by the given subject, predicate, and object.
	 * @param subjectURI The URI of the subject of the assertion.
	 * @param predicateURI The URI of the predicate of the assertion.
	 * @param object The object of the assertion; a value object or a {@link ReferenceResource}.
	 * @exception NullPointerException if the given subject URI, predicate URI, and/or object is <code>null</code>.
	 */
	public boolean hasAssertion(final URI subjectURI, final URI predicateURI, final Object object);

	/**
	 * Retrieves assertions based upon the given query.
	 * @param query The query that specifies which assertions should be returned.
	 * @return The assertions meeting the given criteria.
	 */
	public Iterable<URFAssertion> getAssertions(final URFAssertionQuery assertionQuery);

	/**
	 * Counts the number of assertions available based upon the given query. If the number of assertions to be returned for the given query is unknown, this
	 * method returns {@link Long#MAX_VALUE}.
	 * @param query The query that specifies which assertions should be counted.
	 * @return The number of assertions meeting the given criteria, or {@link Long#MAX_VALUE} if the number of assertions is unknown.
	 */
	public long countAssertions(final URFAssertionQuery assertionQuery);

	/**
	 * Retrieves all assertions for the identified subject.
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @return The assertions meeting the given criteria.
	 * @throws NullPointerException if the given subject URI is <code>null</code>.
	 */
	public Iterable<URFAssertion> getAssertionsBySubject(final URI subjectURI);

	/**
	 * Retrieves all assertions for the identified predicate.
	 * @param predicateURI The URI of the predicate resource for which assertions should be returned.
	 * @return The assertions meeting the given criteria.
	 * @throws NullPointerException if the given predicate URI is <code>null</code>.
	 */
	public Iterable<URFAssertion> getAssertionsByPredicate(final URI predicateURI);

	/**
	 * Retrieves all assertions for the identified subject and predicate.
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @param predicateURI The URI of the predicate resource for which assertions should be returned.
	 * @return The assertions meeting the given criteria.
	 * @throws NullPointerException if the given subject URI and/or predicate URI is <code>null</code>.
	 */
	public Iterable<URFAssertion> getAssertionsBySubjectAndPredicate(final URI subjectURI, final URI predicateURI);

	/**
	 * Retrieves all assertions for which there exists the indicated predicate value. If a subject does not have a predicate/value pair matching all of the one
	 * indicated here, no assertions will be returned for that subject.
	 * <p>
	 * The value of the predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param predicateURI The the predicateURI identifying which subjects should be returned.
	 * @param predicateValue The the value of the predicate identifying which subjects should be returned.
	 * @return The assertions meeting the given criteria.
	 * @throws NullPointerException if the given predicate URI and/or predicate value is <code>null</code>.
	 */
	public Iterable<URFAssertion> getAssertionsWithPredicateValue(final URI predicateURI, final Object predicateValue);

	/**
	 * Retrieves all assertions for the identified subject for which there exists the indicated predicate value. If a subject does not have a predicate/value pair
	 * matching all of the one indicated here, no assertions will be returned for that subject.
	 * <p>
	 * The value of the predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @param predicateURI The the predicateURI identifying which subjects should be returned.
	 * @param predicateValue The the value of the predicate identifying which subjects should be returned.
	 * @return The assertions meeting the given criteria.
	 * @throws NullPointerException if the given subject URI, predicate URI, and/or predicate value pair is <code>null</code>.
	 */
	public Iterable<URFAssertion> getAssertionsBySubjectWithPredicateValue(final URI subjectURI, final URI predicateURI, final Object predicateValue);

	/**
	 * Retrieves all assertions for which there exist all the indicated predicate values. If a subject does not have predicate/value pairs matching all of those
	 * indicated here, no assertions will be returned for that subject.
	 * <p>
	 * The value of each predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param predicateURIValues The the predicate/value pairs identifying which subjects should be returned.
	 * @return The assertions meeting the given criteria.
	 * @throws NullPointerException if the given and/or predicate/value pairs map is <code>null</code>.
	 */
	public Iterable<URFAssertion> getAssertionsWithPredicateValues(final Map<URI, Object> predicateURIValues);

	/**
	 * Retrieves all assertions for the identified subject for which there exist all the indicated predicate values. If a subject does not have predicate/value
	 * pairs matching all of those indicated here, no assertions will be returned for that subject.
	 * <p>
	 * The value of each predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @param predicateURIValues The the predicate/value pairs identifying which subjects should be returned.
	 * @return The assertions meeting the given criteria.
	 * @throws NullPointerException if the given subject URI and/or predicate/value pairs map is <code>null</code>.
	 */
	public Iterable<URFAssertion> getAssertionsBySubjectWithPredicateValues(final URI subjectURI, final Map<URI, Object> predicateURIValues);

}
