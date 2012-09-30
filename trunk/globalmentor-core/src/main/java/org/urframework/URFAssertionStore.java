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

package org.urframework;

import java.net.URI;
import java.util.*;

import com.globalmentor.net.ReferenceResource;

/**
 * A storage of URF assertions, such as a database or a TURF file.
 * @author Garret Wilson
 */
public interface URFAssertionStore extends URFAssertionSource
{

	/**
	 * Store an assertion in the assertion store.
	 * @param assertion The assertion to store.
	 * @throws NullPointerException if the given assertion is <code>null</code>.
	 */
	public void addAssertion(final URFAssertion assertion);

	/**
	 * Adds a new assertion by a subject, predicate, and object.
	 * @param subjectURI The URI of the subject of the assertion.
	 * @param predicateURI The URI of the predicate of the assertion.
	 * @param object The object of the assertion; a value object or a {@link ReferenceResource}.
	 * @exception NullPointerException if the given subject URI, predicate URI, and/or object is <code>null</code>.
	 */
	public void addAssertion(final URI subjectURI, final URI predicateURI, final Object object);
	
	/**
	 * Stores assertions in the assertion store.
	 * @param assertions The assertions to store.
	 * @throws NullPointerException if the given assertion collection is <code>null</code>.
	 */
	public void addAssertions(final Iterable<? extends URFAssertion> assertions);

	/**
	 * Removes an assertion from the assertion store.
	 * @param assertion The assertion to remove.
	 * @throws NullPointerException if the given assertion is <code>null</code>.
	 */
	public void removeAssertion(final URFAssertion assertion);

	/**
	 * Removes an assertion by a subject, predicate, and object.
	 * @param subjectURI The URI of the subject of the assertion.
	 * @param predicateURI The URI of the predicate of the assertion.
	 * @param object The object of the assertion; a value object or a {@link ReferenceResource}.
	 * @exception NullPointerException if the given subject URI, predicate URI, and/or object is <code>null</code>.
	 */
	public void removeAssertion(final URI subjectURI, final URI predicateURI, final Object object);
	
	/**
	 * Removes assertions from the assertion store.
	 * @param assertions The assertions to remove.
	 * @throws NullPointerException if the given assertion collection is <code>null</code>.
	 */
	public void removeAssertions(final Iterable<? extends URFAssertion> assertions);

	/**
	 * Removes assertions based upon the given query.
	 * @param query The query that specifies which assertions should be removed.
	 */
	public void removeAssertions(final URFAssertionQuery assertionQuery);

	/**
	 * Removes all assertions for the identified subject.
	 * @param subjectURI The URI of the subject resource for which assertions should be removed.
	 * @throws NullPointerException if the given subject URI is <code>null</code>.
	 */
	public void removeAssertionsBySubject(final URI subjectURI);

	/**
	 * Removes all assertions for the identified predicate.
	 * @param predicateURI The URI of the predicate resource for which assertions should be removed.
	 * @throws NullPointerException if the given predicate URI is <code>null</code>.
	 */
	public void removeAssertionsByPredicate(final URI predicateURI);
	
	/**
	 * Removes all assertions for the identified subject and predicate.
	 * @param subjectURI The URI of the subject resource for which assertions should be removed.
	 * @param predicateURI The URI of the predicate resource for which assertions should be removed.
	 * @throws NullPointerException if the given subject URI and/or predicate URI is <code>null</code>.
	 */
	public void removeAssertionsBySubjectAndPredicate(final URI subjectURI, final URI predicateURI);
	
	/**
	 * Removes all assertions for which there exists the indicated predicate value. If a subject does not have a predicate/value
	 * pair matching all of the one indicated here, no assertions will be removed for that subject.
	 * <p>
	 * The value of the predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param predicateURI The the predicateURI identifying which subjects should be removed.
	 * @param predicateValue The the value of the predicate identifying which subjects should be removed.
	 * @throws NullPointerException if the given predicate URI and/or predicate value is <code>null</code>.
	 */
	public void removeAssertionsWithPredicateValue(final URI predicateURI, final Object predicateValue);
	
	/**
	 * Removes all assertions for the identified subject for which there exists the indicated predicate value. If a subject does not have a predicate/value
	 * pair matching all of the one indicated here, no assertions will be removed for that subject.
	 * <p>
	 * The value of the predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param subjectURI The URI of the subject resource for which assertions should be removed.
	 * @param predicateURI The the predicateURI identifying which subjects should be removed.
	 * @param predicateValue The the value of the predicate identifying which subjects should be removed.
	 * @throws NullPointerException if the given subject URI, predicate URI, and/or predicate value pair is <code>null</code>.
	 */
	public void removeAssertionsBySubjectWithPredicateValue(final URI subjectURI, final URI predicateURI, final Object predicateValue);

	/**
	 * Removes all assertions for which there exist all the indicated predicate values. If a subject does not have predicate/value
	 * pairs matching all of those indicated here, no assertions will be removed for that subject.
	 * <p>
	 * The value of each predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param predicateURIValues The the predicate/value pairs identifying which subjects should be removed.
	 * @throws NullPointerException if the given and/or predicate/value pairs map is <code>null</code>.
	 */
	public void removeAssertionsWithPredicateValues(final Map<URI, Object> predicateURIValues);
	
	/**
	 * Removes all assertions for the identified subject for which there exist all the indicated predicate values. If a subject does not have predicate/value
	 * pairs matching all of those indicated here, no assertions will be removed for that subject.
	 * <p>
	 * The value of each predicate/value pair is a value object or a {@link ReferenceResource}.
	 * </p>
	 * @param subjectURI The URI of the subject resource for which assertions should be removed.
	 * @param predicateURIValues The the predicate/value pairs identifying which subjects should be removed.
	 * @throws NullPointerException if the given subject URI and/or predicate/value pairs map is <code>null</code>.
	 */
	public void removeAssertionsBySubjectWithPredicateValues(final URI subjectURI, final Map<URI, Object> predicateURIValues);

	/**
	 * Sets an assertion in the assertion store.
	 * All other assertions with the same subject and predicate URI will be removed.
	 * @param assertion The assertion to set.
	 * @throws NullPointerException if the given assertion is <code>null</code>.
	 */
	public void setAssertion(final URFAssertion assertion);

	/**
	 * Sets an assertion by a subject, predicate, and object.
	 * All other assertions with the same subject and predicate URI will be removed.
	 * @param subjectURI The URI of the subject of the assertion.
	 * @param predicateURI The URI of the predicate of the assertion.
	 * @param object The object of the assertion; a value object or a {@link ReferenceResource}.
	 * @exception NullPointerException if the given subject URI, predicate URI, and/or object is <code>null</code>.
	 */
	public void setAssertion(final URI subjectURI, final URI predicateURI, final Object object);
	
	/**
	 * Removes assertions from the assertion store.
	 * All other assertions with the same subject and predicate URI as one of the given assertions will be removed.
	 * @param assertions The assertions to set.
	 * @throws NullPointerException if the given assertion collection is <code>null</code>.
	 */
	public void setAssertions(final Iterable<? extends URFAssertion> assertions);

}
