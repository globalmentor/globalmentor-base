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
import java.util.Set;

import com.globalmentor.model.NameValuePair;

/**
 * A source of URF assertions, such as a database or a TURF file.
 * @author Garret Wilson
 */
public interface URFAssertionSource extends Iterable<URFAssertion>
{

	/**
	 * Retrieves assertions based upon the given query.
	 * @param query The query that specifies which assertions should be returned.
	 * @return The assertions meeting the given criteria.
	 */
	public Set<URFAssertion> getAssertions(final URFAssertionQuery assertionQuery);

	/**
	 * Retrieves all assertions for the identified subject.
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @return The assertions meeting the given criteria.
	 */
	public Set<URFAssertion> getAssertions(final URI subjectURI);

	/**
	 * Retrieves all assertions for the identified subject for which there exist all the indicated predicate values. If a subject does not have predicate/value
	 * pairs matching all of those indicated here, no assertions will be returned for that subject.
	 * @param subjectURI The URI of the subject resource for which assertions should be returned.
	 * @param predicateValues The URIs of the predicate/value pairs restricting which subjects should be returned, or <code>null</code> if there are no required
	 *          predicate values.
	 * @return The assertions meeting the given criteria.
	 */
	public Set<URFAssertion> getAssertions(final URI subjectURI, final Set<NameValuePair<URI, URI>> predicateValues);

}
