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
 * A simple query query describing which assertions should be retrieved from an {@link URFAssertionSource}.
 * @author Garret Wilson
 * @see URFAssertionSource
 */
public interface SimpleURFAssertionQuery extends URFAssertionQuery
{

	/**
	 * Indicates a restriction on assertions based upon subject URIs.
	 * @return The URIs identifying the subjects of the assertions to be returned, or <code>null</code> if there is no restriction of subjects.
	 */
	public Set<URI> getAssertionSubjectURIs();

	/**
	 * Indicates a restriction on assertions based upon predicate URIs
	 * @return The URIs identifying the predicates of the assertions to be returned, or <code>null</code> if there is no restriction of predicates.
	 */
	public Set<URI> getAssertionPredicateURIs();

	/**
	 * Indicates a restriction on assertions with certain subjects, based upon whether those subjects have the identified predicate/value pairs. That is,
	 * if a subject does not have predicate/value pairs matching all of those indicated here, no assertions will be returned for that subject.
	 * 
	 * <p>The value of each pair is a value object or a {@link ReferenceResource}</p>
	 * 
	 * @return The the predicate/value pairs identifying which subjects should be returned, or <code>null</code> if there are no required predicate values.
	 */
	public Map<URI, Object> getSubjectPredicateURIValues();

}
