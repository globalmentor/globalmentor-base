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

import com.globalmentor.collections.Sets;
import com.globalmentor.model.NameValuePair;

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

	private final Set<NameValuePair<URI, URI>> subjectPredicateValueURIs;

	/** {@inheritDoc} */
	public Set<NameValuePair<URI, URI>> getSubjectPredicateValueURIs()
	{
		return subjectPredicateValueURIs;
	}

	/**
	 * Full constructor.
	 * @param assertionSubjectURIs The URIs identifying the subjects of the assertions to be returned, or <code>null</code> if there is no restriction of
	 *          subjects.
	 * @param assertionPredicateURIs The URIs identifying the predicates of the assertions to be returned, or <code>null</code> if there is no restriction of
	 *          predicates.
	 * @param subjectPredicateValueURIs The URIs identifying the predicate/value pairs identifying which subjects should be returned, or <code>null</code> if
	 *          there are no required predicate values.
	 */
	public DefaultSimpleURFAssertionQuery(final Set<URI> assertionSubjectURIs, final Set<URI> assertionPredicateURIs,
			final Set<NameValuePair<URI, URI>> subjectPredicateValueURIs)
	{
		this.assertionSubjectURIs = assertionSubjectURIs != null ? Sets.toImmutableSet(assertionSubjectURIs) : null;
		this.assertionPredicateURIs = assertionPredicateURIs != null ? Sets.toImmutableSet(assertionPredicateURIs) : null;
		this.subjectPredicateValueURIs = subjectPredicateValueURIs != null ? Sets.toImmutableSet(subjectPredicateValueURIs) : null;
	}

	//TODO create query builder
}
