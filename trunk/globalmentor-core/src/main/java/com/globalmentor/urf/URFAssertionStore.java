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
import java.util.Collection;

import com.globalmentor.net.ReferenceResource;

/**
 * A source of URF assertions, such as a database or a TURF file.
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
	 * Adds a new assertion from a subject, predicate, and object.
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
	public void addAssertions(final Collection<? extends URFAssertion> assertions);

}
