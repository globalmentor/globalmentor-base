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

package com.globalmentor.urf.ploop;

import java.net.URI;
import java.util.Iterator;
import java.util.Set;

import com.globalmentor.java.Objects;
import com.globalmentor.urf.AbstractURFAssertionSource;
import com.globalmentor.urf.URFAssertion;
import com.globalmentor.urf.URFAssertionQuery;
import com.globalmentor.urf.URFAssertionSource;
import com.globalmentor.urf.URFAssertionStore;

/**
 * A PLOOP session based upon an {@link URFAssertionStore}
 * 
 * <p>
 * This class decorates its {@link URFAssertionStore}, allowing other objects to use this object as an {@link URFAssertionSource}. Every assertion that is
 * retrieved from the decorated {@link URFAssertionStore} will be cached locally in the map of TODO
 * 
 * @author Garret Wilson
 * 
 */
public class URFPLOOPSession extends AbstractURFAssertionSource
{

	/** The source of assertions. */
	private final URFAssertionStore assertionStore;

	/** @return The source of assertions. */
	public URFAssertionStore getAssertionStore()
	{
		return assertionStore;
	}

	/**
	 * Assertion store constructor.
	 * @param assertionStore The source of assertions.
	 */
	public URFPLOOPSession(final URFAssertionStore assertionStore)
	{
		this.assertionStore = Objects.checkInstance(assertionStore, "Assertion store cannot be null.");
	}

	/**
	 * {@inheritDoc} This version retrieves assertions from TODO
	 */
	public Set<URFAssertion> getAssertions(URFAssertionQuery assertionQuery)
	{
		//TODO cache
		return getAssertionStore().getAssertions(assertionQuery);
	}

	/**
	 * {@inheritDoc} This implementation delegates to the decorated assertion store.
	 * @see #getAssertionStore().
	 */
	public Iterator<URFAssertion> iterator()
	{
		return getAssertionStore().iterator();
	}

	/**
	 * Finds a resource by its URI.
	 * @param <T> The type of resource to return.
	 * @param resourceURI The URI of the resource to look up.
	 * @return The instance of the resource with the given URI, or <code>null</code> if the given resource was not found in the assertion store.
	 * @throws NullPointerException if the given resource class and/or resource URI is <code>null</code>.
	 */
	public <T> T findByURI(final Class<T> resourceClass, final URI resourceURI)
	{
		final URFAssertionStore assertionStore = getAssertionStore();

		throw new UnsupportedOperationException();
	}

}
