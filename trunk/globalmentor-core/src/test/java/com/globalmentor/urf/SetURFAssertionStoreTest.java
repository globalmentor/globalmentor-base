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

import java.util.Set;

import org.junit.*;
import static org.junit.Assert.*;

/**
 * Tests an URF assertion store backed by a set.
 * 
 * @author Garret Wilson
 * 
 */
public class SetURFAssertionStoreTest extends AbstractURFAssertionStoreTest
{

	/**
	 * Creates a test assertion store containing a test instance graph.
	 * @return A populated assertion store for testing.
	 * @see AbstractURFAssertionStoreTest#populateTestURFAssertionStore(URFAssertionStore)
	 */
	protected URFAssertionStore createTestURFAssertionStore()
	{
		return populateTestURFAssertionStore(new SetURFAssertionStore());
	}

	/** Tests whether a resource can be retrieved by resource URI. */
	@Test
	public void testQueryBySubject()
	{
		final URFAssertionStore assertionStore = createTestURFAssertionStore();
		final Set<URFAssertion> assertions = assertionStore.getAssertionsBySubject(CUSTOMER1_URI);
		assertTrue(assertions.size() == 3);
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER1_URI, URF.TYPE_PROPERTY_URI, CUSTOMER_CLASS_URI)));
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER1_URI, NAME_PROPERTY_URI, CUSTOMER1_NAME)));
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER1_URI, AGE_PROPERTY_URI, CUSTOMER1_AGE)));
	}

	/** Tests whether resources can be retrieved by predicate URI. */
	@Test
	public void testQueryByPredicate()
	{
		final URFAssertionStore assertionStore = createTestURFAssertionStore();
		final Set<URFAssertion> assertions = assertionStore.getAssertionsByPredicate(URF.TYPE_PROPERTY_URI);
		assertTrue(assertions.size() == 3);
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER1_URI, URF.TYPE_PROPERTY_URI, CUSTOMER_CLASS_URI)));
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER2_URI, URF.TYPE_PROPERTY_URI, CUSTOMER_CLASS_URI)));
		assertTrue(assertions.contains(new DefaultURFAssertion(ORDER1_URI, URF.TYPE_PROPERTY_URI, ORDER_CLASS_URI)));
	}

	/** Tests whether resources can be retrieved by subject URI and predicate URI. */
	@Test
	public void testQueryBySubjectAndPredicate()
	{
		final URFAssertionStore assertionStore = createTestURFAssertionStore();
		final Set<URFAssertion> assertions = assertionStore.getAssertionsBySubjectAndPredicate(CUSTOMER1_URI, URF.TYPE_PROPERTY_URI);
		assertTrue(assertions.size() == 1);
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER1_URI, URF.TYPE_PROPERTY_URI, CUSTOMER_CLASS_URI)));
	}

	/** Tests whether resources can be retrieved by predicate value. */
	@Test
	public void testQueryWithPredicateValue()
	{
		final URFAssertionStore assertionStore = createTestURFAssertionStore();
		final Set<URFAssertion> assertions = assertionStore.getAssertionsWithPredicateValue(URF.TYPE_PROPERTY_URI, CUSTOMER_CLASS_URI);
		assertTrue(assertions.size() == 6);
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER1_URI, URF.TYPE_PROPERTY_URI, CUSTOMER_CLASS_URI)));
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER1_URI, NAME_PROPERTY_URI, CUSTOMER1_NAME)));
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER1_URI, AGE_PROPERTY_URI, CUSTOMER1_AGE)));
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER2_URI, URF.TYPE_PROPERTY_URI, CUSTOMER_CLASS_URI)));
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER2_URI, NAME_PROPERTY_URI, CUSTOMER2_NAME)));
		assertTrue(assertions.contains(new DefaultURFAssertion(CUSTOMER2_URI, AGE_PROPERTY_URI, CUSTOMER2_AGE)));
	}
}
