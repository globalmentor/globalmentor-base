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

import java.lang.reflect.InvocationTargetException;

import org.junit.*;
import static org.junit.Assert.*;

import com.globalmentor.model.Customer;

import static com.globalmentor.urf.SetURFAssertionStoreTest.*;
import com.globalmentor.urf.*;
import com.globalmentor.util.DataException;

/**
 * Various tests of a PLOOP URF assertion processor.
 * 
 * @author Garret Wilson
 * 
 */
public class PLOOPURFAssertionProcessorTest
{

	@Test
	public void testCustomerCreate() throws DataException, InvocationTargetException
	{
		final URFAssertionStore assertionStore = createTestURFAssertionStore();
		final PLOOPURFAssertionProcessor processor = new PLOOPURFAssertionProcessor(assertionStore);
		final Customer customer1 = (Customer) processor.getObject(CUSTOMER1_URI);
		assertEquals(CUSTOMER1_NAME, customer1.getName());
		assertEquals(CUSTOMER1_AGE, customer1.getAge());
		assertTrue(customer1.getAliases().size() == 3);
		assertTrue(customer1.getAliases().contains(CUSTOMER1_ALIAS1));
		assertTrue(customer1.getAliases().contains(CUSTOMER1_ALIAS2));
		assertTrue(customer1.getAliases().contains(CUSTOMER1_ALIAS3));
	}

}
