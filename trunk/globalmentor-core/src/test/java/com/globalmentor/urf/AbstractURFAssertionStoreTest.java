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

import com.globalmentor.java.Classes;

/**
 * Helper methods for testing assertion stores.
 * 
 * @author Garret Wilson
 * 
 */
public abstract class AbstractURFAssertionStoreTest
{

	//test classes
	public final static URI CUSTOMER_CLASS_URI = Classes.createJavaClassURI("com.example.Customer");
	public final static URI ORDER_CLASS_URI = Classes.createJavaClassURI("com.example.Order");
	public final static URI USER_CLASS_URI = Classes.createJavaClassURI("com.example.User");
	//test properties
	public final static URI AGE_PROPERTY_URI = URI.create("http://example.com/properties/age");
	public final static URI NAME_PROPERTY_URI = URI.create("http://example.com/properties/name");
	//test classes
	public final static URI CUSTOMER1_URI = URI.create("http://example.com/entities/customer1");
	public final static URI CUSTOMER2_URI = URI.create("http://example.com/entities/customer2");
	public final static URI ORDER1_URI = URI.create("http://example.com/entities/order1");
	//test values
	public final static String CUSTOMER1_NAME="Customer One";
	public final static Long CUSTOMER1_AGE=20L;
	public final static String CUSTOMER2_NAME="Customer Two";
	public final static Long CUSTOMER2_AGE=30L;

	/**
	 * Populates an assertion store with a test instance graph:
	 * <ul>
	 * <li>{@value #CUSTOMER1_URI}
	 * <ul>
	 * <li>urf.type: Customer</li>
	 * <li>name: {@value #CUSTOMER1_NAME}</li>
	 * <li>age: {@value #CUSTOMER1_AGE}</li>
	 * </ul>
	 * </li>
	 * <li>{@value #CUSTOMER2_URI}
	 * <ul>
	 * <li>urf.type: Customer</li>
	 * <li>name: {@value #CUSTOMER2_NAME}</li>
	 * <li>age: {@value #CUSTOMER2_AGE}</li>
	 * </ul>
	 * </li>
	 * <li>{@value #ORDER2_URI}
	 * <ul>
	 * <li>urf.type: Order</li>
	 * </ul>
	 * </li>
	 * </ul>
	 * @param assertionStore The assertion store to populate.
	 * @return A populated assertion store for testing.
	 * @throws NullPointerException if the given assertion store is <code>null</code>.
	 */
	protected URFAssertionStore populateTestURFAssertionStore(final URFAssertionStore assertionStore)
	{
		//customer1
		assertionStore.addAssertion(CUSTOMER1_URI, URF.TYPE_PROPERTY_URI, CUSTOMER_CLASS_URI);
		assertionStore.addAssertion(CUSTOMER1_URI, NAME_PROPERTY_URI, CUSTOMER1_NAME);
		assertionStore.addAssertion(CUSTOMER1_URI, AGE_PROPERTY_URI, CUSTOMER1_AGE);
		//customer2
		assertionStore.addAssertion(CUSTOMER2_URI, URF.TYPE_PROPERTY_URI, CUSTOMER_CLASS_URI);
		assertionStore.addAssertion(CUSTOMER2_URI, NAME_PROPERTY_URI, CUSTOMER2_NAME);
		assertionStore.addAssertion(CUSTOMER2_URI, AGE_PROPERTY_URI, CUSTOMER2_AGE);
		//order1
		assertionStore.addAssertion(ORDER1_URI, URF.TYPE_PROPERTY_URI, ORDER_CLASS_URI);
		return assertionStore;
	}

}
