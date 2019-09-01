/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

import static org.hamcrest.Matchers.*;

import static org.hamcrest.MatcherAssert.*;

import org.junit.jupiter.api.Test;

/**
 * unit tests for the class {@link Arrays}.
 * 
 * @author Magno Nascimento
 */
public class ArraysTest {

	/** Tests whether {@link Arrays#createArray(Object[], int)} is working properly. */
	@Test
	public void testCreateEmptyArray() {
		assertThat(Arrays.createArray(String.class, 10).getClass().getComponentType(), equalTo(String.class));
		assertThat(Arrays.createArray(String.class, 10), equalTo(new String[10]));
	}

	/** Tests whether {@link Arrays#createArray(Class, Object...)} is working properly. */
	@Test
	public void testCreateProvidedValuesArray() {
		final String[] stringArray = Arrays.createArray(String.class, "foo", "bar", "foobar");

		assertThat(stringArray.getClass().getComponentType(), equalTo(String.class));
		assertThat(stringArray, arrayContaining("foo", "bar", "foobar"));
		assertThat(stringArray.length, is(3));
	}

	/** Tests whether {@link Arrays#createArray(Class, int, Object)} is working properly. */
	@Test
	public void testCreateFilledValuesArray() {
		final String[] stringArray = Arrays.createArray(String.class, 3, "foobar");

		assertThat(stringArray.getClass().getComponentType(), equalTo(String.class));
		assertThat(stringArray, arrayContaining("foobar", "foobar", "foobar"));
		assertThat(stringArray.length, is(3));
	}

	/** Tests whether {@link Arrays#createCopy(Object[], int, int)} is working properly. */
	@Test
	public void testCreateArrayCopy() {
		assertThat(Arrays.createCopy(new String[] {"foo", "bar", "foobar"}, 0, 2), arrayContaining("foo", "bar"));
		assertThat(Arrays.createCopy(new String[] {"foo", "bar", "foobar"}, 0, 1).getClass().getComponentType(), equalTo(String.class));
	}

}
