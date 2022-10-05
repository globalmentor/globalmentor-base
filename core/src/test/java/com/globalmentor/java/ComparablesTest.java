/*
 * Copyright © 2022 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.java;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Comparables}.
 * @author Garret Wilson
 */
public class ComparablesTest {

	/** @see Comparables#min(Comparable, Comparable) */
	@Test
	void testMin() {
		assertThat(Comparables.min("foo", "bar"), is("bar"));
		assertThat(Comparables.min("bar", "foo"), is("bar"));
		assertThat(Comparables.min("foo", "foo"), is("foo"));
	}

	/** @see Comparables#min(Comparable, Comparable) */
	@Test
	void testMax() {
		assertThat(Comparables.max("foo", "bar"), is("foo"));
		assertThat(Comparables.max("bar", "foo"), is("foo"));
		assertThat(Comparables.max("foo", "foo"), is("foo"));
	}

}
