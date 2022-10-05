/*
 * Copyright © 2019 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.collections.iterables;

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static java.util.Arrays.*;
import static java.util.Collections.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.*;

/**
 * Tests the {@link Iterables} utilities.
 * @author Garret Wilson
 * @see Iterables
 */
public class IterablesTest {

	/** @see Iterables#findFirst(Iterable) */
	@Test
	void testFindFirst() {
		assertThat(Iterables.findFirst(emptySet()), isEmpty());
		assertThat(Iterables.findFirst(emptyList()), isEmpty());
		assertThat(Iterables.findFirst(singleton("foo")), isPresentAndIs("foo"));
		assertThat(Iterables.findFirst(singletonList("foo")), isPresentAndIs("foo"));
		assertThat(Iterables.findFirst(asList("foo", "bar")), isPresentAndIs("foo"));
		assertThat(Iterables.findFirst(asList(1, 2, 3)), isPresentAndIs(1));
	}

	/** @see Iterables#findOnly(Iterable) */
	@Test
	void testFindOnly() {
		assertThat(Iterables.findOnly(emptySet()), isEmpty());
		assertThat(Iterables.findOnly(emptyList()), isEmpty());
		assertThat(Iterables.findOnly(singleton("foo")), isPresentAndIs("foo"));
		assertThat(Iterables.findOnly(singletonList("foo")), isPresentAndIs("foo"));
		assertThrows(IllegalArgumentException.class, () -> Iterables.findOnly(asList("foo", "bar")));
		assertThrows(IllegalArgumentException.class, () -> Iterables.findOnly(asList(1, 2, 3)));
	}

	/** @see Iterables#getOnly(Iterable) */
	@Test
	void testGetOnly() {
		assertThrows(NoSuchElementException.class, () -> Iterables.getOnly(emptySet()));
		assertThrows(NoSuchElementException.class, () -> Iterables.getOnly(emptyList()));
		assertThat(Iterables.getOnly(singleton("foo")), is("foo"));
		assertThat(Iterables.getOnly(singletonList("foo")), is("foo"));
		assertThrows(IllegalArgumentException.class, () -> Iterables.getOnly(asList("foo", "bar")));
		assertThrows(IllegalArgumentException.class, () -> Iterables.getOnly(asList(1, 2, 3)));
	}

}
