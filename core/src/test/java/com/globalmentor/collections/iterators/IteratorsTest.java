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

package com.globalmentor.collections.iterators;

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static java.util.Arrays.*;
import static java.util.Collections.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Iterator;
import java.util.NoSuchElementException;

import org.junit.jupiter.api.*;

/**
 * Tests the {@link Iterators} utilities.
 * @author Garret Wilson
 * @see Iterators
 */
public class IteratorsTest {

	/** @see Iterators#findNext(Iterator) */
	@Test
	public void testFindFirst() {
		assertThat(Iterators.findNext(emptySet().iterator()), isEmpty());
		assertThat(Iterators.findNext(emptyList().iterator()), isEmpty());
		assertThat(Iterators.findNext(singleton("foo").iterator()), isPresentAndIs("foo"));
		assertThat(Iterators.findNext(singletonList("foo").iterator()), isPresentAndIs("foo"));
		assertThat(Iterators.findNext(asList("foo", "bar").iterator()), isPresentAndIs("foo"));
	}

	/** @see Iterators#findOnly(Iterator) */
	@Test
	public void testFindOnly() {
		assertThat(Iterators.findOnly(emptySet().iterator()), isEmpty());
		assertThat(Iterators.findOnly(emptyList().iterator()), isEmpty());
		assertThat(Iterators.findOnly(singleton("foo").iterator()), isPresentAndIs("foo"));
		assertThat(Iterators.findOnly(singletonList("foo").iterator()), isPresentAndIs("foo"));
		assertThrows(IllegalArgumentException.class, () -> Iterators.findOnly(asList("foo", "bar").iterator()));
		assertThrows(IllegalArgumentException.class, () -> Iterators.findOnly(asList(1, 2, 3).iterator()));
	}

	/** @see Iterators#getOnly(Iterator) */
	@Test
	public void testGetOnly() {
		assertThrows(NoSuchElementException.class, () -> Iterators.getOnly(emptySet().iterator()));
		assertThrows(NoSuchElementException.class, () -> Iterators.getOnly(emptyList().iterator()));
		assertThat(Iterators.getOnly(singleton("foo").iterator()), is("foo"));
		assertThat(Iterators.getOnly(singletonList("foo").iterator()), is("foo"));
		assertThrows(IllegalArgumentException.class, () -> Iterators.getOnly(asList("foo", "bar").iterator()));
		assertThrows(IllegalArgumentException.class, () -> Iterators.getOnly(asList(1, 2, 3).iterator()));
	}

}
