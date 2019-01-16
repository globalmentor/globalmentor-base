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

import java.util.Iterator;

import org.junit.jupiter.api.*;

/**
 * Test of the {@link Iterators} utilities.
 * @author Garret Wilson
 * @see Iterators
 */
public class IteratorsTest {

	/** @see Iterators#findNext(Iterator) */
	@Test
	public void testFindFirst() {
		assertThat(Iterators.findNext(emptySet().iterator()), not(isPresent()));
		assertThat(Iterators.findNext(emptyList().iterator()), not(isPresent()));
		assertThat(Iterators.findNext(singleton("foo").iterator()), isPresentAndIs("foo"));
		assertThat(Iterators.findNext(singletonList("foo").iterator()), isPresentAndIs("foo"));
		assertThat(Iterators.findNext(asList("foo", "bar").iterator()), isPresentAndIs("foo"));
	}

}
