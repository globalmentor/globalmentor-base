/*
 * Copyright © 2023 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.collections;

import static java.util.Collections.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Sets}.
 * @author Garret Wilson
 */
public class SetsTest {

	/** @see Sets#union(Set, Set) */
	@Test
	void testUnion() {
		assertThat(Sets.union(Set.of(), Set.of()), is(emptySet()));
		assertThat(Sets.union(Set.of("a"), Set.of()), is(Set.of("a")));
		assertThat(Sets.union(Set.of("a", "b"), Set.of()), is(Set.of("a", "b")));
		assertThat(Sets.union(Set.of("a", "b", "c"), Set.of()), is(Set.of("a", "b", "c")));
		assertThat(Sets.union(Set.of(), Set.of("x")), is(Set.of("x")));
		assertThat(Sets.union(Set.of(), Set.of("x", "y")), is(Set.of("x", "y")));
		assertThat(Sets.union(Set.of(), Set.of("x", "y", "z")), is(Set.of("x", "y", "z")));
		assertThat(Sets.union(Set.of("a"), Set.of("x")), is(Set.of("a", "x")));
		assertThat(Sets.union(Set.of("a", "b"), Set.of("x")), is(Set.of("a", "b", "x")));
		assertThat(Sets.union(Set.of("a"), Set.of("x", "y")), is(Set.of("a", "x", "y")));
		assertThat(Sets.union(Set.of("a", "b"), Set.of("x", "y")), is(Set.of("a", "b", "x", "y")));
		assertThat(Sets.union(Set.of("a", "b", "c"), Set.of("x", "y", "z")), is(Set.of("a", "b", "c", "x", "y", "z")));
	}

	/** @see Sets#unionCopyOf(Set, Object) */
	@Test
	void testUnionCopyOfSetElement() {
		assertThrows(NullPointerException.class, () -> Sets.unionCopyOf(Set.of(), null));
		assertThat(Sets.unionCopyOf(Set.of(), "other")::iterator, containsInAnyOrder("other"));
		assertThat(Sets.unionCopyOf(Set.of("foo"), "other")::iterator, containsInAnyOrder("foo", "other"));
		assertThat(Sets.unionCopyOf(Set.of("foo", "bar"), "other")::iterator, containsInAnyOrder("foo", "bar", "other"));
		assertThat(Sets.unionCopyOf(Set.of("one", "two", "three"), "other")::iterator, containsInAnyOrder("one", "two", "three", "other"));
		assertThrows(NullPointerException.class, () -> Sets.unionCopyOf(Set.of("one", "two", "three"), null));
	}

}
