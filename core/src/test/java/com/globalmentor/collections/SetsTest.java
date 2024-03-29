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

	/** @see Sets#toUnion(Set, Set) */
	@Test
	void testToUnion() {
		assertThat(Sets.toUnion(Set.of(), Set.of()), is(emptySet()));
		final var setA = Set.of("a");
		assertThat(Sets.toUnion(setA, Set.of()), is(sameInstance(setA)));
		assertThat(Sets.toUnion(Set.of("a", "b"), Set.of()), is(Set.of("a", "b")));
		assertThat(Sets.toUnion(Set.of("a", "b", "c"), Set.of()), is(Set.of("a", "b", "c")));
		final var setX = Set.of("x");
		assertThat(Sets.toUnion(Set.of(), setX), is(sameInstance(setX)));
		assertThat(Sets.toUnion(Set.of(), Set.of("x", "y")), is(Set.of("x", "y")));
		assertThat(Sets.toUnion(Set.of(), Set.of("x", "y", "z")), is(Set.of("x", "y", "z")));
		assertThat(Sets.toUnion(Set.of("a"), Set.of("x")), is(Set.of("a", "x")));
		assertThat(Sets.toUnion(Set.of("a", "b"), Set.of("x")), is(Set.of("a", "b", "x")));
		assertThat(Sets.toUnion(Set.of("a"), Set.of("x", "y")), is(Set.of("a", "x", "y")));
		assertThat(Sets.toUnion(Set.of("a", "b"), Set.of("x", "y")), is(Set.of("a", "b", "x", "y")));
		assertThat(Sets.toUnion(Set.of("a", "b", "c"), Set.of("x", "y", "z")), is(Set.of("a", "b", "c", "x", "y", "z")));
	}

	/** @see Sets#unionCopyOf(Collection, Collection) */
	@Test
	void testUnionCopyOfCollectionCollection() {
		assertThat(Sets.unionCopyOf(Set.of(), Set.of()), is(emptySet()));
		assertThat(Sets.unionCopyOf(Set.of("foo"), Set.of()), containsInAnyOrder("foo"));
		assertThat(Sets.unionCopyOf(Set.of(), Set.of("other")), containsInAnyOrder("other"));
		assertThat(Sets.unionCopyOf(Set.of("foo"), Set.of("other")), containsInAnyOrder("foo", "other"));
		assertThat(Sets.unionCopyOf(Set.of("foo", "bar"), Set.of("other")), containsInAnyOrder("foo", "bar", "other"));
		assertThrows(NullPointerException.class, () -> Sets.unionCopyOf(Arrays.asList("foo", null, "bar"), Set.of("other")));
		assertThat(Sets.unionCopyOf(Set.of("foo", "bar"), Set.of("other", "one")), containsInAnyOrder("foo", "bar", "other", "one"));
		assertThat(Sets.unionCopyOf(Set.of("one", "two", "three"), Set.of("other")), containsInAnyOrder("one", "two", "three", "other"));
		assertThat(Sets.unionCopyOf(Set.of("one", "two", "three"), Set.of("other", "one")), containsInAnyOrder("one", "two", "three", "other"));
		assertThat(Sets.unionCopyOf(List.of("one", "two", "three", "two", "three"), List.of("other", "one")), containsInAnyOrder("one", "two", "three", "other"));
		assertThrows(NullPointerException.class, () -> Sets.unionCopyOf(Set.of("one", "two", "three"), singleton(null)));
	}

	/** @see Sets#unionCopyOf(Collection, Object) */
	@Test
	void testUnionCopyOfCollectionElement() {
		assertThrows(NullPointerException.class, () -> Sets.unionCopyOf(Set.of(), null));
		assertThat(Sets.unionCopyOf(Set.of(), "other"), containsInAnyOrder("other"));
		assertThat(Sets.unionCopyOf(Set.of("foo"), "other"), containsInAnyOrder("foo", "other"));
		assertThat(Sets.unionCopyOf(Set.of("foo", "bar"), "other"), containsInAnyOrder("foo", "bar", "other"));
		assertThat(Sets.unionCopyOf(Set.of("one", "two", "three"), "other"), containsInAnyOrder("one", "two", "three", "other"));
		assertThat(Sets.unionCopyOf(List.of("one", "two", "three", "two", "three"), "other"), containsInAnyOrder("one", "two", "three", "other"));
		assertThrows(NullPointerException.class, () -> Sets.unionCopyOf(Set.of("one", "two", "three"), null));
	}

}
