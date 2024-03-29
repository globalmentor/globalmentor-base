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

package com.globalmentor.util.stream;

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static com.globalmentor.util.stream.Streams.*;
import static java.util.stream.Collectors.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;
import java.util.stream.Stream;

import org.junit.jupiter.api.*;

import com.globalmentor.collections.Maps;

/**
 * Tests the {@link Streams} utilities.
 * @author Garret Wilson
 */
public class StreamsTest {

	/** @see Streams#toFindOnly() */
	@Test
	void testToFindOnly() {
		assertThat(Stream.empty().reduce(toFindOnly()), isEmpty());
		assertThat(Stream.of("first").reduce(toFindOnly()), isPresentAndIs("first"));
		assertThrows(IllegalArgumentException.class, () -> Stream.of("first", "second").reduce(toFindOnly()));
		assertThrows(IllegalArgumentException.class, () -> Stream.of(1, 2, 3).reduce(toFindOnly()));
	}

	/** @see Streams#toOnly() */
	@Test
	void testToOnly() {
		assertThrows(NoSuchElementException.class, () -> Stream.empty().collect(toOnly()));
		assertThat(Stream.of("first").collect(toOnly()), is("first"));
		assertThrows(IllegalArgumentException.class, () -> Stream.of("first", "second").collect(toOnly()));
		assertThrows(IllegalArgumentException.class, () -> Stream.of(1, 2, 3).collect(toOnly()));
	}

	/** @see Streams#zip(Stream, Stream, java.util.function.BiFunction, Object, Object, boolean) */
	@Test
	void testZip() {
		assertThat(Streams.zip(Stream.empty(), Stream.empty(), null, null, true, (a, b) -> null).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.empty(), Stream.empty(), null, null, false, (a, b) -> null).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of("foo"), Stream.of("bar"), null, null, true, (a, b) -> a + b).collect(toList()), contains("foobar"));
		assertThat(Streams.zip(Stream.of("foo"), Stream.of("bar"), null, null, false, (a, b) -> a + b).collect(toList()), contains("foobar"));
		assertThat(Streams.zip(Stream.of("foo"), Stream.of(), "X", "Y", true, (a, b) -> a + b).collect(toList()), contains("fooY"));
		assertThat(Streams.zip(Stream.of("foo"), Stream.of(), "X", "Y", false, (a, b) -> a + b).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of(), Stream.of("bar"), "X", "Y", true, (a, b) -> a + b).collect(toList()), contains("Xbar"));
		assertThat(Streams.zip(Stream.of(), Stream.of("bar"), "X", "Y", false, (a, b) -> a + b).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2, 3), null, null, true, (a, b) -> a + b).collect(toList()), contains("A1", "B2", "C3"));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2, 3), null, null, false, (a, b) -> a + b).collect(toList()), contains("A1", "B2", "C3"));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2, 3), "X", 0, true, Maps::entryOfNullables).collect(toList()),
				contains(Map.entry("A", 1), Map.entry("B", 2), Map.entry("C", 3)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2, 3), "X", 0, false, Maps::entryOfNullables).collect(toList()),
				contains(Map.entry("A", 1), Map.entry("B", 2), Map.entry("C", 3)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2), "X", 0, true, Maps::entryOfNullables).collect(toList()),
				contains(Map.entry("A", 1), Map.entry("B", 2), Map.entry("C", 0)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2), "X", 0, false, Maps::entryOfNullables).collect(toList()),
				contains(Map.entry("A", 1), Map.entry("B", 2)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1), "X", 0, true, Maps::entryOfNullables).collect(toList()),
				contains(Map.entry("A", 1), Map.entry("B", 0), Map.entry("C", 0)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1), "X", 0, false, Maps::entryOfNullables).collect(toList()), contains(Map.entry("A", 1)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(), "X", 0, true, Maps::entryOfNullables).collect(toList()),
				contains(Map.entry("A", 0), Map.entry("B", 0), Map.entry("C", 0)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(), "X", 0, false, Maps::entryOfNullables).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(), null, null, true, Maps::entryOfNullables).collect(toList()),
				contains(Maps.entryOfNullables("A", null), Maps.entryOfNullables("B", null), Maps.entryOfNullables("C", null)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(), null, null, false, Maps::entryOfNullables).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of("A", "B"), Stream.of(1, 2, 3), "X", 0, true, Maps::entryOfNullables).collect(toList()),
				contains(Map.entry("A", 1), Map.entry("B", 2), Map.entry("X", 3)));
		assertThat(Streams.zip(Stream.of("A", "B"), Stream.of(1, 2, 3), "X", 0, false, Maps::entryOfNullables).collect(toList()),
				contains(Map.entry("A", 1), Map.entry("B", 2)));
		assertThat(Streams.zip(Stream.of("A"), Stream.of(1, 2, 3), "X", 0, true, Maps::entryOfNullables).collect(toList()),
				contains(Map.entry("A", 1), Map.entry("X", 2), Map.entry("X", 3)));
		assertThat(Streams.zip(Stream.of("A"), Stream.of(1, 2, 3), "X", 0, false, Maps::entryOfNullables).collect(toList()), contains(Map.entry("A", 1)));
		assertThat(Streams.zip(Stream.of(), Stream.of(1, 2, 3), "X", 0, true, Maps::entryOfNullables).collect(toList()),
				contains(Map.entry("X", 1), Map.entry("X", 2), Map.entry("X", 3)));
		assertThat(Streams.zip(Stream.of(), Stream.of(1, 2, 3), "X", 0, false, Maps::entryOfNullables).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of(), Stream.of(1, 2, 3), null, null, true, Maps::entryOfNullables).collect(toList()),
				contains(Maps.entryOfNullables(null, 1), Maps.entryOfNullables(null, 2), Maps.entryOfNullables(null, 3)));
		assertThat(Streams.zip(Stream.of(), Stream.of(1, 2, 3), null, null, false, Maps::entryOfNullables).collect(toList()), is(empty()));
	}

}
