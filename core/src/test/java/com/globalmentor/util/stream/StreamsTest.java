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
		assertThat(Streams.zip(Stream.empty(), Stream.empty(), (a, b) -> null, null, null, true).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.empty(), Stream.empty(), (a, b) -> null, null, null, false).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of("foo"), Stream.of("bar"), (a, b) -> a + b, null, null, true).collect(toList()), contains("foobar"));
		assertThat(Streams.zip(Stream.of("foo"), Stream.of("bar"), (a, b) -> a + b, null, null, false).collect(toList()), contains("foobar"));
		assertThat(Streams.zip(Stream.of("foo"), Stream.of(), (a, b) -> a + b, "X", "Y", true).collect(toList()), contains("fooY"));
		assertThat(Streams.zip(Stream.of("foo"), Stream.of(), (a, b) -> a + b, "X", "Y", false).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of(), Stream.of("bar"), (a, b) -> a + b, "X", "Y", true).collect(toList()), contains("Xbar"));
		assertThat(Streams.zip(Stream.of(), Stream.of("bar"), (a, b) -> a + b, "X", "Y", false).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2, 3), (a, b) -> a + b, null, null, true).collect(toList()), contains("A1", "B2", "C3"));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2, 3), (a, b) -> a + b, null, null, false).collect(toList()), contains("A1", "B2", "C3"));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2, 3), AbstractMap.SimpleEntry::new, "X", 0, true).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 1), new AbstractMap.SimpleEntry<>("B", 2), new AbstractMap.SimpleEntry<>("C", 3)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2, 3), AbstractMap.SimpleEntry::new, "X", 0, false).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 1), new AbstractMap.SimpleEntry<>("B", 2), new AbstractMap.SimpleEntry<>("C", 3)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2), AbstractMap.SimpleEntry::new, "X", 0, true).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 1), new AbstractMap.SimpleEntry<>("B", 2), new AbstractMap.SimpleEntry<>("C", 0)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1, 2), AbstractMap.SimpleEntry::new, "X", 0, false).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 1), new AbstractMap.SimpleEntry<>("B", 2)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1), AbstractMap.SimpleEntry::new, "X", 0, true).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 1), new AbstractMap.SimpleEntry<>("B", 0), new AbstractMap.SimpleEntry<>("C", 0)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(1), AbstractMap.SimpleEntry::new, "X", 0, false).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 1)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(), AbstractMap.SimpleEntry::new, "X", 0, true).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 0), new AbstractMap.SimpleEntry<>("B", 0), new AbstractMap.SimpleEntry<>("C", 0)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(), AbstractMap.SimpleEntry::new, "X", 0, false).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(), AbstractMap.SimpleEntry::new, null, null, true).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", null), new AbstractMap.SimpleEntry<>("B", null), new AbstractMap.SimpleEntry<>("C", null)));
		assertThat(Streams.zip(Stream.of("A", "B", "C"), Stream.of(), AbstractMap.SimpleEntry::new, null, null, false).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of("A", "B"), Stream.of(1, 2, 3), AbstractMap.SimpleEntry::new, "X", 0, true).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 1), new AbstractMap.SimpleEntry<>("B", 2), new AbstractMap.SimpleEntry<>("X", 3)));
		assertThat(Streams.zip(Stream.of("A", "B"), Stream.of(1, 2, 3), AbstractMap.SimpleEntry::new, "X", 0, false).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 1), new AbstractMap.SimpleEntry<>("B", 2)));
		assertThat(Streams.zip(Stream.of("A"), Stream.of(1, 2, 3), AbstractMap.SimpleEntry::new, "X", 0, true).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 1), new AbstractMap.SimpleEntry<>("X", 2), new AbstractMap.SimpleEntry<>("X", 3)));
		assertThat(Streams.zip(Stream.of("A"), Stream.of(1, 2, 3), AbstractMap.SimpleEntry::new, "X", 0, false).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("A", 1)));
		assertThat(Streams.zip(Stream.of(), Stream.of(1, 2, 3), AbstractMap.SimpleEntry::new, "X", 0, true).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>("X", 1), new AbstractMap.SimpleEntry<>("X", 2), new AbstractMap.SimpleEntry<>("X", 3)));
		assertThat(Streams.zip(Stream.of(), Stream.of(1, 2, 3), AbstractMap.SimpleEntry::new, "X", 0, false).collect(toList()), is(empty()));
		assertThat(Streams.zip(Stream.of(), Stream.of(1, 2, 3), AbstractMap.SimpleEntry::new, null, null, true).collect(toList()),
				contains(new AbstractMap.SimpleEntry<>(null, 1), new AbstractMap.SimpleEntry<>(null, 2), new AbstractMap.SimpleEntry<>(null, 3)));
		assertThat(Streams.zip(Stream.of(), Stream.of(1, 2, 3), AbstractMap.SimpleEntry::new, null, null, false).collect(toList()), is(empty()));
	}

}
