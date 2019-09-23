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
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.NoSuchElementException;
import java.util.stream.Stream;

import org.junit.jupiter.api.*;

/**
 * Tests the {@link Streams} utilities.
 * @author Garret Wilson
 */
public class StreamsTest {

	/** @see Streams#toFindOnly() */
	@Test
	public void testToFindOnly() {
		assertThat(Stream.empty().reduce(toFindOnly()), isEmpty());
		assertThat(Stream.of("first").reduce(toFindOnly()), isPresentAndIs("first"));
		assertThrows(IllegalArgumentException.class, () -> Stream.of("first", "second").reduce(toFindOnly()));
		assertThrows(IllegalArgumentException.class, () -> Stream.of(1, 2, 3).reduce(toFindOnly()));
	}

	/** @see Streams#toOnly() */
	@Test
	public void testToOnly() {
		assertThrows(NoSuchElementException.class, () -> Stream.empty().collect(toOnly()));
		assertThat(Stream.of("first").collect(toOnly()), is("first"));
		assertThrows(IllegalArgumentException.class, () -> Stream.of("first", "second").collect(toOnly()));
		assertThrows(IllegalArgumentException.class, () -> Stream.of(1, 2, 3).collect(toOnly()));
	}

}
