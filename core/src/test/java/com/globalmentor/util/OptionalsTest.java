/*
 * Copyright © 2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.util;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Optional;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Optionals}.
 * @author Garret Wilson
 */
public class OptionalsTest {

	/** @see Optionals#optionally(boolean) */
	@Test
	void testOptionally() {
		assertThat("true produces Optional containing true", Optionals.optionally(true), is(Optional.of(Boolean.TRUE)));
		assertThat("false produces empty Optional", Optionals.optionally(false), is(Optional.empty()));
	}

	/** @see Optionals#fold(Optional, Optional, java.util.function.BinaryOperator) */
	@Test
	void testFold() {
		assertThat(Optionals.fold(Optional.of("foo"), Optional.of("bar"), String::concat), is(Optional.of("foobar")));
		assertThat(Optionals.fold(Optional.of("foo"), Optional.empty(), String::concat), is(Optional.of("foo")));
		assertThat(Optionals.fold(Optional.empty(), Optional.of("bar"), String::concat), is(Optional.of("bar")));
		assertThat(Optionals.fold(Optional.empty(), Optional.empty(), String::concat), is(Optional.empty()));
	}

	/** @see Optionals#or(Optional, Optional) */
	@Test
	void testOr() {
		assertThat(Optionals.or(Optional.of("foo"), Optional.of("bar")), is(Optional.of("foo")));
		assertThat(Optionals.or(Optional.of("foo"), Optional.empty()), is(Optional.of("foo")));
		assertThat(Optionals.or(Optional.empty(), Optional.of("bar")), is(Optional.of("bar")));
		assertThat(Optionals.or(Optional.empty(), Optional.empty()), is(Optional.empty()));
	}

}
