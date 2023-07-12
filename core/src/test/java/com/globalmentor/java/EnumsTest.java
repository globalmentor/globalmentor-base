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

package com.globalmentor.java;

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.StandardOpenOption;
import java.time.Month;
import java.util.Optional;

import org.junit.jupiter.api.*;

import com.globalmentor.lex.Identifier;

/**
 * Tests of {@link Enums}.
 * @author Garret Wilson
 */
public class EnumsTest {

	@Test
	void testAsEnum() {
		assertThat(Enums.asEnum(Month.class, ""), is(Optional.empty()));
		assertThat(Enums.asEnum(Month.class, "JANUARY"), isPresentAndIs(Month.JANUARY));
		assertThat(Enums.asEnum(Month.class, "january"), is(Optional.empty()));
		assertThat(Enums.asEnum(Month.class, "January"), is(Optional.empty()));
		assertThat(Enums.asEnum(Month.class, "FEBRUARY"), isPresentAndIs(Month.FEBRUARY));
		assertThat(Enums.asEnum(Month.class, "JULY"), isPresentAndIs(Month.JULY));
		assertThat(Enums.asEnum(Month.class, "DECEMBER"), isPresentAndIs(Month.DECEMBER));
		assertThat(Enums.asEnum(Month.class, "FOOBAR"), is(Optional.empty()));
		assertThat(Enums.asEnum(Month.class, "foobar"), is(Optional.empty()));
	}

	/**
	 * Identifier enum for testing serialization/deserialization.
	 * @see Enums#getSerializationName(Enum)
	 * @see Enums#getSerializedEnum(Class, String)
	 */
	private enum TestIdentifier implements Identifier {
		FOO, FOO_BAR
	}

	/**
	 * @see Enums#getSerializationName(Enum)
	 * @see TestIdentifier
	 */
	@Test
	void testGetSerializationName() {
		assertThat(Enums.getSerializationName(TestIdentifier.FOO), is("foo"));
		assertThat(Enums.getSerializationName(TestIdentifier.FOO_BAR), is("foo-bar"));
		assertThat(Enums.getSerializationName(StandardOpenOption.READ), is("READ")); //not marked as identifier
		assertThat(Enums.getSerializationName(StandardOpenOption.CREATE_NEW), is("CREATE_NEW")); //not marked as identifier
	}

	/**
	 * @see Enums#getSerializedEnum(Class, String)
	 * @see TestIdentifier
	 */
	@Test
	void testGetSerializedEnum() {
		assertThat(Enums.getSerializedEnum(TestIdentifier.class, "foo"), is(TestIdentifier.FOO));
		assertThrows(IllegalArgumentException.class, () -> Enums.getSerializedEnum(TestIdentifier.class, "bar"), "No such serialized identifier enum.");
		assertThat(Enums.getSerializedEnum(TestIdentifier.class, "foo-bar"), is(TestIdentifier.FOO_BAR));
		assertThat(Enums.getSerializedEnum(StandardOpenOption.class, "READ"), is(StandardOpenOption.READ)); //not marked as identifier
		assertThrows(IllegalArgumentException.class, () -> Enums.getSerializedEnum(StandardOpenOption.class, "read"), "No such serialized non-identifier enum.");
		assertThat(Enums.getSerializedEnum(StandardOpenOption.class, "CREATE_NEW"), is(StandardOpenOption.CREATE_NEW)); //not marked as identifier
	}

}
