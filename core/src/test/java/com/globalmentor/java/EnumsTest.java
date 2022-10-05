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

import java.time.Month;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Enums}.
 * @author Garret Wilson
 */
public class EnumsTest {

	@Test
	public void testAsEnum() {
		assertThat(Enums.asEnum(Month.class, ""), isEmpty());
		assertThat(Enums.asEnum(Month.class, "JANUARY"), isPresentAndIs(Month.JANUARY));
		assertThat(Enums.asEnum(Month.class, "january"), isEmpty());
		assertThat(Enums.asEnum(Month.class, "January"), isEmpty());
		assertThat(Enums.asEnum(Month.class, "FEBRUARY"), isPresentAndIs(Month.FEBRUARY));
		assertThat(Enums.asEnum(Month.class, "JULY"), isPresentAndIs(Month.JULY));
		assertThat(Enums.asEnum(Month.class, "DECEMBER"), isPresentAndIs(Month.DECEMBER));
		assertThat(Enums.asEnum(Month.class, "FOOBAR"), isEmpty());
		assertThat(Enums.asEnum(Month.class, "foobar"), isEmpty());
	}
}
