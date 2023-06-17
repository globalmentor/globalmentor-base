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

package com.globalmentor.model;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link LanguageTag}.
 * @author Garret Wilson
 */
public class LanguageTagTest {

	/** @see LanguageTag#hashCode() */
	@Test
	void testHashCode() {
		assertThat("Language tags with identical case.", LanguageTag.of("en-US").hashCode(), is(LanguageTag.of("en-US").hashCode()));
		assertThat("Language tags with different case.", LanguageTag.of("en-UK").hashCode(), is(LanguageTag.of("EN-uk").hashCode()));
	}

	/** @see LanguageTag#equals(Object)() */
	@Test
	void testEquals() {
		assertThat("Language tags with identical case.", LanguageTag.of("en-US"), is(equalTo(LanguageTag.of("en-US"))));
		assertThat("Language tags with different case.", LanguageTag.of("en-UK"), is(equalTo(LanguageTag.of("EN-uk"))));
	}

	/** @see LanguageTag#toString() */
	@Test
	void testToString() {
		assertThat(LanguageTag.of("en-US").toString(), is("en-US"));
	}

}
