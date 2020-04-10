/*
 * Copyright © 2020 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.collections.comparators;

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link SortOrder}.
 * @author Garret Wilson
 */
public class SortOrderTest {

	/** @see SortOrder#getSign() */
	@Test
	public void getSign() {
		assertThat(SortOrder.DESCENDING.getSign(), is('-'));
		assertThat(SortOrder.ASCENDING.getSign(), is('+'));
	}

	/** @see SortOrder#findFromSign(char) */
	@Test
	public void testFindFromSign() {
		assertThat(SortOrder.findFromSign('-'), isPresentAndIs(SortOrder.DESCENDING));
		assertThat(SortOrder.findFromSign('+'), isPresentAndIs(SortOrder.ASCENDING));
		assertThat(SortOrder.findFromSign('$'), isEmpty());
	}

	/** @see SortOrder#fromSign(char) */
	@Test
	public void testFromSign() {
		assertThat(SortOrder.fromSign('-'), is(SortOrder.DESCENDING));
		assertThat(SortOrder.fromSign('+'), is(SortOrder.ASCENDING));
		assertThrows(IllegalArgumentException.class, () -> SortOrder.fromSign('$'));
	}

	/** @see SortOrder#parse(CharSequence) */
	@Test
	public void testParse() {
		assertThat(SortOrder.parse("-"), is(SortOrder.DESCENDING));
		assertThat(SortOrder.parse("+"), is(SortOrder.ASCENDING));
		assertThrows(IllegalArgumentException.class, () -> SortOrder.parse(""));
		assertThrows(IllegalArgumentException.class, () -> SortOrder.parse("$"));
		assertThrows(IllegalArgumentException.class, () -> SortOrder.parse("fooBar"));
	}

}
