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

package com.globalmentor.text;

import static com.globalmentor.text.RegularExpressions.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link RegularExpressions}.
 * @implNote Some test strings are from <cite>Mastering Regular Expressions, Third Edition</cite>.
 * @author Garret Wilson
 *
 */
public class RegularExpressionsTest {

	/** @see RegularExpressions#QUOTED_STRING */
	@Test
	public void testQuotedString() {
		assertThat("".matches(QUOTED_STRING), is(false));
		assertThat("\"".matches(QUOTED_STRING), is(false));
		assertThat("\"\"".matches(QUOTED_STRING), is(true));
		assertThat("\"a\"".matches(QUOTED_STRING), is(true));
		assertThat("\"ab\"".matches(QUOTED_STRING), is(true));
		assertThat("\"abc\"".matches(QUOTED_STRING), is(true));
		assertThat("\"abc123\"".matches(QUOTED_STRING), is(true));
		assertThat("\"foobar\"".matches(QUOTED_STRING), is(true));
		assertThat("\\\"foobar\"".matches(QUOTED_STRING), is(false));
		assertThat("\"foobar\\\"".matches(QUOTED_STRING), is(true));
		assertThat("foobar\"".matches(QUOTED_STRING), is(false));
		assertThat("\"foobar".matches(QUOTED_STRING), is(false));
		assertThat("\"foo\\\"bar\"".matches(QUOTED_STRING), is(false));
		assertThat("\"\\\"foobar\"".matches(QUOTED_STRING), is(false));
		assertThat("\"foobar\\\"\"".matches(QUOTED_STRING), is(false));
		assertThat("\"fo\\\"oba\\\"r\\\"\"".matches(QUOTED_STRING), is(false));
		assertThat("\"foo\\nbar\"".matches(QUOTED_STRING), is(true));
		assertThat("\"foo\\tbar\"".matches(QUOTED_STRING), is(true));
		assertThat("\"foo\\xbar\"".matches(QUOTED_STRING), is(true));
	}

	/** @see RegularExpressions#QUOTED_STRING_ALLOWING_ESCAPE_QUOTE */
	@Test
	public void testQuotedStringAllowingEscapeQuote() {
		assertThat("".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(false));
		assertThat("\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(false));
		assertThat("\"\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(true));
		assertThat("\"a\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(true));
		assertThat("\"ab\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(true));
		assertThat("\"abc\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(true));
		assertThat("\"abc123\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(true));
		assertThat("\"foobar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(true));
		assertThat("\\\"foobar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(false));
		assertThat("\"foobar\\\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(false));
		assertThat("foobar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(false));
		assertThat("\"foobar".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(false));
		assertThat("\"foo\\\"bar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(true));
		assertThat("\"\\\"foobar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(true));
		assertThat("\"foobar\\\"\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(true));
		assertThat("\"fo\\\"oba\\\"r\\\"\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(true));
		assertThat("\"foo\\nbar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(false));
		assertThat("\"foo\\tbar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(false));
		assertThat("\"foo\\xbar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_QUOTE), is(false));
	}

	/** @see RegularExpressions#QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING */
	@Test
	public void testQuotedStringAllowingEscapeAnything() {
		assertThat("".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(false));
		assertThat("\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(false));
		assertThat("\"\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"a\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"ab\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"abc\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"abc123\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"foobar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\\\"foobar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(false));
		assertThat("\"foobar\\\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(false));
		assertThat("foobar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(false));
		assertThat("\"foobar".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(false));
		assertThat("\"foo\\\"bar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"\\\"foobar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"foobar\\\"\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"fo\\\"oba\\\"r\\\"\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"foo\\nbar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"foo\\tbar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
		assertThat("\"foo\\xbar\"".matches(QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING), is(true));
	}

}
