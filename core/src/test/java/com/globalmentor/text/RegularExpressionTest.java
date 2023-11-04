/*
 * Copyright © 2020 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.text;

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static com.globalmentor.text.RegularExpression.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Optional;
import java.util.regex.Pattern;

import static org.hamcrest.MatcherAssert.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link RegularExpression}.
 * @author Garret Wilson
 *
 */
public class RegularExpressionTest {

	/** @see RegularExpression#QUOTED_STRING */
	@Test
	void testQuotedString() {
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

	/** @see RegularExpression#QUOTED_STRING_ALLOWING_ESCAPE_QUOTE */
	@Test
	void testQuotedStringAllowingEscapeQuote() {
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

	/** @see RegularExpression#QUOTED_STRING_ALLOWING_ESCAPE_ANYTHING */
	@Test
	void testQuotedStringAllowingEscapeAnything() {
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

	/**
	 * @see RegularExpression#checkArgumentMatches(CharSequence, Pattern)
	 * @see RegularExpression#checkArgumentMatches(CharSequence, Pattern, String, Object...)
	 */
	@Test
	void testCheckArgumentMatches() {
		final Pattern fooBarPattern = Pattern.compile("(foo)(?:bar)?");
		checkArgumentMatches("foo", fooBarPattern);
		checkArgumentMatches("foobar", fooBarPattern);
		checkArgumentMatches("foo", fooBarPattern, "error message");
		checkArgumentMatches("foo", fooBarPattern, "error message", 123);
		checkArgumentMatches("foo", fooBarPattern, "error message %d", 123);
		final IllegalArgumentException illegalArgumentException = assertThrows(IllegalArgumentException.class, () -> checkArgumentMatches("bar", fooBarPattern),
				"non-matching input");
		assertThat(illegalArgumentException.getMessage(), is("Input `bar` did not match pattern `(foo)(?:bar)?`."));
	}

	/** @see RegularExpression#findMatch(CharSequence, Pattern) */
	@Test
	void testFindMatch() {
		final Pattern fooBarPattern = Pattern.compile("(foo)(?:bar)?");
		assertThat(findMatch(fooBarPattern, "foo"), isPresent());
		assertThat(findMatch(fooBarPattern, "foo").orElseThrow().group(1), is("foo"));
		assertThat(findMatch(fooBarPattern, "bar"), is(Optional.empty()));
		assertThat(findMatch(fooBarPattern, "foobar"), isPresent());
		assertThat(findMatch(fooBarPattern, "foobar").orElseThrow().group(1), is("foo"));
	}

}
