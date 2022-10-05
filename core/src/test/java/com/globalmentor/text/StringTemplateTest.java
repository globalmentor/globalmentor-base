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

package com.globalmentor.text;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import org.junit.jupiter.api.*;

/**
 * Tests of {@link StringTemplate}.
 * @author Garret Wilson
 */
public class StringTemplateTest {

	@Test
	public void testNoReplacements() {
		assertThat(StringTemplate.of().apply(), is(""));
		assertThat(StringTemplate.of("foo").apply(), is("foo"));
		assertThat(StringTemplate.of("foo", "bar").apply(), is("foobar"));
		assertThat(StringTemplate.of("foo", "12345", "Bar").apply(), is("foo12345Bar"));
	}

	@Test
	public void testStringParameter() {
		assertThat(StringTemplate.of(StringTemplate.STRING_PARAMETER).apply("example"), is("example"));
		assertThat(StringTemplate.of(StringTemplate.STRING_PARAMETER, "foo").apply("example"), is("examplefoo"));
		assertThat(StringTemplate.of("foo", StringTemplate.STRING_PARAMETER).apply("example"), is("fooexample"));
		assertThat(StringTemplate.of("foo", StringTemplate.STRING_PARAMETER, "bar").apply("example"), is("fooexamplebar"));
		assertThat(StringTemplate.of("foo", "bar", StringTemplate.STRING_PARAMETER).apply("example"), is("foobarexample"));
	}

	@Test
	public void testNewline() {
		assertThat(StringTemplate.of(StringTemplate.NEWLINE).apply(), is(System.lineSeparator()));
		assertThat(StringTemplate.of(StringTemplate.NEWLINE, "foo").apply(), is(System.lineSeparator() + "foo"));
		assertThat(StringTemplate.of("foo", StringTemplate.NEWLINE).apply(), is("foo" + System.lineSeparator()));
		assertThat(StringTemplate.of(StringTemplate.STRING_PARAMETER, StringTemplate.NEWLINE).apply("example"), is("example" + System.lineSeparator()));
		assertThat(StringTemplate.of("foo", StringTemplate.NEWLINE, StringTemplate.STRING_PARAMETER).apply("example"),
				is("foo" + System.lineSeparator() + "example"));
		assertThat(StringTemplate.of("foo", StringTemplate.STRING_PARAMETER, StringTemplate.NEWLINE).apply("example"), is("fooexample" + System.lineSeparator()));
		assertThat(StringTemplate.of("foo", StringTemplate.STRING_PARAMETER, "bar", StringTemplate.NEWLINE).apply("example"),
				is("fooexamplebar" + System.lineSeparator()));
	}

}
