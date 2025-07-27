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

package com.globalmentor.text.csv;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.text.ParseException;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link CsvParser}.
 * @author Garret Wilson
 */
public class CsvParserTest {

	/** @see CsvParser#parseLine(CharSequence). */
	@Test
	void testParseLine() throws ParseException {
		assertThat(CsvParser.parseLine(""), arrayContaining(hasToString("")));
		assertThat(CsvParser.parseLine("x"), arrayContaining(hasToString("x")));
		assertThat(CsvParser.parseLine("foo"), arrayContaining(hasToString("foo")));
		assertThat(CsvParser.parseLine("foo,bar"), arrayContaining(hasToString("foo"), hasToString("bar")));
		assertThat(CsvParser.parseLine("foo,bar,baz"), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("baz")));
		assertThat(CsvParser.parseLine(",bar"), arrayContaining(hasToString(""), hasToString("bar")));
		assertThat(CsvParser.parseLine("foo ,bar"), arrayContaining(hasToString("foo "), hasToString("bar")));
		assertThat(CsvParser.parseLine("foo, bar"), arrayContaining(hasToString("foo"), hasToString(" bar")));
		assertThat(CsvParser.parseLine("foo,bar,"), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("")));
		assertThat(CsvParser.parseLine("\"\""), arrayContaining(hasToString("")));
		assertThrows(ParseException.class, () -> CsvParser.parseLine("\""));
		assertThrows(ParseException.class, () -> CsvParser.parseLine("\"x"));
		assertThrows(ParseException.class, () -> CsvParser.parseLine("\"foo"));
		assertThat(CsvParser.parseLine("\"foo\""), arrayContaining(hasToString("foo")));
		assertThat(CsvParser.parseLine("\"foo\nbar\""), arrayContaining(hasToString("foo\nbar")));
		assertThrows(ParseException.class, () -> CsvParser.parseLine("\"foo\" "));
		assertThrows(ParseException.class, () -> CsvParser.parseLine("\"foo\"x"));
		assertThat(CsvParser.parseLine("\"foo\","), arrayContaining(hasToString("foo"), hasToString("")));
		assertThat(CsvParser.parseLine("\"foo\",bar"), arrayContaining(hasToString("foo"), hasToString("bar")));
		assertThrows(ParseException.class, () -> CsvParser.parseLine("\"foo\" ,bar"));
		assertThrows(ParseException.class, () -> CsvParser.parseLine("\"foo\" ,\"bar\""));
		assertThat(CsvParser.parseLine("\"foo\",\"bar\""), arrayContaining(hasToString("foo"), hasToString("bar")));
		assertThat(CsvParser.parseLine("\"foo\",\"bar\",baz"), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("baz")));
		assertThat(CsvParser.parseLine("\"foo\",\"bar\",\"baz\""), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("baz")));
		assertThat(CsvParser.parseLine("\"foo\",bar,\"baz\""), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("baz")));
		assertThat(CsvParser.parseLine("foo,bar,\"baz\""), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("baz")));
		assertThrows(ParseException.class, () -> CsvParser.parseLine("\"\"\""));
		assertThat(CsvParser.parseLine("\"\"\"\""), arrayContaining(hasToString("\"")));
		assertThrows(ParseException.class, () -> CsvParser.parseLine("\"\"\"\"\""));
		assertThat(CsvParser.parseLine("\"\"\"\"\"\""), arrayContaining(hasToString("\"\"")));
		assertThat(CsvParser.parseLine("\"a\"\"\""), arrayContaining(hasToString("a\"")));
		assertThat(CsvParser.parseLine("\"\"\"z\""), arrayContaining(hasToString("\"z")));
		assertThat(CsvParser.parseLine("\"a\"\"z\""), arrayContaining(hasToString("a\"z")));
		assertThat(CsvParser.parseLine("foo\"\"bar\"\""), arrayContaining(hasToString("foo\"\"bar\"\"")));
		assertThat(CsvParser.parseLine("\"foo\"\"bar\"\"\""), arrayContaining(hasToString("foo\"bar\"")));
		assertThat(CsvParser.parseLine("\"foo\"\"bar\"\"baz\""), arrayContaining(hasToString("foo\"bar\"baz")));
		assertThat(CsvParser.parseLine("\"foo\"\"bar\"\"\",baz"), arrayContaining(hasToString("foo\"bar\""), hasToString("baz")));
	}

}
