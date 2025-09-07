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
		final CsvParser testCsvParser = new CsvParser();
		assertThat(testCsvParser.parseLine(""), arrayContaining(hasToString("")));
		assertThat(testCsvParser.parseLine("x"), arrayContaining(hasToString("x")));
		assertThat(testCsvParser.parseLine("foo"), arrayContaining(hasToString("foo")));
		assertThat(testCsvParser.parseLine("foo,bar"), arrayContaining(hasToString("foo"), hasToString("bar")));
		assertThat(testCsvParser.parseLine("foo,bar,baz"), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("baz")));
		assertThat(testCsvParser.parseLine(",bar"), arrayContaining(hasToString(""), hasToString("bar")));
		assertThat(testCsvParser.parseLine("foo ,bar"), arrayContaining(hasToString("foo "), hasToString("bar")));
		assertThat(testCsvParser.parseLine("foo, bar"), arrayContaining(hasToString("foo"), hasToString(" bar")));
		assertThat(testCsvParser.parseLine("foo,bar,"), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("")));
		assertThat(testCsvParser.parseLine("\"\""), arrayContaining(hasToString("")));
		assertThrows(ParseException.class, () -> testCsvParser.parseLine("\""));
		assertThrows(ParseException.class, () -> testCsvParser.parseLine("\"x"));
		assertThrows(ParseException.class, () -> testCsvParser.parseLine("\"foo"));
		assertThat(testCsvParser.parseLine("\"foo\""), arrayContaining(hasToString("foo")));
		assertThat(testCsvParser.parseLine("\"foo\nbar\""), arrayContaining(hasToString("foo\nbar")));
		assertThrows(ParseException.class, () -> testCsvParser.parseLine("\"foo\" "));
		assertThrows(ParseException.class, () -> testCsvParser.parseLine("\"foo\"x"));
		assertThat(testCsvParser.parseLine("\"foo\","), arrayContaining(hasToString("foo"), hasToString("")));
		assertThat(testCsvParser.parseLine("\"foo\",bar"), arrayContaining(hasToString("foo"), hasToString("bar")));
		assertThrows(ParseException.class, () -> testCsvParser.parseLine("\"foo\" ,bar"));
		assertThrows(ParseException.class, () -> testCsvParser.parseLine("\"foo\" ,\"bar\""));
		assertThat(testCsvParser.parseLine("\"foo\",\"bar\""), arrayContaining(hasToString("foo"), hasToString("bar")));
		assertThat(testCsvParser.parseLine("\"foo\",\"bar\",baz"), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("baz")));
		assertThat(testCsvParser.parseLine("\"foo\",\"bar\",\"baz\""), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("baz")));
		assertThat(testCsvParser.parseLine("\"foo\",bar,\"baz\""), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("baz")));
		assertThat(testCsvParser.parseLine("foo,bar,\"baz\""), arrayContaining(hasToString("foo"), hasToString("bar"), hasToString("baz")));
		assertThrows(ParseException.class, () -> testCsvParser.parseLine("\"\"\""));
		assertThat(testCsvParser.parseLine("\"\"\"\""), arrayContaining(hasToString("\"")));
		assertThrows(ParseException.class, () -> testCsvParser.parseLine("\"\"\"\"\""));
		assertThat(testCsvParser.parseLine("\"\"\"\"\"\""), arrayContaining(hasToString("\"\"")));
		assertThat(testCsvParser.parseLine("\"a\"\"\""), arrayContaining(hasToString("a\"")));
		assertThat(testCsvParser.parseLine("\"\"\"z\""), arrayContaining(hasToString("\"z")));
		assertThat(testCsvParser.parseLine("\"a\"\"z\""), arrayContaining(hasToString("a\"z")));
		assertThat(testCsvParser.parseLine("foo\"\"bar\"\""), arrayContaining(hasToString("foo\"\"bar\"\"")));
		assertThat(testCsvParser.parseLine("\"foo\"\"bar\"\"\""), arrayContaining(hasToString("foo\"bar\"")));
		assertThat(testCsvParser.parseLine("\"foo\"\"bar\"\"baz\""), arrayContaining(hasToString("foo\"bar\"baz")));
		assertThat(testCsvParser.parseLine("\"foo\"\"bar\"\"\",baz"), arrayContaining(hasToString("foo\"bar\""), hasToString("baz")));
	}

}
