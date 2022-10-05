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

package com.globalmentor.net;

import static com.globalmentor.net.ResourceRecord.*;
import static java.util.Collections.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link ResourceRecord} and its utilities.
 * @author Garret Wilson
 */
public class ResourceRecordTest {

	private final static Map<String, String> TEST_CHARACTER_STRING_ENCODINGS;

	static {
		final Map<String, String> testCharacterStringEncodings = new HashMap<>();
		testCharacterStringEncodings.put("", "");
		testCharacterStringEncodings.put("foo", "foo");
		testCharacterStringEncodings.put("\"foo\"", "\"\\\"foo\\\"\"");
		testCharacterStringEncodings.put("foo\\bar", "foo\\bar");
		testCharacterStringEncodings.put("foo bar", "\"foo bar\"");
		testCharacterStringEncodings.put("foo \\bar", "\"foo \\\\bar\"");
		testCharacterStringEncodings.put("\"foo bar\"", "\"\\\"foo bar\\\"\"");
		TEST_CHARACTER_STRING_ENCODINGS = unmodifiableMap(testCharacterStringEncodings);
	}

	/** @see ResourceRecord#detectCharacterStringEncoded(String) */
	@Test
	public void testDetectCharacterStringEncoded() {
		assertThat(ResourceRecord.detectCharacterStringEncoded(""), is(false));
		assertThat(ResourceRecord.detectCharacterStringEncoded("foo"), is(false));
		assertThat(ResourceRecord.detectCharacterStringEncoded("\"foo\""), is(true));
		assertThrows(IllegalArgumentException.class, () -> ResourceRecord.detectCharacterStringEncoded("\"foo"));
		assertThat(ResourceRecord.detectCharacterStringEncoded("foo bar"), is(false));
		assertThat(ResourceRecord.detectCharacterStringEncoded("\"foo bar\""), is(true));
	}

	/** @see ResourceRecord#normalizeCharacterString(String) */
	@Test
	public void testNormalizeCharacterString() {
		assertThat(ResourceRecord.normalizeCharacterString(""), is(""));
		assertThat(ResourceRecord.normalizeCharacterString("foo"), is("foo"));
		assertThat(ResourceRecord.normalizeCharacterString("\"foo\""), is("\"foo\""));
		assertThrows(IllegalArgumentException.class, () -> ResourceRecord.normalizeCharacterString("\"foo"));
		assertThat(ResourceRecord.normalizeCharacterString("foo\\bar"), is("foo\\bar"));
		assertThat(ResourceRecord.normalizeCharacterString("foo bar"), is("\"foo bar\""));
		assertThat(ResourceRecord.normalizeCharacterString("foo \\bar"), is("\"foo \\\\bar\""));
		assertThat(ResourceRecord.normalizeCharacterString("\"foo bar\""), is("\"foo bar\""));
	}

	/** @see ResourceRecord#normalizeCharacterString(String, boolean) */
	@Test
	public void testNormalizeCharacterStringAlwaysQuote() {
		assertThat(ResourceRecord.normalizeCharacterString("", true), is("\"\""));
		assertThat(ResourceRecord.normalizeCharacterString("foo", true), is("\"foo\""));
		assertThat(ResourceRecord.normalizeCharacterString("\"foo\"", true), is("\"foo\""));
		assertThrows(IllegalArgumentException.class, () -> ResourceRecord.normalizeCharacterString("\"foo", true));
		assertThat(ResourceRecord.normalizeCharacterString("foo\\bar", true), is("\"foo\\\\bar\""));
		assertThat(ResourceRecord.normalizeCharacterString("foo bar", true), is("\"foo bar\""));
		assertThat(ResourceRecord.normalizeCharacterString("foo \\bar", true), is("\"foo \\\\bar\""));
		assertThat(ResourceRecord.normalizeCharacterString("\"foo bar\"", true), is("\"foo bar\""));
	}

	/**
	 * @see ResourceRecord#normalizeCharacterString(String)
	 * @see #TEST_CHARACTER_STRING_ENCODINGS
	 */
	@Test
	public void testEncodeCharacterString() {
		TEST_CHARACTER_STRING_ENCODINGS.forEach((key, value) -> {
			assertThat(key, ResourceRecord.encodeCharacterString(key), is(value));
		});
	}

	/**
	 * @see ResourceRecord#normalizeCharacterString(String, boolean)
	 * @see #TEST_CHARACTER_STRING_ENCODINGS
	 */
	@Test
	public void testEncodeCharacterStringAlwaysQuote() {
		TEST_CHARACTER_STRING_ENCODINGS.forEach((key, value) -> {
			//note that we don't check the entire encoding (e.g. escaped characters), just that all values are quoted
			assertThat(key, ResourceRecord.encodeCharacterString(key, true), startsWith(String.valueOf(CHARACTER_STRING_QUOTE_CHAR)));
			assertThat(key, ResourceRecord.encodeCharacterString(key, true), endsWith(String.valueOf(CHARACTER_STRING_QUOTE_CHAR)));
		});
	}

	/**
	 * @see ResourceRecord#decodeCharactString(String)
	 * @see #TEST_CHARACTER_STRING_ENCODINGS
	 */
	@Test
	public void testDecodeCharacterString() {
		TEST_CHARACTER_STRING_ENCODINGS.forEach((key, value) -> {
			assertThat(value, ResourceRecord.decodeCharactString(value), is(key));
		});
	}

}
