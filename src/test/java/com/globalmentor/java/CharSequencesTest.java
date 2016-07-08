/*
 * Copyright © 2016 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

import static org.junit.Assert.*;

import java.util.AbstractMap.SimpleEntry;
import java.util.Map;
import java.util.stream.Stream;

import static com.globalmentor.java.CharSequences.*;
import static java.util.stream.Collectors.toMap;
import static org.hamcrest.Matchers.*;
import org.junit.*;

/**
 * Tests of {@link CharSequences}.
 * @author Garret Wilson
 */
public class CharSequencesTest {

	/*
	 * Four lengths of UTF-8 sequences:
	 * $: 0x24
	 * ¢: 0xC2 0xA2
	 * €: 0xE2 0x82 0xAC
	 * 😂 : 0xF0 0x9F 0x98 0x82 
	 */

	/** @see CharSequences#unescapeHex(CharSequence, char, int). */
	@Test
	public void testUnescapeHex() {
		final String input = "abc";
		final Map<String, String> escapeSequences = Stream
				.of( //TODO switch to Java 8 Map.of()
						new SimpleEntry<>("$", "^24"), new SimpleEntry<>("¢", "^C2^A2"), new SimpleEntry<>("€", "^E2^82^AC"), new SimpleEntry<>("😂", "^F0^9F^98^82"))
				.collect(toMap(SimpleEntry::getKey, SimpleEntry::getValue));

		assertThat(unescapeHex("", '^', 2).toString(), is(""));
		assertThat(unescapeHex("a", '^', 2).toString(), is("a"));
		assertThat(unescapeHex("ab", '^', 2).toString(), is("ab"));
		assertThat(unescapeHex("abc", '^', 2).toString(), is("abc"));

		escapeSequences.forEach((decoded, encoded) -> {

			assertThat(unescapeHex(encoded, '^', 2).toString(), is(decoded));
			assertThat(unescapeHex("^58" + encoded, '^', 2).toString(), is("X" + decoded));
			assertThat(unescapeHex(encoded + "^58", '^', 2).toString(), is(decoded + "X"));

			//try the escape sequence in every position, including the last
			//e.g. "^24abc", "a^24bc", "ab^24c", "abc^24"
			for(int i = 0; i <= input.length(); i++) {
				final String test = Strings.insert(input, i, encoded);
				final String expected = Strings.insert(input, i, decoded);
				assertThat(unescapeHex(test, '^', 2).toString(), is(expected));
				assertThat(unescapeHex("^58" + test, '^', 2).toString(), is("X" + expected));
				assertThat(unescapeHex(test + "^58", '^', 2).toString(), is(expected + "X"));
			}
		});

	}

}
