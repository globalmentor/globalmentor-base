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

import java.util.*;
import java.util.AbstractMap.SimpleEntry;
import java.util.stream.Stream;

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static com.globalmentor.java.CharSequences.*;
import static java.util.Arrays.*;
import static java.util.stream.Collectors.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import org.junit.jupiter.api.*;

import com.globalmentor.collections.ListsTest;

/**
 * Tests of {@link CharSequences}.
 * @author Garret Wilson
 */
public class CharSequencesTest {

	/** @see CharSequences#unescapeHex(CharSequence, char, int) */
	@Test
	public void testUnescapeHex() {
		final String input = "abc";
		//Four lengths of UTF-8 sequences:
		//$: 0x24
		//¢: 0xC2 0xA2
		//€: 0xE2 0x82 0xAC
		//😂 : 0xF0 0x9F 0x98 0x82 
		final Map<String, String> escapeSequences = Stream.of( //TODO switch to Java 9 Map.of()
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

	/**
	 * @see CharSequences#longestCommonSegmentSuffix(List, char)
	 * @see ListsTest#testLongestCommonSuffix()
	 */
	@Test
	public void testLongestCommonSegmentSuffix() {
		assertThat(longestCommonSegmentSuffix(asList(), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("foo", ""), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("foo.", ""), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("", "bar"), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList(".", "bar"), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("foo.bar", ""), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("foo.bar.", ""), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("", "foo.bar"), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList(".", "foo.bar"), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("foo", "bar"), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("foo.", "bar"), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("foo.bar", "bar.foo"), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("foo.bar.", "bar.foo"), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("foo.bar", "bar.foo."), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("foo.bar.", "bar.foo."), '.'), isEmpty());
		assertThat(longestCommonSegmentSuffix(asList("bar", "bar"), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("bar.", "bar"), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("bar", "bar."), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("bar.", "bar."), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("foo.bar", "bar"), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("foo.bar.", "bar"), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("foo.bar", "bar."), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("foo.bar.", "bar."), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("bar", "foo.bar"), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("bar.", "foo.bar"), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("bar", "foo.bar."), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("bar.", "foo.bar."), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList(".bar.", "foo.bar."), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList(".bar", "foo.bar."), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("bar.", ".foo.bar."), '.'), isPresentAndIs("bar"));
		assertThat(longestCommonSegmentSuffix(asList("foo.bar", "foo.bar"), '.'), isPresentAndIs("foo.bar"));
		assertThat(longestCommonSegmentSuffix(asList("foo.bar.", "foo.bar"), '.'), isPresentAndIs("foo.bar"));
		assertThat(longestCommonSegmentSuffix(asList("foo.bar", "foo.bar."), '.'), isPresentAndIs("foo.bar"));
		assertThat(longestCommonSegmentSuffix(asList("foo.bar.", "foo.bar."), '.'), isPresentAndIs("foo.bar"));
		assertThat(longestCommonSegmentSuffix(asList("example.foo.bar", "test.foo.bar"), '.'), isPresentAndIs("foo.bar"));
		assertThat(longestCommonSegmentSuffix(asList("www.example.com", "example.com"), '.'), isPresentAndIs("example.com"));
		assertThat(longestCommonSegmentSuffix(asList("example.com", "www.example.com"), '.'), isPresentAndIs("example.com"));
		assertThat(longestCommonSegmentSuffix(asList("example.com", "www.example.com", "test.example.com"), '.'), isPresentAndIs("example.com"));
		assertThat(longestCommonSegmentSuffix(asList("www.example.com", "test.example.com"), '.'), isPresentAndIs("example.com"));
	}

	/** @see CharSequences#removeMarks(CharSequence) */
	@Test
	public void testRemoveMarks() {
		assertThat(removeMarks("foo"), is("foo"));
		assertThat(removeMarks("touch\u00E9"), is("touche")); //touché precomposed
		assertThat(removeMarks("touch\u0065\u0301"), is("touche")); //touché decomposed
		assertThat(removeMarks("Æneas"), is("Æneas"));
		assertThat(removeMarks("ﬁ"), is("ﬁ")); //removing marks doesn't change ligatures
		assertThat(removeMarks("हिंदी"), is("हद")); //hindi->hd
		assertThat(removeMarks("x\u20DD"), is("x")); //enclosing circle
	}

	/** @see CharSequences#normalizeForSearch(CharSequence) */
	@Test
	public void testNormalizeForSearch() {
		assertThat(normalizeForSearch("foo"), is("foo"));
		assertThat(normalizeForSearch("touch\u00E9"), is("touche")); //touché precomposed
		assertThat(normalizeForSearch("touch\u0065\u0301"), is("touche")); //touché decomposed
		assertThat(normalizeForSearch("Æneas"), is("æneas")); //TODO check into how we can wind up with "aeneas"
		assertThat(normalizeForSearch("ﬁ"), is("fi"));
		assertThat(normalizeForSearch("हिंदी"), is("हद")); //hindi->hd
		assertThat(normalizeForSearch("x\u20DD"), is("x")); //enclosing circle
	}

}
