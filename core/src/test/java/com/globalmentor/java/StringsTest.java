/*
 * Copyright © 2018 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.java;

import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for the class {@link Strings}.
 * 
 * @author Magno Nascimento
 */
public class StringsTest {

	/** Tests whether {@link Strings#trimBeginning(String, char, int)} is working properly. */
	@Test
	public void trimBeginning() {
		assertThat(Strings.trimBeginningFirst("foobar", 'f'), equalTo("oobar"));
		assertThat(Strings.trimBeginningFirst("foobar", 'o'), equalTo("obar"));
		assertThat(Strings.trimBeginningFirst("foobar", 'r'), equalTo(""));

		assertThat(Strings.trimBeginningLast("foobar", 'f'), equalTo("oobar"));
		assertThat(Strings.trimBeginningLast("foobar", 'o'), equalTo("bar"));
		assertThat(Strings.trimBeginningLast("foobar", 'r'), equalTo(""));

		// tests trimming from the first occurrence.
		assertThat(Strings.trimBeginning("somewhere over the rainbow", 'e', 1), equalTo("where over the rainbow"));
		// tests trimming from an occurrence in the middle of the string.
		assertThat(Strings.trimBeginning("somewhere over the rainbow", 'e', 3), equalTo(" over the rainbow"));
		// tests trimming from the last occurrence.
		assertThat(Strings.trimBeginning("somewhere over the rainbow", 'e', Integer.MAX_VALUE), equalTo(" rainbow"));
	}

	/** Tests whether {@link Strings#trimBeginningFirst(String, char)} is working properly. */
	@Test
	public void trimBeginningFirst() {
		assertThat(Strings.trimBeginningFirst("foobar", 'f'), equalTo("oobar"));
		assertThat(Strings.trimBeginningFirst("foobar", 'o'), equalTo("obar"));
		assertThat(Strings.trimBeginningFirst("foobar", 'r'), equalTo(""));

	}

	/** Tests whether {@link Strings#trimBeginningLast(String, char)} is working properly. */
	@Test
	public void trimBeginningLast() {
		assertThat(Strings.trimBeginningLast("foobar", 'f'), equalTo("oobar"));
		assertThat(Strings.trimBeginningLast("foobar", 'o'), equalTo("bar"));
		assertThat(Strings.trimBeginningLast("foobar", 'r'), equalTo(""));
	}

	/** Tests whether {@link Strings#trimEnd(String, char, int)} is working properly. */
	@Test
	public void trimEnd() {
		assertThat(Strings.trimEndFirst("foobar", 'f'), equalTo(""));
		assertThat(Strings.trimEndFirst("foobar", 'o'), equalTo("fo"));
		assertThat(Strings.trimEndFirst("foobar", 'r'), equalTo("fooba"));

		assertThat(Strings.trimEndLast("foobar", 'f'), equalTo(""));
		assertThat(Strings.trimEndLast("foobar", 'o'), equalTo("f"));
		assertThat(Strings.trimEndLast("foobar", 'r'), equalTo("fooba"));

		// tests trimming from the first occurrence.
		assertThat(Strings.trimEnd("somewhere over the rainbow", 'e', 1), equalTo("somewhere over th"));
		// tests trimming from an occurrence in the middle of the string.
		assertThat(Strings.trimEnd("somewhere over the rainbow", 'e', 3), equalTo("somewher"));
		// tests trimming from the last occurrence.
		assertThat(Strings.trimEnd("somewhere over the rainbow", 'e', Integer.MAX_VALUE), equalTo("som"));
	}

	/** Tests whether {@link Strings#trimEndFirst(String, char)} is working properly. */
	@Test
	public void trimEndFirst() {
		assertThat(Strings.trimEndFirst("foobar", 'f'), equalTo(""));
		assertThat(Strings.trimEndFirst("foobar", 'o'), equalTo("fo"));
		assertThat(Strings.trimEndFirst("foobar", 'r'), equalTo("fooba"));

	}

	/** Tests whether {@link Strings#trimEndLast(String, char)} is working properly. */
	@Test
	public void trimEndLast() {
		assertThat(Strings.trimEndLast("foobar", 'f'), equalTo(""));
		assertThat(Strings.trimEndLast("foobar", 'o'), equalTo("f"));
		assertThat(Strings.trimEndLast("foobar", 'r'), equalTo("fooba"));
	}

	/**
	 * @see Strings#truncate(String, int) *
	 */
	@Test
	public void testTruncate() {
		assertThat(Strings.truncate("", 0), is(""));
		assertThat(Strings.truncate("", 1), is(""));
		assertThat(Strings.truncate("", 2), is(""));
		assertThat(Strings.truncate("", 3), is(""));
		assertThat(Strings.truncate("", 4), is(""));
		assertThat(Strings.truncate("", 123), is(""));
		assertThat(Strings.truncate("", 9999), is(""));
		assertThat(Strings.truncate("a", 0), is(""));
		assertThat(Strings.truncate("a", 1), is("a"));
		assertThat(Strings.truncate("a", 2), is("a"));
		assertThat(Strings.truncate("a", 3), is("a"));
		assertThat(Strings.truncate("a", 4), is("a"));
		assertThat(Strings.truncate("a", 123), is("a"));
		assertThat(Strings.truncate("a", 9999), is("a"));
		assertThat(Strings.truncate("ab", 0), is(""));
		assertThat(Strings.truncate("ab", 1), is("a"));
		assertThat(Strings.truncate("ab", 2), is("ab"));
		assertThat(Strings.truncate("ab", 3), is("ab"));
		assertThat(Strings.truncate("ab", 4), is("ab"));
		assertThat(Strings.truncate("ab", 123), is("ab"));
		assertThat(Strings.truncate("ab", 9999), is("ab"));
		assertThat(Strings.truncate("abc", 0), is(""));
		assertThat(Strings.truncate("abc", 1), is("a"));
		assertThat(Strings.truncate("abc", 2), is("ab"));
		assertThat(Strings.truncate("abc", 3), is("abc"));
		assertThat(Strings.truncate("abc", 4), is("abc"));
		assertThat(Strings.truncate("abc", 123), is("abc"));
		assertThat(Strings.truncate("abc", 9999), is("abc"));
	}
}
