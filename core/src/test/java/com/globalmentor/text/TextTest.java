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

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link Text}.
 * @author Garret Wilson
 */
public class TextTest {

	/**
	 * @see Text#normalizeEol(CharSequence)
	 * @see Text#normalizeEol(CharSequence, CharSequence)
	 */
	@Test
	void testNormalizeEol() {
		//happy path
		assertThat(Text.normalizeEol("foo\r\nbar"), hasToString("foo\nbar"));
		//edge cases
		assertThat(Text.normalizeEol("\r\nfoobar"), hasToString("\nfoobar"));
		assertThat(Text.normalizeEol("foobar\r\n"), hasToString("foobar\n"));
		assertThat(Text.normalizeEol("\r\nfoobar\r\n"), hasToString("\nfoobar\n"));
		assertThat(Text.normalizeEol("fo\r\noba\r\nr"), hasToString("fo\noba\nr"));
		//runs
		assertThat(Text.normalizeEol("foo\r\n\r\nbar"), hasToString("foo\n\nbar"));
		assertThat(Text.normalizeEol("foo\r\n\r\n\r\nbar"), hasToString("foo\n\n\nbar"));
		//no change
		assertThat(Text.normalizeEol("foo\nbar"), hasToString("foo\nbar"));
		assertThat(Text.normalizeEol("\nfoobar"), hasToString("\nfoobar"));
		assertThat(Text.normalizeEol("foobar\n"), hasToString("foobar\n"));
		assertThat(Text.normalizeEol("\nfoobar\n"), hasToString("\nfoobar\n"));
		assertThat(Text.normalizeEol("fo\noba\nr"), hasToString("fo\noba\nr"));
		assertThat(Text.normalizeEol("foo\n\nbar"), hasToString("foo\n\nbar"));
		assertThat(Text.normalizeEol("foo\n\n\nbar"), hasToString("foo\n\n\nbar"));
		final StringBuilder textNeedingNoChange = new StringBuilder("\nfoo\n\n\nbar\n");
		assertThat("No modifications made if no change required.", Text.normalizeEol(textNeedingNoChange), is(sameInstance(textNeedingNoChange)));
		//mixed
		assertThat(Text.normalizeEol("\rfoobar"), hasToString("\nfoobar"));
		assertThat(Text.normalizeEol("\r\n\rfoobar"), hasToString("\n\nfoobar"));
		assertThat(Text.normalizeEol("foobar\r"), hasToString("foobar\n"));
		assertThat(Text.normalizeEol("foobar\n\r"), hasToString("foobar\n\n"));
		assertThat(Text.normalizeEol("foo\rbar"), hasToString("foo\nbar"));
		assertThat(Text.normalizeEol("foo\nbar"), hasToString("foo\nbar"));
		assertThat(Text.normalizeEol("foo\n\rbar"), hasToString("foo\n\nbar"));
		assertThat(Text.normalizeEol("foo\n\r\nbar"), hasToString("foo\n\nbar"));
		assertThat(Text.normalizeEol("foo\n\r\r\nbar"), hasToString("foo\n\n\nbar"));
		assertThat(Text.normalizeEol("foo\n\r\n\rbar"), hasToString("foo\n\n\nbar"));
		assertThat(Text.normalizeEol("foo\r\n\r\n\rbar"), hasToString("foo\n\n\nbar"));
	}

	/**
	 * Ensures that the "no-change" detection doesn't prevent a <code>CRLF</code> from being converted to a single <code>CR</code> if normalizing to
	 * <code>CR</code> (even though this normalization is obsolete and never really used anymore).
	 * @see Text#normalizeEol(CharSequence, CharSequence)
	 */
	@Test
	void shouldNormalizeCrlfToCrIfNormalizingToCr() {
		assertThat(Text.normalizeEol("foo\rbar", "\r"), hasToString("foo\rbar"));
		assertThat(Text.normalizeEol("foo\r\nbar", "\r"), hasToString("foo\rbar"));
		assertThat(Text.normalizeEol("foo\n\rbar", "\r"), hasToString("foo\r\rbar"));
		assertThat(Text.normalizeEol("foo\r\rbar", "\r"), hasToString("foo\r\rbar"));
	}

}
