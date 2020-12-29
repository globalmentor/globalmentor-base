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
		assertThat(Text.normalizeEol("foo\r\nbar").toString(), is("foo\nbar"));
		//edge cases
		assertThat(Text.normalizeEol("\r\nfoobar").toString(), is("\nfoobar"));
		assertThat(Text.normalizeEol("foobar\r\n").toString(), is("foobar\n"));
		assertThat(Text.normalizeEol("\r\nfoobar\r\n").toString(), is("\nfoobar\n"));
		assertThat(Text.normalizeEol("fo\r\noba\r\nr").toString(), is("fo\noba\nr"));
		//runs
		assertThat(Text.normalizeEol("foo\r\n\r\nbar").toString(), is("foo\n\nbar"));
		assertThat(Text.normalizeEol("foo\r\n\r\n\r\nbar").toString(), is("foo\n\n\nbar"));
		//no change
		assertThat(Text.normalizeEol("foo\nbar").toString(), is("foo\nbar"));
		assertThat(Text.normalizeEol("\nfoobar").toString(), is("\nfoobar"));
		assertThat(Text.normalizeEol("foobar\n").toString(), is("foobar\n"));
		assertThat(Text.normalizeEol("\nfoobar\n").toString(), is("\nfoobar\n"));
		assertThat(Text.normalizeEol("fo\noba\nr").toString(), is("fo\noba\nr"));
		assertThat(Text.normalizeEol("foo\n\nbar").toString(), is("foo\n\nbar"));
		assertThat(Text.normalizeEol("foo\n\n\nbar").toString(), is("foo\n\n\nbar"));
		final StringBuilder textNeedingNoChange = new StringBuilder("\nfoo\n\n\nbar\n");
		assertThat("No modifications made if no change required.", Text.normalizeEol(textNeedingNoChange), is(sameInstance(textNeedingNoChange)));
		//mixed
		assertThat(Text.normalizeEol("\rfoobar").toString(), is("\nfoobar"));
		assertThat(Text.normalizeEol("\r\n\rfoobar").toString(), is("\n\nfoobar"));
		assertThat(Text.normalizeEol("foobar\r").toString(), is("foobar\n"));
		assertThat(Text.normalizeEol("foobar\n\r").toString(), is("foobar\n\n"));
		assertThat(Text.normalizeEol("foo\rbar").toString(), is("foo\nbar"));
		assertThat(Text.normalizeEol("foo\nbar").toString(), is("foo\nbar"));
		assertThat(Text.normalizeEol("foo\n\rbar").toString(), is("foo\n\nbar"));
		assertThat(Text.normalizeEol("foo\n\r\nbar").toString(), is("foo\n\nbar"));
		assertThat(Text.normalizeEol("foo\n\r\r\nbar").toString(), is("foo\n\n\nbar"));
		assertThat(Text.normalizeEol("foo\n\r\n\rbar").toString(), is("foo\n\n\nbar"));
		assertThat(Text.normalizeEol("foo\r\n\r\n\rbar").toString(), is("foo\n\n\nbar"));
	}

	/**
	 * Ensures that the "no-change" detection doesn't prevent a <code>CRLF</code> from being converted to a single <code>CR</code> if normalizing to
	 * <code>CR</code> (even though this normalization is obsolete and never really used anymore).
	 * @see Text#normalizeEol(CharSequence, CharSequence)
	 */
	@Test
	void shouldNormalizeCrlfToCrIfNormalizingToCr() {
		assertThat(Text.normalizeEol("foo\rbar", "\r").toString(), is("foo\rbar"));
		assertThat(Text.normalizeEol("foo\r\nbar", "\r").toString(), is("foo\rbar"));
		assertThat(Text.normalizeEol("foo\n\rbar", "\r").toString(), is("foo\r\rbar"));
		assertThat(Text.normalizeEol("foo\r\rbar", "\r").toString(), is("foo\r\rbar"));
	}

}
