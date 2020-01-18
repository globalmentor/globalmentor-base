/*
 * Copyright © 2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.net;

import com.globalmentor.text.ArgumentSyntaxException;

import static com.globalmentor.collections.Sets.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link ContentType}.
 * @author Garret Wilson
 */
public class ContentTypeTest {

	/** Tests of {@link ContentType#parse(CharSequence)}. */
	@Test
	public void testParseContentTypes() {
		//simple
		assertThat(ContentType.parse("text/plain").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain").getSubType(), is("plain"));
		//one parameter
		assertThat(ContentType.parse("text/plain; charset=us-ascii").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii").getSubType(), is("plain"));
		assertEquals(immutableSetOf(ContentType.Parameter.of("charset", "us-ascii")), ContentType.parse("text/plain; charset=us-ascii").getParameters());
		//two parameters
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar").getSubType(), is("plain"));
		assertEquals(immutableSetOf(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")),
				ContentType.parse("text/plain; charset=us-ascii; foo=bar").getParameters());
		//without spaces
		assertThat(ContentType.parse("text/plain;charset=us-ascii;foo=bar").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain;charset=us-ascii;foo=bar").getSubType(), is("plain"));
		assertEquals(immutableSetOf(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")),
				ContentType.parse("text/plain;charset=us-ascii;foo=bar").getParameters());
		//many spaces
		assertThat(ContentType.parse("text/plain;  charset=us-ascii;   foo=bar").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain;  charset=us-ascii;   foo=bar").getSubType(), is("plain"));
		assertEquals(immutableSetOf(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")),
				ContentType.parse("text/plain;  charset=us-ascii;   foo=bar").getParameters());
	}

	/** Tests equality, including parameter order and case insensitivity of content type names. */
	@Test
	public void testEquality() {
		//parsing versus construction
		assertEquals(ContentType.of("text", "plain"), ContentType.parse("text/plain"));
		assertEquals(ContentType.of("text", "plain", ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")),
				ContentType.parse("text/plain; charset=us-ascii; foo=bar"));
		//parameter order
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar"), equalTo(ContentType.parse("text/plain; foo=bar; charset=us-ascii")));
		//no spaces
		assertThat(ContentType.parse("text/plain;charset=us-ascii;foo=bar"), equalTo(ContentType.parse("text/plain; foo=bar; charset=us-ascii")));
		//case insensitivity
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), equalTo(ContentType.parse("TEXT/PLAIN; CHARSET=us-ascii")));
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), not(equalTo(ContentType.parse("TEXT/PLAIN; CHARSET=US-ASCII"))));
		//optional parameter value quotes
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), equalTo(ContentType.parse("text/plain; charset=\"us-ascii\"")));
	}

	/**
	 * @see ContentType#toString()
	 * @see ContentType#toString(boolean)
	 */
	@Test
	public void testTotring() {
		//text/plain
		assertThat(ContentType.of("text", "plain").toString(), is("text/plain"));
		assertThat(ContentType.of("text", "plain").toString(false), is("text/plain"));
		assertThat(ContentType.of("text", "plain").toString(true), is("text/plain"));

		//text/html; charset=UTF-8
		assertThat(ContentType.parse("text/html; charset=UTF-8").toString(), is("text/html;charset=UTF-8"));
		assertThat(ContentType.parse("text/html; charset=UTF-8").toString(false), is("text/html;charset=UTF-8"));
		assertThat(ContentType.parse("text/html; charset=UTF-8").toString(true), is("text/html; charset=UTF-8"));

		//TODO add tests with multiple parameters with some normalized order
	}

	@Test
	public void testMissingDelimiter() {
		assertThrows(ArgumentSyntaxException.class, () -> ContentType.parse("text"));
	}

	@Test
	public void testMissingSubType() {
		assertThrows(ArgumentSyntaxException.class, () -> ContentType.parse("text/"));
	}

	@Test
	public void testMissingParameters() {
		assertThrows(ArgumentSyntaxException.class, () -> ContentType.parse("text/plain;"));
	}

	@Test
	public void testMissingParameterDelimiter() {
		assertThrows(ArgumentSyntaxException.class, () -> ContentType.parse("text/plain; charset"));
	}

	@Test
	public void testMissingParameterValue() {
		assertThrows(ArgumentSyntaxException.class, () -> ContentType.parse("text/plain; charset="));
	}

	@Test
	public void testMissingSecondParameter() {
		assertThrows(ArgumentSyntaxException.class, () -> ContentType.parse("text/plain; charset=us-ascii;"));
	}
}
