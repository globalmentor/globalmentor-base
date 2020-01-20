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

import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link ContentType}.
 * @author Garret Wilson
 * @see <a href="https://en.wikipedia.org/wiki/Media_type">Media type</a>
 */
public class ContentTypeTest {

	/** Tests of {@link ContentType#parse(CharSequence)}. */
	@Test
	public void testParse() {
		//simple
		assertThat(ContentType.parse("text/plain").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain").getSubType(), is("plain"));
		//one parameter
		assertThat(ContentType.parse("text/plain; charset=us-ascii").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii").getSubType(), is("plain"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii").getParameters(), containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii")));
		//two parameters
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar").getSubType(), is("plain"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar").getParameters(),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")));
		//three parameters
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar; test=example").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar; test=example").getSubType(), is("plain"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar; test=example").getParameters(), containsInAnyOrder(
				ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar"), ContentType.Parameter.of("test", "example")));
		//quoted parameters
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=\"bar\"").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=\"bar\"").getSubType(), is("plain"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=\"bar\"").getParameters(),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")));
		//quoted parameters with special characters
		assertThat(ContentType.parse("text/plain; charset=us-ascii; test=\"(foo)<bar>@\\\",;:\\\\/[foobar]?=\"").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; test=\"(foo)<bar>@\\\",;:\\\\/[foobar]?=\"").getSubType(), is("plain"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; test=\"(foo)<bar>@\\\",;:\\\\/[foobar]?=\"").getParameters(),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("test", "(foo)<bar>@\",;:\\/[foobar]?=")));
		//without spaces
		assertThat(ContentType.parse("text/plain;charset=us-ascii;foo=bar").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain;charset=us-ascii;foo=bar").getSubType(), is("plain"));
		assertThat(ContentType.parse("text/plain;charset=us-ascii;foo=bar").getParameters(),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")));
		//many spaces
		assertThat(ContentType.parse("text/plain;  charset=us-ascii;   foo=bar").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain;  charset=us-ascii;   foo=bar").getSubType(), is("plain"));
		assertThat(ContentType.parse("text/plain;  charset=us-ascii;   foo=bar").getParameters(),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")));
		//known media types
		assertThat(ContentType.parse("application/x-www-form-urlencoded").getPrimaryType(), is("application"));
		assertThat(ContentType.parse("application/x-www-form-urlencoded").getSubType(), is("x-www-form-urlencoded"));
		assertThat(ContentType.parse("application/vnd.openxmlformats-officedocument.wordprocessingml.document").getPrimaryType(), is("application"));
		assertThat(ContentType.parse("application/vnd.openxmlformats-officedocument.wordprocessingml.document").getSubType(),
				is("vnd.openxmlformats-officedocument.wordprocessingml.document"));
		assertThat(ContentType.parse("application/vnd.openxmlformats-officedocument.presentationml.presentation").getPrimaryType(), is("application"));
		assertThat(ContentType.parse("application/vnd.openxmlformats-officedocument.presentationml.presentation").getSubType(),
				is("vnd.openxmlformats-officedocument.presentationml.presentation"));
		assertThat(ContentType.parse("multipart/form-data").getPrimaryType(), is("multipart"));
		assertThat(ContentType.parse("multipart/form-data").getSubType(), is("form-data"));
		assertThat(ContentType.parse("application/vnd.api+json").getPrimaryType(), is("application"));
		assertThat(ContentType.parse("application/vnd.api+json").getSubType(), is("vnd.api+json"));

	}

	/** @see ContentType#PARAMETER_PATTERN */
	@Test
	public void testParametersPattern() {
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; charset=us-ascii").matches(), is(true));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; test=foobar").matches(), is(true));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; x=foobar").matches(), is(true));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; 3=foobar").matches(), is(true));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; ?=foobar").matches(), is(false));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; _=foobar").matches(), is(false));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; x?=foobar").matches(), is(false));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; x_=foobar").matches(), is(true));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; x3=foobar").matches(), is(true));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; test=foo;bar").matches(), is(false));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; charset=\"us-ascii\"").matches(), is(true));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; test=\"foo;bar\"").matches(), is(true));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; test=\"foo\\\"bar\"").matches(), is(true));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; test=\"foo\\\\bar\"").matches(), is(true));
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; test=\"foo\\xbar\"").matches(), is(false));
	}

	/** Tests of {@link ContentType#parseParameters(CharSequence)}. */
	@Test
	public void testParseParameters() {
		assertThat(ContentType.parseParameters("; charset=us-ascii; foo=bar"),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")));
		assertThat(ContentType.parseParameters("; charset=US-ASCII; foo=bar"),
				containsInAnyOrder(ContentType.Parameter.of("charset", "US-ASCII"), ContentType.Parameter.of("foo", "bar"))); //TODO fix charset case comparison
		assertThat(ContentType.parseParameters("; charset=\"us-ascii\""), containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii")));
		assertThat(ContentType.parseParameters("; charset=\"us-ascii\"; foo=bar"),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")));
		assertThat(ContentType.parseParameters("; charset=us-ascii; test=\"foo;bar\""),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("test", "foo;bar")));
		assertThat(ContentType.parseParameters("; test=\"(foo)<bar>@\\\",;:\\\\/[foobar]?=\""),
				containsInAnyOrder(ContentType.Parameter.of("test", "(foo)<bar>@\",;:\\/[foobar]?=")));
		assertThrows(ArgumentSyntaxException.class, () -> ContentType.parseParameters("charset=us-ascii foo=bar"));
		assertThrows(ArgumentSyntaxException.class, () -> ContentType.parseParameters("; charset=us-ascii= foo=bar"));
		assertThrows(ArgumentSyntaxException.class, () -> ContentType.parseParameters("charset=us-ascii=foo=bar"));
	}

	/** Tests equality, including parameter order and case insensitivity of content type names. */
	@Test
	public void testEquality() {
		//parsing versus construction
		assertThat(ContentType.parse("text/plain"), is(ContentType.of("text", "plain")));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar"),
				is(ContentType.of("text", "plain", ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar"))));
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
	public void testToString() {
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
