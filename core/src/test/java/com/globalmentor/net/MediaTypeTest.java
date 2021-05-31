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

import static org.hamcrest.Matchers.*;
import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static java.nio.charset.StandardCharsets.*;
import static java.util.stream.Collectors.*;
import static org.hamcrest.MatcherAssert.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.zalando.fauxpas.FauxPas.*;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.*;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.stream.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link MediaType}.
 * @author Garret Wilson
 * @see <a href="https://en.wikipedia.org/wiki/Media_type">Media type</a>
 */
public class MediaTypeTest {

	/** Tests of {@link MediaType#parse(CharSequence)}. */
	@Test
	public void testParse() {
		//simple
		assertThat(MediaType.parse("text/plain").getPrimaryType(), is("text"));
		assertThat(MediaType.parse("text/plain").getSubType(), is("plain"));
		//one parameter
		assertThat(MediaType.parse("text/plain; charset=us-ascii").getPrimaryType(), is("text"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii").getSubType(), is("plain"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii").getParameters(), containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii")));
		//two parameters
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=bar").getPrimaryType(), is("text"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=bar").getSubType(), is("plain"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=bar").getParameters(),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar")));
		//three parameters
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=bar; test=example").getPrimaryType(), is("text"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=bar; test=example").getSubType(), is("plain"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=bar; test=example").getParameters(),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar"), MediaType.Parameter.of("test", "example")));
		//duplicate parameter names
		assertThat(MediaType.parse("text/plain; test=foo; charset=us-ascii; test=bar").getPrimaryType(), is("text"));
		assertThat(MediaType.parse("text/plain; test=foo; charset=us-ascii; test=bar").getSubType(), is("plain"));
		assertThat(MediaType.parse("text/plain; test=foo; charset=us-ascii; test=bar").getParameters(),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("test", "foo"), MediaType.Parameter.of("test", "bar")));
		//quoted parameters
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=\"bar\"").getPrimaryType(), is("text"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=\"bar\"").getSubType(), is("plain"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=\"bar\"").getParameters(),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar")));
		//quoted parameters with special characters
		assertThat(MediaType.parse("text/plain; charset=us-ascii; test=\"(foo)<bar>@\\\",;:\\\\/[foobar]?=\"").getPrimaryType(), is("text"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; test=\"(foo)<bar>@\\\",;:\\\\/[foobar]?=\"").getSubType(), is("plain"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; test=\"(foo)<bar>@\\\",;:\\\\/[foobar]?=\"").getParameters(),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("test", "(foo)<bar>@\",;:\\/[foobar]?=")));
		//quoted parameters with escaped parameters and tab
		assertThat(MediaType.parse("text/plain; charset=us-ascii; test=\"foo\\\"bar\tand\\xmore\\\\stuff\"").getPrimaryType(), is("text"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; test=\"foo\\\"bar\tand\\xmore\\\\stuff\"").getSubType(), is("plain"));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; test=\"foo\\\"bar\tand\\xmore\\\\stuff\"").getParameters(),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("test", "foo\"bar\tandxmore\\stuff")));
		//without spaces
		assertThat(MediaType.parse("text/plain;charset=us-ascii;foo=bar").getPrimaryType(), is("text"));
		assertThat(MediaType.parse("text/plain;charset=us-ascii;foo=bar").getSubType(), is("plain"));
		assertThat(MediaType.parse("text/plain;charset=us-ascii;foo=bar").getParameters(),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar")));
		//many spaces
		assertThat(MediaType.parse("text/plain;  charset=us-ascii;   foo=bar").getPrimaryType(), is("text"));
		assertThat(MediaType.parse("text/plain;  charset=us-ascii;   foo=bar").getSubType(), is("plain"));
		assertThat(MediaType.parse("text/plain;  charset=us-ascii;   foo=bar").getParameters(),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar")));
		//known media types
		assertThat(MediaType.parse("application/x-www-form-urlencoded").getPrimaryType(), is("application"));
		assertThat(MediaType.parse("application/x-www-form-urlencoded").getSubType(), is("x-www-form-urlencoded"));
		assertThat(MediaType.parse("application/vnd.openxmlformats-officedocument.wordprocessingml.document").getPrimaryType(), is("application"));
		assertThat(MediaType.parse("application/vnd.openxmlformats-officedocument.wordprocessingml.document").getSubType(),
				is("vnd.openxmlformats-officedocument.wordprocessingml.document"));
		assertThat(MediaType.parse("application/vnd.oasis.opendocument.text").getPrimaryType(), is("application"));
		assertThat(MediaType.parse("application/vnd.oasis.opendocument.text").getSubType(), is("vnd.oasis.opendocument.text"));
		assertThat(MediaType.parse("multipart/form-data; charset=utf-8; boundary=\"q1w2e3r4ty:9-5xyz\"").getPrimaryType(), is("multipart"));
		assertThat(MediaType.parse("multipart/form-data; charset=utf-8; boundary=\"q1w2e3r4ty:9-5xyz\"").getSubType(), is("form-data"));
		assertThat(MediaType.parse("multipart/form-data; charset=utf-8; boundary=\"q1w2e3r4ty:9-5xyz\"").getParameters(),
				containsInAnyOrder(MediaType.Parameter.of("charset", "utf-8"), MediaType.Parameter.of("boundary", "q1w2e3r4ty:9-5xyz")));
		assertThat(MediaType.parse("application/vnd.api+json").getPrimaryType(), is("application"));
		assertThat(MediaType.parse("application/vnd.api+json").getSubType(), is("vnd.api+json"));
	}

	/** @see MediaType#RESTRICTED_NAME_PATTERN */
	@Test
	public void testRestrictedNamePattern() {
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("").matches(), is(false));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("charset").matches(), is(true));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("x").matches(), is(true));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("3").matches(), is(true));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("?").matches(), is(false));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("_").matches(), is(false));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("x?").matches(), is(false));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("x_").matches(), is(true));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("x3").matches(), is(true));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("fooBar").matches(), is(true));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("foo bar").matches(), is(false));
		assertThat(MediaType.RESTRICTED_NAME_PATTERN.matcher("foo:bar").matches(), is(false));
	}

	/** @see MediaType#PARAMETER_PATTERN */
	@Test
	public void testParametersPattern() {
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; charset=us-ascii").matches(), is(true));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; test=foobar").matches(), is(true));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; x=foobar").matches(), is(true));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; 3=foobar").matches(), is(true));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; ?=foobar").matches(), is(false));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; _=foobar").matches(), is(false));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; x?=foobar").matches(), is(false));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; x_=foobar").matches(), is(true));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; x3=foobar").matches(), is(true));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; test=foo;bar").matches(), is(false));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; charset=\"us-ascii\"").matches(), is(true));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; test=\"foo;bar\"").matches(), is(true));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; test=\"foo\\\"bar\"").matches(), is(true));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; test=\"foo\\\\bar\"").matches(), is(true));
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; test=\"foo\tbar\"").matches(), is(true)); //literal tab
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; test=\"foo\\tbar\"").matches(), is(true)); //escaped `t` (not tab)
		assertThat(MediaType.PARAMETER_PATTERN.matcher("; test=\"foo\\xbar\"").matches(), is(true)); //escaped `x`
	}

	/** Tests of {@link MediaType#parseParameters(CharSequence)}. */
	@Test
	public void testParseParameters() {
		assertThat(MediaType.parseParameters("; foo=bar"), containsInAnyOrder(MediaType.Parameter.of("foo", "bar")));
		assertThat(MediaType.parseParameters("; foo=\"bar\""), containsInAnyOrder(MediaType.Parameter.of("foo", "bar")));
		assertThat(MediaType.parseParameters("; foo=\" bar\""), containsInAnyOrder(MediaType.Parameter.of("foo", " bar")));
		assertThat(MediaType.parseParameters("; charset=us-ascii; foo=bar"),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar")));
		assertThat(MediaType.parseParameters("; charset=US-ASCII; foo=bar"),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar")));
		assertThat(MediaType.parseParameters("; charset=\"us-ascii\""), containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii")));
		assertThat(MediaType.parseParameters("; charset=\"us-ascii\"; foo=bar"),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar")));
		assertThat(MediaType.parseParameters("; charset=us-ascii; test=\"foo;bar\""),
				containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("test", "foo;bar")));
		assertThat(MediaType.parseParameters("; test=\"(foo)<bar>@\\\",;:\\\\/[foobar]?=\""),
				containsInAnyOrder(MediaType.Parameter.of("test", "(foo)<bar>@\",;:\\/[foobar]?=")));
		assertThrows(IllegalArgumentException.class, () -> MediaType.parseParameters("; foo= bar"));
		assertThrows(IllegalArgumentException.class, () -> MediaType.parseParameters("charset=us-ascii foo=bar"));
		assertThrows(IllegalArgumentException.class, () -> MediaType.parseParameters("; charset=us-ascii= foo=bar"));
		assertThrows(IllegalArgumentException.class, () -> MediaType.parseParameters("charset=us-ascii=foo=bar"));
	}

	/** @see MediaType#findCharset() */
	@Test
	public void testFindCharset() {
		assertThat(MediaType.parse("text/plain").findCharset(), isEmpty());
		assertThat(MediaType.parse("text/plain; foo=bar").findCharset(), isEmpty());
		assertThat(MediaType.parse("text/plain; charset=us-ascii").findCharset(), isPresentAndIs(US_ASCII));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=bar").findCharset(), isPresentAndIs(US_ASCII));
		assertThat(MediaType.parse("text/plain; foo=bar; charset=us-ascii; test=example").findCharset(), isPresentAndIs(US_ASCII));
	}

	/** @see MediaType#matches(CharSequence) */
	@Test
	public void testMatchesString() {
		assertThat(MediaType.parse("image/png").matches("image/png"), is(true));
		assertThat(MediaType.parse("image/png").matches("image/*"), is(true));
		assertThat(MediaType.parse("image/png").matches("image/gif"), is(false));
	}

	/** @see MediaType#matches(String, String) */
	@Test
	public void testMatchesTypeSubtype() {
		assertThat(MediaType.parse("image/png").matches("image", "png"), is(true));
		assertThat(MediaType.parse("image/png").matches("image", "*"), is(true));
		assertThat(MediaType.parse("image/png").matches("image", "gif"), is(false));
	}

	/** @see MediaType#withParameter(String, String) */
	@Test
	public void testWithParameterAddsNewParameterName() {
		{
			final MediaType mediaType = MediaType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final MediaType mediaTypeWithParameter = mediaType.withParameter("new", "foobar");
			assertThat(mediaTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(mediaTypeWithParameter.getSubType(), is("plain"));
			assertThat(mediaTypeWithParameter.getParameters(), containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"),
					MediaType.Parameter.of("foo", "bar"), MediaType.Parameter.of("test", "example"), MediaType.Parameter.of("new", "foobar")));
		}
		{ //different case
			final MediaType mediaType = MediaType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final MediaType mediaTypeWithParameter = mediaType.withParameter("NEW", "foobar");
			assertThat(mediaTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(mediaTypeWithParameter.getSubType(), is("plain"));
			assertThat(mediaTypeWithParameter.getParameters(), containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"),
					MediaType.Parameter.of("foo", "bar"), MediaType.Parameter.of("test", "example"), MediaType.Parameter.of("new", "foobar")));
		}
		{ //duplicate parameter names
			final MediaType mediaType = MediaType.parse("text/plain; test=foo; charset=us-ascii; test=bar");
			final MediaType mediaTypeWithParameter = mediaType.withParameter("new", "foobar");
			assertThat(mediaTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(mediaTypeWithParameter.getSubType(), is("plain"));
			assertThat(mediaTypeWithParameter.getParameters(), containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"),
					MediaType.Parameter.of("test", "foo"), MediaType.Parameter.of("test", "bar"), MediaType.Parameter.of("new", "foobar")));
		}
	}

	/** @see MediaType#withParameter(String, String) */
	@Test
	public void testWithParameterReplacesExistingParameterName() {
		{
			final MediaType mediaType = MediaType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final MediaType mediaTypeWithParameter = mediaType.withParameter("test", "foobar");
			assertThat(mediaTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(mediaTypeWithParameter.getSubType(), is("plain"));
			assertThat(mediaTypeWithParameter.getParameters(),
					containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar"), MediaType.Parameter.of("test", "foobar")));
		}
		{ //different case
			final MediaType mediaType = MediaType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final MediaType mediaTypeWithParameter = mediaType.withParameter("test", "foobar");
			assertThat(mediaTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(mediaTypeWithParameter.getSubType(), is("plain"));
			assertThat(mediaTypeWithParameter.getParameters(),
					containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar"), MediaType.Parameter.of("test", "foobar")));
		}
		{ //duplicate parameter names
			final MediaType mediaType = MediaType.parse("text/plain; test=foo; charset=us-ascii; test=bar");
			final MediaType mediaTypeWithParameter = mediaType.withParameter("test", "foobar");
			assertThat(mediaTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(mediaTypeWithParameter.getSubType(), is("plain"));
			assertThat(mediaTypeWithParameter.getParameters(),
					containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("test", "foobar")));
		}
		{ //duplicate parameter names; value matches that to replace
			final MediaType mediaType = MediaType.parse("text/plain; test=foo; charset=us-ascii; test=bar");
			final MediaType mediaTypeWithParameter = mediaType.withParameter("test", "foo");
			assertThat(mediaTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(mediaTypeWithParameter.getSubType(), is("plain"));
			assertThat(mediaTypeWithParameter.getParameters(),
					containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("test", "foo")));
		}
	}

	/** @see MediaType#withCharset(Charset) */
	@Test
	public void testWithCharset() {
		{
			final MediaType mediaType = MediaType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final MediaType mediaTypeWithCharset = mediaType.withCharset(US_ASCII);
			assertThat(mediaTypeWithCharset.getPrimaryType(), is("text"));
			assertThat(mediaTypeWithCharset.getSubType(), is("plain"));
			assertThat(mediaTypeWithCharset.getParameters(),
					containsInAnyOrder(MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar"), MediaType.Parameter.of("test", "example")));
		}
		{
			final MediaType mediaType = MediaType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final MediaType mediaTypeWithCharset = mediaType.withCharset(UTF_8);
			assertThat(mediaTypeWithCharset.getPrimaryType(), is("text"));
			assertThat(mediaTypeWithCharset.getSubType(), is("plain"));
			assertThat(mediaTypeWithCharset.getParameters(),
					containsInAnyOrder(MediaType.Parameter.of("charset", "utf-8"), MediaType.Parameter.of("foo", "bar"), MediaType.Parameter.of("test", "example")));
		}
		{
			final MediaType mediaType = MediaType.parse("text/plain; foo=bar; test=example");
			final MediaType mediaTypeWithCharset = mediaType.withCharset(UTF_8);
			assertThat(mediaTypeWithCharset.getPrimaryType(), is("text"));
			assertThat(mediaTypeWithCharset.getSubType(), is("plain"));
			assertThat(mediaTypeWithCharset.getParameters(),
					containsInAnyOrder(MediaType.Parameter.of("charset", "utf-8"), MediaType.Parameter.of("foo", "bar"), MediaType.Parameter.of("test", "example")));
		}
	}

	/** Tests equality, including parameter order and case insensitivity of media type names. */
	@Test
	public void testEquality() {
		//parsing versus construction
		assertThat(MediaType.parse("text/plain"), is(MediaType.of("text", "plain")));
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=bar"),
				is(MediaType.of("text", "plain", MediaType.Parameter.of("charset", "us-ascii"), MediaType.Parameter.of("foo", "bar"))));
		//parameter order
		assertThat(MediaType.parse("text/plain; charset=us-ascii; foo=bar"), equalTo(MediaType.parse("text/plain; foo=bar; charset=us-ascii")));
		//no spaces
		assertThat(MediaType.parse("text/plain;charset=us-ascii;foo=bar"), equalTo(MediaType.parse("text/plain; foo=bar; charset=us-ascii")));
		//case insensitivity
		assertThat(MediaType.parse("text/plain; charset=us-ascii"), equalTo(MediaType.parse("text/plain; charset=us-ascii")));
		assertThat(MediaType.parse("text/plain; charset=us-ascii"), equalTo(MediaType.parse("text/Plain; charset=us-ascii")));
		assertThat(MediaType.parse("text/plain; charset=us-ascii"), equalTo(MediaType.parse("text/plain; charSet=us-ascii")));
		assertThat(MediaType.parse("text/plain; charset=us-ascii"), equalTo(MediaType.parse("text/plain; charset=us-Ascii")));
		assertThat(MediaType.parse("text/plain; charset=us-ascii"), equalTo(MediaType.parse("Text/Plain; charset=US-ascii")));
		assertThat(MediaType.parse("text/plain; charset=us-ascii"), equalTo(MediaType.parse("text/plain; charset=us-ascii")));
		assertThat(MediaType.parse("text/plain; charset=us-ascii"), equalTo(MediaType.parse("TEXT/PLAIN; CHARSET=us-ascii")));
		assertThat(MediaType.parse("text/plain; charset=us-ascii"), equalTo(MediaType.parse("TEXT/PLAIN; CHARSET=US-ASCII")));
		//optional parameter value quotes
		assertThat(MediaType.parse("text/plain; charset=us-ascii"), equalTo(MediaType.parse("text/plain; charset=\"us-ascii\"")));
	}

	/**
	 * @see MediaType#toString()
	 * @see MediaType#toString(boolean)
	 */
	@Test
	public void testToString() {
		//text/plain
		assertThat(MediaType.of("text", "plain").toString(), is("text/plain"));
		assertThat(MediaType.of("text", "plain").toString(false), is("text/plain"));
		assertThat(MediaType.of("text", "plain").toString(true), is("text/plain"));

		//text/html; charset=UTF-8
		assertThat(MediaType.parse("text/html; charset=UTF-8").toString(), is("text/html;charset=utf-8"));
		assertThat(MediaType.parse("text/html; charset=UTF-8").toString(false), is("text/html;charset=utf-8"));
		assertThat(MediaType.parse("text/html; charset=UTF-8").toString(true), is("text/html; charset=utf-8"));
	}

	@Test
	public void testMissingDelimiter() {
		assertThrows(IllegalArgumentException.class, () -> MediaType.parse("text"));
	}

	@Test
	public void testMissingSubType() {
		assertThrows(IllegalArgumentException.class, () -> MediaType.parse("text/"));
	}

	@Test
	public void testMissingParameters() {
		assertThrows(IllegalArgumentException.class, () -> MediaType.parse("text/plain;"));
	}

	@Test
	public void testMissingParameterDelimiter() {
		assertThrows(IllegalArgumentException.class, () -> MediaType.parse("text/plain; charset"));
	}

	@Test
	public void testMissingParameterValue() {
		assertThrows(IllegalArgumentException.class, () -> MediaType.parse("text/plain; charset="));
	}

	@Test
	public void testMissingSecondParameter() {
		assertThrows(IllegalArgumentException.class, () -> MediaType.parse("text/plain; charset=us-ascii;"));
	}

	/** @see MediaType#hasSubTypeSuffix(String...) */
	@Test
	public void testHasSubTypeSuffix() {
		assertThat(MediaType.of("text", "plain").hasSubTypeSuffix("foo"), is(false));
		assertThat(MediaType.of("text", "test").hasSubTypeSuffix("foo"), is(false));
		assertThat(MediaType.of("text", "test+foo").hasSubTypeSuffix("foo"), is(true));
		assertThat(MediaType.of("text", "TEST+Foo").hasSubTypeSuffix("FOO"), is(true));
		assertThat(MediaType.of("text", "test+foo+bar").hasSubTypeSuffix("foo"), is(false));
		assertThat(MediaType.of("text", "test+foo").hasSubTypeSuffix("foo", "bar"), is(false));
		assertThat(MediaType.of("text", "test+foo+bar").hasSubTypeSuffix("foo", "bar"), is(true));
		assertThat(MediaType.of("TEXT", "TEST+FOO+BAR").hasSubTypeSuffix("fOO", "Bar"), is(true));
	}

	/** @see MediaType#createSubTypeSuffix(String...) */
	@Test
	public void testCreateSubTypeSuffix() {
		assertThat(MediaType.createSubTypeSuffix("foo"), is("+foo"));
		assertThat(MediaType.createSubTypeSuffix("123"), is("+123"));
		assertThat(MediaType.createSubTypeSuffix("FOO"), is("+foo"));
		assertThat(MediaType.createSubTypeSuffix("foo", "bar"), is("+foo+bar"));
		assertThat(MediaType.createSubTypeSuffix("Foo", "BAR"), is("+foo+bar"));
		assertThrows(IllegalArgumentException.class, () -> MediaType.createSubTypeSuffix(""));
		assertThrows(IllegalArgumentException.class, () -> MediaType.createSubTypeSuffix("+"));
		assertThrows(IllegalArgumentException.class, () -> MediaType.createSubTypeSuffix("?"));
		assertThrows(IllegalArgumentException.class, () -> MediaType.createSubTypeSuffix("+foo"));
	}

	//parameters

	/**
	 * Tests that {@link MediaType.Parameter#of(String, String)} functions correctly with standard charsets, which the implementation may cache or use existing
	 * singletons.
	 * @implSpec This implementation uses the non-public constructor to bypass the static factory method to confirm it functions correctly.
	 */
	@Test
	public void testParameterOfStandardCharsets() {
		assertThat(MediaType.Parameter.of("charset", "us-ascii"), is(equalTo(new MediaType.Parameter("charset", "us-ascii"))));
		assertThat(MediaType.Parameter.of("charset", "iso-8859-1"), is(equalTo(new MediaType.Parameter("charset", "iso-8859-1"))));
		assertThat(MediaType.Parameter.of("charset", "utf-8"), is(equalTo(new MediaType.Parameter("charset", "utf-8"))));
	}

	/** @see MediaType.Parameter#getName() */
	@Test
	public void testParameterCharsetNameNormalizedToLowercase() {
		assertThat(MediaType.Parameter.of("foo", "bar").getName(), equalTo("foo"));
		assertThat(MediaType.Parameter.of("foo", "BAR").getName(), equalTo("foo"));
		assertThat(MediaType.Parameter.of("Foo", "bar").getName(), equalTo("foo"));
		assertThat(MediaType.Parameter.of("FOO", "bar").getName(), equalTo("foo"));
		assertThat(MediaType.Parameter.of("FOO", "BAR").getName(), equalTo("foo"));
		assertThat(MediaType.Parameter.of("charset", "us-ascii").getName(), equalTo("charset"));
		assertThat(MediaType.Parameter.of("charset", "US-ASCII").getName(), equalTo("charset"));
		assertThat(MediaType.Parameter.of("Charset", "us-ascii").getName(), equalTo("charset"));
		assertThat(MediaType.Parameter.of("charSet", "us-ascii").getName(), equalTo("charset"));
		assertThat(MediaType.Parameter.of("CHARSET", "us-ascii").getName(), equalTo("charset"));
	}

	/** @see MediaType.Parameter#getValue() */
	@Test
	public void testParameterOnlyCharsetValueNormalizedToLowercase() {
		assertThat(MediaType.Parameter.of("foo", "bar").getValue(), equalTo("bar"));
		assertThat(MediaType.Parameter.of("foo", "Bar").getValue(), equalTo("Bar"));
		assertThat(MediaType.Parameter.of("foo", "baR").getValue(), equalTo("baR"));
		assertThat(MediaType.Parameter.of("foo", "BAR").getValue(), equalTo("BAR"));
		assertThat(MediaType.Parameter.of("FOO", "bar").getValue(), equalTo("bar"));
		assertThat(MediaType.Parameter.of("FOO", "Bar").getValue(), equalTo("Bar"));
		assertThat(MediaType.Parameter.of("FOO", "BAR").getValue(), equalTo("BAR"));
		assertThat(MediaType.Parameter.of("charset", "us-ascii").getValue(), equalTo("us-ascii"));
		assertThat(MediaType.Parameter.of("charset", "US-ascii").getValue(), equalTo("us-ascii"));
		assertThat(MediaType.Parameter.of("charset", "US-ASCII").getValue(), equalTo("us-ascii"));
		assertThat(MediaType.Parameter.of("charSet", "us-Ascii").getValue(), equalTo("us-ascii"));
		assertThat(MediaType.Parameter.of("CHARSET", "us-ascii").getValue(), equalTo("us-ascii"));
		assertThat(MediaType.Parameter.of("CHARSET", "US-ASCII").getValue(), equalTo("us-ascii"));
	}

	/** @see MediaType.Parameter#equals(Object) */
	@Test
	public void testParameterEquality() {
		assertThat(MediaType.Parameter.of("foo", "bar"), equalTo(MediaType.Parameter.of("foo", "bar")));
		assertThat(MediaType.Parameter.of("foo", "bar"), equalTo(MediaType.Parameter.of("Foo", "bar")));
		assertThat(MediaType.Parameter.of("fOO", "bar"), equalTo(MediaType.Parameter.of("FOO", "bar")));
		assertThat(MediaType.Parameter.of("foo", "bar"), not(equalTo(MediaType.Parameter.of("foo", "Bar"))));
		assertThat(MediaType.Parameter.of("foo", "bar"), not(equalTo(MediaType.Parameter.of("foo", "BAR"))));
		//charset value case insensitive
		assertThat(MediaType.Parameter.of("charset", "utf-8"), equalTo(MediaType.Parameter.of("charset", "utf-8")));
		assertThat(MediaType.Parameter.of("charset", "utf-8"), equalTo(MediaType.Parameter.of("charset", "UTF-8")));
	}

	/** A map of parameter serializations, quoted if needed, associated with their raw values. */
	private static final Map<String, String> PARAMETER_VALUE_STRINGS_BY_VALUE = Stream.of(
			//TODO switch to Java 11 Entry.of()
			new SimpleImmutableEntry<>("", "\"\""), new SimpleImmutableEntry<>("x", "x"), new SimpleImmutableEntry<>("3", "3"),
			new SimpleImmutableEntry<>("foo", "foo"), new SimpleImmutableEntry<>("fooBar", "fooBar"), new SimpleImmutableEntry<>("FooBar", "FooBar"),
			new SimpleImmutableEntry<>("FOOBAR", "FOOBAR"), new SimpleImmutableEntry<>("foo bar", "\"foo bar\""),
			new SimpleImmutableEntry<>("foo@bar", "\"foo@bar\""), new SimpleImmutableEntry<>("foo?bar", "\"foo?bar\""),
			new SimpleImmutableEntry<>("foo=bar", "\"foo=bar\""), new SimpleImmutableEntry<>("foo\tbar", "\"foo\tbar\""),
			new SimpleImmutableEntry<>("foo\"bar\"", "\"foo\\\"bar\\\"\""), new SimpleImmutableEntry<>("\\foo\\\"bar\"", "\"\\\\foo\\\\\\\"bar\\\"\""),
			new SimpleImmutableEntry<>("foo(bar)", "\"foo(bar)\""))
			//TODO switch to Java 10 Collectors.toUnmodifiableMap()
			.collect(Collectors.collectingAndThen(toMap(Map.Entry::getKey, Map.Entry::getValue), Collections::unmodifiableMap));

	/** @see MediaType.Parameter#parseValue(CharSequence) */
	@Test
	public void testParameterParseValue() {
		PARAMETER_VALUE_STRINGS_BY_VALUE.forEach((value, valueString) -> {
			assertThat(MediaType.Parameter.parseValue(valueString), is(value));
		});
		assertThat(MediaType.Parameter.parseValue("\\"), is("\\"));
		assertThat(MediaType.Parameter.parseValue("a\\bc"), is("a\\bc"));
		assertThat(MediaType.Parameter.parseValue("a\\\\bc"), is("a\\\\bc"));
		assertThat(MediaType.Parameter.parseValue("\"\\abc\""), is("abc"));
		assertThat(MediaType.Parameter.parseValue("\"a\\bc\""), is("abc"));
		assertThat(MediaType.Parameter.parseValue("\"ab\\c\""), is("abc"));
		assertThat(MediaType.Parameter.parseValue("\"\"a\\bc\""), is("\"abc"));
		//unfinished escape sequences
		assertThrows(IllegalArgumentException.class, () -> MediaType.Parameter.parseValue("\"\\\""));
		assertThrows(IllegalArgumentException.class, () -> MediaType.Parameter.parseValue("\"abc\\\""));
	}

	/** @see MediaType.Parameter#appendValueTo(Appendable, String) */
	@Test
	public void testParameterAppendValueTo() throws IOException {
		PARAMETER_VALUE_STRINGS_BY_VALUE.forEach(throwingBiConsumer((value, valueString) -> {
			assertThat(MediaType.Parameter.appendValueTo(new StringBuilder(), value).toString(), is(valueString));
		}));
	}

}
