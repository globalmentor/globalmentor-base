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
		//duplicate parameter names
		assertThat(ContentType.parse("text/plain; test=foo; charset=us-ascii; test=bar").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain; test=foo; charset=us-ascii; test=bar").getSubType(), is("plain"));
		assertThat(ContentType.parse("text/plain; test=foo; charset=us-ascii; test=bar").getParameters(),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("test", "foo"), ContentType.Parameter.of("test", "bar")));
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
		//quoted parameters with escaped parameters and tab
		assertThat(ContentType.parse("text/plain; charset=us-ascii; test=\"foo\\\"bar\tand\\xmore\\\\stuff\"").getPrimaryType(), is("text"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; test=\"foo\\\"bar\tand\\xmore\\\\stuff\"").getSubType(), is("plain"));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; test=\"foo\\\"bar\tand\\xmore\\\\stuff\"").getParameters(),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("test", "foo\"bar\tandxmore\\stuff")));
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
		assertThat(ContentType.parse("application/vnd.oasis.opendocument.text").getPrimaryType(), is("application"));
		assertThat(ContentType.parse("application/vnd.oasis.opendocument.text").getSubType(), is("vnd.oasis.opendocument.text"));
		assertThat(ContentType.parse("multipart/form-data; charset=utf-8; boundary=\"q1w2e3r4ty:9-5xyz\"").getPrimaryType(), is("multipart"));
		assertThat(ContentType.parse("multipart/form-data; charset=utf-8; boundary=\"q1w2e3r4ty:9-5xyz\"").getSubType(), is("form-data"));
		assertThat(ContentType.parse("multipart/form-data; charset=utf-8; boundary=\"q1w2e3r4ty:9-5xyz\"").getParameters(),
				containsInAnyOrder(ContentType.Parameter.of("charset", "utf-8"), ContentType.Parameter.of("boundary", "q1w2e3r4ty:9-5xyz")));
		assertThat(ContentType.parse("application/vnd.api+json").getPrimaryType(), is("application"));
		assertThat(ContentType.parse("application/vnd.api+json").getSubType(), is("vnd.api+json"));
	}

	/** @see ContentType#RESTRICTED_NAME_PATTERN */
	@Test
	public void testRestrictedNamePattern() {
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("").matches(), is(false));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("charset").matches(), is(true));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("x").matches(), is(true));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("3").matches(), is(true));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("?").matches(), is(false));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("_").matches(), is(false));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("x?").matches(), is(false));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("x_").matches(), is(true));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("x3").matches(), is(true));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("fooBar").matches(), is(true));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("foo bar").matches(), is(false));
		assertThat(ContentType.RESTRICTED_NAME_PATTERN.matcher("foo:bar").matches(), is(false));
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
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; test=\"foo\tbar\"").matches(), is(true)); //literal tab
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; test=\"foo\\tbar\"").matches(), is(true)); //escaped `t` (not tab)
		assertThat(ContentType.PARAMETER_PATTERN.matcher("; test=\"foo\\xbar\"").matches(), is(true)); //escaped `x`
	}

	/** Tests of {@link ContentType#parseParameters(CharSequence)}. */
	@Test
	public void testParseParameters() {
		assertThat(ContentType.parseParameters("; foo=bar"), containsInAnyOrder(ContentType.Parameter.of("foo", "bar")));
		assertThat(ContentType.parseParameters("; foo=\"bar\""), containsInAnyOrder(ContentType.Parameter.of("foo", "bar")));
		assertThat(ContentType.parseParameters("; foo=\" bar\""), containsInAnyOrder(ContentType.Parameter.of("foo", " bar")));
		assertThat(ContentType.parseParameters("; charset=us-ascii; foo=bar"),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")));
		assertThat(ContentType.parseParameters("; charset=US-ASCII; foo=bar"),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")));
		assertThat(ContentType.parseParameters("; charset=\"us-ascii\""), containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii")));
		assertThat(ContentType.parseParameters("; charset=\"us-ascii\"; foo=bar"),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("foo", "bar")));
		assertThat(ContentType.parseParameters("; charset=us-ascii; test=\"foo;bar\""),
				containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("test", "foo;bar")));
		assertThat(ContentType.parseParameters("; test=\"(foo)<bar>@\\\",;:\\\\/[foobar]?=\""),
				containsInAnyOrder(ContentType.Parameter.of("test", "(foo)<bar>@\",;:\\/[foobar]?=")));
		assertThrows(IllegalArgumentException.class, () -> ContentType.parseParameters("; foo= bar"));
		assertThrows(IllegalArgumentException.class, () -> ContentType.parseParameters("charset=us-ascii foo=bar"));
		assertThrows(IllegalArgumentException.class, () -> ContentType.parseParameters("; charset=us-ascii= foo=bar"));
		assertThrows(IllegalArgumentException.class, () -> ContentType.parseParameters("charset=us-ascii=foo=bar"));
	}

	/** @see ContentType#findCharset() */
	@Test
	public void testFindCharset() {
		assertThat(ContentType.parse("text/plain").findCharset(), isEmpty());
		assertThat(ContentType.parse("text/plain; foo=bar").findCharset(), isEmpty());
		assertThat(ContentType.parse("text/plain; charset=us-ascii").findCharset(), isPresentAndIs(US_ASCII));
		assertThat(ContentType.parse("text/plain; charset=us-ascii; foo=bar").findCharset(), isPresentAndIs(US_ASCII));
		assertThat(ContentType.parse("text/plain; foo=bar; charset=us-ascii; test=example").findCharset(), isPresentAndIs(US_ASCII));
	}

	/** @see ContentType#withParameter(String, String) */
	@Test
	public void testWithParameterAddsNewParameterName() {
		{
			final ContentType contentType = ContentType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final ContentType contentTypeWithParameter = contentType.withParameter("new", "foobar");
			assertThat(contentTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(contentTypeWithParameter.getSubType(), is("plain"));
			assertThat(contentTypeWithParameter.getParameters(), containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"),
					ContentType.Parameter.of("foo", "bar"), ContentType.Parameter.of("test", "example"), ContentType.Parameter.of("new", "foobar")));
		}
		{ //different case
			final ContentType contentType = ContentType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final ContentType contentTypeWithParameter = contentType.withParameter("NEW", "foobar");
			assertThat(contentTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(contentTypeWithParameter.getSubType(), is("plain"));
			assertThat(contentTypeWithParameter.getParameters(), containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"),
					ContentType.Parameter.of("foo", "bar"), ContentType.Parameter.of("test", "example"), ContentType.Parameter.of("new", "foobar")));
		}
		{ //duplicate parameter names
			final ContentType contentType = ContentType.parse("text/plain; test=foo; charset=us-ascii; test=bar");
			final ContentType contentTypeWithParameter = contentType.withParameter("new", "foobar");
			assertThat(contentTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(contentTypeWithParameter.getSubType(), is("plain"));
			assertThat(contentTypeWithParameter.getParameters(), containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"),
					ContentType.Parameter.of("test", "foo"), ContentType.Parameter.of("test", "bar"), ContentType.Parameter.of("new", "foobar")));
		}
	}

	/** @see ContentType#withParameter(String, String) */
	@Test
	public void testWithParameterReplacesExistingParameterName() {
		{
			final ContentType contentType = ContentType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final ContentType contentTypeWithParameter = contentType.withParameter("test", "foobar");
			assertThat(contentTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(contentTypeWithParameter.getSubType(), is("plain"));
			assertThat(contentTypeWithParameter.getParameters(), containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"),
					ContentType.Parameter.of("foo", "bar"), ContentType.Parameter.of("test", "foobar")));
		}
		{ //different case
			final ContentType contentType = ContentType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final ContentType contentTypeWithParameter = contentType.withParameter("test", "foobar");
			assertThat(contentTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(contentTypeWithParameter.getSubType(), is("plain"));
			assertThat(contentTypeWithParameter.getParameters(), containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"),
					ContentType.Parameter.of("foo", "bar"), ContentType.Parameter.of("test", "foobar")));
		}
		{ //duplicate parameter names
			final ContentType contentType = ContentType.parse("text/plain; test=foo; charset=us-ascii; test=bar");
			final ContentType contentTypeWithParameter = contentType.withParameter("test", "foobar");
			assertThat(contentTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(contentTypeWithParameter.getSubType(), is("plain"));
			assertThat(contentTypeWithParameter.getParameters(),
					containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("test", "foobar")));
		}
		{ //duplicate parameter names; value matches that to replace
			final ContentType contentType = ContentType.parse("text/plain; test=foo; charset=us-ascii; test=bar");
			final ContentType contentTypeWithParameter = contentType.withParameter("test", "foo");
			assertThat(contentTypeWithParameter.getPrimaryType(), is("text"));
			assertThat(contentTypeWithParameter.getSubType(), is("plain"));
			assertThat(contentTypeWithParameter.getParameters(),
					containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"), ContentType.Parameter.of("test", "foo")));
		}
	}

	/** @see ContentType#withCharset(Charset) */
	@Test
	public void testWithCharset() {
		{
			final ContentType contentType = ContentType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final ContentType contentTypeWithCharset = contentType.withCharset(US_ASCII);
			assertThat(contentTypeWithCharset.getPrimaryType(), is("text"));
			assertThat(contentTypeWithCharset.getSubType(), is("plain"));
			assertThat(contentTypeWithCharset.getParameters(), containsInAnyOrder(ContentType.Parameter.of("charset", "us-ascii"),
					ContentType.Parameter.of("foo", "bar"), ContentType.Parameter.of("test", "example")));
		}
		{
			final ContentType contentType = ContentType.parse("text/plain; charset=us-ascii; foo=bar; test=example");
			final ContentType contentTypeWithCharset = contentType.withCharset(UTF_8);
			assertThat(contentTypeWithCharset.getPrimaryType(), is("text"));
			assertThat(contentTypeWithCharset.getSubType(), is("plain"));
			assertThat(contentTypeWithCharset.getParameters(), containsInAnyOrder(ContentType.Parameter.of("charset", "utf-8"),
					ContentType.Parameter.of("foo", "bar"), ContentType.Parameter.of("test", "example")));
		}
		{
			final ContentType contentType = ContentType.parse("text/plain; foo=bar; test=example");
			final ContentType contentTypeWithCharset = contentType.withCharset(UTF_8);
			assertThat(contentTypeWithCharset.getPrimaryType(), is("text"));
			assertThat(contentTypeWithCharset.getSubType(), is("plain"));
			assertThat(contentTypeWithCharset.getParameters(), containsInAnyOrder(ContentType.Parameter.of("charset", "utf-8"),
					ContentType.Parameter.of("foo", "bar"), ContentType.Parameter.of("test", "example")));
		}
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
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), equalTo(ContentType.parse("text/plain; charset=us-ascii")));
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), equalTo(ContentType.parse("text/Plain; charset=us-ascii")));
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), equalTo(ContentType.parse("text/plain; charSet=us-ascii")));
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), equalTo(ContentType.parse("text/plain; charset=us-Ascii")));
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), equalTo(ContentType.parse("Text/Plain; charset=US-ascii")));
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), equalTo(ContentType.parse("text/plain; charset=us-ascii")));
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), equalTo(ContentType.parse("TEXT/PLAIN; CHARSET=us-ascii")));
		assertThat(ContentType.parse("text/plain; charset=us-ascii"), equalTo(ContentType.parse("TEXT/PLAIN; CHARSET=US-ASCII")));
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
		assertThat(ContentType.parse("text/html; charset=UTF-8").toString(), is("text/html;charset=utf-8"));
		assertThat(ContentType.parse("text/html; charset=UTF-8").toString(false), is("text/html;charset=utf-8"));
		assertThat(ContentType.parse("text/html; charset=UTF-8").toString(true), is("text/html; charset=utf-8"));
	}

	@Test
	public void testMissingDelimiter() {
		assertThrows(IllegalArgumentException.class, () -> ContentType.parse("text"));
	}

	@Test
	public void testMissingSubType() {
		assertThrows(IllegalArgumentException.class, () -> ContentType.parse("text/"));
	}

	@Test
	public void testMissingParameters() {
		assertThrows(IllegalArgumentException.class, () -> ContentType.parse("text/plain;"));
	}

	@Test
	public void testMissingParameterDelimiter() {
		assertThrows(IllegalArgumentException.class, () -> ContentType.parse("text/plain; charset"));
	}

	@Test
	public void testMissingParameterValue() {
		assertThrows(IllegalArgumentException.class, () -> ContentType.parse("text/plain; charset="));
	}

	@Test
	public void testMissingSecondParameter() {
		assertThrows(IllegalArgumentException.class, () -> ContentType.parse("text/plain; charset=us-ascii;"));
	}

	/** @see ContentType#hasSubTypeSuffix(String...) */
	@Test
	public void testHasSubTypeSuffix() {
		assertThat(ContentType.of("text", "plain").hasSubTypeSuffix("foo"), is(false));
		assertThat(ContentType.of("text", "test").hasSubTypeSuffix("foo"), is(false));
		assertThat(ContentType.of("text", "test+foo").hasSubTypeSuffix("foo"), is(true));
		assertThat(ContentType.of("text", "TEST+Foo").hasSubTypeSuffix("FOO"), is(true));
		assertThat(ContentType.of("text", "test+foo+bar").hasSubTypeSuffix("foo"), is(false));
		assertThat(ContentType.of("text", "test+foo").hasSubTypeSuffix("foo", "bar"), is(false));
		assertThat(ContentType.of("text", "test+foo+bar").hasSubTypeSuffix("foo", "bar"), is(true));
		assertThat(ContentType.of("TEXT", "TEST+FOO+BAR").hasSubTypeSuffix("fOO", "Bar"), is(true));
	}

	/** @see ContentType#createSubTypeSuffix(String...) */
	@Test
	public void testCreateSubTypeSuffix() {
		assertThat(ContentType.createSubTypeSuffix("foo"), is("+foo"));
		assertThat(ContentType.createSubTypeSuffix("123"), is("+123"));
		assertThat(ContentType.createSubTypeSuffix("FOO"), is("+foo"));
		assertThat(ContentType.createSubTypeSuffix("foo", "bar"), is("+foo+bar"));
		assertThat(ContentType.createSubTypeSuffix("Foo", "BAR"), is("+foo+bar"));
		assertThrows(IllegalArgumentException.class, () -> ContentType.createSubTypeSuffix(""));
		assertThrows(IllegalArgumentException.class, () -> ContentType.createSubTypeSuffix("+"));
		assertThrows(IllegalArgumentException.class, () -> ContentType.createSubTypeSuffix("?"));
		assertThrows(IllegalArgumentException.class, () -> ContentType.createSubTypeSuffix("+foo"));
	}

	//parameters

	/** @see ContentType.Parameter#getName() */
	@Test
	public void testParameterCharsetNameNormalizedToLowercase() {
		assertThat(ContentType.Parameter.of("foo", "bar").getName(), equalTo("foo"));
		assertThat(ContentType.Parameter.of("foo", "BAR").getName(), equalTo("foo"));
		assertThat(ContentType.Parameter.of("Foo", "bar").getName(), equalTo("foo"));
		assertThat(ContentType.Parameter.of("FOO", "bar").getName(), equalTo("foo"));
		assertThat(ContentType.Parameter.of("FOO", "BAR").getName(), equalTo("foo"));
		assertThat(ContentType.Parameter.of("charset", "us-ascii").getName(), equalTo("charset"));
		assertThat(ContentType.Parameter.of("charset", "US-ASCII").getName(), equalTo("charset"));
		assertThat(ContentType.Parameter.of("Charset", "us-ascii").getName(), equalTo("charset"));
		assertThat(ContentType.Parameter.of("charSet", "us-ascii").getName(), equalTo("charset"));
		assertThat(ContentType.Parameter.of("CHARSET", "us-ascii").getName(), equalTo("charset"));
	}

	/** @see ContentType.Parameter#getValue() */
	@Test
	public void testParameterOnlyCharsetValueNormalizedToLowercase() {
		assertThat(ContentType.Parameter.of("foo", "bar").getValue(), equalTo("bar"));
		assertThat(ContentType.Parameter.of("foo", "Bar").getValue(), equalTo("Bar"));
		assertThat(ContentType.Parameter.of("foo", "baR").getValue(), equalTo("baR"));
		assertThat(ContentType.Parameter.of("foo", "BAR").getValue(), equalTo("BAR"));
		assertThat(ContentType.Parameter.of("FOO", "bar").getValue(), equalTo("bar"));
		assertThat(ContentType.Parameter.of("FOO", "Bar").getValue(), equalTo("Bar"));
		assertThat(ContentType.Parameter.of("FOO", "BAR").getValue(), equalTo("BAR"));
		assertThat(ContentType.Parameter.of("charset", "us-ascii").getValue(), equalTo("us-ascii"));
		assertThat(ContentType.Parameter.of("charset", "US-ascii").getValue(), equalTo("us-ascii"));
		assertThat(ContentType.Parameter.of("charset", "US-ASCII").getValue(), equalTo("us-ascii"));
		assertThat(ContentType.Parameter.of("charSet", "us-Ascii").getValue(), equalTo("us-ascii"));
		assertThat(ContentType.Parameter.of("CHARSET", "us-ascii").getValue(), equalTo("us-ascii"));
		assertThat(ContentType.Parameter.of("CHARSET", "US-ASCII").getValue(), equalTo("us-ascii"));
	}

	/** @see ContentType.Parameter#equals(Object) */
	@Test
	public void testParameterEquality() {
		assertThat(ContentType.Parameter.of("foo", "bar"), equalTo(ContentType.Parameter.of("foo", "bar")));
		assertThat(ContentType.Parameter.of("foo", "bar"), equalTo(ContentType.Parameter.of("Foo", "bar")));
		assertThat(ContentType.Parameter.of("fOO", "bar"), equalTo(ContentType.Parameter.of("FOO", "bar")));
		assertThat(ContentType.Parameter.of("foo", "bar"), not(equalTo(ContentType.Parameter.of("foo", "Bar"))));
		assertThat(ContentType.Parameter.of("foo", "bar"), not(equalTo(ContentType.Parameter.of("foo", "BAR"))));
		//charset value case insensitive
		assertThat(ContentType.Parameter.of("charset", "utf-8"), equalTo(ContentType.Parameter.of("charset", "utf-8")));
		assertThat(ContentType.Parameter.of("charset", "utf-8"), equalTo(ContentType.Parameter.of("charset", "UTF-8")));
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

	/** @see ContentType.Parameter#parseValue(CharSequence) */
	@Test
	public void testParameterParseValue() {
		PARAMETER_VALUE_STRINGS_BY_VALUE.forEach((value, valueString) -> {
			assertThat(ContentType.Parameter.parseValue(valueString), is(value));
		});
		assertThat(ContentType.Parameter.parseValue("\\"), is("\\"));
		assertThat(ContentType.Parameter.parseValue("a\\bc"), is("a\\bc"));
		assertThat(ContentType.Parameter.parseValue("a\\\\bc"), is("a\\\\bc"));
		assertThat(ContentType.Parameter.parseValue("\"\\abc\""), is("abc"));
		assertThat(ContentType.Parameter.parseValue("\"a\\bc\""), is("abc"));
		assertThat(ContentType.Parameter.parseValue("\"ab\\c\""), is("abc"));
		assertThat(ContentType.Parameter.parseValue("\"\"a\\bc\""), is("\"abc"));
		//unfinished escape sequences
		assertThrows(IllegalArgumentException.class, () -> ContentType.Parameter.parseValue("\"\\\""));
		assertThrows(IllegalArgumentException.class, () -> ContentType.Parameter.parseValue("\"abc\\\""));
	}

	/** @see ContentType.Parameter#appendValueTo(Appendable, String) */
	@Test
	public void testParameterAppendValueTo() throws IOException {
		PARAMETER_VALUE_STRINGS_BY_VALUE.forEach(throwingBiConsumer((value, valueString) -> {
			assertThat(ContentType.Parameter.appendValueTo(new StringBuilder(), value).toString(), is(valueString));
		}));
	}

}
