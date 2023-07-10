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

package com.globalmentor.lex;

import static com.globalmentor.lex.CompoundTokenization.*;
import static java.util.Arrays.*;
import static java.util.Collections.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link CompoundTokenization}.
 * @author Garret Wilson
 */
public class CompoundTokenizationTest {

	/** The implementations tested by this class that support round-trip split+join. */
	private static final List<CompoundTokenization> IMPLEMENTATIONS = unmodifiableList(asList(CAMEL_CASE, KEBAB_CASE, SNAKE_CASE));

	/** A list of tokens to use for testing. */
	private static final List<String> TEST_TOKENS = unmodifiableList(
			asList("foobar", "fooBar", "FooBar", "foo-bar", "foo-Bar", "foo_bar", "foo_Bar", "x", "X", "CDlibrary", "CdLibrary", "CD-library", "CD_library",
					"userCDlibrary", "userCdLibrary", "user-CD-library", "user_CD_library", "URL", "URLconverter", "UrlConverter", "URL-converter", "URL_converter",
					"oldURLconverter", "oldUrlConverter", "old-URL-converter", "old_URL_converter", "oldURL", "old-URL", "old_URL"));

	/**
	 * @see CompoundTokenization#split(CharSequence)
	 * @see CompoundTokenization#join(Iterable)
	 * @see #IMPLEMENTATIONS
	 * @see #TEST_TOKENS
	 */
	@Test
	void testSplitJoinRoundTripForAllImplementations() {
		for(final CompoundTokenization tokenization : IMPLEMENTATIONS) {
			for(final String token : TEST_TOKENS) {
				assertThat("(" + tokenization.getName() + ") " + token, tokenization.join(tokenization.split(token)), is(token));
			}
		}
	}

	//## camelCase

	/**
	 * @see CompoundTokenization#CAMEL_CASE
	 * @see CamelCase#isDromedaryCase(CharSequence)
	 */
	@Test
	void testCamelCaseIsDromedaryCase() {
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.isDromedaryCase(""));
		assertThat(CAMEL_CASE.isDromedaryCase("fooBar"), is(true));
		assertThat(CAMEL_CASE.isDromedaryCase("$fooBar"), is(false));
		assertThat(CAMEL_CASE.isDromedaryCase("$FooBar"), is(false));
		assertThat(CAMEL_CASE.isDromedaryCase("FooBar"), is(false));
	}

	/**
	 * @see CompoundTokenization#CAMEL_CASE
	 * @see CamelCase#toDromedaryCase(CharSequence)
	 */
	@Test
	void testCamelCaseToDromedaryCase() {
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.toDromedaryCase(""));
		assertThat(CAMEL_CASE.toDromedaryCase("fooBar"), is("fooBar"));
		assertThat(CAMEL_CASE.toDromedaryCase("$fooBar"), is("$fooBar"));
		assertThat(CAMEL_CASE.toDromedaryCase("$FooBar"), is("$FooBar"));
		assertThat(CAMEL_CASE.toDromedaryCase("FooBar"), is("fooBar"));
	}

	/**
	 * @see CompoundTokenization#CAMEL_CASE
	 * @see CamelCase#isPascalCase(CharSequence)
	 */
	@Test
	void testCamelCaseIsPascalCase() {
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.isPascalCase(""));
		assertThat(CAMEL_CASE.isPascalCase("fooBar"), is(false));
		assertThat(CAMEL_CASE.isPascalCase("$fooBar"), is(false));
		assertThat(CAMEL_CASE.isPascalCase("$FooBar"), is(false));
		assertThat(CAMEL_CASE.isPascalCase("FooBar"), is(true));
	}

	/**
	 * @see CompoundTokenization#CAMEL_CASE
	 * @see CamelCase#toDromedaryCase(CharSequence)
	 */
	@Test
	void testCamelCaseToPascalCase() {
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.toPascalCase(""));
		assertThat(CAMEL_CASE.toPascalCase("fooBar"), is("FooBar"));
		assertThat(CAMEL_CASE.toPascalCase("$fooBar"), is("$fooBar"));
		assertThat(CAMEL_CASE.toPascalCase("$FooBar"), is("$FooBar"));
		assertThat(CAMEL_CASE.toPascalCase("FooBar"), is("FooBar"));
	}

	/**
	 * @see CompoundTokenization#CAMEL_CASE
	 * @see CompoundTokenization#split(CharSequence)
	 */
	@Test
	void testCamelCaseSplit() {
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.split(""));
		assertThat(CAMEL_CASE.split("foobar"), contains("foobar"));
		assertThat(CAMEL_CASE.split("fooBar"), contains("foo", "bar"));
		assertThat(CAMEL_CASE.split("FooBar"), contains("Foo", "bar"));
		assertThat(CAMEL_CASE.split("foo.bar"), contains("foo.bar"));
		assertThat(CAMEL_CASE.split("foo.Bar"), contains("foo.", "bar"));
		assertThat(CAMEL_CASE.split("foo-bar"), contains("foo-bar"));
		assertThat(CAMEL_CASE.split("foo-Bar"), contains("foo-", "bar"));
		assertThat(CAMEL_CASE.split("foo_bar"), contains("foo_bar"));
		assertThat(CAMEL_CASE.split("foo_Bar"), contains("foo_", "bar"));
		assertThat(CAMEL_CASE.split("x"), contains("x"));
		assertThat(CAMEL_CASE.split("X"), contains("X"));
		assertThat(CAMEL_CASE.split("CDlibrary"), contains("CDlibrary"));
		assertThat(CAMEL_CASE.split("CdLibrary"), contains("Cd", "library"));
		assertThat(CAMEL_CASE.split("userCDlibrary"), contains("user", "CDlibrary"));
		assertThat(CAMEL_CASE.split("userCdLibrary"), contains("user", "cd", "library"));
		assertThat(CAMEL_CASE.split("URL"), contains("URL"));
		assertThat(CAMEL_CASE.split("URLconverter"), contains("URLconverter"));
		assertThat(CAMEL_CASE.split("UrlConverter"), contains("Url", "converter"));
		assertThat(CAMEL_CASE.split("oldURLconverter"), contains("old", "URLconverter"));
		assertThat(CAMEL_CASE.split("oldUrlConverter"), contains("old", "url", "converter"));
		assertThat(CAMEL_CASE.split("oldURL"), contains("old", "URL"));
	}

	/**
	 * @see CompoundTokenization#CAMEL_CASE
	 * @see CompoundTokenization#join(Iterable)
	 */
	@Test
	void testCamelCaseJoin() {
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.join(asList()));
		assertThat(CAMEL_CASE.join(asList("foobar")), is("foobar"));
		assertThat(CAMEL_CASE.join(asList("foo", "bar")), is("fooBar"));
		assertThat(CAMEL_CASE.join(asList("Foo", "bar")), is("FooBar"));
		assertThat(CAMEL_CASE.join(asList("foo.bar")), is("foo.bar"));
		assertThat(CAMEL_CASE.join(asList("foo.", "bar")), is("foo.Bar"));
		assertThat(CAMEL_CASE.join(asList("foo-bar")), is("foo-bar"));
		assertThat(CAMEL_CASE.join(asList("foo-", "bar")), is("foo-Bar"));
		assertThat(CAMEL_CASE.join(asList("foo_bar")), is("foo_bar"));
		assertThat(CAMEL_CASE.join(asList("foo_", "bar")), is("foo_Bar"));
		assertThat(CAMEL_CASE.join(asList("x")), is("x"));
		assertThat(CAMEL_CASE.join(asList("X")), is("X"));
		assertThat(CAMEL_CASE.join(asList("CDlibrary")), is("CDlibrary"));
		assertThat(CAMEL_CASE.join(asList("cd", "library")), is("cdLibrary"));
		assertThat(CAMEL_CASE.join(asList("user", "CDlibrary")), is("userCDlibrary"));
		assertThat(CAMEL_CASE.join(asList("user", "cd", "library")), is("userCdLibrary"));
		assertThat(CAMEL_CASE.join(asList("URL")), is("URL"));
		assertThat(CAMEL_CASE.join(asList("URLconverter")), is("URLconverter"));
		assertThat(CAMEL_CASE.join(asList("url", "converter")), is("urlConverter"));
		assertThat(CAMEL_CASE.join(asList("old", "URLconverter")), is("oldURLconverter"));
		assertThat(CAMEL_CASE.join(asList("old", "url", "converter")), is("oldUrlConverter"));
		assertThat(CAMEL_CASE.join(asList("old", "URL")), is("oldURL"));
	}

	/** @see CompoundTokenization#CAMEL_CASE */
	@Test
	void testCamelCaseToCamelCase() {
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.to(CAMEL_CASE, ""));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "foobar"), is("foobar"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "fooBar"), is("fooBar"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "FooBar"), is("FooBar"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "foo.bar"), is("foo.bar"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "foo.Bar"), is("foo.Bar"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "foo-bar"), is("foo-bar"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "foo-Bar"), is("foo-Bar"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "foo_bar"), is("foo_bar"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "foo_Bar"), is("foo_Bar"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "x"), is("x"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "X"), is("X"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "CD-library"), is("CD-library"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "user-CD-library"), is("user-CD-library"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "URL"), is("URL"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "URL-converter"), is("URL-converter"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "old-URL-converter"), is("old-URL-converter"));
		assertThat(CAMEL_CASE.to(CAMEL_CASE, "old-URL"), is("old-URL"));
	}

	/**
	 * @see CompoundTokenization#CAMEL_CASE
	 * @see CompoundTokenization#KEBAB_CASE
	 */
	@Test
	void testCamelCaseToKebabCase() {
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.to(KEBAB_CASE, ""));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "foobar"), is("foobar"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "fooBar"), is("foo-bar"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "FooBar"), is("Foo-bar"));
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.to(KEBAB_CASE, "foo-bar"),
				"Kebab case components, split from camel case, must not already use kebab case delimiter.");
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.to(KEBAB_CASE, "foo-Bar"),
				"Kebab case components, split from camel case, must not already use kebab case delimiter.");
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "foo_bar"), is("foo_bar"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "foo_Bar"), is("foo_-bar"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "x"), is("x"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "X"), is("X"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "CDlibrary"), is("CDlibrary"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "CdLibrary"), is("Cd-library"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "userCDlibrary"), is("user-CDlibrary"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "userCdLibrary"), is("user-cd-library"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "URL"), is("URL"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "URLconverter"), is("URLconverter"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "UrlConverter"), is("Url-converter"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "oldURLconverter"), is("old-URLconverter"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "oldUrlConverter"), is("old-url-converter"));
		assertThat(CAMEL_CASE.to(KEBAB_CASE, "oldURL"), is("old-URL"));
	}

	/**
	 * @see CompoundTokenization#CAMEL_CASE
	 * @see CompoundTokenization#SNAKE_CASE
	 */
	@Test
	void testCamelCaseToSnakeCase() {
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.to(SNAKE_CASE, ""));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "foobar"), is("foobar"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "fooBar"), is("foo_bar"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "FooBar"), is("Foo_bar"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "foo.bar"), is("foo.bar"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "foo.Bar"), is("foo._bar"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "foo-bar"), is("foo-bar"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "foo-Bar"), is("foo-_bar"));
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.to(SNAKE_CASE, "foo_bar"),
				"Snake case components, split from camel case, must not already use snake case delimiter.");
		assertThrows(IllegalArgumentException.class, () -> CAMEL_CASE.to(SNAKE_CASE, "foo_Bar"),
				"Snake case components, split from camel case, must not already use snake case delimiter.");
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "x"), is("x"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "X"), is("X"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "CDlibrary"), is("CDlibrary"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "CdLibrary"), is("Cd_library"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "userCDlibrary"), is("user_CDlibrary"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "userCdLibrary"), is("user_cd_library"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "URL"), is("URL"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "URLconverter"), is("URLconverter"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "UrlConverter"), is("Url_converter"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "oldURLconverter"), is("old_URLconverter"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "oldUrlConverter"), is("old_url_converter"));
		assertThat(CAMEL_CASE.to(SNAKE_CASE, "oldURL"), is("old_URL"));
	}

	//## dot.case

	/**
	 * @see CompoundTokenization#DOT_CASE
	 * @see CompoundTokenization#split(CharSequence)
	 */
	@Test
	void testDotCaseSplit() {
		assertThrows(IllegalArgumentException.class, () -> DOT_CASE.split(""));
		assertThat(DOT_CASE.split("foobar"), contains("foobar"));
		assertThat(DOT_CASE.split("fooBar"), contains("fooBar"));
		assertThat(DOT_CASE.split("FooBar"), contains("FooBar"));
		assertThat(DOT_CASE.split("foo.bar"), contains("foo", "bar"));
		assertThat(DOT_CASE.split("foo.Bar"), contains("foo", "Bar"));
		assertThat(DOT_CASE.split("foo-bar"), contains("foo-bar"));
		assertThat(DOT_CASE.split("foo-Bar"), contains("foo-Bar"));
		assertThat(DOT_CASE.split("foo_bar"), contains("foo_bar"));
		assertThat(DOT_CASE.split("foo_Bar"), contains("foo_Bar"));
		assertThat(DOT_CASE.split("x"), contains("x"));
		assertThat(DOT_CASE.split("X"), contains("X"));
	}

	/**
	 * @see CompoundTokenization#DOT_CASE
	 * @see CompoundTokenization#join(Iterable)
	 */
	@Test
	void testDotCaseJoin() {
		assertThrows(IllegalArgumentException.class, () -> DOT_CASE.join(asList()));
		assertThat(DOT_CASE.join(asList("foobar")), is("foobar"));
		assertThat(DOT_CASE.join(asList("foo", "bar")), is("foo.bar"));
		assertThat(DOT_CASE.join(asList("Foo", "bar")), is("Foo.bar"));
		assertThrows(IllegalArgumentException.class, () -> DOT_CASE.join(asList("foo.bar")));
		assertThrows(IllegalArgumentException.class, () -> DOT_CASE.join(asList("foo.", "bar")));
		assertThat(DOT_CASE.join(asList("foo-bar")), is("foo-bar"));
		assertThat(DOT_CASE.join(asList("foo-", "bar")), is("foo-.bar"));
		assertThat(DOT_CASE.join(asList("foo_bar")), is("foo_bar"));
		assertThat(DOT_CASE.join(asList("foo_", "bar")), is("foo_.bar"));
		assertThat(DOT_CASE.join(asList("x")), is("x"));
		assertThat(DOT_CASE.join(asList("X")), is("X"));
	}

	/**
	 * @see CompoundTokenization#DOT_CASE
	 * @see CompoundTokenization#CAMEL_CASE
	 */
	@Test
	void testDotCaseToCamelCase() {
		assertThrows(IllegalArgumentException.class, () -> DOT_CASE.to(CAMEL_CASE, ""));
		assertThat(DOT_CASE.to(CAMEL_CASE, "foobar"), is("foobar"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "fooBar"), is("fooBar"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "FooBar"), is("FooBar"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "foo.bar"), is("fooBar"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "foo.Bar"), is("fooBar"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "foo-bar"), is("foo-bar"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "foo-Bar"), is("foo-Bar"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "foo_bar"), is("foo_bar"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "foo_Bar"), is("foo_Bar"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "x"), is("x"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "X"), is("X"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "CD.library"), is("CDLibrary"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "user.CD.library"), is("userCDLibrary"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "URL"), is("URL"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "URL.converter"), is("URLConverter"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "old.URL.converter"), is("oldURLConverter"));
		assertThat(DOT_CASE.to(CAMEL_CASE, "old.URL"), is("oldURL"));
	}

	/** @see CompoundTokenization#DOT_CASE */
	@Test
	void testDotCaseToDotCase() {
		assertThrows(IllegalArgumentException.class, () -> DOT_CASE.to(DOT_CASE, ""));
		assertThat(DOT_CASE.to(DOT_CASE, "foobar"), is("foobar"));
		assertThat(DOT_CASE.to(DOT_CASE, "fooBar"), is("fooBar"));
		assertThat(DOT_CASE.to(DOT_CASE, "FooBar"), is("FooBar"));
		assertThat(DOT_CASE.to(DOT_CASE, "foo.bar"), is("foo.bar"));
		assertThat(DOT_CASE.to(DOT_CASE, "foo.Bar"), is("foo.Bar"));
		assertThat(DOT_CASE.to(DOT_CASE, "foo-bar"), is("foo-bar"));
		assertThat(DOT_CASE.to(DOT_CASE, "foo-Bar"), is("foo-Bar"));
		assertThat(DOT_CASE.to(DOT_CASE, "foo_bar"), is("foo_bar"));
		assertThat(DOT_CASE.to(DOT_CASE, "foo_Bar"), is("foo_Bar"));
		assertThat(DOT_CASE.to(DOT_CASE, "x"), is("x"));
		assertThat(DOT_CASE.to(DOT_CASE, "X"), is("X"));
		assertThat(DOT_CASE.to(DOT_CASE, "CD-library"), is("CD-library"));
		assertThat(DOT_CASE.to(DOT_CASE, "user-CD-library"), is("user-CD-library"));
		assertThat(DOT_CASE.to(DOT_CASE, "URL"), is("URL"));
		assertThat(DOT_CASE.to(DOT_CASE, "URL-converter"), is("URL-converter"));
		assertThat(DOT_CASE.to(DOT_CASE, "old-URL-converter"), is("old-URL-converter"));
		assertThat(DOT_CASE.to(DOT_CASE, "old-URL"), is("old-URL"));
	}

	/**
	 * @see CompoundTokenization#DOT_CASE
	 * @see CompoundTokenization#SNAKE_CASE
	 */
	@Test
	void testDotCaseToSnakeCase() {
		assertThrows(IllegalArgumentException.class, () -> DOT_CASE.to(SNAKE_CASE, ""));
		assertThat(DOT_CASE.to(SNAKE_CASE, "foobar"), is("foobar"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "fooBar"), is("fooBar"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "FooBar"), is("FooBar"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "foo.bar"), is("foo_bar"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "foo.Bar"), is("foo_Bar"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "foo-bar"), is("foo-bar"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "foo-Bar"), is("foo-Bar"));
		assertThrows(IllegalArgumentException.class, () -> DOT_CASE.to(SNAKE_CASE, "foo_bar"),
				"Snake case components, split from dot case, must not already use snake case delimiter.");
		assertThrows(IllegalArgumentException.class, () -> DOT_CASE.to(SNAKE_CASE, "foo_Bar"),
				"Snake case components, split from dot case, must not already use snake case delimiter.");
		assertThat(DOT_CASE.to(SNAKE_CASE, "x"), is("x"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "X"), is("X"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "CD.library"), is("CD_library"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "user.CD.library"), is("user_CD_library"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "URL"), is("URL"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "URL.converter"), is("URL_converter"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "old.URL.converter"), is("old_URL_converter"));
		assertThat(DOT_CASE.to(SNAKE_CASE, "old.URL"), is("old_URL"));
	}

	//## kebab-case

	/**
	 * @see CompoundTokenization#KEBAB_CASE
	 * @see CompoundTokenization#split(CharSequence)
	 */
	@Test
	void testKebabCaseSplit() {
		assertThrows(IllegalArgumentException.class, () -> KEBAB_CASE.split(""));
		assertThat(KEBAB_CASE.split("foobar"), contains("foobar"));
		assertThat(KEBAB_CASE.split("fooBar"), contains("fooBar"));
		assertThat(KEBAB_CASE.split("FooBar"), contains("FooBar"));
		assertThat(KEBAB_CASE.split("foo.bar"), contains("foo.bar"));
		assertThat(KEBAB_CASE.split("foo.Bar"), contains("foo.Bar"));
		assertThat(KEBAB_CASE.split("foo-bar"), contains("foo", "bar"));
		assertThat(KEBAB_CASE.split("foo-Bar"), contains("foo", "Bar"));
		assertThat(KEBAB_CASE.split("foo_bar"), contains("foo_bar"));
		assertThat(KEBAB_CASE.split("foo_Bar"), contains("foo_Bar"));
		assertThat(KEBAB_CASE.split("x"), contains("x"));
		assertThat(KEBAB_CASE.split("X"), contains("X"));
	}

	/**
	 * @see CompoundTokenization#KEBAB_CASE
	 * @see CompoundTokenization#join(Iterable)
	 */
	@Test
	void testKebabCaseJoin() {
		assertThrows(IllegalArgumentException.class, () -> KEBAB_CASE.join(asList()));
		assertThat(KEBAB_CASE.join(asList("foobar")), is("foobar"));
		assertThat(KEBAB_CASE.join(asList("foo", "bar")), is("foo-bar"));
		assertThat(KEBAB_CASE.join(asList("Foo", "bar")), is("Foo-bar"));
		assertThat(KEBAB_CASE.join(asList("foo.bar")), is("foo.bar"));
		assertThat(KEBAB_CASE.join(asList("foo.", "bar")), is("foo.-bar"));
		assertThrows(IllegalArgumentException.class, () -> KEBAB_CASE.join(asList("foo-bar")));
		assertThrows(IllegalArgumentException.class, () -> KEBAB_CASE.join(asList("foo-", "bar")));
		assertThat(KEBAB_CASE.join(asList("foo_bar")), is("foo_bar"));
		assertThat(KEBAB_CASE.join(asList("foo_", "bar")), is("foo_-bar"));
		assertThat(KEBAB_CASE.join(asList("x")), is("x"));
		assertThat(KEBAB_CASE.join(asList("X")), is("X"));
	}

	/**
	 * @see CompoundTokenization#KEBAB_CASE
	 * @see CompoundTokenization#CAMEL_CASE
	 */
	@Test
	void testKebabCaseToCamelCase() {
		assertThrows(IllegalArgumentException.class, () -> KEBAB_CASE.to(CAMEL_CASE, ""));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "foobar"), is("foobar"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "fooBar"), is("fooBar"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "FooBar"), is("FooBar"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "foo.bar"), is("foo.bar"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "foo.Bar"), is("foo.Bar"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "foo-bar"), is("fooBar"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "foo-Bar"), is("fooBar"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "foo_bar"), is("foo_bar"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "foo_Bar"), is("foo_Bar"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "x"), is("x"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "X"), is("X"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "CD-library"), is("CDLibrary"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "user-CD-library"), is("userCDLibrary"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "URL"), is("URL"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "URL-converter"), is("URLConverter"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "old-URL-converter"), is("oldURLConverter"));
		assertThat(KEBAB_CASE.to(CAMEL_CASE, "old-URL"), is("oldURL"));
	}

	/** @see CompoundTokenization#KEBAB_CASE */
	@Test
	void testKebabCaseToKebabCase() {
		assertThrows(IllegalArgumentException.class, () -> KEBAB_CASE.to(KEBAB_CASE, ""));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "foobar"), is("foobar"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "fooBar"), is("fooBar"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "FooBar"), is("FooBar"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "foo.bar"), is("foo.bar"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "foo.Bar"), is("foo.Bar"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "foo-bar"), is("foo-bar"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "foo-Bar"), is("foo-Bar"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "foo_bar"), is("foo_bar"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "foo_Bar"), is("foo_Bar"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "x"), is("x"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "X"), is("X"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "CD-library"), is("CD-library"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "user-CD-library"), is("user-CD-library"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "URL"), is("URL"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "URL-converter"), is("URL-converter"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "old-URL-converter"), is("old-URL-converter"));
		assertThat(KEBAB_CASE.to(KEBAB_CASE, "old-URL"), is("old-URL"));
	}

	/**
	 * @see CompoundTokenization#KEBAB_CASE
	 * @see CompoundTokenization#SNAKE_CASE
	 */
	@Test
	void testKebabCaseToSnakeCase() {
		assertThrows(IllegalArgumentException.class, () -> KEBAB_CASE.to(SNAKE_CASE, ""));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "foobar"), is("foobar"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "fooBar"), is("fooBar"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "FooBar"), is("FooBar"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "foo.bar"), is("foo.bar"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "foo.Bar"), is("foo.Bar"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "foo-bar"), is("foo_bar"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "foo-Bar"), is("foo_Bar"));
		assertThrows(IllegalArgumentException.class, () -> KEBAB_CASE.to(SNAKE_CASE, "foo_bar"),
				"Snake case components, split from kebab case, must not already use snake case delimiter.");
		assertThrows(IllegalArgumentException.class, () -> KEBAB_CASE.to(SNAKE_CASE, "foo_Bar"),
				"Snake case components, split from kebab case, must not already use snake case delimiter.");
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "x"), is("x"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "X"), is("X"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "CD-library"), is("CD_library"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "user-CD-library"), is("user_CD_library"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "URL"), is("URL"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "URL-converter"), is("URL_converter"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "old-URL-converter"), is("old_URL_converter"));
		assertThat(KEBAB_CASE.to(SNAKE_CASE, "old-URL"), is("old_URL"));
	}

	//## snake-case

	/**
	 * @see CompoundTokenization#SNAKE_CASE
	 * @see CompoundTokenization#split(CharSequence)
	 */
	@Test
	void testSnakeCaseSplit() {
		assertThrows(IllegalArgumentException.class, () -> SNAKE_CASE.split(""));
		assertThat(SNAKE_CASE.split("foobar"), contains("foobar"));
		assertThat(SNAKE_CASE.split("fooBar"), contains("fooBar"));
		assertThat(SNAKE_CASE.split("FooBar"), contains("FooBar"));
		assertThat(SNAKE_CASE.split("foo.bar"), contains("foo.bar"));
		assertThat(SNAKE_CASE.split("foo.Bar"), contains("foo.Bar"));
		assertThat(SNAKE_CASE.split("foo-bar"), contains("foo-bar"));
		assertThat(SNAKE_CASE.split("foo-Bar"), contains("foo-Bar"));
		assertThat(SNAKE_CASE.split("foo_bar"), contains("foo", "bar"));
		assertThat(SNAKE_CASE.split("foo_Bar"), contains("foo", "Bar"));
		assertThat(SNAKE_CASE.split("x"), contains("x"));
		assertThat(SNAKE_CASE.split("X"), contains("X"));
	}

	/**
	 * @see CompoundTokenization#SNAKE_CASE
	 * @see CompoundTokenization#join(Iterable)
	 */
	@Test
	void testSnakeCaseJoin() {
		assertThrows(IllegalArgumentException.class, () -> SNAKE_CASE.join(asList()));
		assertThat(SNAKE_CASE.join(asList("foobar")), is("foobar"));
		assertThat(SNAKE_CASE.join(asList("foo", "bar")), is("foo_bar"));
		assertThat(SNAKE_CASE.join(asList("Foo", "bar")), is("Foo_bar"));
		assertThat(SNAKE_CASE.join(asList("foo.bar")), is("foo.bar"));
		assertThat(SNAKE_CASE.join(asList("foo.", "bar")), is("foo._bar"));
		assertThat(SNAKE_CASE.join(asList("foo-bar")), is("foo-bar"));
		assertThat(SNAKE_CASE.join(asList("foo-", "bar")), is("foo-_bar"));
		assertThrows(IllegalArgumentException.class, () -> SNAKE_CASE.join(asList("foo_bar")));
		assertThrows(IllegalArgumentException.class, () -> SNAKE_CASE.join(asList("foo_", "bar")));
		assertThat(SNAKE_CASE.join(asList("x")), is("x"));
		assertThat(SNAKE_CASE.join(asList("X")), is("X"));
	}

	/**
	 * @see CompoundTokenization#SNAKE_CASE
	 * @see CompoundTokenization#CAMEL_CASE
	 */
	@Test
	void testSnakeCaseToCamelCase() {
		assertThrows(IllegalArgumentException.class, () -> SNAKE_CASE.to(CAMEL_CASE, ""));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "foobar"), is("foobar"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "fooBar"), is("fooBar"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "FooBar"), is("FooBar"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "foo.bar"), is("foo.bar"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "foo.Bar"), is("foo.Bar"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "foo-bar"), is("foo-bar"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "foo-Bar"), is("foo-Bar"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "foo_bar"), is("fooBar"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "foo_Bar"), is("fooBar"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "x"), is("x"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "X"), is("X"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "CD_library"), is("CDLibrary"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "user_CD_library"), is("userCDLibrary"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "URL"), is("URL"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "URL_converter"), is("URLConverter"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "old_URL_converter"), is("oldURLConverter"));
		assertThat(SNAKE_CASE.to(CAMEL_CASE, "old_URL"), is("oldURL"));
	}

	/**
	 * @see CompoundTokenization#SNAKE_CASE
	 * @see CompoundTokenization#KEBAB_CASE
	 */
	@Test
	void testSnakeCaseToKebabCase() {
		assertThrows(IllegalArgumentException.class, () -> SNAKE_CASE.to(KEBAB_CASE, ""));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "foobar"), is("foobar"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "fooBar"), is("fooBar"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "FooBar"), is("FooBar"));
		assertThrows(IllegalArgumentException.class, () -> SNAKE_CASE.to(KEBAB_CASE, "foo-bar"),
				"Kebab case components, split from snake case, must not already use kebab case delimiter.");
		assertThrows(IllegalArgumentException.class, () -> SNAKE_CASE.to(KEBAB_CASE, "foo-Bar"),
				"Kebab case components, split from snake case, must not already use kebab case delimiter.");
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "foo_bar"), is("foo-bar"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "foo_Bar"), is("foo-Bar"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "x"), is("x"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "X"), is("X"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "CD_library"), is("CD-library"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "user_CD_library"), is("user-CD-library"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "URL"), is("URL"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "URL_converter"), is("URL-converter"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "old_URL_converter"), is("old-URL-converter"));
		assertThat(SNAKE_CASE.to(KEBAB_CASE, "old_URL"), is("old-URL"));
	}

	/** @see CompoundTokenization#SNAKE_CASE */
	@Test
	void testSnakeCaseToSnakeCase() {
		assertThrows(IllegalArgumentException.class, () -> SNAKE_CASE.to(SNAKE_CASE, ""));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "foobar"), is("foobar"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "fooBar"), is("fooBar"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "FooBar"), is("FooBar"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "foo.bar"), is("foo.bar"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "foo.Bar"), is("foo.Bar"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "foo-bar"), is("foo-bar"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "foo-Bar"), is("foo-Bar"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "foo_bar"), is("foo_bar"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "foo_Bar"), is("foo_Bar"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "x"), is("x"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "X"), is("X"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "CD_library"), is("CD_library"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "user_CD_library"), is("user_CD_library"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "URL"), is("URL"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "URL_converter"), is("URL_converter"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "old_URL_converter"), is("old_URL_converter"));
		assertThat(SNAKE_CASE.to(SNAKE_CASE, "old_URL"), is("old_URL"));
	}

}
