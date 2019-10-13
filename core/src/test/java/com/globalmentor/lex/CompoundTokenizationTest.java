/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.lex;

import static com.globalmentor.lex.CompoundTokenization.*;
import static java.util.Arrays.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link CompoundTokenization}.
 * @author Garret Wilson
 */
public class CompoundTokenizationTest {

	//TODO test splitting/joining for kebab-case and snake_case

	//TODO add round-trip testing for all instances

	//camelCase

	/**
	 * @see CompoundTokenization#CAMEL_CASE
	 * @see CompoundTokenization#split(CharSequence)
	 */
	@Test
	public void testCamelCaseSplit() {
		assertThat(CAMEL_CASE.split(""), is(empty()));
		assertThat(CAMEL_CASE.split("foobar"), is(asList("foobar")));
		assertThat(CAMEL_CASE.split("fooBar"), is(asList("foo", "bar")));
		assertThat(CAMEL_CASE.split("foo-bar"), is(asList("foo-bar")));
		assertThat(CAMEL_CASE.split("foo-Bar"), is(asList("foo-", "bar")));
		assertThat(CAMEL_CASE.split("foo_bar"), is(asList("foo_bar")));
		assertThat(CAMEL_CASE.split("foo_Bar"), is(asList("foo_", "bar")));
		assertThat(CAMEL_CASE.split("x"), is(asList("x")));
		assertThat(CAMEL_CASE.split("X"), is(asList("x"))); //TODO fix not to change initial case
		assertThat(CAMEL_CASE.split("CDlibrary"), is(asList("CDlibrary")));
		assertThat(CAMEL_CASE.split("CdLibrary"), is(asList("cd", "library")));
		assertThat(CAMEL_CASE.split("userCDlibrary"), is(asList("user", "CDlibrary")));
		assertThat(CAMEL_CASE.split("userCdLibrary"), is(asList("user", "cd", "library")));
		assertThat(CAMEL_CASE.split("URL"), is(asList("URL")));
		assertThat(CAMEL_CASE.split("URLconverter"), is(asList("URLconverter")));
		assertThat(CAMEL_CASE.split("UrlConverter"), is(asList("url", "converter")));
		assertThat(CAMEL_CASE.split("oldURLconverter"), is(asList("old", "URLconverter")));
		assertThat(CAMEL_CASE.split("oldUrlConverter"), is(asList("old", "url", "converter")));
		assertThat(CAMEL_CASE.split("oldURL"), is(asList("old", "URL")));
	}

	/**
	 * @see CompoundTokenization#CAMEL_CASE
	 * @see CompoundTokenization#join(Iterable)
	 */
	@Test
	public void testCamelCaseJoin() {
		assertThat(CAMEL_CASE.join(asList()), is(""));
		assertThat(CAMEL_CASE.join(asList("foobar")), is("foobar"));
		assertThat(CAMEL_CASE.join(asList("foo", "bar")), is("fooBar"));
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

	//PascalCase

	/**
	 * @see CompoundTokenization#PASCAL_CASE
	 * @see CompoundTokenization#split(CharSequence)
	 */
	@Test
	public void testPascalCaseSplit() {
		assertThat(PASCAL_CASE.split(""), is(empty()));
		assertThat(PASCAL_CASE.split("foobar"), is(asList("foobar")));
		assertThat(PASCAL_CASE.split("fooBar"), is(asList("foo", "bar")));
		assertThat(PASCAL_CASE.split("foo-bar"), is(asList("foo-bar")));
		assertThat(PASCAL_CASE.split("foo-Bar"), is(asList("foo-", "bar")));
		assertThat(PASCAL_CASE.split("foo_bar"), is(asList("foo_bar")));
		assertThat(PASCAL_CASE.split("foo_Bar"), is(asList("foo_", "bar")));
		assertThat(PASCAL_CASE.split("x"), is(asList("x")));
		assertThat(PASCAL_CASE.split("X"), is(asList("x"))); //TODO fix not to change initial case
		assertThat(PASCAL_CASE.split("CDlibrary"), is(asList("CDlibrary")));
		assertThat(PASCAL_CASE.split("CdLibrary"), is(asList("cd", "library")));
		assertThat(PASCAL_CASE.split("userCDlibrary"), is(asList("user", "CDlibrary")));
		assertThat(PASCAL_CASE.split("userCdLibrary"), is(asList("user", "cd", "library")));
		assertThat(PASCAL_CASE.split("URL"), is(asList("URL")));
		assertThat(PASCAL_CASE.split("URLconverter"), is(asList("URLconverter")));
		assertThat(PASCAL_CASE.split("UrlConverter"), is(asList("url", "converter")));
		assertThat(PASCAL_CASE.split("oldURLconverter"), is(asList("old", "URLconverter")));
		assertThat(PASCAL_CASE.split("oldUrlConverter"), is(asList("old", "url", "converter")));
		assertThat(PASCAL_CASE.split("oldURL"), is(asList("old", "URL")));
	}

	/**
	 * @see CompoundTokenization#PASCAL_CASE
	 * @see CompoundTokenization#join(Iterable)
	 */
	@Test
	public void testPascalCaseJoin() {
		assertThat(PASCAL_CASE.join(asList()), is(""));
		assertThat(PASCAL_CASE.join(asList("foobar")), is("Foobar"));
		assertThat(PASCAL_CASE.join(asList("foo", "bar")), is("FooBar"));
		assertThat(PASCAL_CASE.join(asList("foo-bar")), is("Foo-bar"));
		assertThat(PASCAL_CASE.join(asList("foo-", "bar")), is("Foo-Bar"));
		assertThat(PASCAL_CASE.join(asList("foo_bar")), is("Foo_bar"));
		assertThat(PASCAL_CASE.join(asList("foo_", "bar")), is("Foo_Bar"));
		assertThat(PASCAL_CASE.join(asList("x")), is("X"));
		assertThat(PASCAL_CASE.join(asList("X")), is("X"));
		assertThat(PASCAL_CASE.join(asList("CDlibrary")), is("CDlibrary"));
		assertThat(PASCAL_CASE.join(asList("cd", "library")), is("CdLibrary"));
		assertThat(PASCAL_CASE.join(asList("user", "CDlibrary")), is("UserCDlibrary"));
		assertThat(PASCAL_CASE.join(asList("user", "cd", "library")), is("UserCdLibrary"));
		assertThat(PASCAL_CASE.join(asList("URL")), is("URL"));
		assertThat(PASCAL_CASE.join(asList("URLconverter")), is("URLconverter"));
		assertThat(PASCAL_CASE.join(asList("url", "converter")), is("UrlConverter"));
		assertThat(PASCAL_CASE.join(asList("old", "URLconverter")), is("OldURLconverter"));
		assertThat(PASCAL_CASE.join(asList("old", "url", "converter")), is("OldUrlConverter"));
		assertThat(PASCAL_CASE.join(asList("old", "URL")), is("OldURL"));
	}

	//kebab-case

	/**
	 * @see CompoundTokenization#KEBAB_CASE
	 * @see CompoundTokenization#toKebabCase(String)
	 */
	@Test
	public void testKebabCaseToKebabCase() {
		assertThat(KEBAB_CASE.toKebabCase("foobar"), is("foobar"));
		assertThat(KEBAB_CASE.toKebabCase("fooBar"), is("fooBar"));
		assertThat(KEBAB_CASE.toKebabCase("foo-bar"), is("foo-bar"));
		assertThat(KEBAB_CASE.toKebabCase("foo-Bar"), is("foo-Bar"));
		assertThat(KEBAB_CASE.toKebabCase("foo_bar"), is("foo_bar"));
		assertThat(KEBAB_CASE.toKebabCase("foo_Bar"), is("foo_Bar"));
		assertThat(KEBAB_CASE.toKebabCase("x"), is("x"));
		assertThat(KEBAB_CASE.toKebabCase("X"), is("X"));
		assertThat(KEBAB_CASE.toKebabCase("CD-library"), is("CD-library"));
		assertThat(KEBAB_CASE.toKebabCase("user-CD-library"), is("user-CD-library"));
		assertThat(KEBAB_CASE.toKebabCase("URL"), is("URL"));
		assertThat(KEBAB_CASE.toKebabCase("URL-converter"), is("URL-converter"));
		assertThat(KEBAB_CASE.toKebabCase("old-URL-converter"), is("old-URL-converter"));
		assertThat(KEBAB_CASE.toKebabCase("old-URL"), is("old-URL"));
	}

	/**
	 * @see CompoundTokenization#KEBAB_CASE
	 * @see CompoundTokenization#toSnakeCase(String)
	 */
	@Test
	public void testKebabCaseToSnakeCase() {
		assertThat(KEBAB_CASE.toSnakeCase("foobar"), is("foobar"));
		assertThat(KEBAB_CASE.toSnakeCase("fooBar"), is("fooBar"));
		assertThat(KEBAB_CASE.toSnakeCase("foo-bar"), is("foo_bar"));
		assertThat(KEBAB_CASE.toSnakeCase("foo-Bar"), is("foo_Bar"));
		assertThat(KEBAB_CASE.toSnakeCase("foo_bar"), is("foo_bar"));
		assertThat(KEBAB_CASE.toSnakeCase("foo_Bar"), is("foo_Bar"));
		assertThat(KEBAB_CASE.toSnakeCase("x"), is("x"));
		assertThat(KEBAB_CASE.toSnakeCase("X"), is("X"));
		assertThat(KEBAB_CASE.toSnakeCase("CD-library"), is("CD_library"));
		assertThat(KEBAB_CASE.toSnakeCase("user-CD-library"), is("user_CD_library"));
		assertThat(KEBAB_CASE.toSnakeCase("URL"), is("URL"));
		assertThat(KEBAB_CASE.toSnakeCase("URL-converter"), is("URL_converter"));
		assertThat(KEBAB_CASE.toSnakeCase("old-URL-converter"), is("old_URL_converter"));
		assertThat(KEBAB_CASE.toSnakeCase("old-URL"), is("old_URL"));
	}

	//snake-case

	/**
	 * @see CompoundTokenization#SNAKE_CASE
	 * @see CompoundTokenization#toKebabCase(String)
	 */
	@Test
	public void testSnakeCaseToKebabCase() {
		assertThat(SNAKE_CASE.toKebabCase("foobar"), is("foobar"));
		assertThat(SNAKE_CASE.toKebabCase("fooBar"), is("fooBar"));
		assertThat(SNAKE_CASE.toKebabCase("foo-bar"), is("foo-bar"));
		assertThat(SNAKE_CASE.toKebabCase("foo-Bar"), is("foo-Bar"));
		assertThat(SNAKE_CASE.toKebabCase("foo_bar"), is("foo-bar"));
		assertThat(SNAKE_CASE.toKebabCase("foo_Bar"), is("foo-Bar"));
		assertThat(SNAKE_CASE.toKebabCase("x"), is("x"));
		assertThat(SNAKE_CASE.toKebabCase("X"), is("X"));
		assertThat(SNAKE_CASE.toKebabCase("CD_library"), is("CD-library"));
		assertThat(SNAKE_CASE.toKebabCase("user_CD_library"), is("user-CD-library"));
		assertThat(SNAKE_CASE.toKebabCase("URL"), is("URL"));
		assertThat(SNAKE_CASE.toKebabCase("URL_converter"), is("URL-converter"));
		assertThat(SNAKE_CASE.toKebabCase("old_URL_converter"), is("old-URL-converter"));
		assertThat(SNAKE_CASE.toKebabCase("old_URL"), is("old-URL"));
	}

	/**
	 * @see CompoundTokenization#SNAKE_CASE
	 * @see CompoundTokenization#toSnakeCase(String)
	 */
	@Test
	public void testSnakeCaseToSnakeCase() {
		assertThat(SNAKE_CASE.toSnakeCase("foobar"), is("foobar"));
		assertThat(SNAKE_CASE.toSnakeCase("fooBar"), is("fooBar"));
		assertThat(SNAKE_CASE.toSnakeCase("foo-bar"), is("foo-bar"));
		assertThat(SNAKE_CASE.toSnakeCase("foo-Bar"), is("foo-Bar"));
		assertThat(SNAKE_CASE.toSnakeCase("foo_bar"), is("foo_bar"));
		assertThat(SNAKE_CASE.toSnakeCase("foo_Bar"), is("foo_Bar"));
		assertThat(SNAKE_CASE.toSnakeCase("x"), is("x"));
		assertThat(SNAKE_CASE.toSnakeCase("X"), is("X"));
		assertThat(SNAKE_CASE.toSnakeCase("CD_library"), is("CD_library"));
		assertThat(SNAKE_CASE.toSnakeCase("user_CD_library"), is("user_CD_library"));
		assertThat(SNAKE_CASE.toSnakeCase("URL"), is("URL"));
		assertThat(SNAKE_CASE.toSnakeCase("URL_converter"), is("URL_converter"));
		assertThat(SNAKE_CASE.toSnakeCase("old_URL_converter"), is("old_URL_converter"));
		assertThat(SNAKE_CASE.toSnakeCase("old_URL"), is("old_URL"));
	}

}
