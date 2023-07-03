/*
 * Copyright © 2023 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.model;

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static com.globalmentor.model.LanguageTag.*;
import static java.util.Arrays.*;
import static java.util.stream.Collectors.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;
import java.util.regex.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link LanguageTag}.
 * @author Garret Wilson
 */
public class LanguageTagTest {

	//## patterns

	/** @see LanguageTag#PRIVATEUSE_PATTERN */
	@Test
	void testPrivateusePattern() {
		assertThat(PRIVATEUSE_PATTERN.matcher("x-y").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-yz").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-whatever").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-just").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-just-a").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-just-a-thing").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-just-2-things").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-just-a-345").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-just-a-few123").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-whatever-x-and-a-thing").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-whatever-x-and-a-thing-another").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("X-WHATEVER-X-AND-A-THING-ANOTHER").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("x-whatever-x-and-x-another-thing").matches(), is(true));
		assertThat(PRIVATEUSE_PATTERN.matcher("X-WHATEVER-X-AND-X-ANOTHER-THING").matches(), is(true));
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternEN() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("en");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "en");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "en");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternEN_US() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("en-US");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "en");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "en");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "US");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternMN_CYRL() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("mn-Cyrl");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "mn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "mn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Cyrl");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternMN_CYRL_MN() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("mn-Cyrl-MN");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "mn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "mn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Cyrl");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "MN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternEN_CA_X_CA() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("en-CA-x-ca");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "en");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "en");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CA");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-ca");
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_CMN_HANS_CN() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-cmn-Hans-CN");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh-cmn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, "cmn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Hans");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_CN() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-CN");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN_X_WADEGILE() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-x-wadegile");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile");
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN_X_WADEGILE_PRIVATE1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-x-wadegile-private1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile-private1");
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN_A_EXTEND1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-a-extend1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "-a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN_A_EXTEND1_X_WADEGILE() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-a-extend1-x-wadegile");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "-a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile");
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN_A_EXTEND1_X_WADEGILE_PRIVATE1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-a-extend1-x-wadegile-private1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "-a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile-private1");
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN_VARIANT1_X_WADEGILE_PRIVATE1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-variant1-x-wadegile-private1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, "variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "-variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile-private1");
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN_VARIANT1_A_EXTEND1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-variant1-a-extend1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, "variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "-variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "-a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN_VARIANT1_A_EXTEND1_X_WADEGILE() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-variant1-a-extend1-x-wadegile");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, "variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "-variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "-a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile");
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN_VARIANT1_A_EXTEND1_X_WADEGILE_PRIVATE1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-variant1-a-extend1-x-wadegile-private1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, "variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "-variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "-a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile-private1");
	}

	/** @see LanguageTag#LANGTAG_PATTERN */
	@Test
	void testLangtagPatternZH_LATN_CN_VARIANT1_VARIANT2_A_EXTEND1_X_WADEGILE_PRIVATE1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-variant1-variant2-a-extend1-x-wadegile-private1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE_CODE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTLANG, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, "variant2");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANTS, "-variant1-variant2");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSIONS, "-a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile-private1");
	}

	/** Utility matching method to include the group name in the description. */
	private static void assertMatcherGroupIs(final Matcher matcher, final String group, final String value) {
		assertThat(group, matcher.group(group), is(value));
	}

	//## extension parsing

	/** @see LanguageTag#parseExtensions(String) */
	@Test
	void testParseExtension() {
		assertThat(LanguageTag.parseExtensions("-a-bc"), contains("a-bc"));
		assertThat(LanguageTag.parseExtensions("-a-myext"), contains("a-myext"));
		assertThat(LanguageTag.parseExtensions("-a-myext-b-another-one"), contains("a-myext", "b-another-one"));
		assertThat(LanguageTag.parseExtensions("-a-myext-b-another-one-c-eightlen"), contains("a-myext", "b-another-one", "c-eightlen"));
		assertThat(LanguageTag.parseExtensions("-a-myext-b-another-one-c-eightlen-z-yx"), contains("a-myext", "b-another-one", "c-eightlen", "z-yx"));
		assertThat(LanguageTag.parseExtensions("-a-really-long-subgroup-ij"), contains("a-really-long-subgroup-ij"));
		assertThat(LanguageTag.parseExtensions("-a-really-long-subgroup-7z"), contains("a-really-long-subgroup-7z"));
		assertThat(LanguageTag.parseExtensions("-a-myext-a-really-long-subgroup-z-yx"), contains("a-myext", "a-really-long-subgroup", "z-yx"));
		assertThat(LanguageTag.parseExtensions("-A-MYEXT-A-REALLY-LONG-SUBGROUP-Z-YX"), contains("A-MYEXT", "A-REALLY-LONG-SUBGROUP", "Z-YX"));
	}

	//## normalize

	/** @see LanguageTag#normalize(CharSequence) */
	@Test
	void testNormalize() {
		assertThat(normalize("en"), hasToString("en"));
		assertThat(normalize("En"), hasToString("en"));
		assertThat(normalize("eN"), hasToString("en"));
		assertThat(normalize("EN"), hasToString("en"));
		assertThat(normalize("mn"), hasToString("mn"));
		assertThat(normalize("Mn"), hasToString("mn"));
		assertThat(normalize("mN"), hasToString("mn"));
		assertThat(normalize("MN"), hasToString("mn"));
		assertThat(normalize("mn-Cyrl"), hasToString("mn-Cyrl"));
		assertThat(normalize("mn-cyrl"), hasToString("mn-Cyrl"));
		assertThat(normalize("mn-cYrL"), hasToString("mn-Cyrl"));
		assertThat(normalize("MN-CYRL"), hasToString("mn-Cyrl"));
		assertThat(normalize("mn-MN"), hasToString("mn-MN"));
		assertThat(normalize("mn-mn"), hasToString("mn-MN"));
		assertThat(normalize("MN-MN"), hasToString("mn-MN"));
		assertThat(normalize("mn-Cyrl-MN"), hasToString("mn-Cyrl-MN"));
		assertThat(normalize("mn-cyrl-mn"), hasToString("mn-Cyrl-MN"));
		assertThat(normalize("MN-CYRL-MN"), hasToString("mn-Cyrl-MN"));
		assertThat(normalize("en-CA-x-ca"), hasToString("en-CA-x-ca"));
		assertThat(normalize("en-ca-x-ca"), hasToString("en-CA-x-ca"));
		assertThat(normalize("EN-CA-X-CA"), hasToString("en-CA-x-ca"));
		assertThat(normalize("az-Latn-x-latn"), hasToString("az-Latn-x-latn"));
		assertThat(normalize("az-latn-x-latn"), hasToString("az-Latn-x-latn"));
		assertThat(normalize("AZ-LATN-X-LATN"), hasToString("az-Latn-x-latn"));
		//irregular grandfathered tags
		assertThat(normalize("ART-LOJBAN"), hasToString("art-lojban"));
		assertThat(normalize("en-gb-oed"), hasToString("en-GB-oed"));
		assertThat(normalize("en-gb-oed"), hasToString("en-GB-oed"));
		//regular grandfathered tags
		assertThat(normalize("en-gb-OED"), hasToString("en-GB-oed"));
		assertThat(normalize("NO-NYN"), hasToString("no-nyn"));
		assertThat(normalize("zh-hAKKa"), hasToString("zh-hakka"));
		assertThat(normalize("sgn-be-fr"), hasToString("sgn-BE-FR"));
		//private use tags
		assertThat(normalize("x-whatever"), hasToString("x-whatever"));
		assertThat(normalize("X-WHATEVER"), hasToString("x-whatever"));
	}

	//## type

	@Test
	void testTypes() {
		assertThat(new LanguageTag("en").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("en-US").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("mn-Cyrl").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("mn-Cyrl-MN").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("en-CA-x-ca").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("x-whatever").getType(), is(Type.PRIVATE_USE));
		assertThat(new LanguageTag("en-GB-oed").getType(), is(Type.GRANDFATHERED));
		assertThat(new LanguageTag("i-klingon").getType(), is(Type.GRANDFATHERED));
		assertThat(new LanguageTag("sgn-CH-DE").getType(), is(Type.GRANDFATHERED));
		assertThat(new LanguageTag("art-lojban").getType(), is(Type.GRANDFATHERED));
		assertThat(new LanguageTag("no-nyn").getType(), is(Type.GRANDFATHERED));
		assertThat(new LanguageTag("zh-xiang").getType(), is(Type.GRANDFATHERED));
	}

	//## components

	/** @see LanguageTag#findPrimaryLanguage() */
	@Test
	void testFindPrimaryLanguage() {
		assertThat(new LanguageTag("hy-Latn-IT-arevela").findPrimaryLanguage(), isPresentAndIs("hy"));
		assertThat(new LanguageTag("hy-latn-it-arevela").findPrimaryLanguage(), isPresentAndIs("hy"));
		assertThat(new LanguageTag("HY-LATN-IT-AREVELA").findPrimaryLanguage(), isPresentAndIs("hy"));
	}

	/** @see LanguageTag#findLanguage() */
	@Test
	void testFindLanguage() {
		assertThat(new LanguageTag("hy-Latn-IT-arevela").findLanguage(), isPresentAndIs("hy"));
		assertThat(new LanguageTag("hy-latn-it-arevela").findLanguage(), isPresentAndIs("hy"));
		assertThat(new LanguageTag("HY-LATN-IT-AREVELA").findLanguage(), isPresentAndIs("hy"));
	}

	/** @see LanguageTag#findScript() */
	@Test
	void testFindScript() {
		assertThat(new LanguageTag("hy-Latn-IT-arevela").findScript(), isPresentAndIs("Latn"));
		assertThat(new LanguageTag("hy-latn-it-arevela").findScript(), isPresentAndIs("Latn"));
		assertThat(new LanguageTag("HY-LATN-IT-AREVELA").findScript(), isPresentAndIs("Latn"));
	}

	/** @see LanguageTag#findRegion() */
	@Test
	void testFindRegion() {
		assertThat(new LanguageTag("hy-Latn-IT-arevela").findRegion(), isPresentAndIs("IT"));
		assertThat(new LanguageTag("hy-latn-it-arevela").findRegion(), isPresentAndIs("IT"));
		assertThat(new LanguageTag("HY-LATN-IT-AREVELA").findRegion(), isPresentAndIs("IT"));
	}

	/** @see LanguageTag#getVariants() */
	@Test
	void testGetVariants() {
		assertThat(new LanguageTag("hy-Latn-IT-arevela").getVariants(), containsInAnyOrder("arevela"));
		assertThat(new LanguageTag("hy-latn-it-arevela").getVariants(), containsInAnyOrder("arevela"));
		assertThat(new LanguageTag("HY-LATN-IT-AREVELA").getVariants(), containsInAnyOrder("arevela"));
	}

	/** @see LanguageTag#getExtensions() */
	@Test
	void testGetExtensions() {
		assertThat(new LanguageTag("en-a-bc").getExtensions(), containsInAnyOrder("a-bc"));
		assertThat(new LanguageTag("en-a-myext").getExtensions(), containsInAnyOrder("a-myext"));
		assertThat(new LanguageTag("en-a-myext-b-another-one").getExtensions(), containsInAnyOrder("a-myext", "b-another-one"));
		assertThat(new LanguageTag("en-a-myext-b-another-one-c-eightlen").getExtensions(), containsInAnyOrder("a-myext", "b-another-one", "c-eightlen"));
		assertThat(new LanguageTag("en-a-myext-b-another-one-c-eightlen-z-yx").getExtensions(),
				containsInAnyOrder("a-myext", "b-another-one", "c-eightlen", "z-yx"));
		assertThat(new LanguageTag("en-a-really-long-subgroup-ij").getExtensions(), containsInAnyOrder("a-really-Long-subgroup-IJ"));
		assertThat(new LanguageTag("en-a-really-long-subgroup-7z").getExtensions(), containsInAnyOrder("a-really-Long-subgroup-7Z"));
		assertThat(new LanguageTag("en-a-myext-b-really-long-subgroup-z-yx").getExtensions(), containsInAnyOrder("a-myext", "b-really-Long-subgroup", "z-yx"));
		assertThat(new LanguageTag("EN-A-MYEXT-B-REALLY-LONG-SUBGROUP-Z-YX").getExtensions(), containsInAnyOrder("a-myext", "b-really-Long-subgroup", "z-yx"));
	}

	/** @see LanguageTag#findPrivateUse() */
	@Test
	void testGetPrivateUse() {
		assertThat(new LanguageTag("en-x-y").findPrivateUse(), isPresentAndIs("x-y"));
		assertThat(new LanguageTag("en-x-yz").findPrivateUse(), isPresentAndIs("x-yz"));
		assertThat(new LanguageTag("en-x-whatever").findPrivateUse(), isPresentAndIs("x-whatever"));
		assertThat(new LanguageTag("en-x-just").findPrivateUse(), isPresentAndIs("x-just"));
		assertThat(new LanguageTag("en-x-just-a").findPrivateUse(), isPresentAndIs("x-just-a"));
		assertThat(new LanguageTag("en-x-just-a-thing").findPrivateUse(), isPresentAndIs("x-just-a-thing"));
		assertThat(new LanguageTag("en-x-just-2-things").findPrivateUse(), isPresentAndIs("x-just-2-things"));
		assertThat(new LanguageTag("en-x-just-a-few345").findPrivateUse(), isPresentAndIs("x-just-a-few345"));
		assertThat(new LanguageTag("en-x-just-a-few123").findPrivateUse(), isPresentAndIs("x-just-a-few123"));
		assertThat(new LanguageTag("en-x-whatever-x-and-a-thing").findPrivateUse(), isPresentAndIs("x-whatever-x-and-a-thing"));
		assertThat(new LanguageTag("en-x-whatever-x-and-a-thing-another").findPrivateUse(), isPresentAndIs("x-whatever-x-and-a-thing-another"));
		assertThat(new LanguageTag("EN-X-WHATEVER-X-AND-A-THING-ANOTHER").findPrivateUse(), isPresentAndIs("x-whatever-x-and-a-thing-another"));
		assertThat(new LanguageTag("en-x-whatever-x-and-x-another-thing").findPrivateUse(), isPresentAndIs("x-whatever-x-and-x-another-thing"));
		assertThat(new LanguageTag("EN-X-WHATEVER-X-AND-X-ANOTHER-THING").findPrivateUse(), isPresentAndIs("x-whatever-x-and-x-another-thing"));
	}

	/**
	 * @see LanguageTag#getExtensions()
	 * @see LanguageTag#findPrivateUse()
	 * @see <a href="https://datatracker.ietf.org/doc/html/rfc5646#section-2.2.6">RFC 5646 § 2.2.6. Extension Subtags</a>
	 */
	@Test
	void verifyPrivateUseCanContainExtensionSingleton() {

		final LanguageTag languageTag = new LanguageTag("en-a-bbb-x-a-ccc");
		assertThat(languageTag.getType(), is(Type.NORMAL));
		assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("en"));
		assertThat(languageTag.findLanguage(), isPresentAndIs("en"));
		assertThat(languageTag.findScript(), is(Optional.empty()));
		assertThat(languageTag.findRegion(), is(Optional.empty()));
		assertThat(languageTag.getVariants(), is(empty()));
		assertThat(languageTag.getExtensions(), containsInAnyOrder("a-bbb"));
		assertThat(languageTag.findPrivateUse(), isPresentAndIs("x-a-ccc"));
	}

	//## examples

	/** @see <a href="https://datatracker.ietf.org/doc/html/rfc5646#appendix-A">RFC 5646 Appendix A. Examples of Language Tags (Informative)</a> */
	@Test
	void testAppendixAExamples() {
		//### Simple language subtag
		{ //German
			final LanguageTag languageTag = new LanguageTag("de");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("de"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("de"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //French
			final LanguageTag languageTag = new LanguageTag("fr");
			assertThat(languageTag.findLanguage(), isPresentAndIs("fr"));
		}
		{ //Japanese
			final LanguageTag languageTag = new LanguageTag("ja");
			assertThat(languageTag.findLanguage(), isPresentAndIs("ja"));
		}
		{ //example of a grandfathered tag
			final LanguageTag languageTag = new LanguageTag("i-enochian");
			assertThat(languageTag.getType(), is(Type.GRANDFATHERED));
		}
		//### Language subtag plus Script subtag
		{ //Chinese written using the Traditional Chinese script
			final LanguageTag languageTag = new LanguageTag("zh-Hant");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findScript(), isPresentAndIs("Hant"));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Chinese written using the Simplified Chinese script
			final LanguageTag languageTag = new LanguageTag("zh-Hans");
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findScript(), isPresentAndIs("Hans"));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Serbian written using the Cyrillic script
			final LanguageTag languageTag = new LanguageTag("sr-Cyrl");
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findScript(), isPresentAndIs("Cyrl"));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Serbian written using the Latin script
			final LanguageTag languageTag = new LanguageTag("sr-Latn");
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findScript(), isPresentAndIs("Latn"));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		//### Extended language subtags and their primary language subtag counterparts
		{ //Chinese, Mandarin, Simplified script, as used in China
			final LanguageTag languageTag = new LanguageTag("zh-cmn-Hans-CN");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("zh-cmn"));
			assertThat(languageTag.findScript(), isPresentAndIs("Hans"));
			assertThat(languageTag.findRegion(), isPresentAndIs("CN"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Mandarin Chinese, Simplified script, as used in China
			final LanguageTag languageTag = new LanguageTag("cmn-Hans-CN");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("cmn"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("cmn"));
			assertThat(languageTag.findScript(), isPresentAndIs("Hans"));
			assertThat(languageTag.findRegion(), isPresentAndIs("CN"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Chinese, Cantonese, as used in Hong Kong SAR
			final LanguageTag languageTag = new LanguageTag("zh-yue-HK");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("zh-yue"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), isPresentAndIs("HK"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Cantonese Chinese, as used in Hong Kong SAR
			final LanguageTag languageTag = new LanguageTag("yue-HK");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("yue"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("yue"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), isPresentAndIs("HK"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		//### Language-Script-Region
		{ //Chinese written using the Simplified script as used in mainland China
			final LanguageTag languageTag = new LanguageTag("zh-Hans-CN");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findScript(), isPresentAndIs("Hans"));
			assertThat(languageTag.findRegion(), isPresentAndIs("CN"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Serbian written using the Latin script as used in Serbia
			final LanguageTag languageTag = new LanguageTag("sr-Latn-RS");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findScript(), isPresentAndIs("Latn"));
			assertThat(languageTag.findRegion(), isPresentAndIs("RS"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		//### Language-Variant
		{ //Resian dialect of Slovenian
			final LanguageTag languageTag = new LanguageTag("sl-rozaj");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sl"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sl"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), containsInAnyOrder("rozaj"));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //San Giorgio dialect of Resian dialect of Slovenian
			final LanguageTag languageTag = new LanguageTag("sl-rozaj-biske");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sl"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sl"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), containsInAnyOrder("rozaj", "biske"));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Nadiza dialect of Slovenian
			final LanguageTag languageTag = new LanguageTag("sl-nedis");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sl"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sl"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), containsInAnyOrder("nedis"));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		//### Language-Region-Variant
		{ //German as used in Switzerland using the 1901 variant [orthography]
			final LanguageTag languageTag = new LanguageTag("de-CH-1901");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("de"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("de"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), isPresentAndIs("CH"));
			assertThat(languageTag.getVariants(), containsInAnyOrder("1901"));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Slovenian as used in Italy, Nadiza dialect
			final LanguageTag languageTag = new LanguageTag("sl-IT-nedis");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sl"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sl"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), isPresentAndIs("IT"));
			assertThat(languageTag.getVariants(), containsInAnyOrder("nedis"));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		//### Language-Script-Region-Variant
		{ //Eastern Armenian written in Latin script, as used in Italy
			final LanguageTag languageTag = new LanguageTag("hy-Latn-IT-arevela");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("hy"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("hy"));
			assertThat(languageTag.findScript(), isPresentAndIs("Latn"));
			assertThat(languageTag.findRegion(), isPresentAndIs("IT"));
			assertThat(languageTag.getVariants(), containsInAnyOrder("arevela"));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		//### Language-Region
		{ //German for Germany
			final LanguageTag languageTag = new LanguageTag("de-DE");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("de"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("de"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), isPresentAndIs("DE"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //English as used in the United States
			final LanguageTag languageTag = new LanguageTag("en-US");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("en"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("en"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), isPresentAndIs("US"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Spanish appropriate for the Latin America and Caribbean region using the UN region code
			final LanguageTag languageTag = new LanguageTag("es-419");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("es"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("es"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), isPresentAndIs("419"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		//### Private use subtags
		{
			final LanguageTag languageTag = new LanguageTag("de-CH-x-phonebk");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("de"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("de"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), isPresentAndIs("CH"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), isPresentAndIs("x-phonebk"));
		}
		{
			final LanguageTag languageTag = new LanguageTag("az-Arab-x-AZE-derbend");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("az"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("az"));
			assertThat(languageTag.findScript(), isPresentAndIs("Arab"));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			//the example in RFC 5646 Appendix A apparently does not have its private-use section in conventional form
			assertThat(languageTag.findPrivateUse(), isPresentAndIs("x-aze-derbend"));
		}
		//### Private use registry values
		{ //private use using the singleton 'x'
			final LanguageTag languageTag = new LanguageTag("x-whatever");
			assertThat(languageTag.getType(), is(Type.PRIVATE_USE));
			assertThat(languageTag.findPrimaryLanguage(), is(Optional.empty()));
			assertThat(languageTag.findLanguage(), is(Optional.empty()));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //all private tags
			final LanguageTag languageTag = new LanguageTag("qaa-Qaaa-QM-x-southern");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("qaa"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("qaa"));
			assertThat(languageTag.findScript(), isPresentAndIs("Qaaa"));
			assertThat(languageTag.findRegion(), isPresentAndIs("QM"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), isPresentAndIs("x-southern"));
		}
		{ //German, with a private script
			final LanguageTag languageTag = new LanguageTag("de-Qaaa");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("de"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("de"));
			assertThat(languageTag.findScript(), isPresentAndIs("Qaaa"));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Serbian, Latin script, private region
			final LanguageTag languageTag = new LanguageTag("sr-Latn-QM");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findScript(), isPresentAndIs("Latn"));
			assertThat(languageTag.findRegion(), isPresentAndIs("QM"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{ //Serbian, private script, for Serbia
			final LanguageTag languageTag = new LanguageTag("sr-Qaaa-RS");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findScript(), isPresentAndIs("Qaaa"));
			assertThat(languageTag.findRegion(), isPresentAndIs("RS"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		//### Tags that use extensions (examples ONLY -- extensions MUST be defined by revision or update …, or by RFC)
		{
			final LanguageTag languageTag = new LanguageTag("en-US-u-islamcal");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("en"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("en"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), isPresentAndIs("US"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), containsInAnyOrder("u-islamcal"));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		{
			final LanguageTag languageTag = new LanguageTag("zh-CN-a-myext-x-private");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), isPresentAndIs("CN"));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), containsInAnyOrder("a-myext"));
			assertThat(languageTag.findPrivateUse(), isPresentAndIs("x-private"));
		}
		{
			final LanguageTag languageTag = new LanguageTag("en-a-myext-b-another");
			assertThat(languageTag.getType(), is(Type.NORMAL));
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("en"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("en"));
			assertThat(languageTag.findScript(), is(Optional.empty()));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), containsInAnyOrder("a-myext", "b-another"));
			assertThat(languageTag.findPrivateUse(), is(Optional.empty()));
		}
		//### Some Invalid Tags
		//two region tags
		assertThrows(IllegalArgumentException.class, () -> new LanguageTag("de-419-DE"), "two region tags");
		//use of a single-character subtag in primary position; note that there are a few grandfathered tags that start with "i-" that are valid
		assertThrows(IllegalArgumentException.class, () -> new LanguageTag("a-DE"), "use of a single-character subtag in primary position");
		//two extensions with same single-letter prefix
		assertThrows(IllegalArgumentException.class, () -> new LanguageTag("ar-a-aaa-b-bbb-a-ccc"), "two extensions with same single-letter prefix");
	}

	//## static factory method

	/** @see LanguageTag#parse(CharSequence) */
	@Test
	void testCache() {
		//list of test language tags based on Locale constants 
		final List<String> languageTagStrings = asList("en", "fr", "de", "it", "ja", "ko", "zh", "zh-CN", "zh-TW", "fr-FR", "de-DE", "it-IT", "ja-JP", "ko-KR",
				"en-GB", "en-US", "en-CA", "fr-CA");
		final List<LanguageTag> languageTags = languageTagStrings.stream().map(LanguageTag::parse).collect(toList());
		for(int i = 0, len = languageTags.size(); i < len; i++) {
			final LanguageTag languageTag1 = languageTags.get(i);
			final String languageTagString = languageTagStrings.get(i);
			assertThat("Cache stores correct tag.", languageTag1.toString(), is(languageTagString));
			final LanguageTag languageTag2 = LanguageTag.parse(languageTagString);
			assertThat("Cache retrieved existing language tag.", languageTag2, is(sameInstance(languageTag1)));
		}
	}

	//## hash code

	/** @see LanguageTag#hashCode() */
	@Test
	void testHashCode() {
		assertThat("Language tags with identical case.", new LanguageTag("en-US").hashCode(), is(new LanguageTag("en-US").hashCode()));
		assertThat("Language tags with different case.", new LanguageTag("en-UK").hashCode(), is(new LanguageTag("EN-uk").hashCode()));
	}

	/** @see LanguageTag#equals(Object)() */
	@Test
	void testEquals() {
		assertThat("Language tags with identical case.", new LanguageTag("en-US"), is(equalTo(new LanguageTag("en-US"))));
		assertThat("Language tags with different case.", new LanguageTag("en-UK"), is(equalTo(new LanguageTag("EN-uk"))));
	}

	//## string form

	/**
	 * Tests that the string form of the language tag is correct and normalized
	 * @see LanguageTag#toString()
	 */
	@Test
	void testToString() {
		assertThat(new LanguageTag("en"), hasToString("en"));
		assertThat(new LanguageTag("En"), hasToString("en"));
		assertThat(new LanguageTag("eN"), hasToString("en"));
		assertThat(new LanguageTag("EN"), hasToString("en"));
		assertThat(new LanguageTag("en-US"), hasToString("en-US"));
		assertThat(new LanguageTag("en-us"), hasToString("en-US"));
		assertThat(new LanguageTag("EN-US"), hasToString("en-US"));
		assertThat(new LanguageTag("mn"), hasToString("mn"));
		assertThat(new LanguageTag("Mn"), hasToString("mn"));
		assertThat(new LanguageTag("mN"), hasToString("mn"));
		assertThat(new LanguageTag("MN"), hasToString("mn"));
		assertThat(new LanguageTag("mn-Cyrl"), hasToString("mn-Cyrl"));
		assertThat(new LanguageTag("mn-cyrl"), hasToString("mn-Cyrl"));
		assertThat(new LanguageTag("mn-cYrL"), hasToString("mn-Cyrl"));
		assertThat(new LanguageTag("MN-CYRL"), hasToString("mn-Cyrl"));
		assertThat(new LanguageTag("mn-MN"), hasToString("mn-MN"));
		assertThat(new LanguageTag("mn-mn"), hasToString("mn-MN"));
		assertThat(new LanguageTag("MN-MN"), hasToString("mn-MN"));
		assertThat(new LanguageTag("mn-Cyrl-MN"), hasToString("mn-Cyrl-MN"));
		assertThat(new LanguageTag("mn-cyrl-mn"), hasToString("mn-Cyrl-MN"));
		assertThat(new LanguageTag("MN-CYRL-MN"), hasToString("mn-Cyrl-MN"));
		assertThat(new LanguageTag("en-CA-x-ca"), hasToString("en-CA-x-ca"));
		assertThat(new LanguageTag("en-ca-x-ca"), hasToString("en-CA-x-ca"));
		assertThat(new LanguageTag("EN-CA-X-CA"), hasToString("en-CA-x-ca"));
		assertThat(new LanguageTag("az-Latn-x-latn"), hasToString("az-Latn-x-latn"));
		assertThat(new LanguageTag("az-latn-x-latn"), hasToString("az-Latn-x-latn"));
		assertThat(new LanguageTag("AZ-LATN-X-LATN"), hasToString("az-Latn-x-latn"));
		//irregular grandfathered tags
		assertThat(new LanguageTag("ART-LOJBAN"), hasToString("art-lojban"));
		assertThat(new LanguageTag("en-gb-oed"), hasToString("en-GB-oed"));
		assertThat(new LanguageTag("en-gb-oed"), hasToString("en-GB-oed"));
		//regular grandfathered tags
		assertThat(new LanguageTag("en-gb-OED"), hasToString("en-GB-oed"));
		assertThat(new LanguageTag("NO-NYN"), hasToString("no-nyn"));
		assertThat(new LanguageTag("zh-hAKKa"), hasToString("zh-hakka"));
		assertThat(new LanguageTag("sgn-be-fr"), hasToString("sgn-BE-FR"));
		//private use tags
		assertThat(new LanguageTag("x-whatever"), hasToString("x-whatever"));
		assertThat(new LanguageTag("X-WHATEVER"), hasToString("x-whatever"));
	}

}
