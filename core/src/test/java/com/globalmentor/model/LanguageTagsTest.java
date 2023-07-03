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

import static com.globalmentor.model.LanguageTags.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.regex.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link LanguageTags}.
 * @author Garret Wilson
 */
public class LanguageTagsTest {

	//## patterns

	/** @see LanguageTags#PRIVATEUSE_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#LANGTAG_PATTERN */
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

	/** @see LanguageTags#parseExtensions(String) */
	@Test
	void testParseExtension() {
		assertThat(LanguageTags.parseExtensions("-a-bc"), contains("a-bc"));
		assertThat(LanguageTags.parseExtensions("-a-myext"), contains("a-myext"));
		assertThat(LanguageTags.parseExtensions("-a-myext-b-another-one"), contains("a-myext", "b-another-one"));
		assertThat(LanguageTags.parseExtensions("-a-myext-b-another-one-c-eightlen"), contains("a-myext", "b-another-one", "c-eightlen"));
		assertThat(LanguageTags.parseExtensions("-a-myext-b-another-one-c-eightlen-z-yx"), contains("a-myext", "b-another-one", "c-eightlen", "z-yx"));
		assertThat(LanguageTags.parseExtensions("-a-really-long-subgroup-ij"), contains("a-really-long-subgroup-ij"));
		assertThat(LanguageTags.parseExtensions("-a-really-long-subgroup-7z"), contains("a-really-long-subgroup-7z"));
		assertThat(LanguageTags.parseExtensions("-a-myext-a-really-long-subgroup-z-yx"), contains("a-myext", "a-really-long-subgroup", "z-yx"));
		assertThat(LanguageTags.parseExtensions("-A-MYEXT-A-REALLY-LONG-SUBGROUP-Z-YX"), contains("A-MYEXT", "A-REALLY-LONG-SUBGROUP", "Z-YX"));
	}

	//## normalize

	/** @see LanguageTags#normalize(CharSequence) */
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

}
