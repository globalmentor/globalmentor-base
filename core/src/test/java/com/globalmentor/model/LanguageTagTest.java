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

import static com.github.npathai.hamcrestopt.OptionalMatchers.isPresentAndIs;
import static com.globalmentor.model.LanguageTag.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Optional;
import java.util.regex.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link LanguageTag}.
 * @author Garret Wilson
 */
public class LanguageTagTest {

	//## patterns

	//## privateuse pattern

	@Test
	void testPrivateusePatternX_WHATEVER() {
		assertThat(PRIVATEUSE_PATTERN.matcher("x-whatever").matches(), is(true));
	}

	//## langtag pattern

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

	//## extensions

	/** @see LanguageTag#parseExtensions(String) */
	@Test
	void testParseExtension() {
		assertThat(LanguageTag.parseExtensions("-a-bc"), contains("a-bc"));
		assertThat(LanguageTag.parseExtensions("-a-myext"), contains("a-myext"));
		assertThat(LanguageTag.parseExtensions("-a-myext-b-another-one"), contains("a-myext", "b-another-one"));
		assertThat(LanguageTag.parseExtensions("-a-myext-b-another-one-c-eightlen"), contains("a-myext", "b-another-one", "c-eightlen"));
		assertThat(LanguageTag.parseExtensions("-a-myext-b-another-one-c-eightlen-x-yz"), contains("a-myext", "b-another-one", "c-eightlen", "x-yz"));
		assertThat(LanguageTag.parseExtensions("-a-really-long-extension-ij"), contains("a-really-long-extension-ij"));
		assertThat(LanguageTag.parseExtensions("-a-myext-a-really-long-extension-x-yz"), contains("a-myext", "a-really-long-extension", "x-yz"));
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

	//## examples

	/** @see <a href="https://datatracker.ietf.org/doc/html/rfc5646#appendix-A">RFC 5646 § Appendix A. Examples of Language Tags (Informative)</a> */
	@Test
	void testExamples() {
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
		}
		{ //Chinese written using the Simplified Chinese script
			final LanguageTag languageTag = new LanguageTag("zh-Hans");
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("zh"));
			assertThat(languageTag.findScript(), isPresentAndIs("Hans"));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
		}
		{ //Serbian written using the Cyrillic script
			final LanguageTag languageTag = new LanguageTag("sr-Cyrl");
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findScript(), isPresentAndIs("Cyrl"));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
		}
		{ //Serbian written using the Latin script
			final LanguageTag languageTag = new LanguageTag("sr-Latn");
			assertThat(languageTag.findPrimaryLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findLanguage(), isPresentAndIs("sr"));
			assertThat(languageTag.findScript(), isPresentAndIs("Latn"));
			assertThat(languageTag.findRegion(), is(Optional.empty()));
			assertThat(languageTag.getVariants(), is(empty()));
			assertThat(languageTag.getExtensions(), is(empty()));
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
		}
		//### Private use subtags
		//TODO de-CH-x-phonebk
		//TODO az-Arab-x-AZE-derbend
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
		}
	}

	//TODO implement Locale conversion

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
