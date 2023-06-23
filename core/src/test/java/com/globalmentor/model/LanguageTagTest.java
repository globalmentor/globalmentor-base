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
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

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
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile-private1");
	}

	/** Utility matching method to include the group name in the description. */
	private static void assertMatcherGroupIs(final Matcher matcher, final String group, final String value) {
		assertThat(group, matcher.group(group), is(value));
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
		//German
		assertThat(new LanguageTag("de").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("de").findPrimaryLanguage(), isPresentAndIs("de"));
		assertThat(new LanguageTag("de").findLanguage(), isPresentAndIs("de"));
		assertThat(new LanguageTag("de").findScript(), isEmpty());
		assertThat(new LanguageTag("de").findRegion(), isEmpty());
		assertThat(new LanguageTag("de").getVariants(), is(empty()));
		//French
		assertThat(new LanguageTag("fr").findLanguage(), isPresentAndIs("fr"));
		//Japanese
		assertThat(new LanguageTag("ja").findLanguage(), isPresentAndIs("ja"));
		assertThat(new LanguageTag("i-enochian").getType(), is(Type.GRANDFATHERED)); //example of a grandfathered tag
		//### Language subtag plus Script subtag
		//Chinese written using the Traditional Chinese script
		assertThat(new LanguageTag("zh-Hant").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("zh-Hant").findPrimaryLanguage(), isPresentAndIs("zh"));
		assertThat(new LanguageTag("zh-Hant").findLanguage(), isPresentAndIs("zh"));
		assertThat(new LanguageTag("zh-Hant").findScript(), isPresentAndIs("Hant"));
		//Chinese written using the Simplified Chinese script
		assertThat(new LanguageTag("zh-Hans").findPrimaryLanguage(), isPresentAndIs("zh"));
		assertThat(new LanguageTag("zh-Hans").findLanguage(), isPresentAndIs("zh"));
		assertThat(new LanguageTag("zh-Hans").findScript(), isPresentAndIs("Hans"));
		//Serbian written using the Cyrillic script
		assertThat(new LanguageTag("sr-Cyrl").findPrimaryLanguage(), isPresentAndIs("sr"));
		assertThat(new LanguageTag("sr-Cyrl").findLanguage(), isPresentAndIs("sr"));
		assertThat(new LanguageTag("sr-Cyrl").findScript(), isPresentAndIs("Cyrl"));
		//Serbian written using the Latin script
		assertThat(new LanguageTag("sr-Latn").findPrimaryLanguage(), isPresentAndIs("sr"));
		assertThat(new LanguageTag("sr-Latn").findLanguage(), isPresentAndIs("sr"));
		assertThat(new LanguageTag("sr-Latn").findScript(), isPresentAndIs("Latn"));
		//### Extended language subtags and their primary language subtag counterparts
		//Chinese, Mandarin, Simplified script, as used in China
		assertThat(new LanguageTag("zh-cmn-Hans-CN").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("zh-cmn-Hans-CN").findPrimaryLanguage(), isPresentAndIs("zh"));
		assertThat(new LanguageTag("zh-cmn-Hans-CN").findLanguage(), isPresentAndIs("zh-cmn"));
		assertThat(new LanguageTag("zh-cmn-Hans-CN").findScript(), isPresentAndIs("Hans"));
		assertThat(new LanguageTag("zh-cmn-Hans-CN").findRegion(), isPresentAndIs("CN"));
		//Mandarin Chinese, Simplified script, as used in China
		assertThat(new LanguageTag("cmn-Hans-CN").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("cmn-Hans-CN").findPrimaryLanguage(), isPresentAndIs("cmn"));
		assertThat(new LanguageTag("cmn-Hans-CN").findLanguage(), isPresentAndIs("cmn"));
		assertThat(new LanguageTag("cmn-Hans-CN").findScript(), isPresentAndIs("Hans"));
		assertThat(new LanguageTag("cmn-Hans-CN").findRegion(), isPresentAndIs("CN"));
		//Chinese, Cantonese, as used in Hong Kong SAR
		assertThat(new LanguageTag("zh-yue-HK").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("zh-yue-HK").findPrimaryLanguage(), isPresentAndIs("zh"));
		assertThat(new LanguageTag("zh-yue-HK").findLanguage(), isPresentAndIs("zh-yue"));
		assertThat(new LanguageTag("zh-yue-HK").findRegion(), isPresentAndIs("HK"));
		//Cantonese Chinese, as used in Hong Kong SAR
		assertThat(new LanguageTag("yue-HK").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("yue-HK").findPrimaryLanguage(), isPresentAndIs("yue"));
		assertThat(new LanguageTag("yue-HK").findLanguage(), isPresentAndIs("yue"));
		assertThat(new LanguageTag("yue-HK").findRegion(), isPresentAndIs("HK"));
		//### Language-Script-Region
		//Chinese written using the Simplified script as used in mainland China
		assertThat(new LanguageTag("zh-Hans-CN").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("zh-Hans-CN").findPrimaryLanguage(), isPresentAndIs("zh"));
		assertThat(new LanguageTag("zh-Hans-CN").findLanguage(), isPresentAndIs("zh"));
		assertThat(new LanguageTag("zh-Hans-CN").findScript(), isPresentAndIs("Hans"));
		assertThat(new LanguageTag("zh-Hans-CN").findRegion(), isPresentAndIs("CN"));
		//Serbian written using the Latin script as used in Serbia
		assertThat(new LanguageTag("sr-Latn-RS").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("sr-Latn-RS").findPrimaryLanguage(), isPresentAndIs("sr"));
		assertThat(new LanguageTag("sr-Latn-RS").findLanguage(), isPresentAndIs("sr"));
		assertThat(new LanguageTag("sr-Latn-RS").findScript(), isPresentAndIs("Latn"));
		assertThat(new LanguageTag("sr-Latn-RS").findRegion(), isPresentAndIs("RS"));
		//### Language-Variant
		//Resian dialect of Slovenian
		assertThat(new LanguageTag("sl-rozaj").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("sl-rozaj").findPrimaryLanguage(), isPresentAndIs("sl"));
		assertThat(new LanguageTag("sl-rozaj").findLanguage(), isPresentAndIs("sl"));
		assertThat(new LanguageTag("sl-rozaj").findScript(), isEmpty());
		assertThat(new LanguageTag("sl-rozaj").findRegion(), isEmpty());
		assertThat(new LanguageTag("sl-rozaj").getVariants(), containsInAnyOrder("rozaj"));
		//San Giorgio dialect of Resian dialect of Slovenian
		assertThat(new LanguageTag("sl-rozaj-biske").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("sl-rozaj-biske").findPrimaryLanguage(), isPresentAndIs("sl"));
		assertThat(new LanguageTag("sl-rozaj-biske").findLanguage(), isPresentAndIs("sl"));
		assertThat(new LanguageTag("sl-rozaj-biske").findScript(), isEmpty());
		assertThat(new LanguageTag("sl-rozaj-biske").findRegion(), isEmpty());
		assertThat(new LanguageTag("sl-rozaj-biske").getVariants(), containsInAnyOrder("rozaj", "biske"));
		//Nadiza dialect of Slovenian
		assertThat(new LanguageTag("sl-nedis").getType(), is(Type.NORMAL));
		assertThat(new LanguageTag("sl-nedis").findPrimaryLanguage(), isPresentAndIs("sl"));
		assertThat(new LanguageTag("sl-nedis").findLanguage(), isPresentAndIs("sl"));
		assertThat(new LanguageTag("sl-nedis").findScript(), isEmpty());
		assertThat(new LanguageTag("sl-nedis").findRegion(), isEmpty());
		assertThat(new LanguageTag("sl-nedis").getVariants(), containsInAnyOrder("nedis"));
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
