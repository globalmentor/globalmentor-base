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
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	@Test
	void testLangtagPatternEN_US() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("en-US");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "en");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "US");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	@Test
	void testLangtagPatternMN_CYRL() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("mn-Cyrl");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "mn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Cyrl");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	@Test
	void testLangtagPatternMN_CYRL_MN() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("mn-Cyrl-MN");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "mn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Cyrl");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "MN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	@Test
	void testLangtagPatternEN_CA_X_CA() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("en-CA-x-ca");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "en");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CA");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-ca");
	}

	@Test
	void testLangtagPatternZH() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	@Test
	void testLangtagPatternZH_CN() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-CN");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	@Test
	void testLangtagPatternZH_LATN() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	@Test
	void testLangtagPatternZH_LATN_CN() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	@Test
	void testLangtagPatternZH_LATN_CN_X_WADEGILE() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-x-wadegile");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile");
	}

	@Test
	void testLangtagPatternZH_LATN_CN_X_WADEGILE_PRIVATE1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-x-wadegile-private1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile-private1");
	}

	@Test
	void testLangtagPatternZH_LATN_CN_A_EXTEND1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-a-extend1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	@Test
	void testLangtagPatternZH_LATN_CN_A_EXTEND1_X_WADEGILE() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-a-extend1-x-wadegile");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile");
	}

	@Test
	void testLangtagPatternZH_LATN_CN_A_EXTEND1_X_WADEGILE_PRIVATE1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-a-extend1-x-wadegile-private1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile-private1");
	}

	@Test
	void testLangtagPatternZH_LATN_CN_VARIANT1_X_WADEGILE_PRIVATE1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-variant1-x-wadegile-private1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, "variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, null);
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile-private1");
	}

	@Test
	void testLangtagPatternZH_LATN_CN_VARIANT1_A_EXTEND1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-variant1-a-extend1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, "variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, null);
	}

	@Test
	void testLangtagPatternZH_LATN_CN_VARIANT1_A_EXTEND1_X_WADEGILE() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-variant1-a-extend1-x-wadegile");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, "variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile");
	}

	@Test
	void testLangtagPatternZH_LATN_CN_VARIANT1_A_EXTEND1_X_WADEGILE_PRIVATE1() {
		final Matcher matcher = LANGTAG_PATTERN.matcher("zh-Latn-CN-variant1-a-extend1-x-wadegile-private1");
		assertThat(matcher.matches(), is(true));
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_LANGUAGE, "zh");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_SCRIPT, "Latn");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_REGION, "CN");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_VARIANT, "variant1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_EXTENSION, "a-extend1");
		assertMatcherGroupIs(matcher, LANGTAG_PATTERN_GROUP_PRIVATEUSE, "x-wadegile-private1");
	}

	/** Utility matching method to include the group name in the description. */
	private static void assertMatcherGroupIs(final Matcher matcher, final String group, final String value) {
		assertThat(group, matcher.group(group), is(value));
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

	//## field extraction

	/** @see LanguageTag#hashCode() */
	@Test
	void testHashCode() {
		assertThat("Language tags with identical case.", LanguageTag.of("en-US").hashCode(), is(LanguageTag.of("en-US").hashCode()));
		assertThat("Language tags with different case.", LanguageTag.of("en-UK").hashCode(), is(LanguageTag.of("EN-uk").hashCode()));
	}

	/** @see LanguageTag#equals(Object)() */
	@Test
	void testEquals() {
		assertThat("Language tags with identical case.", LanguageTag.of("en-US"), is(equalTo(LanguageTag.of("en-US"))));
		assertThat("Language tags with different case.", LanguageTag.of("en-UK"), is(equalTo(LanguageTag.of("EN-uk"))));
	}

	/** @see LanguageTag#toString() */
	@Test
	void testToString() {
		assertThat(LanguageTag.of("en-US").toString(), is("en-US"));
	}

}
