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
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Locale;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Locales}.
 * @author Garret Wilson
 * @see <a href="https://www.rfc-editor.org/rfc/rfc5646.html">RFC 5646: Tags for Identifying Languages</a>
 */
public class LocalesTest {

	/** @see Locales#toLanguageTagSubtags(String) */
	@Test
	void testToLanguageTagSubtags() {
		assertThat("Language", Locales.toLanguageTagSubtags("en"), is("en"));
		assertThat("Language_Region", Locales.toLanguageTagSubtags("en_US"), is("en-US"));
		assertThat("Language-Region", Locales.toLanguageTagSubtags("en-US"), is("en-US"));
		assertThat("Language-Variant", Locales.toLanguageTagSubtags("sl_rozaj_biske"), is("sl-rozaj-biske"));
		assertThat("Variant", Locales.toLanguageTagSubtags("rozaj_biske"), is("rozaj-biske")); //e.g. `Locale.forLanguageTag("sl-rozaj-biske").getVariant()`
	}

	/** @see Locales#isLanguagePresent(Locale) */
	@Test
	void testIsLanguagePresent() {
		assertThat(Locales.isLanguagePresent(Locale.ROOT), is(false));
		assertThat(Locales.isLanguagePresent(new Locale("")), is(false));
		assertThat(Locales.isLanguagePresent(new Locale("en")), is(true));
		assertThat(Locales.isLanguagePresent(new Locale("en", "US")), is(true));
		assertThat(Locales.isLanguagePresent(new Locale("", "")), is(false));
		assertThat(Locales.isLanguagePresent(new Locale("", "", "")), is(false));
		assertThat(Locales.isLanguagePresent(Locale.forLanguageTag("hy-Latn-IT-arevela")), is(true));
	}

	/** @see Locales#findLanguage(Locale) */
	@Test
	void testFindLanguage() {
		assertThat(Locales.findLanguage(Locale.forLanguageTag("hy-Latn-IT-arevela")), isPresentAndIs("hy"));
		assertThat("Canonicalized", Locales.findLanguage(Locale.forLanguageTag("zh-hak-CN")), isPresentAndIs("hak")); //canonicalized to `hak-CN`
	}

	/** @see Locales#isPrimaryLanguageLocale(Locale) */
	@Test
	void testIsPrimaryLanguageLocale() {
		assertThat(Locales.isPrimaryLanguageLocale(Locale.forLanguageTag("en")), is(true));
		assertThat(Locales.isPrimaryLanguageLocale(Locale.forLanguageTag("en-US")), is(false));
		assertThat(Locales.isPrimaryLanguageLocale(Locale.forLanguageTag("hy-Latn-IT-arevela")), is(false));
		assertThat(Locales.isPrimaryLanguageLocale(Locale.ROOT), is(false));
	}

	/** @see Locales#toPrimaryLanguageLocale(Locale) */
	@Test
	void testToPrimaryLanguageLocale() {
		assertThat(Locales.toPrimaryLanguageLocale(Locale.forLanguageTag("en")), is(Locale.forLanguageTag("en")));
		assertThat(Locales.toPrimaryLanguageLocale(Locale.forLanguageTag("en-US")), is(Locale.forLanguageTag("en")));
		assertThat(Locales.toPrimaryLanguageLocale(Locale.forLanguageTag("hy-Latn-IT-arevela")), is(Locale.forLanguageTag("hy")));
		assertThrows(IllegalArgumentException.class, () -> Locales.toPrimaryLanguageLocale(Locale.ROOT));
	}

	/** @see Locales#findScript(Locale) */
	@Test
	void testFindScript() {
		assertThat(Locales.findScript(Locale.forLanguageTag("hy-Latn-IT-arevela")), isPresentAndIs("Latn"));
	}

	/** @see Locales#findCountry(Locale) */
	@Test
	void testFindCountry() {
		assertThat(Locales.findCountry(Locale.forLanguageTag("hy-Latn-IT-arevela")), isPresentAndIs("IT"));
	}

	/**
	 * Tests Locales#findVariant(Locale).
	 * @implNote One might expect that retrieving the variant of <code>sl-rozaj-biske</code> would yield <code>rozaj-biske</code>, but as of JDK 17 this is
	 *           returning <code>rozaj_biske</code>.
	 * @see Locales#findVariant(Locale)
	 */
	@Test
	void testFindVariant() {
		assertThat(Locales.findVariant(Locale.forLanguageTag("hy-Latn-IT-arevela")), isPresentAndIs("arevela"));
		assertThat(Locales.findVariant(Locale.forLanguageTag("sl-rozaj-biske")), isPresentAndIs("rozaj_biske"));
	}

	/** @see Locales#findVariant(Locale) */
	@Test
	void testFindLanguageTagVariant() {
		assertThat(Locales.findLanguageTagVariant(Locale.forLanguageTag("hy-Latn-IT-arevela")), isPresentAndIs("arevela"));
		assertThat(Locales.findLanguageTagVariant(Locale.forLanguageTag("sl-rozaj-biske")), isPresentAndIs("rozaj-biske"));
	}

}
