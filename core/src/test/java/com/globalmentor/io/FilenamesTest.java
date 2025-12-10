/*
 * Copyright © 2012-2018 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.io;

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Comparator;

import org.junit.jupiter.api.*;

/**
 * Various tests for working with filenames.
 * 
 * @author Garret Wilson
 * @author Magno Nascimento
 * @see Filenames
 */
public class FilenamesTest {

	//# Filenames

	/** @see Filenames#comparator() */
	@Test
	void testFilenameComparator() {
		final Comparator<CharSequence> comparator = Filenames.comparator();
		assertThat("Simple filename comparison.", comparator.compare("foo.txt", "bar.text"), is(greaterThan(0)));
		assertThat("Equal filenames.", comparator.compare("foobar.txt", "foobar.txt"), is(0));
		assertThat("One filename with no extension compares correctly.", comparator.compare("foo.txt", "foo-bar"), is(lessThan(0)));
		assertThat("Filenames both with no extension compare correctly.", comparator.compare("foo", "foo-bar"), is(lessThan(0)));
		assertThat("Filename is considered separately from extension.", comparator.compare("foo.txt", "foo-bar.txt"), is(lessThan(0)));
		assertThat("Accents are sorted greater.", comparator.compare("fóobar.txt", "foobar.txt"), is(greaterThan(0)));
		assertThat("Lexical order supercedes accents.", comparator.compare("fóobar.txt", "foozar.txt"), is(lessThan(0)));
		assertThat("Lowercase is sorted before.", comparator.compare("foobar.txt", "fOobar.txt"), is(lessThan(0)));
		assertThat("Lexical order supercedes case.", comparator.compare("foobar.txt", "fAobar.txt"), is(greaterThan(0)));
		assertThat("Lexical order supercedes case and accents.", comparator.compare("fÓobar.txt", "foozar.txt"), is(lessThan(0)));
		assertThat("Extensions compare correctly.", comparator.compare("foobar.abc", "foobar.abd"), is(lessThan(0)));
	}

	//## dotfiles

	/** @see Filenames #isDotfileFilename(String) */
	@Test
	void testIsDotfileFilename() {
		assertThrows(NullPointerException.class, () -> Filenames.isDotfileFilename(null));
		assertThrows(IllegalArgumentException.class, () -> Filenames.isDotfileFilename(""));
		assertThat(Filenames.isDotfileFilename("."), is(false));
		assertThat(Filenames.isDotfileFilename(".x"), is(true));
		assertThat(Filenames.isDotfileFilename(".x."), is(true));
		assertThat(Filenames.isDotfileFilename(".x.."), is(true));
		assertThat(Filenames.isDotfileFilename(".foo"), is(true));
		assertThat(Filenames.isDotfileFilename(".foo."), is(true));
		assertThat(Filenames.isDotfileFilename(".foo.bar"), is(true));
		assertThat(Filenames.isDotfileFilename(".foo.bar."), is(true));
		assertThat(Filenames.isDotfileFilename(".."), is(false));
		assertThat(Filenames.isDotfileFilename("..x"), is(true));
		assertThat(Filenames.isDotfileFilename("..x."), is(true));
		assertThat(Filenames.isDotfileFilename("..x.."), is(true));
		assertThat(Filenames.isDotfileFilename("..foo"), is(true));
		assertThat(Filenames.isDotfileFilename("..foo."), is(true));
		assertThat(Filenames.isDotfileFilename("..foo.bar"), is(true));
		assertThat(Filenames.isDotfileFilename("..foo.bar."), is(true));
		assertThat(Filenames.isDotfileFilename("x"), is(false));
		assertThat(Filenames.isDotfileFilename("x."), is(false));
		assertThat(Filenames.isDotfileFilename("x.."), is(false));
		assertThat(Filenames.isDotfileFilename("foo"), is(false));
		assertThat(Filenames.isDotfileFilename("foo."), is(false));
		assertThat(Filenames.isDotfileFilename("foo.bar"), is(false));
		assertThat(Filenames.isDotfileFilename("foo.bar."), is(false));
	}

	//## encode/decode

	/**
	 * Test to verify if the method {@link Filenames#encodeCrossPlatformFilename(String)} and {@link Filenames#decodeFilename(String)} are encoding and decoding
	 * the file name correctly
	 */
	@Test
	void testRecoverNotAllowedCharactersWithLatinCharacters() {
		final String publicationName = Filenames.encodeCrossPlatformFilename("Beijing Shanghai");
		assertThat(Filenames.decodeFilename(publicationName), equalTo("Beijing Shanghai"));
	}

	/**
	 * Test to verify if the method {@link Filenames#encodeCrossPlatformFilename(String)} and {@link Filenames#decodeFilename(String)} are encoding and decoding
	 * the file name correctly
	 */
	@Test
	void testRecoverNotAllowedCharactersWithChineseCharacters() {
		final String publicationName = Filenames.encodeCrossPlatformFilename("北京上海 (Beijing Shanghai)"); // Chinese: "Beijing Shanghai"
		assertThat(Filenames.decodeFilename(publicationName), equalTo("北京上海 (Beijing Shanghai)")); // Chinese: "Beijing Shanghai"
	}

	/**
	 * Test to verify if the method {@link Filenames#encodeCrossPlatformFilename(String)} and {@link Filenames#decodeFilename(String)} are encoding and decoding
	 * the file name correctly
	 */
	@Test
	void testRecoverNotAllowedCharactersWithChineseCharactersAndSlash() {
		final String publicationName = Filenames.encodeCrossPlatformFilename("北京/上海 (Beijing/Shanghai)"); // Chinese: "Beijing/Shanghai"
		assertThat(Filenames.decodeFilename(publicationName), equalTo("北京/上海 (Beijing/Shanghai)")); // Chinese: "Beijing/Shanghai"
	}

	/**
	 * Test to verify if the method {@link Filenames#encodeCrossPlatformFilename(String)} and {@link Filenames#decodeFilename(String)} are encoding and decoding
	 * the file name correctly
	 */
	@Test
	void testRecoverNotAllowedCharactersWithSlash() {
		final String publicationName = Filenames.encodeCrossPlatformFilename("/ (Dream of the Red Chamber)");
		assertThat(Filenames.decodeFilename(publicationName), equalTo("/ (Dream of the Red Chamber)"));
	}

	//## base filenames

	/** @see Filenames#appendBase(String, CharSequence) */
	@Test
	void testAppendBase() {
		assertThat(Filenames.appendBase("", ""), is(""));
		assertThat(Filenames.appendBase("foo", ""), is("foo"));
		assertThat(Filenames.appendBase("", "bar"), is("bar"));
		assertThat(Filenames.appendBase("foo", "bar"), is("foobar"));
		assertThat(Filenames.appendBase("foo.test", "-bar"), is("foo-bar.test"));
		assertThat(Filenames.appendBase("foo.bar.test", "-more"), is("foo-more.bar.test"));
		assertThat(Filenames.appendBase("test.tar.xz", "-12.3.456"), is("test-12.3.456.tar.xz"));
		//TODO decide how to handle and fix with JAVA-80: assertThat(Filenames.appendBase("test-12.3.456.tar.xz", "-rc7"), is("test-12.3.456-rc7.tar.xz"));
		assertThat(Filenames.appendBase("test-12.3.456-rc7.tar.xz", ""), is("test-12.3.456-rc7.tar.xz"));
	}

	/** @see Filenames#changeBase(String, String) */
	@Test
	void testChangeBase() {
		assertThrows(IllegalArgumentException.class, () -> Filenames.changeBase("", ""));
		assertThrows(IllegalArgumentException.class, () -> Filenames.changeBase("foo", ""));
		assertThrows(IllegalArgumentException.class, () -> Filenames.changeBase("", "bar"));
		assertThat(Filenames.changeBase("test", "test"), is("test"));
		assertThat(Filenames.changeBase("test", "foo"), is("foo"));
		assertThat(Filenames.changeBase("test.bar", "test"), is("test.bar"));
		assertThat(Filenames.changeBase("test.bar", "foo"), is("foo.bar"));
		assertThat(Filenames.changeBase("test.foo.bar", "test"), is("test.foo.bar"));
		assertThat(Filenames.changeBase("test.foo.bar", "other"), is("other.foo.bar"));
	}

	//## extensions

	/** Tests whether {@link Filenames#addExtension(String, String)} is working properly. */
	@Test
	void testAddExtension() {
		assertThat(Filenames.addExtension("foobar", "txt"), is("foobar.txt"));
		assertThat(Filenames.addExtension("foobar", ""), is("foobar."));

		assertThat(Filenames.addExtension("", ".foobar"), is("..foobar"));
		assertThat(Filenames.addExtension(".foobar", ""), is(".foobar."));
	}

	/** Tests whether {@link Filenames#addExtension(String, String)} is throwing an exception when a <code>null</code> name is provided. */
	void testAddExtensionNullNameFail() {
		assertThrows(NullPointerException.class, () -> Filenames.addExtension(null, "txt"));
	}

	/** Tests whether {@link Filenames#addExtension(String, String)} is throwing an exception when a <code>null</code> extension is provided. */
	void testAddExtensionNullExtensionFail() {
		assertThrows(NullPointerException.class, () -> Filenames.addExtension("foobar", null));
	}

	/** Tests whether {@link Filenames#findExtension(String)} is working properly. */
	@Test
	void testFindExtension() {
		assertThat(Filenames.findExtension("foobar.txt"), isPresentAndIs("txt"));
		assertThat(Filenames.findExtension("foobar"), isEmpty());
		assertThat(Filenames.findExtension(""), isEmpty());

		assertThat(Filenames.findExtension(".foobar"), isPresentAndIs("foobar"));
		assertThat(Filenames.findExtension("."), isPresentAndIs(""));

		assertThrows(NullPointerException.class, () -> Filenames.findExtension(null));
	}

	/** @see Filenames#hasExtension(String, CharSequence) */
	@Test
	void testHasExtension() {
		//check for literal extension
		assertThat(Filenames.hasExtension("foo.bar", "bar"), is(true));
		assertThat(Filenames.hasExtension("foo.bare", "bar"), is(false));
		assertThat(Filenames.hasExtension("foo.bar", "baz"), is(false));
		assertThat(Filenames.hasExtension("foobar", "bar"), is(false));
		assertThat(Filenames.hasExtension("bar", "bar"), is(false));
		//check for ASCII case variation
		assertThat(Filenames.hasExtension("foo.BAR", "bar"), is(true));
		assertThat(Filenames.hasExtension("foo.bar", "BAR"), is(true));
		assertThat(Filenames.hasExtension("foo.bAr", "BaR"), is(true));
		//don't support non-ASCII case variation
		assertThat(Filenames.hasExtension("tou.ché", "ché"), is(true));
		assertThat(Filenames.hasExtension("tou.CHÉ", "ché"), is(false));
	}

	/** @see Filenames#changeExtension(String, String) */
	@Test
	void testChangeExtension() {
		assertThrows(IllegalArgumentException.class, () -> Filenames.changeExtension("", "foo"));
		assertThat(Filenames.changeExtension("test", "foo"), is("test.foo"));
		assertThat(Filenames.changeExtension("test.foo", "foo"), is("test.foo"));
		assertThat(Filenames.changeExtension("test.foo", "bar"), is("test.bar"));
		assertThat(Filenames.changeExtension("test.foo.bar", "other"), is("test.foo.other"));
	}

	//# Filenames.Extensions

	/** @see Filenames.Extensions#normalize(String) */
	@Test
	void testExtensionsNormalize() {
		assertThat(Filenames.Extensions.normalize("txt"), is("txt"));
		assertThat(Filenames.Extensions.normalize("TXT"), is("txt"));
		assertThat(Filenames.Extensions.normalize("tXt"), is("txt"));
		assertThat(Filenames.Extensions.normalize("touché"), is("touché"));
		assertThat(Filenames.Extensions.normalize("TOUCHÉ"), is("touchÉ"));
	}

	/** @see Filenames.Extensions#equals(String, String) */
	@Test
	void testExtensionsEquals() {
		assertThat(Filenames.Extensions.equals(null, null), is(true));
		assertThat(Filenames.Extensions.equals(null, ""), is(false));
		assertThat(Filenames.Extensions.equals("", null), is(false));
		assertThat(Filenames.Extensions.equals(null, "x"), is(false));
		assertThat(Filenames.Extensions.equals("x", null), is(false));
		assertThat(Filenames.Extensions.equals(null, "foo"), is(false));
		assertThat(Filenames.Extensions.equals("foo", null), is(false));
		assertThat(Filenames.Extensions.equals("", ""), is(true));
		assertThat(Filenames.Extensions.equals("txt", "txt"), is(true));
		assertThat(Filenames.Extensions.equals("TXT", "txt"), is(true));
		assertThat(Filenames.Extensions.equals("txt", "TXT"), is(true));
		assertThat(Filenames.Extensions.equals("Txt", "tXt"), is(true));
		assertThat(Filenames.Extensions.equals("touché", "touché"), is(true));
		assertThat(Filenames.Extensions.equals("TOUCHÉ", "touché"), is(false));
	}

}
