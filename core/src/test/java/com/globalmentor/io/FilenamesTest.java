/*
 * Copyright © 2012-2018 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.io;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.*;

import com.globalmentor.io.Filenames;

/**
 * Various tests for working with filenames.
 * 
 * @author Garret Wilson
 * @author Magno Nascimento
 * @see Filenames
 */
public class FilenamesTest {

	//#filenames

	//##dotfiles

	/** @see Filenames #isDotfileFilename(String) */
	@Test
	public void testIsDotfileFilename() {
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

	//#encode/decode

	/**
	 * Test to verify if the method {@link Filenames#encodeCrossPlatformFilename(String)} and {@link Filenames#decodeFilename(String)} are encoding and decoding
	 * the file name correctly
	 * <p>
	 * Test contributed by Magno Nascimento.
	 * </p>
	 */
	@Test
	public void testRecoverNotAllowedCharactersWithLatinCharacters() {
		final String publicationName = Filenames.encodeCrossPlatformFilename("Dream Of The Red Chamber");
		assertThat(Filenames.decodeFilename(publicationName), equalTo("Dream Of The Red Chamber"));
	}

	/**
	 * Test to verify if the method {@link Filenames#encodeCrossPlatformFilename(String)} and {@link Filenames#decodeFilename(String)} are encoding and decoding
	 * the file name correctly
	 * <p>
	 * Test contributed by Magno Nascimento.
	 * </p>
	 */
	@Test
	public void testRecoverNotAllowedCharactersWithChineseCharacters() {
		final String publicationName = Filenames.encodeCrossPlatformFilename("紅樓夢红楼梦 (Dream of the Red Chamber)");
		assertThat(Filenames.decodeFilename(publicationName), equalTo("紅樓夢红楼梦 (Dream of the Red Chamber)"));
	}

	/**
	 * Test to verify if the method {@link Filenames#encodeCrossPlatformFilename(String)} and {@link Filenames#decodeFilename(String)} are encoding and decoding
	 * the file name correctly
	 * <p>
	 * Test contributed by Magno Nascimento.
	 * </p>
	 */
	@Test
	public void testRecoverNotAllowedCharactersWithChineseCharactersAndSlash() {
		final String publicationName = Filenames.encodeCrossPlatformFilename("紅樓夢/红楼梦 (Dream of the Red Chamber)");
		assertThat(Filenames.decodeFilename(publicationName), equalTo("紅樓夢/红楼梦 (Dream of the Red Chamber)"));
	}

	/**
	 * Test to verify if the method {@link Filenames#encodeCrossPlatformFilename(String)} and {@link Filenames#decodeFilename(String)} are encoding and decoding
	 * the file name correctly
	 */
	@Test
	public void testRecoverNotAllowedCharactersWithSlash() {
		final String publicationName = Filenames.encodeCrossPlatformFilename("/ (Dream of the Red Chamber)");
		assertThat(Filenames.decodeFilename(publicationName), equalTo("/ (Dream of the Red Chamber)"));
	}

	//#extensions

	/** Tests whether {@link Filenames#addExtension(String, String)} is working properly. */
	@Test
	public void testAddNameExtension() {
		assertThat(Filenames.addExtension("foobar", "txt"), is("foobar.txt"));
		assertThat(Filenames.addExtension("foobar", ""), is("foobar."));

		assertThat(Filenames.addExtension("", ".foobar"), is("..foobar"));
		assertThat(Filenames.addExtension(".foobar", ""), is(".foobar."));
	}

	/** Tests whether {@link Filenames#addExtension(String, String)} is throwing an exception when a <code>null</code> name is provided. */
	public void testAddNameExtensionNullNameFail() {
		assertThrows(NullPointerException.class, () -> Filenames.addExtension(null, "txt"));
	}

	/** Tests whether {@link Filenames#addExtension(String, String)} is throwing an exception when a <code>null</code> extension is provided. */
	public void testAddNameExtensionNullExtensionFail() {
		assertThrows(NullPointerException.class, () -> Filenames.addExtension("foobar", null));
	}

	/** Tests whether {@link Filenames#getExtension(String)} is working properly. */
	@Test
	public void testGetNameExtensionString() {
		assertThat(Filenames.getExtension("foobar.txt"), is("txt"));
		assertThat(Filenames.getExtension("foobar"), is((String)null));
		assertThat(Filenames.getExtension(""), is((String)null));

		assertThat(Filenames.getExtension(".foobar"), is("foobar"));
		assertThat(Filenames.getExtension("."), is(""));
	}

	/** Tests whether {@link Filenames#getExtension(String)} is throwing an exception when a <code>null</code> name is provided. */
	public void testGetNameExtensionNullStringFail() {
		assertThrows(NullPointerException.class, () -> Filenames.getExtension((String)null));
	}

}
