/*
 * Copyright © 2017 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.OperatingSystem.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Path;

import org.junit.jupiter.api.*;

/**
 * Tests the {@link Paths} utility class.
 * 
 * @author Magno Nascimento
 * @author Garret Wilson
 */
public class PathsTest {

	/** @see Paths#isSubPath(Path, Path) */
	@Test
	public void testIsSubPath() {
		final Path tempDirectory = getTempDirectory();
		assertThat(Paths.isSubPath(tempDirectory, tempDirectory), is(true));
		assertThat(Paths.isSubPath(tempDirectory, tempDirectory.resolve("foo")), is(true));
		assertThat(Paths.isSubPath(tempDirectory, tempDirectory.resolve("foo").resolve("bar")), is(true));
		assertThat(Paths.isSubPath(tempDirectory, tempDirectory.resolve("foo").resolve("bar").resolve("test.txt")), is(true));
		assertThat(Paths.isSubPath(tempDirectory.resolve("foo"), tempDirectory.resolve("foo").resolve("bar").resolve("test.txt")), is(true));
		assertThat(Paths.isSubPath(tempDirectory.resolve("foo"), tempDirectory.resolve("bar").resolve("test.txt")), is(false));
		assertThat(Paths.isSubPath(tempDirectory.resolve("foo"), tempDirectory.resolve("bar")), is(false));
		assertThat(Paths.isSubPath(tempDirectory.resolve("foo"), tempDirectory), is(false));
	}

	/** @see Paths#changeBase(Path, Path, Path) */
	@Test
	public void testChangeBase() {
		final Path tempDirectory = getTempDirectory(); //e.g. /temp
		final Path testFile = tempDirectory.resolve("foo").resolve("test.txt"); //e.g. /temp/foo/test.txt

		//same base
		assertThat(Paths.changeBase(testFile, tempDirectory, tempDirectory), is(testFile));
		assertThat(Paths.changeBase(testFile, tempDirectory.resolve("foo"), tempDirectory.resolve("foo")), is(testFile));

		//different base
		assertThat(Paths.changeBase(testFile, tempDirectory.resolve("foo"), tempDirectory.resolve("bar")), is(tempDirectory.resolve("bar").resolve("test.txt")));

		//nested base
		assertThat(Paths.changeBase(testFile, tempDirectory, tempDirectory.resolve("bar")), is(tempDirectory.resolve("bar").resolve("foo").resolve("test.txt")));

		//not a base
		assertThrows(IllegalArgumentException.class, () -> Paths.changeBase(testFile, tempDirectory.resolve("bad"), tempDirectory.resolve("bar")));
	}

	//#filenames

	/** Tests whether the extension is being added correctly using {@link Paths#addExtension(Path, String)}. */
	@Test
	public void testAddExtension() {
		final Path basePath = getTempDirectory();
		assertThat(Paths.addExtension(basePath.resolve("testFile"), "ext"), is(basePath.resolve("testFile.ext")));
	}

	/** Tests whether an additional extension is being added correctly using {@link Paths#addExtension(Path, String)}. */
	@Test
	public void testAddSecondExtension() {
		final Path basePath = getTempDirectory();
		assertThat(Paths.addExtension(basePath.resolve("testFile.foo"), "ext"), is(basePath.resolve("testFile.foo.ext")));
	}

	/** Tests whether the extension is being added correctly using {@link Paths#addExtension(Path, String)} with a file name path. */
	@Test
	public void testAddExtensionUsingFileName() {
		final Path testPath = java.nio.file.Paths.get("test");
		assertThat(Paths.addExtension(testPath, "ext"), is(java.nio.file.Paths.get("test.ext")));
	}

	/** Tests whether the extension is being changed correctly using {@link Paths#addExtension(Path, String)}. */
	@Test
	public void testCchangeExtension() {
		final Path basePath = getTempDirectory();
		assertThat(Paths.changeExtension(basePath.resolve("testFile.foo"), "bar"), is(basePath.resolve("testFile.bar")));
	}

	/** Tests whether the extension is being removed correctly using {@link Paths#addExtension(Path, String)}. */
	@Test
	public void testRemoveExtension() {
		final Path basePath = getTempDirectory();
		assertThat(Paths.removeExtension(basePath.resolve("testFile.foo")), is(basePath.resolve("testFile")));
	}

}
