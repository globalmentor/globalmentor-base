/*
 * Copyright © 2017 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.OperatingSystem.*;
import static java.util.Arrays.*;
import static java.util.Collections.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.*;
import java.util.Collection;
import java.util.Comparator;

import org.junit.jupiter.api.*;

/**
 * Tests the {@link Paths} utility class.
 * @implNote These tests use the system temp directory rather than using a facility for creating a temporary directory specifically for tests, because these
 *           utilities work in terms of abstract path identifiers and do not need to actually perform disk operations.
 * @author Magno Nascimento
 * @author Garret Wilson
 */
public class PathsTest {

	/** @see Paths#getPath(FileSystem, Collection) */
	@Test
	public void testGetPath() {
		final FileSystem fileSystem = FileSystems.getDefault();
		assertThrows(IllegalArgumentException.class, () -> Paths.getPath(fileSystem, emptyList()));
		assertThat(Paths.getPath(fileSystem, asList("foo")), is(fileSystem.getPath("foo")));
		assertThat(Paths.getPath(fileSystem, asList("foo", "bar")), is(fileSystem.getPath("foo", "bar")));
		assertThat(Paths.getPath(fileSystem, asList("foo", "bar", "test")), is(fileSystem.getPath("foo", "bar", "test")));
	}

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

	/** @see Paths#resolve(Path, Collection) */
	@Test
	public void testResolve() {
		final Path tempDirectory = getTempDirectory();
		assertThat(Paths.resolve(tempDirectory, emptyList()), is(tempDirectory));
		assertThat(Paths.resolve(tempDirectory, asList("foo")), is(tempDirectory.resolve("foo")));
		assertThat(Paths.resolve(tempDirectory, asList("foo", "bar")), is(tempDirectory.resolve("foo").resolve("bar")));
		assertThat(Paths.resolve(tempDirectory, asList("foo", "bar", "test")), is(tempDirectory.resolve("foo").resolve("bar").resolve("test")));
	}

	//# filenames

	/** @see Paths#filenameComparator() */
	@Test
	public void testFilenameComparator() {
		final Comparator<Path> filenameComparator = Paths.filenameComparator();
		final Path fooBarPath = java.nio.file.Paths.get("foo", "bar");
		final Path fooBerryPath = java.nio.file.Paths.get("foo", "berry");
		final Path fooBarApplePath = java.nio.file.Paths.get("foo", "bar", "apple");
		final Path fooBarBerryPath = java.nio.file.Paths.get("foo", "bar", "berry");
		final Path noFilenamePath = java.nio.file.Paths.get("/");
		assertThat("Expect JDK to indicate no filename for path with no components (e.g. `/`).", noFilenamePath.getFileName(), is(nullValue()));
		assertThat("First missing filename sorted less.", filenameComparator.compare(noFilenamePath, fooBarPath), is(lessThan(0)));
		assertThat("Second missing filename sorted greater.", filenameComparator.compare(fooBarPath, noFilenamePath), is(greaterThan(0)));
		assertThat("Both missing filenames sorted equal.", filenameComparator.compare(noFilenamePath, java.nio.file.Paths.get(noFilenamePath.toString())), is(0));
		assertThat("Equal filenames sorted correctly.", filenameComparator.compare(fooBarPath, java.nio.file.Paths.get("foo", "bar")), is(0));
		assertThat("Filenames sorted correctly ascending.", filenameComparator.compare(fooBarApplePath, fooBarBerryPath), is(lessThan(0)));
		assertThat("Filenames sorted correctly descending.", filenameComparator.compare(fooBarBerryPath, fooBarApplePath), is(greaterThan(0)));
		assertThat("Equal filenames at different levels sorted correctly.", filenameComparator.compare(fooBerryPath, fooBarBerryPath), is(0));
		assertThat("Filenames at different levels sorted correctly ascending.", filenameComparator.compare(fooBarPath, fooBarBerryPath), is(lessThan(0)));
		assertThat("Filenames at different levels sorted correctly descending.", filenameComparator.compare(fooBarBerryPath, fooBarPath), is(greaterThan(0)));
	}

	//## extensions

	/** Tests whether the extension is being added correctly using {@link Paths#addFilenameExtension(Path, String)}. */
	@Test
	public void testAddFilenameExtension() {
		final Path basePath = getTempDirectory();
		assertThat(Paths.addFilenameExtension(basePath.resolve("testFile"), "ext"), is(basePath.resolve("testFile.ext")));
	}

	/** Tests whether an additional extension is being added correctly using {@link Paths#addFilenameExtension(Path, String)}. */
	@Test
	public void testAddSecondFilenameExtension() {
		final Path basePath = getTempDirectory();
		assertThat(Paths.addFilenameExtension(basePath.resolve("testFile.foo"), "ext"), is(basePath.resolve("testFile.foo.ext")));
	}

	/** Tests whether the extension is being added correctly using {@link Paths#addFilenameExtension(Path, String)} with a file name path. */
	@Test
	public void testAddFilenameExtensionUsingFileName() {
		final Path testPath = java.nio.file.Paths.get("test");
		assertThat(Paths.addFilenameExtension(testPath, "ext"), is(java.nio.file.Paths.get("test.ext")));
	}

	/** Tests whether the extension is being changed correctly using {@link Paths#addFilenameExtension(Path, String)}. */
	@Test
	public void testChangeFilenameExtension() {
		final Path basePath = getTempDirectory();
		assertThat(Paths.changeFilenameExtension(basePath.resolve("testFile.foo"), "bar"), is(basePath.resolve("testFile.bar")));
	}

	/** Tests whether the extension is being removed correctly using {@link Paths#addFilenameExtension(Path, String)}. */
	@Test
	public void testRemoveFilenameExtension() {
		final Path basePath = getTempDirectory();
		assertThat(Paths.removeFilenameExtension(basePath.resolve("testFile.foo")), is(basePath.resolve("testFile")));
	}

}
