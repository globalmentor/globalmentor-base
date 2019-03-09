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

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.io.IOException;
import java.nio.file.Path;

import org.junit.*;
import org.junit.rules.TemporaryFolder;

/**
 * Tests the {@link Paths} utility class.
 * 
 * @author Magno Nascimento
 */
public class PathsTest {

	@Rule
	public final TemporaryFolder tempFolder = new TemporaryFolder(); //TODO redo tests to use the user home directory; no actual files/directories need to be created 

	/**
	 * Tests whether the extension is being added correctly using {@link Paths#addExtension(Path, String)} with an absolute path.
	 * 
	 * @throws IOException if an I/O error occurs.
	 */
	@Test
	public void addExtensionTest() throws IOException {
		final Path rootPath = tempFolder.getRoot().toPath();
		final Path tempFile = java.nio.file.Files.createFile(rootPath.resolve("testFile"));

		assertThat(Paths.addExtension(tempFile.toAbsolutePath(), "ext"), equalTo(rootPath.resolve("testFile.ext")));
	}

	/**
	 * Tests whether the extension is being added correctly using {@link Paths#addExtension(Path, String)} with a relative path.
	 * 
	 * @throws IOException if an I/O error occurs.
	 */
	@Test
	public void addExtensionUsingRelativePathTest() throws IOException {
		final Path rootPath = tempFolder.getRoot().toPath();
		final Path tempFile = java.nio.file.Files.createFile(rootPath.resolve("testFile"));

		assertThat(Paths.addExtension(tempFile, "ext"), equalTo(rootPath.resolve("testFile.ext")));
	}

	/**
	 * Tests whether the extension is being added correctly using {@link Paths#addExtension(Path, String)} with a file name path.
	 * 
	 * @throws IOException if an I/O error occurs.
	 */
	@Test
	public void addExtensionUsingFileNameTest() throws IOException {
		final Path rootPath = tempFolder.getRoot().toPath();
		final Path tempFile = java.nio.file.Files.createFile(rootPath.resolve("testFile"));

		assertThat(Paths.addExtension(tempFile.getFileName(), "ext"), equalTo(rootPath.resolve("testFile.ext").getFileName()));
	}
}
