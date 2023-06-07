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

package com.globalmentor.io;

import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;
import static java.nio.file.Files.*;
import static org.hamcrest.MatcherAssert.*;

import java.io.*;
import java.nio.file.*;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.*;

/**
 * Integration tests for the {@link Files} utility class.
 * @author Garret Wilson
 */
public class FilesIT {

	//## Path

	/** @see {@link Files#deleteIfExists(Path, boolean)} */
	@Test
	void testDeleteIfExistsRecursiveForFile(@TempDir final Path tempDir) throws IOException {
		final Path file = createFile(tempDir.resolve("foo.bar"));
		assertThat(Files.deleteIfExists(file, true), is(true));
	}

	/** @see {@link Files#deleteIfExists(Path, boolean)} */
	@Test
	void testDeleteIfExistsRecursiveForEmptyDirectory(@TempDir final Path tempDir) throws IOException {
		final Path directory = createDirectory(tempDir.resolve("dir"));
		assertThat(Files.deleteIfExists(directory, true), is(true));
	}

	/**
	 * Verifies that {@link Files#deleteIfExists(Path, boolean)} in recursive mode does not fail if the directory does not exist.
	 * @see {@link Files#deleteIfExists(Path, boolean)}
	 */
	@Test
	void verifyDeleteIfExistsRecursiveThrowsNoExceptionIfPathDoesNotExist(@TempDir final Path tempDir) throws IOException {
		try {
			final Path missingDirectory = tempDir.resolve("missing");
			assertThat(Files.deleteIfExists(missingDirectory, true), is(false));
		} catch(final NoSuchFileException noSuchFileException) {
			fail("Should not throw exception if path is already missing.", noSuchFileException);
		}
	}

}
