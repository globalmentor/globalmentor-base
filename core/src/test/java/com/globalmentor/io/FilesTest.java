/*
 * Copyright © 2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.io.Paths.*;
import static com.globalmentor.net.URIs.*;
import static java.nio.file.Files.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.hamcrest.MatcherAssert.*;

import java.io.*;
import java.net.URI;
import java.nio.file.*;
import java.nio.file.Paths;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.*;

/**
 * Tests the {@link Files} utility class.
 * 
 * @author Garret Wilson
 */
public class FilesTest {

	public static String FOO_TXT_RESOURCE_NAME = "foo.txt";
	public static String SUBDIR_EXAMPLE_TXT_RESOURCE_NAME = "subdir/example.txt";

	//## File

	/**
	 * Tests converting a file to a URI.
	 * @see Files#toURI(File)
	 * @see Uris#createUri(URI)
	 */
	@Test
	public void testFileURI(@TempDir Path tempDir) {
		final File tempFile = tempDir.resolve("foo.bar").toFile();
		final URI fileURI = Files.toURI(tempFile); //create a Java URI directly from a file
		assertThat(fileURI.getScheme(), is(FILE_SCHEME)); //file:
		assertTrue(fileURI.getRawPath().startsWith(ROOT_PATH)); //file:/
		assertFalse(fileURI.getRawPath().startsWith(ROOT_PATH + PATH_SEPARATOR + PATH_SEPARATOR)); //not file:/// (even though that is correct)
	}

	//## Path

	//### Backup

	@Test
	public void testGetBackupPath(@TempDir Path tempDir) throws IOException {
		final Path tempFile = createFile(tempDir.resolve("foo.bar"));

		assertThat(Files.getBackupPath(tempFile), equalTo(addFilenameExtension(tempFile, "bak")));
		assertThat(Files.getBackupPath(tempFile, 3), equalTo(addFilenameExtension(tempFile, "1.bak")));
	}

	/**
	 * Test to see if the method {@link Files#backupFile(Path)} is working as usual when an empty file is given.
	 * @throws IOException if an I/O error occurs.
	 */
	@Test
	public void testEmptyBackupFile(@TempDir Path tempDir) throws IOException {
		Path tempFile = createFile(tempDir.resolve("importantFile.test"));

		Files.backupFile(tempFile);

		assertThat(exists(Files.getBackupPath(tempFile)), is(true));

		try (final BufferedReader reader = newBufferedReader(Files.getBackupPath(tempFile))) {
			assertThat(reader.readLine(), equalTo(null));
		}
	}

	/**
	 * Test to see if the method {@link Files#backupFile(Path)} is working properly when it's asked to create a simple backup to a file.
	 * 
	 * @throws IOException if an I/O error occurs.
	 */
	@Test
	public void testSimpleBackupFile(@TempDir Path tempDir) throws IOException {
		Path tempFile = createFile(tempDir.resolve("importantFile.test"));

		try (final BufferedWriter writer = newBufferedWriter(tempFile)) {
			writer.write("This is the first edition an important file!");
		}

		Files.backupFile(tempFile);

		try (final BufferedReader reader = newBufferedReader(Files.getBackupPath(tempFile))) {
			assertThat(reader.readLine(), equalTo("This is the first edition an important file!"));
		}
	}

	/**
	 * Test to see if the method {@link Files#backupFile(Path)} is working properly when it's asked to create a backup with a rolling policy to a file, and if the
	 * rolling policy is working correctly.
	 * 
	 * @throws IOException if an I/O error occurs.
	 */
	@Test
	public void testRollingBackupFile(@TempDir Path tempDir) throws IOException {
		Path tempFile = createFile(tempDir.resolve("importantFile.test"));

		try (final BufferedWriter writer = newBufferedWriter(tempFile)) {
			writer.write("This is the first edition an important file!");
		}

		Files.backupFile(tempFile, 3);

		assertThat(exists(Paths.get(tempFile + ".1.bak")), is(true));
		assertThat(exists(Paths.get(tempFile + ".2.bak")), is(false));
		assertThat(exists(Paths.get(tempFile + ".3.bak")), is(false));

		try (final BufferedReader reader = newBufferedReader(Files.getBackupPath(tempFile, 3))) {
			assertThat(reader.readLine(), equalTo("This is the first edition an important file!"));
		}

		try (final BufferedWriter writer = newBufferedWriter(tempFile)) {
			writer.write("This is the second edition of an important file!");
		}

		Files.backupFile(tempFile, 3);

		assertThat(exists(Paths.get(tempFile + ".1.bak")), is(true));
		assertThat(exists(Paths.get(tempFile + ".2.bak")), is(true));
		assertThat(exists(Paths.get(tempFile + ".3.bak")), is(false));

		try (final BufferedReader reader = newBufferedReader(Files.getBackupPath(tempFile, 3))) {
			assertThat(reader.readLine(), equalTo("This is the second edition of an important file!"));
		}

		try (final BufferedWriter writer = newBufferedWriter(tempFile)) {
			writer.write("This is the third edition of an important file!");
		}

		Files.backupFile(tempFile, 3);

		assertThat(exists(Paths.get(tempFile + ".1.bak")), is(true));
		assertThat(exists(Paths.get(tempFile + ".2.bak")), is(true));
		assertThat(exists(Paths.get(tempFile + ".3.bak")), is(true));

		try (final BufferedReader reader = newBufferedReader(Files.getBackupPath(tempFile, 3))) {
			assertThat(reader.readLine(), equalTo("This is the third edition of an important file!"));
		}

		try (final BufferedReader reader = newBufferedReader(Paths.get(tempFile + ".2.bak"))) {
			assertThat(reader.readLine(), equalTo("This is the second edition of an important file!"));
		}

		try (final BufferedReader reader = newBufferedReader(Paths.get(tempFile + ".3.bak"))) {
			assertThat(reader.readLine(), equalTo("This is the first edition an important file!"));
		}

		try (final BufferedWriter writer = java.nio.file.Files.newBufferedWriter(tempFile)) {
			writer.write("This is the fourth edition of an important file!");
		}

		Files.backupFile(tempFile, 3);

		assertThat(exists(Paths.get(tempFile + ".1.bak")), is(true));
		assertThat(exists(Paths.get(tempFile + ".2.bak")), is(true));
		assertThat(exists(Paths.get(tempFile + ".3.bak")), is(true));
		assertThat(exists(Paths.get(tempFile + ".4.bak")), is(false));

		try (final BufferedReader reader = newBufferedReader(Files.getBackupPath(tempFile, 3))) {
			assertThat(reader.readLine(), equalTo("This is the fourth edition of an important file!"));
		}

		try (final BufferedReader reader = newBufferedReader(Paths.get(tempFile + ".2.bak"))) {
			assertThat(reader.readLine(), equalTo("This is the third edition of an important file!"));
		}

		try (final BufferedReader reader = newBufferedReader(Paths.get(tempFile + ".3.bak"))) {
			assertThat(reader.readLine(), equalTo("This is the second edition of an important file!"));
		}
	}

	/**
	 * Test to see if the method {@link Files#newOutputStreamWithBackup(Path, OpenOption...)} is working properly.
	 * 
	 * @throws IOException if an I/O error occurs.
	 */
	@Test
	public void testOutputStreamWithBackup(@TempDir Path tempDir) throws IOException {
		Path tempFile = createFile(tempDir.resolve("importantFile.test"));

		try (final BufferedWriter writer = newBufferedWriter(tempFile)) {
			writer.write("This is the first edition an important file!");
		}

		try (final BufferedOutputStream outputStream = new BufferedOutputStream(Files.newOutputStreamWithBackup(tempFile))) {
			outputStream.write(new byte[] {'t', 'e', 's', 't', 'e'});
		}

		try (final BufferedReader reader = newBufferedReader(tempFile)) {
			assertThat(reader.readLine(), equalTo("teste"));
		}

		assertThat(exists(Files.getBackupPath(tempFile)), is(true));

		try (final BufferedReader reader = newBufferedReader(Files.getBackupPath(tempFile))) {
			assertThat(reader.readLine(), equalTo("This is the first edition an important file!"));
		}
	}

}
