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

import static com.globalmentor.net.URIs.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.io.*;
import java.net.URI;
import java.nio.file.*;

import org.junit.*;

/**
 * Tests the {@link Files} utility class.
 * 
 * @author Garret Wilson
 */
public class FilesTest {

	private static File tempFile;

	@BeforeClass
	public static void createTempFile() throws IOException {
		tempFile = Files.createTempFile(); //create a plain old type file
	}

	@AfterClass
	public static void deleteTempFile() throws IOException {
		Files.delete(tempFile);
	}

	//encode/decode filename

	/**
	 * Test to verify if the method {@link Files#encodeCrossPlatformFilename(String)} and {@link Files#decodeFilename(String)} are encoding and decoding the file
	 * name correctly
	 * <p>
	 * Test contributed by Magno Nascimento.
	 * </p>
	 */
	@Test
	public void testRecoverNotAllowedCharactersWithLatinCharacters() {
		final String publicationName = Files.encodeCrossPlatformFilename("Dream Of The Red Chamber");
		assertThat(Files.decodeFilename(publicationName), equalTo("Dream Of The Red Chamber"));
	}

	/**
	 * Test to verify if the method {@link Files#encodeCrossPlatformFilename(String)} and {@link Files#decodeFilename(String)} are encoding and decoding the file
	 * name correctly
	 * <p>
	 * Test contributed by Magno Nascimento.
	 * </p>
	 */
	@Test
	public void testRecoverNotAllowedCharactersWithChineseCharacters() {
		final String publicationName = Files.encodeCrossPlatformFilename("紅樓夢红楼梦 (Dream of the Red Chamber)");
		assertThat(Files.decodeFilename(publicationName), equalTo("紅樓夢红楼梦 (Dream of the Red Chamber)"));
	}

	/**
	 * Test to verify if the method {@link Files#encodeCrossPlatformFilename(String)} and {@link Files#decodeFilename(String)} are encoding and decoding the file
	 * name correctly
	 * <p>
	 * Test contributed by Magno Nascimento.
	 * </p>
	 */
	@Test
	public void testRecoverNotAllowedCharactersWithChineseCharactersAndSlash() {
		final String publicationName = Files.encodeCrossPlatformFilename("紅樓夢/红楼梦 (Dream of the Red Chamber)");
		assertThat(Files.decodeFilename(publicationName), equalTo("紅樓夢/红楼梦 (Dream of the Red Chamber)"));
	}

	/**
	 * Test to verify if the method {@link Files#encodeCrossPlatformFilename(String)} and {@link Files#decodeFilename(String)} are encoding and decoding the file
	 * name correctly
	 */
	@Test
	public void testRecoverNotAllowedCharactersWithSlash() {
		final String publicationName = Files.encodeCrossPlatformFilename("/ (Dream of the Red Chamber)");
		assertThat(Files.decodeFilename(publicationName), equalTo("/ (Dream of the Red Chamber)"));
	}

	//File.toFileURI(File)

	/**
	 * Tests converting a file to a URI.
	 * @see Files#toURI(File)
	 * @see Uris#createUri(URI)
	 */
	@Test
	public void testFileURI() {
		final URI fileURI = Files.toURI(tempFile); //create a Java URI directly from a file
		assertThat(fileURI.getScheme(), is(FILE_SCHEME)); //file:
		assertTrue(fileURI.getRawPath().startsWith(ROOT_PATH)); //file:/
		assertFalse(fileURI.getRawPath().startsWith(ROOT_PATH + PATH_SEPARATOR + PATH_SEPARATOR)); //not file:/// (even though that is correct)
	}

	@Test
	public void testGetBackupPath() throws IOException {
		Path tempFile = Files.createTempFile().toPath();

		assertThat(Files.getBackupPath(tempFile), equalTo(Paths.get(tempFile + ".bak")));
		assertThat(Files.getBackupPath(tempFile, 3), equalTo(Paths.get(tempFile + ".1.bak")));
	}

	/**
	 * Test to see if the method {@link Files#backupFile(Path)} is working as usual when an empty file is given.
	 * 
	 * @throws IOException if an I/O error occurs.
	 */
	@Test
	public void testEmptyBackupFile() throws IOException {
		Path tempDirectory = java.nio.file.Files.createTempDirectory("backupTestDirectory");
		Path tempFile = java.nio.file.Files.createFile(tempDirectory.resolve("importantFile.test"));

		Files.backupFile(tempFile);

		assertThat(java.nio.file.Files.exists(Files.getBackupPath(tempFile)), is(true));

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Files.getBackupPath(tempFile))) {
			assertThat(reader.readLine(), equalTo(null));
		}
	}

	/**
	 * Test to see if the method {@link Files#backupFile(Path)} is working properly when it's asked to create a simple backup to a file.
	 * 
	 * @throws IOException if an I/O error occurs.
	 */
	@Test
	public void testSimpleBackupFile() throws IOException {
		Path tempDirectory = java.nio.file.Files.createTempDirectory("backupTestDirectory");
		Path tempFile = tempDirectory.resolve("importantFile.test");

		try (final BufferedWriter writer = java.nio.file.Files.newBufferedWriter(tempFile)) {
			writer.write("This is the first edition an important file!");
		}

		Files.backupFile(tempFile);

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Files.getBackupPath(tempFile))) {
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
	public void testRollingBackupFile() throws IOException {
		Path tempDirectory = java.nio.file.Files.createTempDirectory("backupTestDirectory");
		Path tempFile = tempDirectory.resolve("importantFile.test");

		try (final BufferedWriter writer = java.nio.file.Files.newBufferedWriter(tempFile)) {
			writer.write("This is the first edition an important file!");
		}

		Files.backupFile(tempFile, 3);

		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".1.bak")), is(true));
		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".2.bak")), is(false));
		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".3.bak")), is(false));

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Files.getBackupPath(tempFile, 3))) {
			assertThat(reader.readLine(), equalTo("This is the first edition an important file!"));
		}

		try (final BufferedWriter writer = java.nio.file.Files.newBufferedWriter(tempFile)) {
			writer.write("This is the second edition of an important file!");
		}

		Files.backupFile(tempFile, 3);

		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".1.bak")), is(true));
		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".2.bak")), is(true));
		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".3.bak")), is(false));

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Files.getBackupPath(tempFile, 3))) {
			assertThat(reader.readLine(), equalTo("This is the second edition of an important file!"));
		}

		try (final BufferedWriter writer = java.nio.file.Files.newBufferedWriter(tempFile)) {
			writer.write("This is the third edition of an important file!");
		}

		Files.backupFile(tempFile, 3);

		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".1.bak")), is(true));
		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".2.bak")), is(true));
		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".3.bak")), is(true));

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Files.getBackupPath(tempFile, 3))) {
			assertThat(reader.readLine(), equalTo("This is the third edition of an important file!"));
		}

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Paths.get(tempFile + ".2.bak"))) {
			assertThat(reader.readLine(), equalTo("This is the second edition of an important file!"));
		}

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Paths.get(tempFile + ".3.bak"))) {
			assertThat(reader.readLine(), equalTo("This is the first edition an important file!"));
		}

		try (final BufferedWriter writer = java.nio.file.Files.newBufferedWriter(tempFile)) {
			writer.write("This is the fourth edition of an important file!");
		}

		Files.backupFile(tempFile, 3);

		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".1.bak")), is(true));
		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".2.bak")), is(true));
		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".3.bak")), is(true));
		assertThat(java.nio.file.Files.exists(Paths.get(tempFile + ".4.bak")), is(false));

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Files.getBackupPath(tempFile, 3))) {
			assertThat(reader.readLine(), equalTo("This is the fourth edition of an important file!"));
		}

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Paths.get(tempFile + ".2.bak"))) {
			assertThat(reader.readLine(), equalTo("This is the third edition of an important file!"));
		}

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Paths.get(tempFile + ".3.bak"))) {
			assertThat(reader.readLine(), equalTo("This is the second edition of an important file!"));
		}
	}

	/**
	 * Test to see if the method {@link Files#newOutputStreamWithBackup(Path, OpenOption...)} is working properly.
	 * 
	 * @throws IOException if an I/O error occurs.
	 */
	@Test
	public void testOutputStreamWithBackup() throws IOException {
		Path tempDirectory = java.nio.file.Files.createTempDirectory("backupTestDirectory");
		Path tempFile = tempDirectory.resolve("importantFile.test");

		try (final BufferedWriter writer = java.nio.file.Files.newBufferedWriter(tempFile)) {
			writer.write("This is the first edition an important file!");
		}

		try (final BufferedOutputStream outputStream = new BufferedOutputStream(Files.newOutputStreamWithBackup(tempFile))) {
			outputStream.write(new byte[] {'t', 'e', 's', 't', 'e'});
		}

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(tempFile)) {
			assertThat(reader.readLine(), equalTo("teste"));
		}

		assertThat(java.nio.file.Files.exists(Files.getBackupPath(tempFile)), is(true));

		try (final BufferedReader reader = java.nio.file.Files.newBufferedReader(Files.getBackupPath(tempFile))) {
			assertThat(reader.readLine(), equalTo("This is the first edition an important file!"));
		}
	}

}
