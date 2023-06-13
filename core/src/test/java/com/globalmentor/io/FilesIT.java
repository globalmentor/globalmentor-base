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
import static org.junit.jupiter.api.Assumptions.*;
import static com.globalmentor.io.Files.*;
import static com.globalmentor.io.Paths.*;
import static java.nio.file.Files.*;
import static org.hamcrest.MatcherAssert.*;

import java.io.*;
import java.nio.file.*;
import java.nio.file.Paths;
import java.nio.file.attribute.*;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.*;
import org.junit.jupiter.api.io.*;

/**
 * Integration tests for the {@link Files} utility class.
 * @author Garret Wilson
 */
public class FilesIT {

	//## Path

	/** @see {@link Files#deleteIfExists(Path, boolean)} */
	@Test
	@EnabledOnOs(value = OS.WINDOWS, disabledReason = "Working with DOS read-only files should only be done on Windows.")
	void verifyDeleteIfExistsNotForceFailsOnReadOnlyFile(@TempDir final Path tempDir) throws IOException {
		assumeTrue(readAttributes(tempDir, BasicFileAttributes.class) instanceof DosFileAttributes,
				"We assume that on Windows the file system uses DOS attributes; otherwise this test will not work.");
		final Path file = createFile(tempDir.resolve("foo.bar"));
		setAttribute(file, ATTRIBUTE_DOS_READONLY, true);
		assertThat(exists(file), is(true));
		assertThrows(AccessDeniedException.class, () -> Files.deleteIfExists(file, false));
	}

	/** @see {@link Files#deleteIfExists(Path, boolean)} */
	@Test
	@EnabledOnOs(value = OS.WINDOWS, disabledReason = "Working with DOS read-only files should only be done on Windows.")
	void verifyDeleteIfExistsForceDeletesReadOnlyFile(@TempDir final Path tempDir) throws IOException {
		assumeTrue(readAttributes(tempDir, BasicFileAttributes.class) instanceof DosFileAttributes,
				"We assume that on Windows the file system uses DOS attributes; otherwise this test will not work.");
		final Path file = createFile(tempDir.resolve("foo.bar"));
		setAttribute(file, ATTRIBUTE_DOS_READONLY, true);
		assertThat(exists(file), is(true));
		assertThat(Files.deleteIfExists(file, true), is(true));
		assertThat(exists(file), is(false));
	}

	/** @see {@link Files#deleteIfExists(Path, boolean)} */
	@Test
	@EnabledOnOs(value = OS.WINDOWS, disabledReason = "Working with DOS read-only files should only be done on Windows.")
	void verifyDeleteIfExistsNotForceFailsOnReadOnlyDirectory(@TempDir final Path tempDir) throws IOException {
		assumeTrue(readAttributes(tempDir, BasicFileAttributes.class) instanceof DosFileAttributes,
				"We assume that on Windows the file system uses DOS attributes; otherwise this test will not work.");
		final Path directory = createDirectory(tempDir.resolve("dir"));
		setAttribute(directory, ATTRIBUTE_DOS_READONLY, true);
		try {
			assertThat(exists(directory), is(true));
			assertThrows(AccessDeniedException.class, () -> Files.deleteIfExists(directory, false));
		} finally {
			setAttribute(directory, ATTRIBUTE_DOS_READONLY, false); //always remove the read-only attribute to allow JUnit to clean up; see [JUnit Issue #3352](https://github.com/junit-team/junit5/issues/3352)
		}
	}

	/** @see {@link Files#deleteIfExists(Path, boolean)} */
	@Test
	@EnabledOnOs(value = OS.WINDOWS, disabledReason = "Working with DOS read-only files should only be done on Windows.")
	void verifyDeleteIfExistsForceDeletesReadOnlyDirectory(@TempDir final Path tempDir) throws IOException {
		assumeTrue(readAttributes(tempDir, BasicFileAttributes.class) instanceof DosFileAttributes,
				"We assume that on Windows the file system uses DOS attributes; otherwise this test will not work.");
		final Path directory = createDirectory(tempDir.resolve("dir"));
		setAttribute(directory, ATTRIBUTE_DOS_READONLY, true);
		assertThat(exists(directory), is(true));
		assertThat(Files.deleteIfExists(directory, true), is(true));
		assertThat(exists(directory), is(false));
	}

	/** @see {@link Files#deleteFileTree(Path)} */
	@Test
	void testDeleteFileTreeForFile(@TempDir final Path tempDir) throws IOException {
		final Path file = createFile(tempDir.resolve("foo.bar"));
		assertThat(exists(file), is(true));
		assertThat(Files.deleteFileTree(file), is(true));
		assertThat(exists(file), is(false));
	}

	/** @see {@link Files#deleteFileTree(Path)} */
	@Test
	void testDeleteFileTreeForEmptyDirectory(@TempDir final Path tempDir) throws IOException {
		final Path directory = createDirectory(tempDir.resolve("dir"));
		assertThat(exists(directory), is(true));
		assertThat(Files.deleteFileTree(directory), is(true));
		assertThat(exists(directory), is(false));
	}

	/** @see {@link Files#deleteFileTree(Path)} */
	@Test
	void testDeleteFileTreeForNonEmptyDirectory(@TempDir final Path tempDir) throws IOException {
		final Path directory = createDirectory(tempDir.resolve("dir"));
		createFile(directory.resolve("foo.bar"));
		assertThat(exists(directory), is(true));
		assertThat(Files.deleteFileTree(directory), is(true));
		assertThat(exists(directory), is(false));
	}

	/** @see {@link Files#deleteFileTree(Path)} */
	@Test
	void testDeleteFileTreeForTree(@TempDir final Path tempDir) throws IOException {
		final Path fooDirectory = createDirectory(tempDir.resolve("foo"));
		createFile(fooDirectory.resolve("level1.bin"));
		final Path fooBarDirectory = createDirectory(fooDirectory.resolve("bar"));
		createFile(fooBarDirectory.resolve("level2.bin"));
		createDirectory(fooBarDirectory.resolve("bottom"));
		assertThat(exists(fooDirectory), is(true));
		assertThat(Files.deleteFileTree(fooDirectory), is(true));
		assertThat(exists(fooDirectory), is(false));
	}

	/** @see {@link Files#deleteFileTree(Path, boolean)} */
	@Test
	@EnabledOnOs(value = OS.WINDOWS, disabledReason = "Working with DOS read-only files should only be done on Windows.")
	void verifyDeleteFileTreeNotForceFailsOnReadOnlyFile(@TempDir final Path tempDir) throws IOException {
		assumeTrue(readAttributes(tempDir, BasicFileAttributes.class) instanceof DosFileAttributes,
				"We assume that on Windows the file system uses DOS attributes; otherwise this test will not work.");
		final Path fooDirectory = createDirectory(tempDir.resolve("foo"));
		final Path level1File = createFile(fooDirectory.resolve("level1.bin"));
		setAttribute(level1File, ATTRIBUTE_DOS_READONLY, true);
		final Path fooBarDirectory = createDirectory(fooDirectory.resolve("bar"));
		final Path level2File = createFile(fooBarDirectory.resolve("level2.bin"));
		setAttribute(level2File, ATTRIBUTE_DOS_READONLY, true);
		createDirectory(fooBarDirectory.resolve("bottom"));
		assertThat(exists(fooDirectory), is(true));
		assertThrows(AccessDeniedException.class, () -> Files.deleteFileTree(fooDirectory, false));
	}

	/** @see {@link Files#deleteFileTree(Path, boolean)} */
	@Test
	@EnabledOnOs(value = OS.WINDOWS, disabledReason = "Working with DOS read-only files should only be done on Windows.")
	void verifyDeleteFileTreeForceDeletesReadOnlyFile(@TempDir final Path tempDir) throws IOException {
		assumeTrue(readAttributes(tempDir, BasicFileAttributes.class) instanceof DosFileAttributes,
				"We assume that on Windows the file system uses DOS attributes; otherwise this test will not work.");
		final Path fooDirectory = createDirectory(tempDir.resolve("foo"));
		final Path level1File = createFile(fooDirectory.resolve("level1.bin"));
		setAttribute(level1File, ATTRIBUTE_DOS_READONLY, true);
		final Path fooBarDirectory = createDirectory(fooDirectory.resolve("bar"));
		final Path level2File = createFile(fooBarDirectory.resolve("level2.bin"));
		setAttribute(level2File, ATTRIBUTE_DOS_READONLY, true);
		createDirectory(fooBarDirectory.resolve("bottom"));
		assertThat(exists(fooDirectory), is(true));
		assertThat(Files.deleteFileTree(fooDirectory, true), is(true));
		assertThat(exists(fooDirectory), is(false));
	}

	/**
	 * Verifies that {@link Files#deleteFileTree(Path)} does not fail if the directory does not exist.
	 * @see {@link Files#deleteFileTree(Path)}
	 */
	@Test
	void verifyDeleteFileTreeThrowsNoExceptionIfPathDoesNotExist(@TempDir final Path tempDir) throws IOException {
		try {
			final Path missingDirectory = tempDir.resolve("missing");
			assertThat(exists(missingDirectory), is(false));
			assertThat(Files.deleteFileTree(missingDirectory), is(false));
			assertThat(exists(missingDirectory), is(false));
		} catch(final NoSuchFileException noSuchFileException) {
			fail("Should not throw exception if path is already missing.", noSuchFileException);
		}
	}

	/**
	 * Verifies that force-deleting a file tree successfully deletes a JGit repository, including its <code>.git/objects</code> subtree, which contains read-only
	 * files as an append-only object database.
	 * @implNote This implementation is specifically not restricted to any particular platform. This is a broader use-case smoke test; the other more focused test
	 *           address the underlying techniques used to make Git repository objects read-only on individual platforms. Currently only Windows systems need
	 *           forcing to remove Git repository directories; on POSIX file systems files in <code>.git/objects</code> are read-only directories are not,
	 *           allowing the to be deleted normally.
	 * @see <a href="https://bugs.eclipse.org/bugs/show_bug.cgi?id=582051">JGit Bug 582051 - access denied trying to delete .git directory .idx file on Windows
	 *      10</a>
	 * @see {@link Files#deleteFileTree(Path, boolean)}
	 */
	@Test
	@Disabled("Test disabled until a way is found to a way to disable/move JGit `~/.config/jgit` directory.") //see https://bugs.eclipse.org/bugs/show_bug.cgi?id=582064
	void verifyDeleteFileTreeForceDeletesGitRepository(@TempDir final Path tempDir) throws IOException, GitAPIException {
		final Path repoDirectory = createDirectory(tempDir.resolve("repo"));
		try (final Git git = Git.init().setDirectory(repoDirectory.toFile()).call()) {
			final Path newFile = createFile(repoDirectory.resolve("new.dat"));
			git.add().addFilepattern(newFile.getFileName().toString()).call();
			git.commit().setMessage("Added new file.").call();
		}
		assertThat(exists(repoDirectory), is(true));
		assertThat(Files.deleteFileTree(repoDirectory, true), is(true));
		assertThat(exists(repoDirectory), is(false));
	}

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
