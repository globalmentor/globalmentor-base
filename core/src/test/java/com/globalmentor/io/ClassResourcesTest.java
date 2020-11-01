/*
 * Copyright © 2020 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static java.nio.charset.StandardCharsets.*;
import static java.nio.file.Files.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.hamcrest.MatcherAssert.*;

import java.io.*;
import java.nio.file.*;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.*;

/**
 * Tests of {@link ClassResources}.
 * @author Garret Wilson
 */
public class ClassResourcesTest {

	public static String FOO_TXT_RESOURCE_NAME = "foo.txt";
	public static String SUBDIR_EXAMPLE_TXT_RESOURCE_NAME = "subdir/example.txt";

	//## resource paths

	/** @see ClassResources#getClassLoaderResourcePath(Class, String) */
	@Test
	public void testGetClassLoaderResourcePath() {
		//relative paths
		assertThat(ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, ""), is("java/lang/"));
		assertThat(ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "foo"), is("java/lang/foo"));
		assertThat(ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "foo.bar"), is("java/lang/foo.bar"));
		assertThat(ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "foo/bar"), is("java/lang/foo/bar"));
		assertThat(ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "foo/bar.txt"), is("java/lang/foo/bar.txt"));
		//absolute paths
		assertThat(ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "/"), is(""));
		assertThat(ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "/foo"), is("foo"));
		assertThat(ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "/foo.bar"), is("foo.bar"));
		assertThat(ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "/foo/bar"), is("foo/bar"));
		//invalid absolute paths
		assertThat(ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "/foo/bar.txt"), is("foo/bar.txt"));
		assertThrows(IllegalArgumentException.class, () -> ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "//foo"));
		assertThrows(IllegalArgumentException.class, () -> ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "//foo.bar"));
		assertThrows(IllegalArgumentException.class, () -> ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "//foo/bar"));
		assertThrows(IllegalArgumentException.class, () -> ClassResources.getClassLoaderResourcePath(java.lang.Integer.class, "//foo/bar.txt"));
	}

	//## resource contents

	/** @see ClassResources#copy(ClassLoader, String, Path, CopyOption...) */
	@Test
	public void testCopyFromResourceToFile(@TempDir Path tempDir) throws IOException {
		final Path targetFooFile = tempDir.resolve("dest.txt");
		ClassResources.copy(ClassResourcesTest.class, FOO_TXT_RESOURCE_NAME, targetFooFile);
		assertThat(new String(readAllBytes(targetFooFile), UTF_8), is("bar"));
	}

	/** @see ClassResources#copy(ClassLoader, String, Path, CopyOption...) */
	@Test
	public void testCopyFromSubDirResourceToFile(@TempDir Path tempDir) throws IOException {
		final Path targetFile = tempDir.resolve("dest.txt");
		ClassResources.copy(ClassResourcesTest.class, SUBDIR_EXAMPLE_TXT_RESOURCE_NAME, targetFile);
		assertThat(new String(readAllBytes(targetFile), UTF_8), is("test"));
	}

	/** @see ClassResources#copy(ClassLoader, String, Path, CopyOption...) */
	@Test
	public void testCopyFromMissingResourceThrowsFileNotFoundException(@TempDir Path tempDir) throws IOException {
		final Path targetFile = tempDir.resolve("dest.txt");
		assertThrows(FileNotFoundException.class, () -> ClassResources.copy(ClassResourcesTest.class, "missing.txt", targetFile));
	}

	/** @see ClassResources#copy(ClassLoader, String, Path, CopyOption...) */
	@Test
	public void testCopyFromResourceToSubDirFileCreatesSubDir(@TempDir Path tempDir) throws IOException {
		final Path targetFile = tempDir.resolve("first").resolve("second").resolve("test.txt");
		ClassResources.copy(ClassResourcesTest.class, FOO_TXT_RESOURCE_NAME, targetFile);
		assertThat(new String(readAllBytes(targetFile), UTF_8), is("bar"));
	}

}
