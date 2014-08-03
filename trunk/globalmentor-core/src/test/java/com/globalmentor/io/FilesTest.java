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

import java.io.File;
import java.io.IOException;
import java.net.URI;

import org.junit.*;

import com.globalmentor.test.AbstractTest;

/**
 * Tests the {@link Files} utility class.
 * 
 * @author Garret Wilson
 */
public class FilesTest extends AbstractTest {

	private static File tempFile;

	@BeforeClass
	public static void createTempFile() throws IOException {
		tempFile = Files.createTempFile(); //create a plain old type file
	}

	@AfterClass
	public static void deleteTempFile() throws IOException {
		Files.delete(tempFile);
	}

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

}
