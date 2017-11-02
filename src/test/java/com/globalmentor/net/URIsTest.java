/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.net;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

import java.net.URI;
import java.util.*;

import org.junit.*;

/**
 * Various tests for URI utilities.
 * 
 * @author Garret Wilson
 */
public class URIsTest {

	//URIs

	/** @see URIs#getCurrentLevel(URI). */
	@Test
	public void testGetCurrentLevel() {
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/foo/./bar/test/.")), is(URI.create("http://example.com/foo/bar/test/")));
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/foo/../bar/test/.")), is(URI.create("http://example.com/bar/test/")));
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/foo/bar/test/.")), is(URI.create("http://example.com/foo/bar/test/")));
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/foo/bar/test/..")), is(URI.create("http://example.com/foo/bar/test/"))); //counter-intuitive
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/foo/bar/file.ext")), is(URI.create("http://example.com/foo/bar/")));
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/foo/bar/")), is(URI.create("http://example.com/foo/bar/")));
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/foo/bar")), is(URI.create("http://example.com/foo/")));
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/foo/")), is(URI.create("http://example.com/foo/")));
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/foo")), is(URI.create("http://example.com/")));
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/")), is(URI.create("http://example.com/")));
		assertThat(URIs.getCurrentLevel(URI.create("http://example.com/foo/../..")), is(URI.create("http://example.com/")));
	}

	/** @see URIs#getParentLevel(URI). */
	@Test
	public void testGetParentLevel() {
		assertThat(URIs.getParentLevel(URI.create("http://example.com/foo/./bar/test/.")), is(URI.create("http://example.com/foo/bar/")));
		assertThat(URIs.getParentLevel(URI.create("http://example.com/foo/../bar/test/.")), is(URI.create("http://example.com/bar/")));
		assertThat(URIs.getParentLevel(URI.create("http://example.com/foo/bar/test/.")), is(URI.create("http://example.com/foo/bar/")));
		assertThat(URIs.getParentLevel(URI.create("http://example.com/foo/bar/test/..")), is(URI.create("http://example.com/foo/bar/"))); //counter-intuitive
		assertThat(URIs.getParentLevel(URI.create("http://example.com/foo/bar/file.ext")), is(URI.create("http://example.com/foo/")));
		assertThat(URIs.getParentLevel(URI.create("http://example.com/foo/bar/")), is(URI.create("http://example.com/foo/")));
		assertThat(URIs.getParentLevel(URI.create("http://example.com/foo/bar")), is(URI.create("http://example.com/")));
		assertThat(URIs.getParentLevel(URI.create("http://example.com/foo/")), is(URI.create("http://example.com/")));
		assertThat(URIs.getParentLevel(URI.create("http://example.com/foo")), is(nullValue()));
		assertThat(URIs.getParentLevel(URI.create("http://example.com/")), is(nullValue()));
		assertThat(URIs.getParentLevel(URI.create("http://example.com/foo/../..")), is(nullValue()));
	}

	/** @see URIs#getParentURI(URI). */
	@Test
	public void testGetParentURI() {
		assertThat(URIs.getParentURI(URI.create("http://example.com/foo/bar/test/.")), is(URI.create("http://example.com/foo/bar/test/"))); //non-normalized collection URI not supported
		assertThat(URIs.getParentURI(URI.create("http://example.com/foo/bar/test/..")), is(URI.create("http://example.com/foo/bar/test/")));
		assertThat(URIs.getParentURI(URI.create("http://example.com/foo/bar/file.ext")), is(URI.create("http://example.com/foo/bar/")));
		assertThat(URIs.getParentURI(URI.create("http://example.com/foo/bar/")), is(URI.create("http://example.com/foo/")));
		assertThat(URIs.getParentURI(URI.create("http://example.com/foo/bar")), is(URI.create("http://example.com/foo/")));
		assertThat(URIs.getParentURI(URI.create("http://example.com/foo/")), is(URI.create("http://example.com/")));
		assertThat(URIs.getParentURI(URI.create("http://example.com/foo")), is(URI.create("http://example.com/")));
		assertThat(URIs.getParentURI(URI.create("http://example.com/")), is(nullValue()));
		assertThat(URIs.getParentURI(URI.create("http://example.com/foo/../..")), is(URI.create("http://example.com/"))); //non-normalized collection URI not supported
	}

	/**
	 * Illustrates a bug in the JDK in which {@link URI#hashCode()} does not meet it obligations in regard to {@link URI#equals(Object)}.
	 * <p>
	 * <em>Apparently fixed in Java 1.8.0_75 as part of <a href="https://bugs.openjdk.java.net/browse/JDK-7171415">JDK-7171415</a>.</em>
	 * </p>
	 * @see <a href="https://stackoverflow.com/q/16257996/421049">How to get recognition of Java URI hashCode() bug that has been inappropriately denied</a>
	 * @see <a href="http://bugs.java.com/bugdatabase/view_bug.do?bug_id=7054089">JDK-7054089</a>
	 * @see <a href="https://bugs.openjdk.java.net/browse/JDK-7171415">JDK-7171415</a>
	 */
	@Test
	@Ignore //TODO un-ignore and remove workaround code across projects
	public void testURIHashCode() {
		final URI uri1 = URI.create("http://www.example.com/foo%2Abar");
		final URI uri2 = URI.create("http://www.example.com/foo%2abar");
		assertThat("URIs are not equal.", uri1, equalTo(uri2));
		assertThat("Equal URIs do not have same hash code.", uri1.hashCode(), equalTo(uri2.hashCode()));
	}

	/**
	 * Tests plain encoding of URIs.
	 * @see URIs#plainEncode(URI)
	 */
	@Test
	public void testPlainEncodeDecode() {
		final List<URI> uris = new ArrayList<URI>();
		final List<String> encodedURIs = new ArrayList<String>();
		testPlainEncode(URI.create("http://www.example.com/foo/bar"), "http---www.example.com-foo-bar", uris, encodedURIs); //simple URI
		testPlainEncode(URI.create("x-foo.bar://www.example.com/foo/bar"), "x_2dfoo.bar---www.example.com-foo-bar", uris, encodedURIs); //non-simple scheme
		testPlainEncode(URI.create("http://www.example.com/foo-bar"), "http---www.example.com-foo_2dbar", uris, encodedURIs); //encoded hyphens
		testPlainEncode(URI.create("http://www.example.com/foo_bar"), "http---www.example.com-foo_5fbar", uris, encodedURIs); //encoded underscores
		testPlainEncode(URI.create("http://www.example.com/foo/bar#fooBar"), "http---www.example.com-foo-bar_23fooBar", uris, encodedURIs); //fragments
		testPlainEncode(URI.create("http://www.example.com/foo!bar"), "http---www.example.com-foo_21bar", uris, encodedURIs); //non-name character
		testPlainEncode(URI.create("http://www.example.com/foo%2Abar"), "http---www.example.com-foo_252Abar", uris, encodedURIs); //URI-encoded character
		for(int i = encodedURIs.size() - 1; i >= 0; --i) { //look at the encoded URIs, going backwards only for efficiency
			assertThat(URIs.plainDecode(encodedURIs.get(i)), is(uris.get(i))); //make sure they all decode properly
		}
	}

	/**
	 * Tests the plain encoding of a single URI and adds the URI and its encoded form to the given lists.
	 * @param uri The URI to encode.
	 * @param expectedEncodedURI The expected encoded form of the URI.
	 * @param uris The collected URIs.
	 * @param encodedURIs The collected encoded URIs.
	 */
	protected void testPlainEncode(final URI uri, final String expectedEncodedURI, final List<URI> uris, final List<String> encodedURIs) {
		assertThat(URIs.plainEncode(uri), is(expectedEncodedURI));
		uris.add(uri);
		encodedURIs.add(expectedEncodedURI);
	}

}
