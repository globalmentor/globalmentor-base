/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.net.URI;
import java.util.*;

import org.junit.Test;

import com.globalmentor.test.AbstractTest;

/**
 * Various tests for URI utilities.
 * 
 * @author Garret Wilson
 */
public class URIsTest extends AbstractTest
{

	/**
	 * Tests plain encoding of URIs.
	 * @see URIs#plainEncode(URI)
	 */
	@Test
	public void testPlainEncodeDecode()
	{
		final List<URI> uris = new ArrayList<URI>();
		final List<String> encodedURIs = new ArrayList<String>();
		testPlainEncode(URI.create("http://www.example.com/foo/bar"), "http---www.example.com-foo-bar", uris, encodedURIs); //simple URI
		testPlainEncode(URI.create("x-foo.bar://www.example.com/foo/bar"), "x_2Dfoo.bar---www.example.com-foo-bar", uris, encodedURIs); //non-simple scheme
		testPlainEncode(URI.create("http://www.example.com/foo-bar"), "http---www.example.com-foo_2Dbar", uris, encodedURIs); //encoded hyphens
		testPlainEncode(URI.create("http://www.example.com/foo_bar"), "http---www.example.com-foo_5Fbar", uris, encodedURIs); //encoded underscores
		testPlainEncode(URI.create("http://www.example.com/foo/bar#fooBar"), "http---www.example.com-foo-bar_23fooBar", uris, encodedURIs); //fragments
		testPlainEncode(URI.create("http://www.example.com/foo!bar"), "http---www.example.com-foo_21bar", uris, encodedURIs); //non-name character
		testPlainEncode(URI.create("http://www.example.com/foo%2Abar"), "http---www.example.com-foo_252Abar", uris, encodedURIs); //URI-encoded character
		for(int i = encodedURIs.size() - 1; i >= 0; --i) //look at the encoded URIs, going backwards only for efficiency
		{
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
	protected void testPlainEncode(final URI uri, final String expectedEncodedURI, final List<URI> uris, final List<String> encodedURIs)
	{
		assertThat(URIs.plainEncode(uri), is(expectedEncodedURI));
		uris.add(uri);
		encodedURIs.add(expectedEncodedURI);
	}

}
