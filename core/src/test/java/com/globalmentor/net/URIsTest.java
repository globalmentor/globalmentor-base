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

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.net.URI;
import java.util.*;

import org.junit.jupiter.api.*;

import com.globalmentor.collections.CollectionMap;
import com.globalmentor.model.NameValuePair;

/**
 * Various tests for URI utilities.
 * 
 * @author Garret Wilson
 */
public class URIsTest {

	/** @see URIs#changeBase(URI, URI, URI) */
	@Test
	public void testChangeBase() {
		//same base
		assertThat(URIs.changeBase(URI.create("http://example.com/foo"), URI.create("http://example.com/"), URI.create("http://example.com/")),
				is(URI.create("http://example.com/foo")));
		assertThat(URIs.changeBase(URI.create("http://example.com/foo/"), URI.create("http://example.com/"), URI.create("http://example.com/")),
				is(URI.create("http://example.com/foo/")));
		assertThat(URIs.changeBase(URI.create("http://example.com/foo.txt"), URI.create("http://example.com/"), URI.create("http://example.com/")),
				is(URI.create("http://example.com/foo.txt")));
		assertThat(URIs.changeBase(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/"), URI.create("http://example.com/")),
				is(URI.create("http://example.com/foo/bar")));
		assertThat(URIs.changeBase(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/"), URI.create("http://example.com/")),
				is(URI.create("http://example.com/foo/bar/")));
		assertThat(URIs.changeBase(URI.create("http://example.com/foo/bar.txt"), URI.create("http://example.com/"), URI.create("http://example.com/")),
				is(URI.create("http://example.com/foo/bar.txt")));
		assertThat(
				URIs.changeBase(URI.create("http://example.com/base/base/foo"), URI.create("http://example.com/base/base/"), URI.create("http://example.com/base/")),
				is(URI.create("http://example.com/base/foo")));
		assertThat(URIs.changeBase(URI.create("http://example.com/base/foo/"), URI.create("http://example.com/base/"), URI.create("http://example.com/base/")),
				is(URI.create("http://example.com/base/foo/")));
		assertThat(URIs.changeBase(URI.create("http://example.com/base/foo.txt"), URI.create("http://example.com/base/"), URI.create("http://example.com/base/")),
				is(URI.create("http://example.com/base/foo.txt")));
		assertThat(URIs.changeBase(URI.create("http://example.com/base/foo/bar"), URI.create("http://example.com/base/"), URI.create("http://example.com/base/")),
				is(URI.create("http://example.com/base/foo/bar")));
		assertThat(URIs.changeBase(URI.create("http://example.com/base/foo/bar/"), URI.create("http://example.com/base/"), URI.create("http://example.com/base/")),
				is(URI.create("http://example.com/base/foo/bar/")));
		assertThat(
				URIs.changeBase(URI.create("http://example.com/base/foo/bar.txt"), URI.create("http://example.com/base/"), URI.create("http://example.com/base/")),
				is(URI.create("http://example.com/base/foo/bar.txt")));

		//different base
		assertThat(URIs.changeBase(URI.create("http://example.com/base/foo"), URI.create("http://example.com/base/"), URI.create("http://example.com/other/")),
				is(URI.create("http://example.com/other/foo")));
		assertThat(URIs.changeBase(URI.create("http://example.com/base/foo/"), URI.create("http://example.com/base/"), URI.create("http://example.com/other/")),
				is(URI.create("http://example.com/other/foo/")));
		assertThat(URIs.changeBase(URI.create("http://example.com/base/foo/bar/"), URI.create("http://example.com/base/"), URI.create("http://example.com/other/")),
				is(URI.create("http://example.com/other/foo/bar/")));
		assertThat(URIs.changeBase(URI.create("http://example.com/base/foo/bar/test.txt"), URI.create("http://example.com/base/"),
				URI.create("http://example.com/other/")), is(URI.create("http://example.com/other/foo/bar/test.txt")));

		//nested base
		assertThat(URIs.changeBase(URI.create("http://example.com/foo/test.txt"), URI.create("http://example.com/foo/"), URI.create("http://example.com/foo/bar/")),
				is(URI.create("http://example.com/foo/bar/test.txt")));

		//not a base
		assertThrows(IllegalArgumentException.class, () -> URIs.changeBase(URI.create("http://example.com/base/foo/bar/test.txt"),
				URI.create("http://example.com/bad/"), URI.create("http://example.com/other/")));
	}

	/** Tests whether {@link URIs#createURIList(URI...)} is working properly. */
	@Test
	public void testCreateURIList() {
		// No URI given.
		assertThat(URIs.createURIList(), is(emptyString()));

		final String baseTestURI = "http://example.com/";

		// Single URI given.
		assertThat(URIs.createURIList(URI.create(baseTestURI)), equalTo(baseTestURI));

		final String fooPath = "foo/";
		final String barPath = "bar/";

		// Multiple URI given.
		assertThat(URIs.createURIList(URI.create(baseTestURI), URI.create(baseTestURI + fooPath), URI.create(baseTestURI + fooPath + barPath)),
				equalTo(baseTestURI + "\r\n" + baseTestURI + fooPath + "\r\n" + baseTestURI + fooPath + barPath));
	}

	/** Tests whether {@link URIs#checkScheme(URI, String)} is working properly. */
	@Test
	public void testCheckScheme() {
		assertThat(URIs.checkScheme(URI.create("http://example.com/"), "http"), is(URI.create("http://example.com/")));
		assertThat(URIs.checkScheme(URI.create("https://example.com/"), "https"), is(URI.create("https://example.com/")));
	}

	/** Tests whether {@link URIs#checkScheme(URI, String)} is throwing an exception when an incompatible scheme is provided. */
	@Test
	public void testCheckSchemeFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.checkScheme(URI.create("http://example.com/"), "https"));
	}

	/** Tests whether {@link URIs#checkScheme(URI, String)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testCheckSchemeNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.checkScheme(null, "http"));
	}

	/** Tests whether {@link URIs#checkScheme(URI, String)} is throwing an exception when a null scheme is provided. */
	@Test
	public void testCheckSchemeNullSchemeFail() {
		assertThrows(NullPointerException.class, () -> URIs.checkScheme(URI.create("http://example.com/"), null));
	}

	/** Tests whether {@link URIs#changeScheme(URI, String)} is working properly. */
	@Test
	public void testChangeScheme() {
		assertThat(URIs.changeScheme(URI.create("http://example.com/"), "https"), is(URI.create("https://example.com/")));
		assertThat(URIs.changeScheme(URI.create("https://example.com/"), "http"), is(URI.create("http://example.com/")));
		assertThat(URIs.changeScheme(URI.create("https://example.com/"), "https"), is(URI.create("https://example.com/")));
	}

	/** Tests whether {@link URIs#changeScheme(URI, String)} is throwing an exception when an incompatible scheme is provided. */
	@Test
	public void testChangeSchemeFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.changeScheme(URI.create("http://example.com/"), "http s"));
	}

	/** Tests whether {@link URIs#changeScheme(URI, String)} is throwing an exception when a null {@link URI} provided. */
	@Test
	public void testCheckChangeSchemeNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeScheme(null, "http"));
	}

	/** Tests whether {@link URIs#changeScheme(URI, String)} is throwing an exception when a null scheme provided. */
	@Test
	public void testCheckChangeSchemeNullSchemeFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeScheme(URI.create("http://example.com/"), null));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is working properly. */
	@Test
	public void testCheckInfoNamespace() {
		assertThat(URIs.checkInfoNamespace(URI.create("info:ddc/22/eng//004.678"), "ddc"), is(URI.create("info:ddc/22/eng//004.678")));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when an incompatible info namespace is provided. */
	@Test
	public void testCheckInfoNamespaceFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.checkInfoNamespace(URI.create("info:lccn/22/eng//004.678"), "ddc"));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when a null {@link URI} provided. */
	@Test
	public void testCheckInfoNamespaceNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.checkInfoNamespace(null, "ddc"));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when a null scheme is provided. */
	@Test // TODO The method throws an IllegalArgumentException instead of NullPointerException.
	@Disabled
	public void testCheckInfoNamespaceNullSchemeFail() {
		assertThrows(NullPointerException.class, () -> URIs.checkInfoNamespace(URI.create("info:ddc/22/eng//004.678"), null));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is working properly. */
	@Test
	public void testGetInfoNamespace() {
		assertThat(URIs.getInfoNamespace(URI.create("info:ddc/22/eng//004.678")), is("ddc"));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when a non-info scheme is provided. */
	@Test
	public void testGetInfoNamespaceWrongSchemeFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.getInfoNamespace(URI.create("inf:ddc/22/eng//004.678")));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when an info {@link URI} without namespace is provided. */
	@Test
	public void testGetInfoNamespaceMissingNamespaceFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.getInfoNamespace(URI.create("inf:/22/eng//004.678")));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when a null {@link URI} provided. */
	@Test
	public void testGetInfoNamespaceNullFail() {
		assertThrows(NullPointerException.class, () -> URIs.getInfoNamespace(null));
	}

	/** Tests whether {@link URIs#getInfoIdentifier(URI)} is working properly. */
	@Test
	public void testGetInfoIdentifier() {
		assertThat(URIs.getInfoIdentifier(URI.create("info:ddc/22/eng//004.678")), is("22/eng//004.678"));
		assertThat(URIs.getInfoIdentifier(URI.create("info:ddc/22/eng//00%3F4.678")), is("22/eng//00?4.678"));
	}

	/** Tests whether {@link URIs#getInfoIdentifier(URI)} is throwing an exception when a non-info scheme is provided. */
	@Test
	public void testGetInfoIdentifierWrongSchemeFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.getInfoIdentifier(URI.create("inf:/22/eng//004.678")));
	}

	/** Tests whether {@link URIs#getInfoIdentifier(URI)} is throwing an exception when a null {@link URI} provided. */
	@Test
	public void testGetInfoIdentifierNullFail() {
		assertThrows(NullPointerException.class, () -> URIs.getInfoIdentifier(null));
	}

	/** Tests whether {@link URIs#getInfoRawIdentifier(URI)} is working properly. */
	@Test
	public void testGetInfoRawIdentifier() {
		assertThat(URIs.getInfoIdentifier(URI.create("info:ddc/22/eng//004.678")), is("22/eng//004.678"));
		assertThat(URIs.getInfoRawIdentifier(URI.create("info:ddc/22/eng//00%3F4.678")), is("22/eng//00%3F4.678"));
	}

	/** Tests whether {@link URIs#getInfoRawIdentifier(URI)} is throwing an exception when a non-info scheme is provided. */
	@Test
	public void testGetInfoRawIdentifierWrongSchemeFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.getInfoRawIdentifier(URI.create("inf:/22/eng//004.678")));
	}

	/** Tests whether {@link URIs#getInfoRawIdentifier(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testGetInfoRawIdentifierNullFail() {
		assertThrows(NullPointerException.class, () -> URIs.getInfoRawIdentifier(null));
	}

	/** See {@link URIs#isChild(URI, URI)}. */
	@Test
	public void testIsChild() {
		assertThat(URIs.isChild(URI.create("http://example.com/"), URI.create("http://example.com/")), is(true));
		assertThat(URIs.isChild(URI.create("http://example.com/"), URI.create("http://example.com/foo")), is(true));
		assertThat(URIs.isChild(URI.create("http://example.com/"), URI.create("http://example.com/foo/")), is(true));
		assertThat(URIs.isChild(URI.create("http://example.com/"), URI.create("http://example.com/foo/bar")), is(true));
		assertThat(URIs.isChild(URI.create("http://example.com/foo"), URI.create("http://example.com/")), is(false));
		assertThat(URIs.isChild(URI.create("http://example.com/foo/"), URI.create("http://example.com/")), is(false));
		assertThat(URIs.isChild(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/")), is(false));
		assertThat(URIs.isChild(URI.create("http://example.com/base/"), URI.create("http://example.com/base/")), is(true));
		assertThat(URIs.isChild(URI.create("http://example.com/base/"), URI.create("http://example.com/base/foo/")), is(true));
		assertThat(URIs.isChild(URI.create("http://example.com/base/"), URI.create("http://example.com/base/foo/bar/")), is(true));
		assertThat(URIs.isChild(URI.create("http://example.com/base/"), URI.create("http://example.com/base/foo/bar/test.txt")), is(true));
		assertThat(URIs.isChild(URI.create("http://example.com/base/foo/"), URI.create("http://example.com/base/foo/bar/test.txt")), is(true));
		assertThat(URIs.isChild(URI.create("http://example.com/base/foo/"), URI.create("http://example.com/base/bar/test.txt")), is(false));
		assertThat(URIs.isChild(URI.create("http://example.com/base/foo/"), URI.create("http://example.com/base/bar/")), is(false));
		assertThat(URIs.isChild(URI.create("http://example.com/base/foo/"), URI.create("http://example.com/base/")), is(false));
	}

	/** Tests whether {@link URIs#isInfoNamespace(URI, String)} is working properly. */
	@Test
	public void testIsInfoNamespace() {
		assertThat(URIs.isInfoNamespace(URI.create("info:ddc/22/eng//004.678"), "ddc"), is(true));
		assertThat(URIs.isInfoNamespace(URI.create("info:ddc/22/eng//004.678"), "lccn"), is(false));
		assertThat(URIs.isInfoNamespace(URI.create("info:lccn/22/eng//004.678"), "lccn"), is(true));
	}

	/** Tests whether {@link URIs#isInfoNamespace(URI, String)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testIsInfoNamespaceTestNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.isInfoNamespace(null, "ddc"));
	}

	/** Tests whether {@link URIs#isInfoNamespace(URI, String)} is throwing an exception when a null scheme is provided. */
	@Test
	@Disabled //TODO The method throws an IllegalArgumentException instead of NullPointerException.
	public void testIsInfoNamespaceTestNullSchemeFail() {
		assertThrows(NullPointerException.class, () -> URIs.isInfoNamespace(URI.create("info:ddc/22/eng//004.678"), null));
	}

	/** Tests whether {@link URIs#getPathRawPath(URI)} is working properly. */
	@Test
	public void testGetPathRawPath() {
		assertThat(URIs.getPathRawPath(URI.create("path:foo/")), is("foo/"));
		assertThat(URIs.getPathRawPath(URI.create("path:foo/bar")), is("foo/bar"));
	}

	/** Tests whether {@link URIs#getPathRawPath(URI)} is throwing an exception when a non-path scheme is provided. */
	@Test
	public void testGetPathRawPathWrongSchemeFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.getPathRawPath(URI.create("http://example.com")));
	}

	/** Tests whether {@link URIs#getPathURIPath(URI)} is working properly. */
	@Test
	public void testGetPathURIPath() {
		assertThat(URIs.getPathURIPath(URI.create("path:foo/")), is(URIPath.asPathURIPath(URI.create("path:foo/"))));
		assertThat(URIs.getPathURIPath(URI.create("path:foo/bar")), is(URIPath.asPathURIPath(URI.create("path:foo/bar"))));
	}

	/** Tests whether {@link URIs#getPathURIPath(URI)} is throwing an exception when a non-path scheme is provided. */
	@Test
	public void testGetPathURIPathWrongSchemeFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.getPathURIPath(URI.create("http://example.com")));
	}

	/** Tests whether {@link URIs#changePath(URI, URIPath)} is working properly. */
	@Test
	public void testChangePath() {
		assertThat(URIs.changePath(URI.create("http://example.com/foo/bar"), URIPath.asPathURIPath(URI.create("path:/foobar"))),
				is(URI.create("http://example.com/foobar")));
		assertThat(URIs.changePath(URI.create("http://example.com/foo/bar"), URIPath.asPathURIPath(URI.create("path:/"))), is(URI.create("http://example.com/")));
		assertThat(URIs.changePath(URI.create("http://example.com/"), URIPath.asPathURIPath(URI.create("path:/foobar"))),
				is(URI.create("http://example.com/foobar")));
		assertThat(URIs.changePath(URI.create("http://example.com/foo/bar"), null), is(URI.create("http://example.com")));
	}

	/** Tests whether {@link URIs#changePath(URI, URIPath)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testChangePathNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.changePath(null, URIPath.asPathURIPath(URI.create("path:foobar/"))));
	}

	/** Tests whether {@link URIs#changeRawPath(URI, String)} is working properly. */
	@Test
	public void testChangeRawPath() {
		assertThat(URIs.changeRawPath(URI.create("http://example.com/foo/bar"), "/foobar"), is(URI.create("http://example.com/foobar")));
		assertThat(URIs.changeRawPath(URI.create("http://example.com/foo/bar"), "/"), is(URI.create("http://example.com/")));
		assertThat(URIs.changeRawPath(URI.create("http://example.com/foo/bar"), ""), is(URI.create("http://example.com")));
		assertThat(URIs.changeRawPath(URI.create("http://example.com/foo/bar"), null), is(URI.create("http://example.com")));
		assertThat(URIs.changeRawPath(URI.create("http://example.com/"), "/foobar"), is(URI.create("http://example.com/foobar")));
	}

	/** Tests whether {@link URIs#changeRawPath(URI, String)} is throwing an exception when a null path is provided. */
	@Test
	public void testChangeRawPathNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeRawPath(null, "/foobar"));
	}

	/** Tests whether {@link URIs#changeHost(URI, String)} is working properly. */
	@Test
	public void testChangeHost() {
		assertThat(URIs.changeHost(URI.create("http://example.com"), "globalmentor.com"), is(URI.create("http://globalmentor.com")));
		assertThat(URIs.changeHost(URI.create("http://example.com/"), "globalmentor.com"), is(URI.create("http://globalmentor.com/")));
		assertThat(URIs.changeHost(URI.create("http://example.com/foo"), "globalmentor.com"), is(URI.create("http://globalmentor.com/foo")));
	}

	/** Tests whether {@link URIs#changeHost(URI, String)} is throwing an exception when a null host is provided. */
	@Test
	public void testChangeHostNullHostFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeHost(URI.create("http://example.com/foo/bar"), null));
	}

	/** Tests whether {@link URIs#changeRawSchemeSpecificPart(URI, String)} is working properly. */
	@Test
	public void testChangeRawSchemeSpecificPart() {
		assertThat(URIs.changeRawSchemeSpecificPart(URI.create("http://example.com"), "//globalmentor.io"), is(URI.create("http://globalmentor.io")));
		assertThat(URIs.changeRawSchemeSpecificPart(URI.create("http://example.com/"), "//globalmentor.io/"), is(URI.create("http://globalmentor.io/")));
		assertThat(URIs.changeRawSchemeSpecificPart(URI.create("http://example.com/foo"), "//globalmentor.io/bar"), is(URI.create("http://globalmentor.io/bar")));
	}

	/** Tests whether {@link URIs#changeRawSchemeSpecificPart(URI, String)} is throwing an exception when a null scheme-specific part is provided. */
	@Test
	public void testChangeRawSchemeSpecificPartNullSchemeFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeRawSchemeSpecificPart(URI.create("http://example.com"), null));
	}

	/** Tests whether {@link URIs#changeRawSchemeSpecificPart(URI, String)} is throwing an exception when a null uri is provided. */
	@Test
	public void testChangeRawSchemeSpecificPartNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeRawSchemeSpecificPart(null, "http"));
	}

	/** Tests whether {@link URIs#changeRawSchemeSpecificPart(URI, String)} is throwing an exception when a null uri and scheme-specific part is provided. */
	@Test
	public void testChangeRawSchemeSpecificPartNullSchemeAndURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeRawSchemeSpecificPart(null, null));
	}

	/** Tests whether {@link URIs#toCollectionURI(URI)} is working properly. */
	@Test
	public void testToCollectionURI() {
		assertThat(URIs.toCollectionURI(URI.create("http://example.com")), is(URI.create("http://example.com/")));
		assertThat(URIs.toCollectionURI(URI.create("http://example.com/")), is(URI.create("http://example.com/")));
		assertThat(URIs.toCollectionURI(URI.create("http://example.com/foo")), is(URI.create("http://example.com/foo/")));
		assertThat(URIs.toCollectionURI(URI.create("http://example.com/foo/")), is(URI.create("http://example.com/foo/")));
	}

	/** Tests whether {@link URIs#toCollectionURI(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testToCollectionNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.toCollectionURI(null));
	}

	/** Tests whether {@link URIs#getPath(URI)} is working properly. */
	@Test
	public void testGetPath() {
		assertThat(URIs.getPath(URI.create("http://example.com")), is(URIPath.EMPTY_URI_PATH));
		assertThat(URIs.getPath(URI.create("http://example.com/")), is(URIPath.asPathURIPath(URI.create("path:/"))));
		assertThat(URIs.getPath(URI.create("http://example.com/foo")), is(URIPath.asPathURIPath(URI.create("path:/foo"))));
		assertThat(URIs.getPath(URI.create("http://example.com/foo/")), is(URIPath.asPathURIPath(URI.create("path:/foo/"))));
	}

	/** Tests whether {@link URIs#getPath(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testGetPathNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.getPath(null));
	}

	/** Tests whether {@link URIs#getRawName(URI)} is working properly. */
	@Test
	public void testGetRawName() {
		assertThat(URIs.getRawName(URI.create("http://example.com")), is(""));
		assertThat(URIs.getRawName(URI.create("http://example.com/%2A")), is("%2A"));
		assertThat(URIs.getRawName(URI.create("http://example.com/foo%2A")), is("foo%2A"));
		assertThat(URIs.getRawName(URI.create("http://example.com/foo%2A/")), is("foo%2A"));
		assertThat(URIs.getRawName(URI.create("http://example.com/foo/bar/foobar.txt")), is("foobar.txt"));
		assertThat(URIs.getRawName(URI.create("http://example.com/foo/bar/foo%2Abar.txt")), is("foo%2Abar.txt"));

		assertThat(URIs.getRawName(URI.create("info:ddc/22/eng//004.678")), is("004.678"));
	}

	/** Tests whether {@link URIs#getRawName(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testGetRawNameNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.getRawName(null));
	}

	/** Tests whether {@link URIs#getName(URI)} is working properly. */
	@Test
	public void testGetName() {
		assertThat(URIs.getName(URI.create("http://example.com")), is(""));
		assertThat(URIs.getName(URI.create("http://example.com/%2A")), is("*"));
		assertThat(URIs.getName(URI.create("http://example.com/foo%2A")), is("foo*"));
		assertThat(URIs.getName(URI.create("http://example.com/foo%2A/")), is("foo*"));
		assertThat(URIs.getName(URI.create("http://example.com/foo/bar/foo%2Abar.txt")), is("foo*bar.txt"));

		assertThat(URIs.getName(URI.create("info:ddc/22/eng//004.678")), is("004.678"));
	}

	/** Tests whether {@link URIs#getName(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testGetNameNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.getName((URI)null));
	}

	/** Tests whether {@link URIs#changeName(String, String)} is working properly. */
	@Test
	public void testChangeName() {
		assertThat(URIs.changeName(URI.create("http://example.com/foo"), "foo*bar.txt"), is(URI.create("http://example.com/foo*bar.txt")));
		assertThat(URIs.changeName(URI.create("http://example.com/foo/"), "foo*bar.txt"), is(URI.create("http://example.com/foo*bar.txt/")));

		assertThat(URIs.changeName(URI.create("http://example.com/foo/bar/foobar.txt"), "foo*.txt"), is(URI.create("http://example.com/foo/bar/foo*.txt")));
		assertThat(URIs.changeName(URI.create("http://example.com/foo/bar/foobar.txt"), "*"), is(URI.create("http://example.com/foo/bar/*")));

		assertThat(URIs.changeName(URI.create("info:ddc/22/eng//004.678"), "foo*bar.txt"), is(URI.create("info:ddc/22/eng//foo*bar.txt")));
	}

	/** Tests whether {@link URIs#changeName(String, String)} is throwing an exception when an empty name for the {@link URI} is provided. */
	@Test
	public void testChangeNameEmptyURINameFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.changeName(URI.create("http://example.com"), "foo*bar.txt"));
	}

	/** Tests whether {@link URIs#changeName(String, String)} is throwing an exception when a "/" name for the {@link URI} is provided. */
	@Test
	public void testChangeNameSlashURINameFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.changeName(URI.create("http://example.com/"), "foo*bar.txt"));
	}

	/** Tests whether {@link URIs#changeName(String, String)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testChangeNameNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeName((URI)null, ""));
	}

	/** Tests whether {@link URIs#changeName(String, String)} is throwing an exception when a null name is provided. */
	@Test
	public void testChangeNameNullNameFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeName(URI.create("http://example.com/"), null));
	}

	/** Tests whether {@link URIs#changeRawName(String, String)} is working properly. */
	@Test
	public void testChangeRawName() {
		assertThat(URIs.changeRawName(URI.create("http://example.com/foo"), "foo%2Abar.txt"), is(URI.create("http://example.com/foo%2Abar.txt")));
		assertThat(URIs.changeRawName(URI.create("http://example.com/foo/"), "foo%2Abar.txt"), is(URI.create("http://example.com/foo%2Abar.txt/")));

		assertThat(URIs.changeRawName(URI.create("http://example.com/foo/bar/foobar.txt"), "foo%2A.txt"), is(URI.create("http://example.com/foo/bar/foo%2A.txt")));
		assertThat(URIs.changeRawName(URI.create("http://example.com/foo/bar/foobar.txt"), "%2A"), is(URI.create("http://example.com/foo/bar/%2A")));

		assertThat(URIs.changeRawName(URI.create("info:ddc/22/eng//004.678"), "foo%2Abar.txt"), is(URI.create("info:ddc/22/eng//foo%2Abar.txt")));
	}

	/** Tests whether {@link URIs#changeRawName(String, String)} is throwing an exception when an empty {@link URI} name is provided. */
	@Test
	public void testChangeRawNameEmptyURINameFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.changeRawName(URI.create("http://example.com"), "foo%2Abar.txt"));
	}

	/** Tests whether {@link URIs#changeRawName(String, String)} is throwing an exception when a "/" {@link URI} name is provided. */
	@Test
	public void testChangeRawNameSlashURINameFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.changeRawName(URI.create("http://example.com/"), "foo%2Abar.txt"));
	}

	/** Tests whether {@link URIs#changeRawName(String, String)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testChangeRawNameNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeRawName((URI)null, ""));
	}

	/** Tests whether {@link URIs#changeRawName(String, String)} is throwing an exception when a null name is provided. */
	@Test
	public void testChangeRawNameNullNameFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeRawName(URI.create("http://example.com/foo"), null));
	}

	/** Tests whether {@link URIs#addRawNameExtension(String, String)} is working properly. */
	@Test
	public void testAddRawNameExtension() {
		assertThat(URIs.addRawNameExtension(URI.create("http://example.com/foobar"), "txt"), is(URI.create("http://example.com/foobar.txt")));

		assertThat(URIs.addRawNameExtension(URI.create("http://example.com/.foobar"), ""), is(URI.create("http://example.com/.foobar.")));

		assertThat(URIs.addRawNameExtension(URI.create("http://example.com/foo.txt"), "bar"), is(URI.create("http://example.com/foo.txt.bar")));
	}

	/** Tests whether {@link URIs#addRawNameExtension(String, String)} is throwing an exception when a "/" name is provided. */
	@Test
	public void testAddRawNameExtensionSlashName() {
		assertThrows(IllegalArgumentException.class, () -> URIs.addRawNameExtension(URI.create("http://example.com/"), ""));
	}

	/** Tests whether {@link URIs#addRawNameExtension(String, String)} is throwing an exception when an empty name is provided. */
	@Test
	public void testAddRawNameExtensionEmptyName() {
		assertThrows(IllegalArgumentException.class, () -> URIs.addRawNameExtension(URI.create("http://example.com"), "foobar"));
	}

	/** Tests whether {@link URIs#addRawNameExtension(String, String)} is throwing an exception when a null name is provided. */
	@Test
	public void testAddRawNameExtensionNullNameFail() {
		assertThrows(NullPointerException.class, () -> URIs.addRawNameExtension(null, "txt"));
	}

	/** Tests whether {@link URIs#addRawNameExtension(String, String)} is throwing an exception when a null extension is provided. */
	@Test
	public void testAddRawNameExtensionNullExtensionFail() {
		assertThrows(NullPointerException.class, () -> URIs.addRawNameExtension(URI.create("http://example.com"), null));
	}

	/** Tests whether {@link URIs#getNameExtension(URI)} is working properly. */
	@Test
	public void testGetNameExtensionURI() {
		assertThat(URIs.getNameExtension(URI.create("http://example.com/foobar.txt")), is("txt"));
		assertThat(URIs.getNameExtension(URI.create("http://example.com/foobar")), is((String)null));
		assertThat(URIs.getNameExtension(URI.create("http://example.com/")), is((String)null));

		assertThat(URIs.getNameExtension(URI.create("http://example.com/.foobar")), is("foobar"));
		assertThat(URIs.getNameExtension(URI.create("http://example.com/.")), is(""));
		assertThat(URIs.getNameExtension(URI.create("http://example.com/")), is((String)null));

		assertThat(URIs.getNameExtension(URI.create("http://example.com")), is((String)null));
	}

	/** Tests whether {@link URIs#getNameExtension(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testGetNameExtensionNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.getNameExtension((URI)null));
	}

	/** Tests whether {@link URIs#getNameExtension(URI)} is working properly. */
	@Test
	public void testGetNameExtensionString() {
		assertThat(URIs.getNameExtension(URI.create("http://example.com/foobar.%2Axml")), is("*xml"));
		assertThat(URIs.getNameExtension(URI.create("http://example.com/foobar.*xml")), is("*xml"));
	}

	/** Tests whether {@link URIs#getRawNameExtension(URI, String)} is working properly. */
	@Test
	public void testGetRawNameExtensionString() {
		assertThat(URIs.getRawNameExtension(URI.create("http://example.com/foobar.xml")), is("xml"));
		assertThat(URIs.getRawNameExtension(URI.create("http://example.com/foobar")), is((String)null));
		assertThat(URIs.getRawNameExtension(URI.create("http://example.com/")), is((String)null));
		assertThat(URIs.getRawNameExtension(URI.create("http://example.com")), is((String)null));

		assertThat(URIs.getRawNameExtension(URI.create("http://example.com/foobar.%2Axml")), is("%2Axml"));
		assertThat(URIs.getRawNameExtension(URI.create("http://example.com/foobar.*xml")), is("*xml"));
	}

	/** Tests whether {@link URIs#changeRawNameExtension(URI, String)} is working properly. */
	@Test
	public void testChangeRawNameExtensionString() {
		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar.xml"), "json"), is(URI.create("http://example.com/foobar.json")));
		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar"), "xml"), is(URI.create("http://example.com/foobar.xml")));

		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar.xml"), ".json"), is(URI.create("http://example.com/foobar..json")));
		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar.xml"), ""), is(URI.create("http://example.com/foobar.")));

		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar.xml"), null), is(URI.create("http://example.com/foobar")));
		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar"), null), is(URI.create("http://example.com/foobar")));
	}

	/** Tests whether {@link URIs#changeRawNameExtension(URI, String)} is working properly. */
	@Test
	public void testChangeRawNameExtensionStringSlashName() {
		assertThrows(IllegalArgumentException.class, () -> URIs.changeRawNameExtension(URI.create("http://example.com/"), "foobar"));
	}

	/** Tests whether {@link URIs#changeRawNameExtension(URI, String)} is working properly. */
	@Test
	public void testChangeRawNameExtensionStringEmptyName() {
		assertThrows(IllegalArgumentException.class, () -> URIs.changeRawNameExtension(URI.create("http://example.com"), "foobar"));
	}

	/** Tests whether {@link URIs#changeRawNameExtension(URI, String)} is throwing an exception when a null name is provided. */
	@Test
	public void testChangeRawNameExtensionNullExtensionFail() {
		assertThrows(NullPointerException.class, () -> URIs.changeRawNameExtension((URI)null, ""));
	}

	/** Tests whether {@link URIs#getPlainURI(URI)} is working properly. */
	@Test
	public void testGetPlainURI() {
		assertThat(URIs.getPlainURI(URI.create("http://example.com/foobar.xml")), is(URI.create("http://example.com/foobar.xml")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com/foobar")), is(URI.create("http://example.com/foobar")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com/")), is(URI.create("http://example.com/")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com")), is(URI.create("http://example.com")));

		assertThat(URIs.getPlainURI(URI.create("http://example.com/?type=foo")), is(URI.create("http://example.com/")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com/?type=foo&place=bar")), is(URI.create("http://example.com/")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com/#bar")), is(URI.create("http://example.com/")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com/?type=foo#bar")), is(URI.create("http://example.com/")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com/?type=foo&place=bar#bar")), is(URI.create("http://example.com/")));

		assertThat(URIs.getPlainURI(URI.create("http://example.com/foobar?type=foo")), is(URI.create("http://example.com/foobar")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com/foobar?type=foo&place=bar")), is(URI.create("http://example.com/foobar")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com/foobar#bar")), is(URI.create("http://example.com/foobar")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com/foobar?type=foo#bar")), is(URI.create("http://example.com/foobar")));
		assertThat(URIs.getPlainURI(URI.create("http://example.com/foobar?type=foo&place=bar#bar")), is(URI.create("http://example.com/foobar")));
	}

	/** Tests whether {@link URIs#getPlainURI(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testGetPlainURINullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.getPlainURI((URI)null));
	}

	/** Tests whether {@link URIs#constructQuery(URIQueryParameter...)} is working properly. */
	@Test
	public void testConstructQueryParameters() {
		assertThat(URIs.constructQuery(), is("")); // TODO according to the javadocs, it should return an empty String

		assertThat(URIs.constructQuery(new URIQueryParameter("type", "foo")), is("?type=foo"));
		assertThat(URIs.constructQuery(new URIQueryParameter("type", "foo"), new URIQueryParameter("place", "bar")), is("?type=foo&place=bar"));
	}

	/** Tests whether {@link URIs#constructQuery(URIQueryParameter...)} is working properly. */
	@Test
	public void testConstructQueryParametersNullQueryParameterFail() {
		assertThrows(NullPointerException.class, () -> URIs.constructQuery((URIQueryParameter)null));
	}

	/** Tests whether {@link URIs#constructQuery(URIQueryParameter...)} is working properly. */
	@Test
	public void testConstructQuery() {
		assertThat(URIs.constructQuery(new URIQueryParameter("", "")), is("?="));
		assertThat(URIs.constructQuery(new URIQueryParameter("foo", "")), is("?foo="));
		assertThat(URIs.constructQuery(new URIQueryParameter("", "bar")), is("?=bar"));
		assertThat(URIs.constructQuery(new URIQueryParameter("foo", "bar")), is("?foo=bar"));
		assertThat(URIs.constructQuery(new URIQueryParameter("foo", null)), is("?foo"));
	}

	/** Tests whether {@link URIs#constructQuery(URIQueryParameter...)} is throwing an exception when a null {@link URIQueryParameter} is provided. */
	@Test
	public void testConstructQueryNullParameters() {
		assertThrows(NullPointerException.class, () -> URIs.constructQuery((URIQueryParameter)null));
	}

	/** Tests whether {@link URIs#constructQuery(String)} is working properly. */
	@Test
	public void testConstructQueryString() {
		assertThat(URIs.constructQuery("="), is("?="));
		assertThat(URIs.constructQuery("foo="), is("?foo="));
		assertThat(URIs.constructQuery("=bar"), is("?=bar"));
		assertThat(URIs.constructQuery("foo=bar"), is("?foo=bar"));
		assertThat(URIs.constructQuery("foo"), is("?foo"));
	}

	/** Tests whether {@link URIs#constructQuery(String)} is throwing an exception when a null {@link URIQueryParameter} is provided. */
	@Test
	public void testConstructQueryStringNullParameters() {
		assertThrows(NullPointerException.class, () -> URIs.constructQuery((String)null));
	}

	/** Tests whether {@link URIs#appendRawQuery(URI, String)} is working properly. */
	@Test
	public void testAppendRawQuery() {
		assertThat(URIs.appendRawQuery(URI.create("http://example.com/"), ""), is(URI.create("http://example.com/?")));
		assertThat(URIs.appendRawQuery(URI.create("http://example.com/"), "type=foo"), is(URI.create("http://example.com/?type=foo")));
		assertThat(URIs.appendRawQuery(URI.create("http://example.com/"), "type=foo&place=bar"), is(URI.create("http://example.com/?type=foo&place=bar")));
	}

	/** Tests whether {@link URIs#appendRawQuery(URI, String)} is throwing an exception when a null query parameter is provided. */
	@Test
	public void testAppendRawQueryNullQueryFail() {
		assertThrows(NullPointerException.class, () -> URIs.appendRawQuery(URI.create("http://example.com/"), null));
	}

	/** Tests whether {@link URIs#appendRawQuery(URI, String)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testAppendRawQueryNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.appendRawQuery(null, "type=foo"));
	}

	/** Tests whether {@link URIs#appendQueryParameter(URI, String, String)} is working properly. */
	@Test
	public void testAppendQueryParameter() {
		assertThat(URIs.appendQueryParameter(URI.create("http://example.com/"), "", ""), is(URI.create("http://example.com/?=")));
		assertThat(URIs.appendQueryParameter(URI.create("http://example.com/"), "", null), is(URI.create("http://example.com/?")));
		assertThat(URIs.appendQueryParameter(URI.create("http://example.com/"), "type", "foo"), is(URI.create("http://example.com/?type=foo")));
		assertThat(URIs.appendQueryParameter(URI.create("http://example.com/?type=foo"), "place", "bar"), is(URI.create("http://example.com/?type=foo&place=bar")));
	}

	/** Tests whether {@link URIs#appendQueryParameter(URI, String, String)} is throwing an exception when a null query name is provided. */
	@Test
	public void testAppendQueryParameterNullQueryNameFail() {
		assertThrows(NullPointerException.class, () -> URIs.appendQueryParameter(URI.create("http://example.com/"), null, ""));
	}

	/** Tests whether {@link URIs#appendQueryParameter(URI, String, String)} is throwing an exception when a null query name and value is provided. */
	@Test
	public void testAppendQueryParameterNullQueryNameAndValueFail() {
		assertThrows(NullPointerException.class, () -> URIs.appendQueryParameter(URI.create("http://example.com/"), null, null));
	}

	/** Tests whether {@link URIs#appendQueryParameter(URI, String, String)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testAppendQueryParameterNullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.appendQueryParameter(null, "type", "foo"));
	}

	/** Tests whether {@link URIs#getParameterMap(URI)} is working properly. */
	@Test
	public void testGetParameterMap() {
		assertThat(URIs.getParameterMap(URI.create("http://example.com")).isEmpty(), is(true));
		assertThat(URIs.getParameterMap(URI.create("http://example.com/")).isEmpty(), is(true));

		CollectionMap<String, String, List<String>> queryCollectionMap;

		queryCollectionMap = URIs.getParameterMap(URI.create("http://example.com/?type=foo"));
		assertThat(queryCollectionMap.size(), is(1));
		assertThat(queryCollectionMap.get("type"), is(Arrays.asList("foo")));

		queryCollectionMap = URIs.getParameterMap(URI.create("http://example.com/?type=foo&place=bar"));
		assertThat(queryCollectionMap.size(), is(2));
		assertThat(queryCollectionMap.get("type"), is(Arrays.asList("foo")));
		assertThat(queryCollectionMap.get("place"), is(Arrays.asList("bar")));

		queryCollectionMap = URIs.getParameterMap(URI.create("http://example.com/?type=foo&place=bar&type=foobar"));
		assertThat(queryCollectionMap.size(), is(2));
		assertThat(queryCollectionMap.get("type"), is(Arrays.asList("foo", "foobar")));
		assertThat(queryCollectionMap.get("place"), is(Arrays.asList("bar")));
	}

	/** Tests whether {@link URIs#getParameterMap(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testGetParameterMapNullFail() {
		assertThrows(NullPointerException.class, () -> URIs.getParameterMap(null));
	}

	/** Tests whether {@link URIs#getParameters(URI)} is working properly. */
	@Test
	public void testGetParameters() {
		assertThat(URIs.getParameters(URI.create("http://example.com")), is(nullValue()));
		assertThat(URIs.getParameters(URI.create("http://example.com/")), is(nullValue()));

		NameValuePair<String, String>[] parametersNameValuePairs;

		parametersNameValuePairs = URIs.getParameters(URI.create("http://example.com/?type=foo"));
		assertThat(parametersNameValuePairs.length, is(1));
		assertThat(parametersNameValuePairs[0].getName(), is("type"));
		assertThat(parametersNameValuePairs[0].getValue(), is("foo"));

		parametersNameValuePairs = URIs.getParameters(URI.create("http://example.com/?type=foo&place=bar"));
		assertThat(parametersNameValuePairs.length, is(2));
		assertThat(parametersNameValuePairs[0].getName(), is("type"));
		assertThat(parametersNameValuePairs[0].getValue(), is("foo"));
		assertThat(parametersNameValuePairs[1].getName(), is("place"));
		assertThat(parametersNameValuePairs[1].getValue(), is("bar"));

		parametersNameValuePairs = URIs.getParameters(URI.create("http://example.com/?type=foo&place=bar&type=foobar"));
		assertThat(parametersNameValuePairs.length, is(3));
		assertThat(parametersNameValuePairs[0].getName(), is("type"));
		assertThat(parametersNameValuePairs[0].getValue(), is("foo"));
		assertThat(parametersNameValuePairs[1].getName(), is("place"));
		assertThat(parametersNameValuePairs[1].getValue(), is("bar"));
		assertThat(parametersNameValuePairs[2].getName(), is("type"));
		assertThat(parametersNameValuePairs[2].getValue(), is("foobar"));
	}

	/** Tests whether {@link URIs#createPathURI(String)} is working properly. */
	@Test
	public void testCreatePathURI() {
		assertThat(URIs.createPathURI(""), is(URI.create("")));
		assertThat(URIs.createPathURI("foobar"), is(URI.create("foobar")));

		assertThat(URIs.createPathURI("/"), is(URI.create("/")));
		assertThat(URIs.createPathURI("/foo"), is(URI.create("/foo")));
		assertThat(URIs.createPathURI("/foo/bar"), is(URI.create("/foo/bar")));
		assertThat(URIs.createPathURI("foobar"), is(URI.create("foobar")));
	}

	/** Tests whether {@link URIs#createPathURI(String)} throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testCreatePathURINullFail() {
		assertThrows(NullPointerException.class, () -> URIs.createPathURI(null));
	}

	/** Tests whether {@link URIs#createPathURI(String)} throwing an exception when a scheme is provided. */
	@Test
	public void testCreatePathURISchemeProvidedFail() {
		assertThrows(IllegalArgumentException.class, () -> URIs.createPathURI("http://foobar"));
	}

	/** Tests whether {@link URIs#checkRoot(URI)} is working properly. */
	@Test
	public void testCheckRoot() {
		assertThat(URIs.checkRoot(URI.create("http://example.com/")), is(URI.create("http://example.com/")));
	}

	/** Tests whether {@link URIs#checkRoot(URI)} is throwing an exception when a {@link URI} without the root path is provided. */
	@Test
	public void testCheckRootNotRootPath() {
		assertThrows(IllegalArgumentException.class,
				() -> assertThat(URIs.checkRoot(URI.create("http://example.com/foo")), is(URI.create("http://example.com/foo"))));
	}

	/** Tests whether {@link URIs#checkRoot(URI)} is throwing an exception when a {@link URI} without the root path is provided. */
	@Test
	public void testCheckRootNotRootPath2() {
		assertThrows(IllegalArgumentException.class, () -> assertThat(URIs.checkRoot(URI.create("http://example.com")), is(URI.create("http://example.com/foo"))));
	}

	/** Tests whether {@link URIs#checkRoot(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testCheckRootNullUri() {
		assertThrows(NullPointerException.class, () -> URIs.checkRoot(null));
	}

	/** Tests whether {@link URIs#checkAbsolute(URI)} is working properly. */
	@Test
	public void testCheckAbsolute() {
		assertThat(URIs.checkAbsolute(URI.create("http://example.com")), is(URI.create("http://example.com")));
		assertThat(URIs.checkAbsolute(URI.create("http://example.com/")), is(URI.create("http://example.com/")));
		assertThat(URIs.checkAbsolute(URI.create("http://example.com/foo")), is(URI.create("http://example.com/foo")));
	}

	/** Tests whether {@link URIs#checkAbsolute(URI)} is throwing an exception when a {@link URI} without schema is provided. */
	@Test
	public void testCheckAbsoluteWithoutSchema() {
		assertThrows(IllegalArgumentException.class, () -> assertThat(URIs.checkRoot(URI.create("example.com")), is(URI.create("http://example.com/foo"))));
	}

	/** Tests whether {@link URIs#checkAbsolute(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testCheckAbsoluteNullUri() {
		assertThrows(NullPointerException.class, () -> URIs.checkAbsolute(null));
	}

	/** Tests whether {@link URIs#isPathURI(URI)} is working properly. */
	@Test
	public void testIsPathURI() {
		assertThat(URIs.isPathURI(URI.create("")), is(true));
		assertThat(URIs.isPathURI(URI.create("foobar")), is(true));

		assertThat(URIs.isPathURI(URI.create("/")), is(true));
		assertThat(URIs.isPathURI(URI.create("/foo")), is(true));
		assertThat(URIs.isPathURI(URI.create("/foo/bar")), is(true));

		assertThat(URIs.isPathURI(URI.create("path:/foobar")), is(false));
		assertThat(URIs.isPathURI(URI.create("/foo?type=foo")), is(false));
		assertThat(URIs.isPathURI(URI.create("/foo#bar")), is(false));

		assertThat(URIs.isPathURI(URI.create("/foo?type=foo&place=bar")), is(false));
		assertThat(URIs.isPathURI(URI.create("/foo?type=foo#bar")), is(false));
		assertThat(URIs.isPathURI(URI.create("/foo?type=foo&place=bar#bar")), is(false));

		assertThat(URIs.isPathURI(URI.create("foobar.com")), is(true));
		assertThat(URIs.isPathURI(URI.create("foobar.com:80")), is(false));

		assertThat(URIs.isPathURI(URI.create("/foobar.com")), is(true));
		assertThat(URIs.isPathURI(URI.create("/foobar.com:80")), is(true));
	}

	/** Tests whether {@link URIs#isPathURI(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testIsPathURINullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.isPathURI(null));
	}

	/** Tests whether {@link URIs#getPlainURI(URI)} is working properly. */
	@Test
	public void testIsPlainURI() {
		assertThat(URIs.isPlainURI(URI.create("http://example.com/foobar.xml")), is(true));
		assertThat(URIs.isPlainURI(URI.create("http://example.com/foobar")), is(true));
		assertThat(URIs.isPlainURI(URI.create("http://example.com/")), is(true));
		assertThat(URIs.isPlainURI(URI.create("http://example.com")), is(true));

		assertThat(URIs.isPlainURI(URI.create("http://example.com/?type=foo")), is(false));
		assertThat(URIs.isPlainURI(URI.create("http://example.com/?type=foo&place=bar")), is(false));
		assertThat(URIs.isPlainURI(URI.create("http://example.com/#bar")), is(false));
		assertThat(URIs.isPlainURI(URI.create("http://example.com/?type=foo#bar")), is(false));
		assertThat(URIs.isPlainURI(URI.create("http://example.com/?type=foo&place=bar#bar")), is(false));

		assertThat(URIs.isPlainURI(URI.create("http://example.com/foobar?type=foo")), is(false));
		assertThat(URIs.isPlainURI(URI.create("http://example.com/foobar?type=foo&place=bar")), is(false));
		assertThat(URIs.isPlainURI(URI.create("http://example.com/foobar#bar")), is(false));
		assertThat(URIs.isPlainURI(URI.create("http://example.com/foobar?type=foo#bar")), is(false));
		assertThat(URIs.isPlainURI(URI.create("http://example.com/foobar?type=foo&place=bar#bar")), is(false));
	}

	/** Tests whether {@link URIs#isPlainURI(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test
	public void testIsPlainURINullURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.isPlainURI((URI)null));
	}

	/** @see URIs#findRelativeChildPath(URI, URI) */
	@Test
	public void testFindRelativeChildPath() {
		//collection relativized against itself
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/"), URI.create("http://example.com/")), isPresentAndIs(URI.create("")));
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/"), URI.create("http://example.com/foo/")), isPresentAndIs(URI.create("")));
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/bar/")),
				isPresentAndIs(URI.create("")));

		//non-collection relativized against itself
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/test.txt"), URI.create("http://example.com/foo/bar/test.txt")), isEmpty());

		//same-level resolution
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo"), URI.create("http://example.com/bar")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/other")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo.txt"), URI.create("http://example.com/bar.txt")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/test.txt"), URI.create("http://example.com/foo/other.txt")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/test.txt"), URI.create("http://example.com/foo/bar/other.txt")), isEmpty());

		//child of collection
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/bar/test.txt")),
				isPresentAndIs(URI.create("test.txt")));
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/bar/test")),
				isPresentAndIs(URI.create("test")));
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/"), URI.create("http://example.com/foo/bar/test.txt")),
				isPresentAndIs(URI.create("bar/test.txt")));
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/"), URI.create("http://example.com/foo/bar/test.txt")),
				isPresentAndIs(URI.create("foo/bar/test.txt")));

		//child of non-collection

		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/bar/test.txt")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar.txt"), URI.create("http://example.com/foo/bar/test.txt")), isEmpty());

		//parent references; note that currently <foo/bar> and <foo/bar/> are distinguished regardless
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/test.txt"), URI.create("http://example.com/foo/")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/test.txt"), URI.create("http://example.com/foo/bar/")), isEmpty());

		//sibling references; note that currently <foo/bar> and <foo/bar/> are distinguished regardless
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/other/")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/other/")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/other")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/other")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/example/other/")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/example/other/")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/example/other")), isEmpty());
		assertThat(URIs.findRelativeChildPath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/example/other")), isEmpty());
	}

	/** @see URIs#findRelativePath(URI, URI) */
	@Test
	public void testFindRelativePath() {
		//collection relativized against itself
		assertThat(URIs.findRelativePath(URI.create("http://example.com/"), URI.create("http://example.com/")), isPresentAndIs(URI.create("")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/"), URI.create("http://example.com/foo/")), isPresentAndIs(URI.create("")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/bar/")), isPresentAndIs(URI.create("")));

		//non-collection relativized against itself
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo"), URI.create("http://example.com/foo")), isPresentAndIs(URI.create("foo")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/bar")), isPresentAndIs(URI.create("bar")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/test.txt"), URI.create("http://example.com/test.txt")),
				isPresentAndIs(URI.create("test.txt")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/test.txt"), URI.create("http://example.com/foo/test.txt")),
				isPresentAndIs(URI.create("test.txt")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/test.txt"), URI.create("http://example.com/foo/bar/test.txt")),
				isPresentAndIs(URI.create("test.txt")));

		//same-level resolution
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo"), URI.create("http://example.com/bar")), isPresentAndIs(URI.create("bar")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/other")),
				isPresentAndIs(URI.create("other")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo.txt"), URI.create("http://example.com/bar.txt")),
				isPresentAndIs(URI.create("bar.txt")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/test.txt"), URI.create("http://example.com/foo/other.txt")),
				isPresentAndIs(URI.create("other.txt")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/test.txt"), URI.create("http://example.com/foo/bar/other.txt")),
				isPresentAndIs(URI.create("other.txt")));

		//child of collection
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/bar/test.txt")),
				isPresentAndIs(URI.create("test.txt")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/bar/test")),
				isPresentAndIs(URI.create("test")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/"), URI.create("http://example.com/foo/bar/test.txt")),
				isPresentAndIs(URI.create("bar/test.txt")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/"), URI.create("http://example.com/foo/bar/test.txt")),
				isPresentAndIs(URI.create("foo/bar/test.txt")));

		//child of non-collection
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/bar/test.txt")),
				isPresentAndIs(URI.create("bar/test.txt")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar.txt"), URI.create("http://example.com/foo/bar/test.txt")),
				isPresentAndIs(URI.create("bar/test.txt")));

		//parent references; note that currently <foo/bar> and <foo/bar/> are distinguished regardless
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/")), isPresentAndIs(URI.create("./")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/")), isPresentAndIs(URI.create("../")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/test.txt"), URI.create("http://example.com/foo/")), isPresentAndIs(URI.create("./")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/test.txt"), URI.create("http://example.com/foo/bar/")),
				isPresentAndIs(URI.create("./")));

		//sibling references; note that currently <foo/bar> and <foo/bar/> are distinguished regardless
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/other/")),
				isPresentAndIs(URI.create("../other/")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/other/")),
				isPresentAndIs(URI.create("other/")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/foo/other")),
				isPresentAndIs(URI.create("../other")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/other")),
				isPresentAndIs(URI.create("other")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/example/other/")),
				isPresentAndIs(URI.create("../../example/other/")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/example/other/")),
				isPresentAndIs(URI.create("../example/other/")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar/"), URI.create("http://example.com/example/other")),
				isPresentAndIs(URI.create("../../example/other")));
		assertThat(URIs.findRelativePath(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/example/other")),
				isPresentAndIs(URI.create("../example/other")));
	}

	/** Tests whether {@link URIs#resolve(URI, URI)} is working properly. */
	@Test
	public void testResolve() {
		//tests resolving from-to empty URI
		assertThat(URIs.resolve(URI.create(""), URI.create("http://example.com/")), is(URI.create("http://example.com/")));
		assertThat(URIs.resolve(URI.create("http://example.com/"), URI.create("")), is(URI.create("http://example.com/")));

		//tests resolving simple URIs
		assertThat(URIs.resolve(URI.create("http://example.com/"), URI.create("http://example.com/")), is(URI.create("http://example.com/")));
		assertThat(URIs.resolve(URI.create("http://example.com/"), URI.create("http://example.com/foobar")), is(URI.create("http://example.com/foobar")));
		assertThat(URIs.resolve(URI.create("http://example.com/foobar"), URI.create("http://example.com/")), is(URI.create("http://example.com/")));

		//tests resolving URIs in two levels
		assertThat(URIs.resolve(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo/bar")), is(URI.create("http://example.com/foo/bar")));
		assertThat(URIs.resolve(URI.create("http://example.com/"), URI.create("http://example.com/foo/bar")), is(URI.create("http://example.com/foo/bar")));
		assertThat(URIs.resolve(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/")), is(URI.create("http://example.com/")));

		//tests resolving URIs in one level, but not directly from the root path
		assertThat(URIs.resolve(URI.create("http://example.com/foo"), URI.create("http://example.com/foo/bar")), is(URI.create("http://example.com/foo/bar")));
		assertThat(URIs.resolve(URI.create("http://example.com/foo/bar"), URI.create("http://example.com/foo")), is(URI.create("http://example.com/foo")));

		//tests resolving URIs with a query
		assertThat(URIs.resolve(URI.create("http://example.com/foobar?type=foo"), URI.create("http://example.com/foobar?type=foo")),
				is(URI.create("http://example.com/foobar?type=foo")));
		assertThat(URIs.resolve(URI.create("http://example.com/foobar"), URI.create("http://example.com/foobar?type=foo")),
				is(URI.create("http://example.com/foobar?type=foo")));
		assertThat(URIs.resolve(URI.create("http://example.com/foobar?type=foo"), URI.create("http://example.com/foobar")),
				is(URI.create("http://example.com/foobar")));

		//tests resolving URIs with multiple queries
		assertThat(URIs.resolve(URI.create("http://example.com/foobar?type=foo&place=bar"), URI.create("http://example.com/foobar?type=foo&place=bar")),
				is(URI.create("http://example.com/foobar?type=foo&place=bar")));
		assertThat(URIs.resolve(URI.create("http://example.com/foobar"), URI.create("http://example.com/foobar?type=foo&place=bar")),
				is(URI.create("http://example.com/foobar?type=foo&place=bar")));
		assertThat(URIs.resolve(URI.create("http://example.com/foobar?type=foo&place=bar"), URI.create("http://example.com/foobar")),
				is(URI.create("http://example.com/foobar")));

		assertThat(URIs.resolve(URI.create("http://example.com/foobar?type=foo&place=bar"), URI.create("http://example.com/foobar?type=foo")),
				is(URI.create("http://example.com/foobar?type=foo")));

		//tests resolving URIs with a fragment
		assertThat(URIs.resolve(URI.create("http://example.com/foobar#bar"), URI.create("http://example.com/foobar#bar")),
				is(URI.create("http://example.com/foobar#bar")));
		assertThat(URIs.resolve(URI.create("http://example.com/foobar#bar"), URI.create("http://example.com/foobar")), is(URI.create("http://example.com/foobar")));
		assertThat(URIs.resolve(URI.create("http://example.com/foobar"), URI.create("http://example.com/foobar#bar")),
				is(URI.create("http://example.com/foobar#bar")));

		//tests resolving URIs with multiple fragments
		assertThat(URIs.resolve(URI.create("http://example.com/foobar?type=foo&place=bar#bar"), URI.create("http://example.com/foobar?type=foo&place=bar#bar")),
				is(URI.create("http://example.com/foobar?type=foo&place=bar#bar")));
		assertThat(URIs.resolve(URI.create("http://example.com/foobar"), URI.create("http://example.com/foobar?type=foo&place=bar#bar")),
				is(URI.create("http://example.com/foobar?type=foo&place=bar#bar")));
		assertThat(URIs.resolve(URI.create("http://example.com/foobar?type=foo&place=bar#bar"), URI.create("http://example.com/foobar")),
				is(URI.create("http://example.com/foobar")));
	}

	/** Tests whether {@link URIs#resolve(URI, URI)} is throwing an exception when a null base {@link URI} is provided. */
	@Test
	public void testResolveNullBaseURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.resolve(null, URI.create("http://example.com/foobar")));
	}

	/** Tests whether {@link URIs#resolve(URI, URI)} is throwing an exception when a null child {@link URI} is provided. */
	@Test
	public void testResolveNullChildURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.resolve(URI.create("http://example.com/"), (URI)null));
	}

	/** Tests whether {@link URIs#resolve(URI, URI)} is throwing an exception when a null base and child {@link URI} are provided. */
	@Test
	public void testResolveNullBaseAndChildURIFail() {
		assertThrows(NullPointerException.class, () -> URIs.resolve(null, (URI)null));
	}

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

	/** Tests whether {@link URIs#getCurrentLevel(URI)} is throwing an exception when a null {@link URI} provided. */
	@Test
	public void testGetCurrentLevelTestNullFail() {
		assertThrows(NullPointerException.class, () -> URIs.getCurrentLevel(null));
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

	/** Tests whether {@link URIs#getParentLevel(URI)} is throwing an exception when a null {@link URI} provided. */
	@Test
	public void testGetParentLevelTestNullFail() {
		assertThrows(NullPointerException.class, () -> URIs.getParentLevel(null));
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

	/** Tests whether {@link URIs#getParentURI(URI)} is throwing an exception when a null {@link URI} provided. */
	@Test
	public void testGetParentURITestNullFail() {
		assertThrows(NullPointerException.class, () -> URIs.getParentURI(null));
	}

	/**
	 * Verifies that {@link URI#hashCode()} implementation is consistent with {@link URI#equals(Object)} which was not the case in versions of the JDK before Java
	 * 1.8.0_75, where it was fixed as part of <a href="https://bugs.openjdk.java.net/browse/JDK-7171415">JDK-7171415</a>.
	 * @see <a href="https://stackoverflow.com/q/16257996/421049">How to get recognition of Java URI hashCode() bug that has been inappropriately denied</a>
	 * @see <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=7054089">JDK-7054089</a>
	 * @see <a href="https://bugs.openjdk.java.net/browse/JDK-7171415">JDK-7171415</a>
	 */
	@Test
	public void testURIHashCode() {
		final URI uri1 = URI.create("http://www.example.com/foo%2Abar");
		final URI uri2 = URI.create("http://www.example.com/foo%2abar");
		assertThat("URIs are not equal.", uri1, equalTo(uri2));
		assertThat("Equal URIs do not have same hash code.", uri1.hashCode(), equalTo(uri2.hashCode()));
	}

	/** @see URIs#canonicalize(URI) */
	@Test
	public void testCanonicalize() {
		//TODO decide on whether IRIs should be allowed, and test
		assertThat(URIs.canonicalize(URI.create("http://example.com/")).toString(), is("http://example.com/")); //compare string versions, because equals() accepts both cases
		assertThat(URIs.canonicalize(URI.create("http://example.com/touch%C3%A9")).toString(), is("http://example.com/touch%C3%A9"));
		assertThat(URIs.canonicalize(URI.create("http://example.com/touch%C3%a9")).toString(), is("http://example.com/touch%C3%A9"));
		assertThat(URIs.canonicalize(URI.create("http://example.com/touch%c3%A9")).toString(), is("http://example.com/touch%C3%A9"));
		assertThat(URIs.canonicalize(URI.create("http://example.com/touch%c3%a9")).toString(), is("http://example.com/touch%C3%A9"));
	}

	/** @see URIs#encode(String) */
	@Test
	public void testEncode() {
		assertThat(URIs.encode("%"), is("%25"));
		assertThat(URIs.encode("$"), is("%24"));
		assertThat(URIs.encode("touché"), is("touch%C3%A9"));
		//TODO add higher code UTF-8 tests
		//TODO add URI vs segment tests when implemented
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
