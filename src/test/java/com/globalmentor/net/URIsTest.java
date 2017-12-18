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

import com.globalmentor.collections.CollectionMap;

/**
 * Various tests for URI utilities.
 * 
 * @author Garret Wilson
 */
public class URIsTest {

	/** Tests whether {@link URIs#createURIList(URI...)} is working properly. */
	@Test
	public void testCreateURIList() {
		// No URI given.
		assertThat(URIs.createURIList(), isEmptyString());

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
	@Test(expected = IllegalArgumentException.class)
	public void testCheckSchemeFail() {
		URIs.checkScheme(URI.create("http://example.com/"), "https");
	}

	/** Tests whether {@link URIs#checkScheme(URI, String)} is throwing an exception when a null {@link URI} provided. */
	@Test(expected = NullPointerException.class)
	public void testCheckSchemeNullURIFail() {
		URIs.checkInfoNamespace(null, "http");
	}

	/** Tests whether {@link URIs#checkScheme(URI, String)} is throwing an exception when a null scheme provided. */
	@Test(expected = NullPointerException.class)
	@Ignore // TODO The method throws an IllegalArgumentException instead of NullPointerException.
	public void testCheckSchemeNullSchemeFail() {
		URIs.checkInfoNamespace(URI.create("http://example.com/"), null);
	}

	/** Tests whether {@link URIs#changeScheme(URI, String)} is working properly. */
	@Test
	public void testChangeScheme() {
		assertThat(URIs.changeScheme(URI.create("http://example.com/"), "https"), is(URI.create("https://example.com/")));
		assertThat(URIs.changeScheme(URI.create("https://example.com/"), "http"), is(URI.create("http://example.com/")));
		assertThat(URIs.changeScheme(URI.create("https://example.com/"), "https"), is(URI.create("https://example.com/")));
	}

	/** Tests whether {@link URIs#changeScheme(URI, String)} is throwing an exception when an incompatible scheme is provided. */
	@Test(expected = IllegalArgumentException.class)
	public void testChangeSchemeFail() {
		URIs.changeScheme(URI.create("http://example.com/"), "http s");
	}

	/** Tests whether {@link URIs#changeScheme(URI, String)} is throwing an exception when a null {@link URI} provided. */
	@Test(expected = NullPointerException.class)
	public void testCheckChangeSchemeNullURIFail() {
		URIs.changeScheme(null, "http");
	}

	/** Tests whether {@link URIs#changeScheme(URI, String)} is throwing an exception when a null scheme provided. */
	@Test(expected = NullPointerException.class)
	public void testCheckChangeSchemeNullSchemeFail() {
		URIs.changeScheme(URI.create("http://example.com/"), null);
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is working properly. */
	@Test
	public void testCheckInfoNamespace() {
		assertThat(URIs.checkInfoNamespace(URI.create("info:ddc/22/eng//004.678"), "ddc"), is(URI.create("info:ddc/22/eng//004.678")));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when an incompatible info namespace is provided. */
	@Test(expected = IllegalArgumentException.class)
	public void testCheckInfoNamespaceFail() {
		URIs.checkInfoNamespace(URI.create("info:lccn/22/eng//004.678"), "ddc");
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when a null {@link URI} provided. */
	@Test(expected = NullPointerException.class)
	public void testCheckInfoNamespaceNullURIFail() {
		URIs.checkInfoNamespace(null, "ddc");
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when a null scheme provided. */
	@Test(expected = NullPointerException.class)
	@Ignore // TODO The method throws an IllegalArgumentException instead of NullPointerException.
	public void testCheckInfoNamespaceNullSchemeFail() {
		URIs.checkInfoNamespace(URI.create("info:ddc/22/eng//004.678"), null);
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is working properly. */
	@Test
	public void testGetInfoNamespace() {
		assertThat(URIs.getInfoNamespace(URI.create("info:ddc/22/eng//004.678")), is("ddc"));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when a non-info scheme is provided. */
	@Test(expected = IllegalArgumentException.class)
	public void testGetInfoNamespaceWrongSchemeFail() {
		URIs.getInfoNamespace(URI.create("inf:ddc/22/eng//004.678"));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when an info {@link URI} without namespace is provided. */
	@Test(expected = IllegalArgumentException.class)
	public void testGetInfoNamespaceMissingNamespaceFail() {
		URIs.getInfoNamespace(URI.create("inf:/22/eng//004.678"));
	}

	/** Tests whether {@link URIs#checkInfoNamespace(URI, String)} is throwing an exception when a null {@link URI} provided. */
	@Test(expected = NullPointerException.class)
	public void testGetInfoNamespaceNullFail() {
		URIs.getInfoNamespace(null);
	}

	/** Tests whether {@link URIs#getInfoIdentifier(URI)} is working properly. */
	@Test
	public void testGetInfoIdentifier() {
		assertThat(URIs.getInfoIdentifier(URI.create("info:ddc/22/eng//004.678")), is("22/eng//004.678"));
	}

	/** Tests whether {@link URIs#getInfoIdentifier(URI)} is throwing an exception when a non-info scheme is provided. */
	@Test(expected = IllegalArgumentException.class)
	public void testGetInfoIdentifierWrongSchemeFail() {
		URIs.getInfoIdentifier(URI.create("inf:/22/eng//004.678"));
	}

	/** Tests whether {@link URIs#getInfoIdentifier(URI)} is throwing an exception when a null {@link URI} provided. */
	@Test(expected = NullPointerException.class)
	public void testGetInfoIdentifierNullFail() {
		URIs.getInfoIdentifier(null);
	}

	//TODO clarify differences between getInfoIdentifier and getInfoRawIdentifier.
	/** Tests whether {@link URIs#getInfoRawIdentifier(URI)} is working properly. */
	@Test
	public void testGetInfoRawIdentifier() {
		assertThat(URIs.getInfoRawIdentifier(URI.create("info:ddc/22/eng//004.678")), is("22/eng//004.678"));
	}

	/** Tests whether {@link URIs#getInfoRawIdentifier(URI)} is throwing an exception when a non-info scheme is provided. */
	@Test(expected = IllegalArgumentException.class)
	public void testGetInfoRawIdentifierWrongSchemeFail() {
		URIs.getInfoRawIdentifier(URI.create("inf:/22/eng//004.678"));
	}

	/** Tests whether {@link URIs#getInfoRawIdentifier(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test(expected = NullPointerException.class)
	public void testGetInfoRawIdentifierNullFail() {
		URIs.getInfoRawIdentifier(null);
	}

	/** Tests whether {@link URIs#isInfoNamespace(URI, String)} is working properly. */
	@Test
	public void testIsInfoNamespace() {
		assertThat(URIs.isInfoNamespace(URI.create("info:ddc/22/eng//004.678"), "ddc"), is(true));
		assertThat(URIs.isInfoNamespace(URI.create("info:ddc/22/eng//004.678"), "lccn"), is(false));
		assertThat(URIs.isInfoNamespace(URI.create("info:lccn/22/eng//004.678"), "lccn"), is(true));
	}

	/** Tests whether {@link URIs#isInfoNamespace(URI, String)} is throwing an exception when a null {@link URI} is provided. */
	@Test(expected = NullPointerException.class)
	public void testIsInfoNamespaceTestNullURIFail() {
		URIs.isInfoNamespace(null, "ddc");
	}

	/** Tests whether {@link URIs#isInfoNamespace(URI, String)} is throwing an exception when a null scheme is provided. */
	@Test(expected = NullPointerException.class)
	@Ignore //The method throws an IllegalArgumentException instead of NullPointerException.
	public void testIsInfoNamespaceTestNullSchemeFail() {
		URIs.isInfoNamespace(URI.create("info:ddc/22/eng//004.678"), null);
	}

	/** Tests whether {@link URIs#getPathRawPath(URI)} is working properly. */
	@Test
	public void testGetPathRawPath() {
		assertThat(URIs.getPathRawPath(URI.create("path:foo/")), is("foo/"));
		assertThat(URIs.getPathRawPath(URI.create("path:foo/bar")), is("foo/bar"));
	}

	/** Tests whether {@link URIs#getPathRawPath(URI)} is throwing an exception when a non-path scheme is provided. */
	@Test(expected = IllegalArgumentException.class)
	public void testGetPathRawPathWrongSchemeFail() {
		URIs.getPathRawPath(URI.create("http://example.com"));
	}

	/** Tests whether {@link URIs#getPathURIPath(URI)} is working properly. */
	@Test
	public void testGetPathURIPath() {
		assertThat(URIs.getPathURIPath(URI.create("path:foo/")), is(URIPath.asPathURIPath(URI.create("path:foo/"))));
		assertThat(URIs.getPathURIPath(URI.create("path:foo/bar")), is(URIPath.asPathURIPath(URI.create("path:foo/bar"))));
	}

	/** Tests whether {@link URIs#getPathURIPath(URI)} is throwing an exception when a non-path scheme is provided. */
	@Test(expected = IllegalArgumentException.class)
	public void testGetPathURIPathWrongSchemeFail() {
		URIs.getPathURIPath(URI.create("http://example.com"));
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
	@Test(expected = NullPointerException.class)
	public void testChangePathNullURIFail() {
		URIs.changePath(null, URIPath.asPathURIPath(URI.create("path:foobar/")));
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
	@Test(expected = NullPointerException.class)
	public void testChangeRawPathNullURIFail() {
		URIs.changeRawPath(null, "/foobar");
	}

	/** Tests whether {@link URIs#changeHost(URI, String)} is working properly. */
	@Test
	public void testChangeHost() {
		assertThat(URIs.changeHost(URI.create("http://example.com"), "globalmentor.com"), is(URI.create("http://globalmentor.com")));
		assertThat(URIs.changeHost(URI.create("http://example.com/"), "globalmentor.com"), is(URI.create("http://globalmentor.com/")));
		assertThat(URIs.changeHost(URI.create("http://example.com/foo"), "globalmentor.com"), is(URI.create("http://globalmentor.com/foo")));
	}

	/** Tests whether {@link URIs#changeHost(URI, String)} is throwing an exception when a null host is provided. */
	@Test(expected = NullPointerException.class)
	public void testChangeHostNullHostFail() {
		URIs.changeHost(URI.create("http://example.com/foo/bar"), null);
	}

	/** Tests whether {@link URIs#changeRawSchemeSpecificPart(URI, String)} is working properly. */
	@Test
	public void testChangeRawSchemeSpecificPart() {
		assertThat(URIs.changeRawSchemeSpecificPart(URI.create("http://example.com"), "//globalmentor.io"), is(URI.create("http://globalmentor.io")));
		assertThat(URIs.changeRawSchemeSpecificPart(URI.create("http://example.com/"), "//globalmentor.io/"), is(URI.create("http://globalmentor.io/")));
		assertThat(URIs.changeRawSchemeSpecificPart(URI.create("http://example.com/foo"), "//globalmentor.io/bar"), is(URI.create("http://globalmentor.io/bar")));
	}

	/** Tests whether {@link URIs#changeRawSchemeSpecificPart(URI, String)} is throwing an exception when a null host is provided. */
	@Test(expected = NullPointerException.class)
	@Ignore // TODO GRAVE ISSUE! It's replacing the scheme-specific part for "null", instead of throwing an exception.
	public void testChangeRawSchemeSpecificPartFail() {
		URIs.changeRawSchemeSpecificPart(URI.create("http://example.com/foo/bar"), null);
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
	@Test(expected = NullPointerException.class)
	public void testToCollectionNullURIFail() {
		URIs.toCollectionURI(null);
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
	@Test(expected = NullPointerException.class)
	public void testGetPathNullURIFail() {
		URIs.getPath(null);
	}

	/** Tests whether {@link URIs#getRawName(URI)} is working properly. */
	@Test
	public void testGetRawName() {
		assertThat(URIs.getRawName(URI.create("http://example.com")), is("")); // TODO It should return null according to the Javadocs
		assertThat(URIs.getRawName(URI.create("http://example.com/")), is("/"));
		assertThat(URIs.getRawName(URI.create("http://example.com/foo")), is("foo"));
		assertThat(URIs.getRawName(URI.create("http://example.com/foo/")), is("foo"));
		assertThat(URIs.getRawName(URI.create("http://example.com/foo/bar/foobar.txt")), is("foobar.txt"));

		assertThat(URIs.getRawName(URI.create("info:ddc/22/eng//004.678")), is("004.678"));
	}

	/** Tests whether {@link URIs#getRawName(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test(expected = NullPointerException.class)
	public void testGetRawNameNullURIFail() {
		URIs.getRawName(null);
	}

	// TODO create test and fail test for getName()

	/** Tests whether {@link URIs#changeName(String, String)} is working properly. */
	@Test
	public void testChangeName() {
		assertThat(URIs.changeName(URI.create("http://example.com"), "foobar.txt"), is(URI.create("http://example.comfoobar.txt"))); // TODO It should throw an IllegalArgumentException according to the Javadocs.
		assertThat(URIs.changeName(URI.create("http://example.com/"), "foobar.txt"), is(URI.create("http://example.com/foobar.txt/"))); // TODO Should it really add the trailing slash?
		assertThat(URIs.changeName(URI.create("http://example.com/foo"), "foobar.txt"), is(URI.create("http://example.com/foobar.txt")));
		assertThat(URIs.changeName(URI.create("http://example.com/foo/"), "foobar.txt"), is(URI.create("http://example.com/foobar.txt/")));

		assertThat(URIs.changeName(URI.create("http://example.com/foo/bar/foobar.txt"), "foo.txt"), is(URI.create("http://example.com/foo/bar/foo.txt")));
		assertThat(URIs.changeName(URI.create("http://example.com/foo/bar/foobar.txt"), ""), is(URI.create("http://example.com/foo/bar/")));

		assertThat(URIs.changeName(URI.create("info:ddc/22/eng//004.678"), "foobar.txt"), is(URI.create("info:ddc/22/eng//foobar.txt")));
	}

	/** Tests whether {@link URIs#changeName(String, String)} is throwing an exception when a null {@link URI} is provided. */
	@Test(expected = NullPointerException.class)
	public void testChangeNameNullURIFail() {
		URIs.changeName((URI)null, "");
	}

	/** Tests whether {@link URIs#changeName(String, String)} is throwing an exception when a null name is provided. */
	@Test(expected = NullPointerException.class)
	public void testChangeNameNullNameFail() {
		URIs.changeName(URI.create("http://example.com/"), null);
	}

	//TODO create test and fail test for changeRawName()

	/** Tests whether {@link URIs#addNameExtension(String, String)} is working properly. */
	@Test
	public void testAddNameExtension() {
		assertThat(URIs.addNameExtension("foobar", "txt"), is("foobar.txt"));
		assertThat(URIs.addNameExtension("foobar", ""), is("foobar.")); // TODO It should probably not add anything when an empty extension is provided.

		assertThat(URIs.addNameExtension("", ".foobar"), is("..foobar")); // TODO It should probably not double the "." at the beginning of the name.
		assertThat(URIs.addNameExtension(".foobar", ""), is(".foobar.")); // TODO Clarify how we should deal with files like this, e.g., ".gitignore" and similars.
	}

	/** Tests whether {@link URIs#addNameExtension(String, String)} is throwing an exception when a null name is provided. */
	@Test(expected = NullPointerException.class)
	public void testAddNameExtensionNullNameFail() {
		URIs.addNameExtension(null, "txt");
	}

	/** Tests whether {@link URIs#addNameExtension(String, String)} is throwing an exception when a null extension is provided. */
	@Test(expected = NullPointerException.class)
	public void testAddNameExtensionNullExtensionFail() {
		URIs.addNameExtension("foobar", null);
	}

	/** Tests whether {@link URIs#addRawNameExtension(String, String)} is working properly. */
	@Test
	public void testAddRawNameExtension() {
		assertThat(URIs.addRawNameExtension(URI.create("http://example.com/foobar"), "txt"), is(URI.create("http://example.com/foobar.txt")));
		assertThat(URIs.addRawNameExtension(URI.create("http://example.com/"), ""), is(URI.create("http://example.com//./"))); //TODO It looks a little bit crazy. It should not modify the URI.

		assertThat(URIs.addRawNameExtension(URI.create("http://example.com/"), ".foobar"), is(URI.create("http://example.com//..foobar/"))); //TODO Why is it doubling the slash?
		assertThat(URIs.addRawNameExtension(URI.create("http://example.com/.foobar"), ""), is(URI.create("http://example.com/.foobar."))); //TODO Should it really make a modification in the URI when an empty extension is given?

		assertThat(URIs.addRawNameExtension(URI.create("http://example.com/foo.txt"), "bar"), is(URI.create("http://example.com/foo.txt.bar")));

		assertThat(URIs.addRawNameExtension(URI.create("http://example.com"), "foobar"), is(URI.create("http://example.com.foobar"))); //TODO It should throw an IllegalArgumentException.
	}

	/** Tests whether {@link URIs#addRawNameExtension(String, String)} is throwing an exception when a null name is provided. */
	@Test(expected = NullPointerException.class)
	public void testAddRawNameExtensionNullNameFail() {
		URIs.addRawNameExtension(null, "txt");
	}

	/** Tests whether {@link URIs#addRawNameExtension(String, String)} is throwing an exception when a null extension is provided. */
	@Test(expected = NullPointerException.class)
	public void testAddRawNameExtensionNullExtensionFail() {
		URIs.addRawNameExtension(URI.create("http://example.com"), null);
	}

	/** Tests whether {@link URIs#getNameExtension(URI)} is working properly. */
	@Test
	public void testGetNameExtensionURI() {
		assertThat(URIs.getNameExtension(URI.create("http://example.com/foobar.txt")), is("txt"));
		assertThat(URIs.getNameExtension(URI.create("http://example.com/foobar")), is((String)null));
		assertThat(URIs.getNameExtension(URI.create("http://example.com/")), is((String)null));

		assertThat(URIs.getNameExtension(URI.create("http://example.com/.foobar")), is("foobar"));
		assertThat(URIs.getNameExtension(URI.create("http://example.com/.")), is("")); //TODO Should it really return an empty String instead of null?
		assertThat(URIs.getNameExtension(URI.create("http://example.com/")), is((String)null));

		assertThat(URIs.getNameExtension(URI.create("http://example.com")), is((String)null));
	}

	/** Tests whether {@link URIs#getNameExtension(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test(expected = NullPointerException.class)
	public void testGetNameExtensionNullURIFail() {
		URIs.getNameExtension((URI)null);
	}

	/** Tests whether {@link URIs#getNameExtension(String)} is working properly. */
	@Test
	public void testGetNameExtensionString() {
		assertThat(URIs.getNameExtension("foobar.txt"), is("txt"));
		assertThat(URIs.getNameExtension("foobar"), is((String)null));
		assertThat(URIs.getNameExtension(""), is((String)null));

		assertThat(URIs.getNameExtension(".foobar"), is("foobar"));
		assertThat(URIs.getNameExtension("."), is("")); //TODO Should it really return an empty String instead of null?
	}

	/** Tests whether {@link URIs#getNameExtension(String)} is throwing an exception when a null name is provided. */
	@Test(expected = NullPointerException.class)
	public void testGetNameExtensionNullStringFail() {
		URIs.getNameExtension((String)null);
	}

	//TODO add test and fail test for getRawNameExtension().

	/** Tests whether {@link URIs#getNameExtension(String)} is working properly. */
	@Test
	public void testChangeRawNameExtensionString() {
		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar.xml"), "json"), is(URI.create("http://example.com/foobar.json")));
		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar"), "xml"), is(URI.create("http://example.com/foobar.xml")));
		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/"), "foobar"), is(URI.create("http://example.com//.foobar/"))); // TODO check the slashes

		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar.xml"), ".json"), is(URI.create("http://example.com/foobar..json"))); //TODO maybe we should ignore the "." in front of the extension?
		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar.xml"), ""), is(URI.create("http://example.com/foobar.")));

		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar.xml"), null), is(URI.create("http://example.com/foobar")));
		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com/foobar"), null), is(URI.create("http://example.com/foobar")));

		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com"), "foobar"), is(URI.create("http://example.com.foobar"))); // TODO It should throw an IllegalArgument Exception according to the Javadocs.
		assertThat(URIs.changeRawNameExtension(URI.create("http://example.com"), null), is(URI.create("http://example.com"))); // TODO It should throw an IllegalArgument Exception
	}

	/** Tests whether {@link URIs#getNameExtension(String)} is throwing an exception when a null name is provided. */
	@Test(expected = NullPointerException.class)
	public void testChangeRawNameExtensionNullExtensionFail() {
		URIs.changeRawNameExtension((URI)null, "");
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
	@Test(expected = NullPointerException.class)
	public void testGetPlainURINullURIFail() {
		URIs.getPlainURI((URI)null);
	}

	/** Tests whether {@link URIs#constructQuery(URIQueryParameter...)} is working properly. */
	@Test
	public void testConstructQueryParameters() {
		// assertThat(URIs.constructQuery((URIQueryParameter)null), is("")); // TODO according to the javadocs, it should return an empty String

		assertThat(URIs.constructQuery(new URIQueryParameter("type", "foo")), is("?type=foo"));
		assertThat(URIs.constructQuery(new URIQueryParameter("type", "foo"), new URIQueryParameter("place", "bar")), is("?type=foo&place=bar"));
	}

	//TODO create tests for constructQuery(String) !The implementation looks really awkward. What's the purpose of that method? If someone has to create the raw query String, why wouldn't it add a '?' manually too?

	/** Tests whether {@link URIs#appendRawQuery(URI, String)} is working properly. */
	@Test
	public void testAppendRawQuery() {
		assertThat(URIs.appendRawQuery(URI.create("http://example.com/"), ""), is(URI.create("http://example.com/?")));
		assertThat(URIs.appendRawQuery(URI.create("http://example.com/"), "type=foo"), is(URI.create("http://example.com/?type=foo")));
		assertThat(URIs.appendRawQuery(URI.create("http://example.com/"), "type=foo&place=bar"), is(URI.create("http://example.com/?type=foo&place=bar")));
	}

	/** Tests whether {@link URIs#appendRawQuery(URI, String)} throwing an exception when a null query parameter is provided. */
	@Test(expected = NullPointerException.class)
	public void testAppendRawQueryNullQueryFail() {
		URIs.appendRawQuery(URI.create("http://example.com/"), null);
	}

	/** Tests whether {@link URIs#appendRawQuery(URI, String)} throwing an exception when a null {@link URI} is provided. */
	@Test(expected = NullPointerException.class)
	public void testAppendRawQueryNullURIFail() {
		URIs.appendRawQuery(null, "type=foo");
	}

	/** Tests whether {@link URIs#appendQueryParameter(URI, String, String)} is working properly. */
	@Test
	public void testAppendQueryParameter() {
		assertThat(URIs.appendQueryParameter(URI.create("http://example.com/"), "", ""), is(URI.create("http://example.com/?=")));
		assertThat(URIs.appendQueryParameter(URI.create("http://example.com/"), "", null), is(URI.create("http://example.com/?")));
		assertThat(URIs.appendQueryParameter(URI.create("http://example.com/"), "type", "foo"), is(URI.create("http://example.com/?type=foo")));
		assertThat(URIs.appendQueryParameter(URI.create("http://example.com/?type=foo"), "place", "bar"), is(URI.create("http://example.com/?type=foo&place=bar")));
	}

	/** Tests whether {@link URIs#appendQueryParameter(URI, String, String)} throwing an exception when a null query name is provided. */
	@Test(expected = NullPointerException.class)
	public void testAppendQueryParameterNullQueryNameFail() {
		URIs.appendQueryParameter(URI.create("http://example.com/"), null, "");
	}

	/** Tests whether {@link URIs#appendQueryParameter(URI, String, String)} throwing an exception when a null query name and value is provided. */
	@Test(expected = NullPointerException.class)
	public void testAppendQueryParameterNullQueryNameAndValueFail() {
		URIs.appendQueryParameter(URI.create("http://example.com/"), null, null);
	}

	/** Tests whether {@link URIs#appendQueryParameter(URI, String, String)} is throwing an exception when a null {@link URI} is provided. */
	@Test(expected = NullPointerException.class)
	public void testAppendQueryParameterNullURIFail() {
		URIs.appendQueryParameter(null, "type", "foo");
	}

	/** Tests whether {@link URIs#getParameterMap(URI)} is working properly. */
	@Test
	public void testGetParameterMap() {
		// TODO In what situation will it return a List with multiple values?

		assertThat(URIs.getParameterMap(URI.create("http://example.com/")).isEmpty(), is(true));

		CollectionMap<String, String, List<String>> queryCollectionMap;

		queryCollectionMap = URIs.getParameterMap(URI.create("http://example.com/?type=foo"));
		assertThat(queryCollectionMap.size(), is(1));
		assertThat(queryCollectionMap.get("type"), is(Arrays.asList("foo")));

		queryCollectionMap = URIs.getParameterMap(URI.create("http://example.com/?type=foo&place=bar"));
		assertThat(queryCollectionMap.size(), is(2));
		assertThat(queryCollectionMap.get("type"), is(Arrays.asList("foo")));
		assertThat(queryCollectionMap.get("place"), is(Arrays.asList("bar")));
	}

	/** Tests whether {@link URIs#getParameterMap(URI)} is throwing an exception when a null {@link URI} is provided. */
	@Test(expected = NullPointerException.class)
	public void testGetParameterMapNullFail() {
		URIs.getParameterMap(null);
	}

	//TODO create test to getParameters()

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
	@Test(expected = NullPointerException.class)
	public void testCreatePathURINullFail() {
		URIs.createPathURI(null);
	}

	/** Tests whether {@link URIs#createPathURI(String)} throwing an exception when a scheme is provided. */
	@Test(expected = IllegalArgumentException.class)
	public void testCreatePathURISchemeProvidedFail() {
		URIs.createPathURI("http://foobar");
	}

	// TODO create test to checkRoot() and checkAbsolute()

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
	@Test(expected = NullPointerException.class)
	public void testIsPathURINullURIFail() {
		URIs.isPathURI(null);
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
	@Test(expected = NullPointerException.class)
	public void testIsPlainURINullURIFail() {
		URIs.isPlainURI((URI)null);
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
	@Test(expected = NullPointerException.class)
	public void testResolveNullBaseURIFail() {
		URIs.resolve(null, URI.create("http://example.com/foobar"));
	}

	/** Tests whether {@link URIs#resolve(URI, URI)} is throwing an exception when a null child {@link URI} is provided. */
	@Test(expected = NullPointerException.class)
	public void testResolveNullChildURIFail() {
		URIs.resolve(URI.create("http://example.com/"), (URI)null);
	}

	/** Tests whether {@link URIs#resolve(URI, URI)} is throwing an exception when a null base and child {@link URI} are provided. */
	@Test(expected = NullPointerException.class)
	public void testResolveNullBaseAndChildURIFail() {
		URIs.resolve(null, (URI)null);
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
	@Test(expected = NullPointerException.class)
	public void testGetCurrentLevelTestNullFail() {
		URIs.getCurrentLevel(null);
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
	@Test(expected = NullPointerException.class)
	public void testGetParentLevelTestNullFail() {
		URIs.getParentLevel(null);
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
	@Test(expected = NullPointerException.class)
	public void testGetParentURITestNullFail() {
		URIs.getParentURI(null);
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
