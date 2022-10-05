/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.net;

import static java.util.Arrays.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.junit.jupiter.api.Test;

/**
 * Tests for URI paths.
 * @author Garret Wilson
 */
public class URIPathTest {

	/** Tests retrieving base paths of a path. */
	@Test
	public void testBasePaths() {
		assertThat(URIPath.of("").getBasePaths(), equalTo(asList(URIPath.of(""))));
		assertThat(URIPath.of("/").getBasePaths(), equalTo(asList(URIPath.of("/"))));
		assertThat(URIPath.of("one").getBasePaths(), equalTo(asList(URIPath.of("one"))));
		assertThat(URIPath.of("one/").getBasePaths(), equalTo(asList(URIPath.of("one/"))));
		assertThat(URIPath.of("one/two/three").getBasePaths(), equalTo(asList(URIPath.of("one/"), URIPath.of("one/two/"), URIPath.of("one/two/three"))));
		assertThat(URIPath.of("one/two/three/").getBasePaths(), equalTo(asList(URIPath.of("one/"), URIPath.of("one/two/"), URIPath.of("one/two/three/"))));
		assertThat(URIPath.of("/one").getBasePaths(), equalTo(asList(URIPath.of("/one"))));
		assertThat(URIPath.of("/one/").getBasePaths(), equalTo(asList(URIPath.of("/one/"))));
		assertThat(URIPath.of("/one/two/three").getBasePaths(), equalTo(asList(URIPath.of("/one/"), URIPath.of("/one/two/"), URIPath.of("/one/two/three"))));
		assertThat(URIPath.of("/one/two/three/").getBasePaths(), equalTo(asList(URIPath.of("/one/"), URIPath.of("/one/two/"), URIPath.of("/one/two/three/"))));
	}

	/** @see URIPath#isSubPath() */
	@Test
	public void testIsSubPath() {
		assertThat(URIPath.of("").isSubPath(), is(true));
		assertThat(URIPath.of(".").isSubPath(), is(true));
		assertThat(URIPath.of("./").isSubPath(), is(true));
		assertThat(URIPath.of("..").isSubPath(), is(false));
		assertThat(URIPath.of("../").isSubPath(), is(false));
		assertThat(URIPath.of(".%46/").isSubPath(), is(true));
		assertThat(URIPath.of("..%2F").isSubPath(), is(true));
		assertThat(URIPath.of(".../").isSubPath(), is(true));
		assertThat(URIPath.of("./../").isSubPath(), is(false));
		assertThat(URIPath.of("foo").isSubPath(), is(true));
		assertThat(URIPath.of("foo/").isSubPath(), is(true));
		assertThat(URIPath.of("foo../").isSubPath(), is(true));
		assertThat(URIPath.of("foo/../bar").isSubPath(), is(true));
		assertThat(URIPath.of("foo/../..").isSubPath(), is(false));
		assertThat(URIPath.of("foo/../../").isSubPath(), is(false));
		assertThat(URIPath.of("foo/../../bar").isSubPath(), is(false));
		assertThat(URIPath.of("/").isSubPath(), is(false));
		assertThat(URIPath.of("/.").isSubPath(), is(false));
		assertThat(URIPath.of("/./").isSubPath(), is(false));
		assertThat(URIPath.of("/..").isSubPath(), is(false));
		assertThat(URIPath.of("/../").isSubPath(), is(false));
		assertThat(URIPath.of("/.%46/").isSubPath(), is(false));
		assertThat(URIPath.of("/..%2F").isSubPath(), is(false));
		assertThat(URIPath.of("/.../").isSubPath(), is(false));
		assertThat(URIPath.of("/./../").isSubPath(), is(false));
		assertThat(URIPath.of("/foo").isSubPath(), is(false));
		assertThat(URIPath.of("/foo/").isSubPath(), is(false));
		assertThat(URIPath.of("/foo../").isSubPath(), is(false));
		assertThat(URIPath.of("/foo/../bar").isSubPath(), is(false));
		assertThat(URIPath.of("/foo/../..").isSubPath(), is(false));
		assertThat(URIPath.of("/foo/../../").isSubPath(), is(false));
		assertThat(URIPath.of("/foo/../../bar").isSubPath(), is(false));
	}

}
