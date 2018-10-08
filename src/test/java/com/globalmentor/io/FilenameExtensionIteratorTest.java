/*
 * Copyright © 2018 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static org.junit.Assert.*;
import static org.hamcrest.Matchers.*;

import org.junit.*;

import com.globalmentor.collections.Lists;

/**
 * Tests of {@link FilenameExtensionIterator}.
 * 
 * @author Garret Wilson
 */
public class FilenameExtensionIteratorTest {

	/** Indirectly tests the iterator by examining the results of using it as an {@link Iterable} . */
	@Test
	public void testAsIterable() {
		assertThat(Lists.listOf(new FilenameExtensionIterator("")), empty());
		assertThat(Lists.listOf(new FilenameExtensionIterator("example")), empty());
		assertThat(Lists.listOf(new FilenameExtensionIterator(".example")), contains("example"));
		assertThat(Lists.listOf(new FilenameExtensionIterator(".foo.bar")), contains("foo.bar", "bar"));
		assertThat(Lists.listOf(new FilenameExtensionIterator(".example.foo.bar")), contains("example.foo.bar", "foo.bar", "bar"));
		assertThat(Lists.listOf(new FilenameExtensionIterator("example.foo.bar")), contains("foo.bar", "bar"));
		assertThat(Lists.listOf(new FilenameExtensionIterator("example.foobar")), contains("foobar"));
		assertThat(Lists.listOf(new FilenameExtensionIterator("foo.bar")), contains("bar"));
	}

}
