/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.collections;

import static com.globalmentor.collections.Lists.*;
import static java.util.Arrays.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.List;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Lists} utilities.
 * @author Garret Wilson
 */
public class ListsTest {

	/** @see Lists#longestCommonSuffix(List) */
	@Test
	public void testLongestCommonSuffix() {
		//empty lists
		assertThat(longestCommonSuffix(asList(asList())), is(empty()));
		assertThat(longestCommonSuffix(asList(asList(), asList())), is(empty()));
		assertThat(longestCommonSuffix(asList(asList("bar"), asList())), is(empty()));
		assertThat(longestCommonSuffix(asList(asList("foo", "bar"), asList())), is(empty()));
		assertThat(longestCommonSuffix(asList(asList(), asList("bar"))), is(empty()));
		assertThat(longestCommonSuffix(asList(asList(), asList("foo", "bar"))), is(empty()));

		//single list
		assertThat(longestCommonSuffix(asList(asList("bar"))), is(asList("bar")));
		assertThat(longestCommonSuffix(asList(asList("foo", "bar"))), is(asList("foo", "bar")));
		assertThat(longestCommonSuffix(asList(asList("test", "foo", "bar"))), is(asList("test", "foo", "bar")));

		//two lists
		assertThat(longestCommonSuffix(asList(asList("bar"), asList("foo", "bar"))), is(asList("bar")));
		assertThat(longestCommonSuffix(asList(asList("foo", "bar"), asList("bar"))), is(asList("bar")));
		assertThat(longestCommonSuffix(asList(asList("test", "foo", "bar"), asList("bar"))), is(asList("bar")));
		assertThat(longestCommonSuffix(asList(asList("test", "foo", "bar"), asList("foo", "bar"))), is(asList("foo", "bar")));
		assertThat(longestCommonSuffix(asList(asList("test", "foo", "bar"), asList("test", "foo", "bar"))), is(asList("test", "foo", "bar")));
		assertThat(longestCommonSuffix(asList(asList("test", "foo", "bar"), asList("example", "foo", "bar"))), is(asList("foo", "bar"))); //differing prefixes
		assertThat(longestCommonSuffix(asList(asList("foo", "bar"), asList("test", "foo", "bar"))), is(asList("foo", "bar")));
		assertThat(longestCommonSuffix(asList(asList("test", "bar"), asList("foo", "bar"))), is(asList("bar"))); //differing prefixes
		assertThat(longestCommonSuffix(asList(asList("foo", "bar"), asList("foo", "baz"))), is(empty())); //differing suffix
		assertThat(longestCommonSuffix(asList(asList("bar"), asList("test", "foo", "bar"))), is(asList("bar")));
		assertThat(longestCommonSuffix(asList(asList(), asList("test", "foo", "bar"))), is(empty()));

		//three lists
		assertThat(longestCommonSuffix(asList(asList("example"), asList("foo"), asList("bar"))), is(empty()));
		assertThat(longestCommonSuffix(asList(asList("example", "foo", "bar"), asList("foo", "bar"), asList("test", "foo", "bar"))), is(asList("foo", "bar")));

		//empty string
		assertThat(longestCommonSuffix(asList(asList("test", "", "bar"), asList("example", "", "bar"))), is(asList("", "bar")));
		assertThat(longestCommonSuffix(asList(asList("test", "foo", ""), asList("example", "foo", ""))), is(asList("foo", "")));
		assertThat(longestCommonSuffix(asList(asList("test", "foo", ""), asList("example", "bar", ""))), is(asList("")));
	}

}
