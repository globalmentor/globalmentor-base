/*
 * Copyright © 2019 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.collections.comparators;

import static java.util.Arrays.*;
import static java.util.Comparator.*;
import static java.util.stream.Collectors.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.*;

import org.junit.jupiter.api.*;

import com.globalmentor.collections.Lists;

/**
 * Tests of {@link ExplicitOrderComparator}.
 * @author Garret Wilson
 */
public class ExplicitOrderComparatorTest {

	private final List<String> order = Lists.immutableListOf("foo", "bar");

	/** Sorts <code>"foo"</code> and <code>"bar"</code> first, sorting the remaining strings in case naively by order to make the tests deterministic. */
	private final Comparator<String> FOO_BAR_FIRST_COMPARATOR = ExplicitOrderComparator.explicitOrderFirst(order).thenComparing(naturalOrder());

	/** Sorts <code>"foo"</code> and <code>"bar"</code> last, sorting the remaining strings in case naively by order to make the tests deterministic. */
	private final Comparator<String> FOO_BAR_LAST_COMPARATOR = ExplicitOrderComparator.explicitOrderLast(order).thenComparing(naturalOrder());

	/** @see ExplicitOrderComparator#explicitOrderFirst(List) */
	@Test
	public void testOrderFirst() {
		assertThat(asList("foo").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()), contains("foo"));
		assertThat(asList("foo", "bar").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()), contains("foo", "bar"));
		assertThat(asList("bar", "foo").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()), contains("foo", "bar"));
		assertThat(asList("bar").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()), contains("bar"));

		assertThat(asList("foo", "banana", "apple", "cherry").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()),
				contains("foo", "apple", "banana", "cherry"));
		assertThat(asList("banana", "apple", "foo", "cherry").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()),
				contains("foo", "apple", "banana", "cherry"));
		assertThat(asList("banana", "apple", "cherry", "foo").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()),
				contains("foo", "apple", "banana", "cherry"));

		assertThat(asList("foo", "bar", "banana", "apple", "cherry").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()),
				contains("foo", "bar", "apple", "banana", "cherry"));
		assertThat(asList("foo", "banana", "apple", "cherry", "bar").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()),
				contains("foo", "bar", "apple", "banana", "cherry"));
		assertThat(asList("bar", "banana", "apple", "cherry", "foo").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()),
				contains("foo", "bar", "apple", "banana", "cherry"));
		assertThat(asList("banana", "foo", "apple", "bar", "cherry").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()),
				contains("foo", "bar", "apple", "banana", "cherry"));
		assertThat(asList("banana", "bar", "apple", "foo", "cherry").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()),
				contains("foo", "bar", "apple", "banana", "cherry"));

		assertThat(asList("apple", "banana", "cherry").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()), contains("apple", "banana", "cherry"));
		assertThat(asList("banana", "cherry", "apple").stream().sorted(FOO_BAR_FIRST_COMPARATOR).collect(toList()), contains("apple", "banana", "cherry"));
	}

	/** @see ExplicitOrderComparator#explicitOrderFirst(List) */
	@Test
	public void testOrderLast() {
		assertThat(asList("foo").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()), contains("foo"));
		assertThat(asList("foo", "bar").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()), contains("foo", "bar"));
		assertThat(asList("bar", "foo").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()), contains("foo", "bar"));
		assertThat(asList("bar").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()), contains("bar"));

		System.out.println(asList("foo", "banana", "apple", "cherry").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList())); //TODO delete

		assertThat(asList("foo", "banana", "apple", "cherry").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()),
				contains("apple", "banana", "cherry", "foo"));
		assertThat(asList("banana", "apple", "foo", "cherry").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()),
				contains("apple", "banana", "cherry", "foo"));
		assertThat(asList("banana", "apple", "cherry", "foo").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()),
				contains("apple", "banana", "cherry", "foo"));

		assertThat(asList("foo", "bar", "banana", "apple", "cherry").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()),
				contains("apple", "banana", "cherry", "foo", "bar"));
		assertThat(asList("foo", "banana", "apple", "cherry", "bar").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()),
				contains("apple", "banana", "cherry", "foo", "bar"));
		assertThat(asList("bar", "banana", "apple", "cherry", "foo").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()),
				contains("apple", "banana", "cherry", "foo", "bar"));
		assertThat(asList("banana", "foo", "apple", "bar", "cherry").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()),
				contains("apple", "banana", "cherry", "foo", "bar"));
		assertThat(asList("banana", "bar", "apple", "foo", "cherry").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()),
				contains("apple", "banana", "cherry", "foo", "bar"));

		assertThat(asList("apple", "banana", "cherry").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()), contains("apple", "banana", "cherry"));
		assertThat(asList("banana", "cherry", "apple").stream().sorted(FOO_BAR_LAST_COMPARATOR).collect(toList()), contains("apple", "banana", "cherry"));
	}

}
