/*
 * Copyright © 2016 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.collections.iterators;

import static com.globalmentor.collections.iterators.Iterators.*;
import static java.util.Arrays.*;
import static java.util.stream.Collectors.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;

import org.junit.jupiter.api.Test;

import java.util.List;

/**
 * Test of {@link JoinIterator}.
 * @author Garret Wilson
 */
public class JoinIteratorTest {

	@Test
	public void test() {
		final List<Integer> list1 = asList(1);
		final List<Integer> list12 = asList(1, 2);
		final List<Integer> list123 = asList(1, 2, 3);
		final List<Integer> list4 = asList(4);
		final List<Integer> list45 = asList(4, 5);
		final List<Integer> list456 = asList(4, 5, 6);

		assertThat(toStream(new JoinIterator<Integer>(emptyIterator(), emptyIterator())).collect(toList()), equalTo(asList()));
		assertThat(toStream(new JoinIterator<Integer>(list1.iterator(), emptyIterator())).collect(toList()), equalTo(asList(1)));
		assertThat(toStream(new JoinIterator<Integer>(list12.iterator(), emptyIterator())).collect(toList()), equalTo(asList(1, 2)));
		assertThat(toStream(new JoinIterator<Integer>(list123.iterator(), emptyIterator())).collect(toList()), equalTo(asList(1, 2, 3)));

		assertThat(toStream(new JoinIterator<Integer>(emptyIterator(), list4.iterator())).collect(toList()), equalTo(asList(4)));
		assertThat(toStream(new JoinIterator<Integer>(emptyIterator(), list45.iterator())).collect(toList()), equalTo(asList(4, 5)));
		assertThat(toStream(new JoinIterator<Integer>(emptyIterator(), list456.iterator())).collect(toList()), equalTo(asList(4, 5, 6)));

		assertThat(toStream(new JoinIterator<Integer>(list1.iterator(), list4.iterator())).collect(toList()), equalTo(asList(1, 4)));
		assertThat(toStream(new JoinIterator<Integer>(list12.iterator(), list45.iterator())).collect(toList()), equalTo(asList(1, 2, 4, 5)));
		assertThat(toStream(new JoinIterator<Integer>(list123.iterator(), list456.iterator())).collect(toList()), equalTo(asList(1, 2, 3, 4, 5, 6)));

		assertThat(toStream(new JoinIterator<Integer>(emptyIterator(), list123.iterator(), list456.iterator())).collect(toList()),
				equalTo(asList(1, 2, 3, 4, 5, 6)));
		assertThat(toStream(new JoinIterator<Integer>(list123.iterator(), emptyIterator(), list456.iterator())).collect(toList()),
				equalTo(asList(1, 2, 3, 4, 5, 6)));
		assertThat(toStream(new JoinIterator<Integer>(list123.iterator(), list456.iterator(), emptyIterator())).collect(toList()),
				equalTo(asList(1, 2, 3, 4, 5, 6)));
		assertThat(toStream(new JoinIterator<Integer>(emptyIterator(), list123.iterator(), emptyIterator(), list456.iterator(), emptyIterator())).collect(toList()),
				equalTo(asList(1, 2, 3, 4, 5, 6)));

		assertThat(toStream(new JoinIterator<Integer>(list123.iterator(), list456.iterator(), list123.iterator())).collect(toList()),
				equalTo(asList(1, 2, 3, 4, 5, 6, 1, 2, 3)));
	}

}
