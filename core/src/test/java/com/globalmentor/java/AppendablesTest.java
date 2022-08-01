/*
 * Copyright © 2022 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

import static java.util.Arrays.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.IOException;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Appendables}.
 * @author Garret Wilson
 */
public class AppendablesTest {

	/** @see Appendables#appendJoined(Appendable, char, CharSequence...) */
	@Test
	void testAppendJoinedCharDelimiterVarargs() throws IOException {
		assertThat(Appendables.appendJoined(new StringBuilder(), ',').toString(), is(""));
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', "").toString(), is(""));
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', "one").toString(), is("one"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', "one", "two").toString(), is("one,two"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', "one", "two", "three").toString(), is("one,two,three"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', "one", null, "three").toString(), is("one,null,three"));
	}

	/** @see Appendables#appendJoined(Appendable, char, Iterable) */
	@Test
	void testAppendJoinedCharDelimiterIterable() throws IOException {
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', asList()).toString(), is(""));
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', asList("")).toString(), is(""));
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', asList("one")).toString(), is("one"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', asList("one", "two")).toString(), is("one,two"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', asList("one", "two", "three")).toString(), is("one,two,three"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ',', asList("one", null, "three")).toString(), is("one,null,three"));
	}

	/** @see Appendables#appendJoined(Appendable, CharSequence, CharSequence...) */
	@Test
	void testAppendJoinedCharSequenceDelimiterVarargs() throws IOException {
		assertThat(Appendables.appendJoined(new StringBuilder(), ",").toString(), is(""));
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", "").toString(), is(""));
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", "one").toString(), is("one"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", "one", "two").toString(), is("one,two"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", "one", "two", "three").toString(), is("one,two,three"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", "one", null, "three").toString(), is("one,null,three"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ", ", "one", "two", "three").toString(), is("one, two, three"));
	}

	/** @see Appendables#appendJoined(Appendable, CharSequence, Iterable) */
	@Test
	void testAppendJoinedCharSequenceDelimiterIterable() throws IOException {
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", asList()).toString(), is(""));
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", asList("")).toString(), is(""));
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", asList("one")).toString(), is("one"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", asList("one", "two")).toString(), is("one,two"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", asList("one", "two", "three")).toString(), is("one,two,three"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ",", asList("one", null, "three")).toString(), is("one,null,three"));
		assertThat(Appendables.appendJoined(new StringBuilder(), ", ", asList("one", "two", "three")).toString(), is("one, two, three"));
	}

}
