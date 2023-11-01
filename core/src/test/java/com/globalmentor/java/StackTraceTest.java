/*
 * Copyright © 2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.java;

import static org.hamcrest.Matchers.*;
import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static com.globalmentor.java.StackTrace.*;
import static com.globalmentor.text.RegularExpressions.*;
import static org.hamcrest.MatcherAssert.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Optional;
import java.util.regex.Matcher;

import org.junit.jupiter.api.Test;

/**
 * Tests of a stack trace.
 * @author Garret Wilson
 */
public class StackTraceTest {

	/**
	 * A stack trace element string containing all of the possible components.
	 * @see StackTraceElement#toString()
	 */
	static final String ELEMENT_STRING = "com.foo.loader/foo@9.0/com.foo.Main.run(Main.java:101)";

	/**
	 * The line number is unavailable.
	 * @see StackTraceElement#toString()
	 */
	static final String ELEMENT_STRING_NO_LINE_NUMBER = "com.foo.loader/foo@9.0/com.foo.Main.run(Main.java)";
	/**
	 * Neither the file name nor the line number is available.
	 * @see StackTraceElement#toString()
	 */
	static final String ELEMENT_STRING_UNKNOWN_SOURCE = "com.foo.loader/foo@9.0/com.foo.Main.run(Unknown Source)";
	/**
	 * The method containing the execution point is a native method.
	 * @see StackTraceElement#toString()
	 */
	static final String ELEMENT_STRING_NATIVE_METHOD = "com.foo.loader/foo@9.0/com.foo.Main.run(Native Method)";
	/**
	 * The class of the execution point is defined in the unnamed module of the class loader named <code>com.foo.loader</code>.
	 * @see StackTraceElement#toString()
	 */
	static final String ELEMENT_STRING_UNNAMED_MODULE = "com.foo.loader//com.foo.bar.App.run(App.java:12)";
	/**
	 * The class of the execution point is defined in acme module loaded by a built-in class loader such as the application class loader.
	 * @see StackTraceElement#toString()
	 */
	static final String ELEMENT_STRING_BUILT_IN_CLASSS_LOADER = "acme@2.1/org.acme.Lib.test(Lib.java:80)";
	/**
	 * <code>MyClass</code> class is on the application class path.
	 * @see StackTraceElement#toString()
	 */
	static final String ELEMENT_STRING_APPLICATION_CLASSPATH = "MyClass.mash(MyClass.java:9)";

	private final StackTrace classVariableStackTrace = new StackTrace();

	/**
	 * @see StackTrace.ElementRegEx#PATTERN
	 * @see #ELEMENT_STRING
	 */
	@Test
	void testStackTraceElementPattern() {
		assertThat(
				"No class loader and unnamed module; should not allow trailing slash if neither are present, but currently must allow as side effect of current regex.",
				findMatch(ElementRegEx.PATTERN, "/com.foo.bar.App.run(App.java:12)"), isPresent());
		assertThat("Initial spaces not allowed.", findMatch(ElementRegEx.PATTERN, " " + ELEMENT_STRING), is(Optional.empty()));
		assertThat("Trailing spaces not allowed.", findMatch(ElementRegEx.PATTERN, ELEMENT_STRING + " "), is(Optional.empty()));
		final Matcher matcher = findMatch(ElementRegEx.PATTERN, ELEMENT_STRING).orElseThrow(AssertionError::new);
		assertThat(ElementRegEx.CLASS_LOADER_NAME_GROUP.findIn(matcher), isPresentAndIs("com.foo.loader"));
		assertThat(ElementRegEx.MODULE_NAME_GROUP.findIn(matcher), isPresentAndIs("foo"));
		assertThat(ElementRegEx.MODULE_VERSION_GROUP.findIn(matcher), isPresentAndIs("9.0"));
		assertThat(ElementRegEx.CLASS_NAME_GROUP.findIn(matcher), isPresentAndIs("com.foo.Main"));
		assertThat(ElementRegEx.METHOD_NAME_GROUP.findIn(matcher), isPresentAndIs("run"));
		assertThat(ElementRegEx.FILE_NAME_GROUP.findIn(matcher), isPresentAndIs("Main.java"));
		assertThat(ElementRegEx.LINE_NUMBER_GROUP.findIn(matcher), isPresentAndIs("101"));
	}

	/**
	 * @see StackTrace.ElementRegEx#PATTERN
	 * @see #ELEMENT_STRING_NO_LINE_NUMBER
	 */
	@Test
	void testStackTraceElementPatternNoLineNumber() {
		final Matcher matcher = findMatch(ElementRegEx.PATTERN, ELEMENT_STRING_NO_LINE_NUMBER).orElseThrow(AssertionError::new);
		assertThat(ElementRegEx.CLASS_LOADER_NAME_GROUP.findIn(matcher), isPresentAndIs("com.foo.loader"));
		assertThat(ElementRegEx.MODULE_NAME_GROUP.findIn(matcher), isPresentAndIs("foo"));
		assertThat(ElementRegEx.MODULE_VERSION_GROUP.findIn(matcher), isPresentAndIs("9.0"));
		assertThat(ElementRegEx.CLASS_NAME_GROUP.findIn(matcher), isPresentAndIs("com.foo.Main"));
		assertThat(ElementRegEx.METHOD_NAME_GROUP.findIn(matcher), isPresentAndIs("run"));
		assertThat(ElementRegEx.FILE_NAME_GROUP.findIn(matcher), isPresentAndIs("Main.java"));
		assertThat(ElementRegEx.LINE_NUMBER_GROUP.findIn(matcher), is(Optional.empty()));
	}

	/**
	 * @see StackTrace.ElementRegEx#PATTERN
	 * @see #ELEMENT_STRING_UNKNOWN_SOURCE
	 */
	@Test
	void testStackTraceElementPatternUnknownSource() {
		final Matcher matcher = findMatch(ElementRegEx.PATTERN, ELEMENT_STRING_UNKNOWN_SOURCE).orElseThrow(AssertionError::new);
		assertThat(ElementRegEx.CLASS_LOADER_NAME_GROUP.findIn(matcher), isPresentAndIs("com.foo.loader"));
		assertThat(ElementRegEx.MODULE_NAME_GROUP.findIn(matcher), isPresentAndIs("foo"));
		assertThat(ElementRegEx.MODULE_VERSION_GROUP.findIn(matcher), isPresentAndIs("9.0"));
		assertThat(ElementRegEx.CLASS_NAME_GROUP.findIn(matcher), isPresentAndIs("com.foo.Main"));
		assertThat(ElementRegEx.METHOD_NAME_GROUP.findIn(matcher), isPresentAndIs("run"));
		assertThat(ElementRegEx.FILE_NAME_GROUP.findIn(matcher), isPresentAndIs(ELEMENT_FILE_NAME_UNKNOWN_SOURCE));
		assertThat(ElementRegEx.LINE_NUMBER_GROUP.findIn(matcher), is(Optional.empty()));
	}

	/**
	 * @see StackTrace.ElementRegEx#PATTERN
	 * @see #ELEMENT_STRING_NATIVE_METHOD
	 */
	@Test
	void testStackTraceElementPatternNativeMethod() {
		final Matcher matcher = findMatch(ElementRegEx.PATTERN, ELEMENT_STRING_NATIVE_METHOD).orElseThrow(AssertionError::new);
		assertThat(ElementRegEx.CLASS_LOADER_NAME_GROUP.findIn(matcher), isPresentAndIs("com.foo.loader"));
		assertThat(ElementRegEx.MODULE_NAME_GROUP.findIn(matcher), isPresentAndIs("foo"));
		assertThat(ElementRegEx.MODULE_VERSION_GROUP.findIn(matcher), isPresentAndIs("9.0"));
		assertThat(ElementRegEx.CLASS_NAME_GROUP.findIn(matcher), isPresentAndIs("com.foo.Main"));
		assertThat(ElementRegEx.METHOD_NAME_GROUP.findIn(matcher), isPresentAndIs("run"));
		assertThat(ElementRegEx.FILE_NAME_GROUP.findIn(matcher), isPresentAndIs(ELEMENT_FILE_NAME_NATIVE_METHOD));
		assertThat(ElementRegEx.LINE_NUMBER_GROUP.findIn(matcher), is(Optional.empty()));
	}

	/**
	 * @see StackTrace.ElementRegEx#PATTERN
	 * @see #ELEMENT_STRING_UNNAMED_MODULE
	 */
	@Test
	void testStackTraceElementPatternUnnamedModule() {
		final Matcher matcher = findMatch(ElementRegEx.PATTERN, ELEMENT_STRING_UNNAMED_MODULE).orElseThrow(AssertionError::new);
		assertThat(ElementRegEx.CLASS_LOADER_NAME_GROUP.findIn(matcher), isPresentAndIs("com.foo.loader"));
		assertThat(ElementRegEx.MODULE_NAME_GROUP.findIn(matcher), is(Optional.empty()));
		assertThat(ElementRegEx.MODULE_VERSION_GROUP.findIn(matcher), is(Optional.empty()));
		assertThat(ElementRegEx.CLASS_NAME_GROUP.findIn(matcher), isPresentAndIs("com.foo.bar.App"));
		assertThat(ElementRegEx.METHOD_NAME_GROUP.findIn(matcher), isPresentAndIs("run"));
		assertThat(ElementRegEx.FILE_NAME_GROUP.findIn(matcher), isPresentAndIs("App.java"));
		assertThat(ElementRegEx.LINE_NUMBER_GROUP.findIn(matcher), isPresentAndIs("12"));
	}

	/**
	 * @see StackTrace.ElementRegEx#PATTERN
	 * @see #ELEMENT_STRING_BUILT_IN_CLASSS_LOADER
	 */
	@Test
	void testStackTraceElementPatternBuiltInClassLoader() {
		final Matcher matcher = findMatch(ElementRegEx.PATTERN, ELEMENT_STRING_BUILT_IN_CLASSS_LOADER).orElseThrow(AssertionError::new);
		assertThat(ElementRegEx.CLASS_LOADER_NAME_GROUP.findIn(matcher), is(Optional.empty()));
		assertThat(ElementRegEx.MODULE_NAME_GROUP.findIn(matcher), isPresentAndIs("acme"));
		assertThat(ElementRegEx.MODULE_VERSION_GROUP.findIn(matcher), isPresentAndIs("2.1"));
		assertThat(ElementRegEx.CLASS_NAME_GROUP.findIn(matcher), isPresentAndIs("org.acme.Lib"));
		assertThat(ElementRegEx.METHOD_NAME_GROUP.findIn(matcher), isPresentAndIs("test"));
		assertThat(ElementRegEx.FILE_NAME_GROUP.findIn(matcher), isPresentAndIs("Lib.java"));
		assertThat(ElementRegEx.LINE_NUMBER_GROUP.findIn(matcher), isPresentAndIs("80"));
	}

	/**
	 * @see StackTrace.ElementRegEx#PATTERN
	 * @see #ELEMENT_STRING_APPLICATION_CLASSPATH
	 */
	@Test
	void testStackTraceElementPatternApplicationClasspath() {
		final Matcher matcher = findMatch(ElementRegEx.PATTERN, ELEMENT_STRING_APPLICATION_CLASSPATH).orElseThrow(AssertionError::new);
		assertThat(ElementRegEx.CLASS_LOADER_NAME_GROUP.findIn(matcher), is(Optional.empty()));
		assertThat(ElementRegEx.MODULE_NAME_GROUP.findIn(matcher), is(Optional.empty()));
		assertThat(ElementRegEx.MODULE_VERSION_GROUP.findIn(matcher), is(Optional.empty()));
		assertThat(ElementRegEx.CLASS_NAME_GROUP.findIn(matcher), isPresentAndIs("MyClass"));
		assertThat(ElementRegEx.METHOD_NAME_GROUP.findIn(matcher), isPresentAndIs("mash"));
		assertThat(ElementRegEx.FILE_NAME_GROUP.findIn(matcher), isPresentAndIs("MyClass.java"));
		assertThat(ElementRegEx.LINE_NUMBER_GROUP.findIn(matcher), isPresentAndIs("9"));
	}

	/** @see StackTrace#parseElement(CharSequence) */
	@Test
	void testParseElement() {
		assertThat(StackTrace.parseElement(ELEMENT_STRING), is(new StackTraceElement("com.foo.loader", "foo", "9.0", "com.foo.Main", "run", "Main.java", 101)));
		assertThat(StackTrace.parseElement(ELEMENT_STRING_NO_LINE_NUMBER),
				is(new StackTraceElement("com.foo.loader", "foo", "9.0", "com.foo.Main", "run", "Main.java", ELEMENT_LINE_NUMBER_UNKNOWN)));
		assertThat(StackTrace.parseElement(ELEMENT_STRING_UNKNOWN_SOURCE),
				is(new StackTraceElement("com.foo.loader", "foo", "9.0", "com.foo.Main", "run", ELEMENT_FILE_NAME_UNKNOWN_SOURCE, ELEMENT_LINE_NUMBER_UNKNOWN)));
		assertThat(StackTrace.parseElement(ELEMENT_STRING_NATIVE_METHOD),
				is(new StackTraceElement("com.foo.loader", "foo", "9.0", "com.foo.Main", "run", ELEMENT_FILE_NAME_NATIVE_METHOD, ELEMENT_LINE_NUMBER_NATIVE_METHOD)));
		assertThat(StackTrace.parseElement(ELEMENT_STRING_UNNAMED_MODULE),
				is(new StackTraceElement("com.foo.loader", null, null, "com.foo.bar.App", "run", "App.java", 12)));
		assertThat(StackTrace.parseElement(ELEMENT_STRING_BUILT_IN_CLASSS_LOADER),
				is(new StackTraceElement(null, "acme", "2.1", "org.acme.Lib", "test", "Lib.java", 80)));
		assertThat(StackTrace.parseElement(ELEMENT_STRING_APPLICATION_CLASSPATH),
				is(new StackTraceElement(null, null, null, "MyClass", "mash", "MyClass.java", 9)));
	}

	@Test
	void testStackElements() {
		final StackTraceElement[] stackTraceElements = new Throwable().getStackTrace();
		assertThat("Stack trace elements not the length expected.", new StackTrace().getStackTraceElements().size(), equalTo(stackTraceElements.length));
	}

	@Test
	void testEqualsMethod() {
		final StackTrace methodStackTrace1 = new StackTrace();
		final StackTrace methodStackTrace2 = new StackTrace();
		assertTrue(methodStackTrace1.isCurrentMethodIntersected(methodStackTrace2));
		assertFalse(methodStackTrace1.isCurrentMethodIntersected(classVariableStackTrace));
	}

}
