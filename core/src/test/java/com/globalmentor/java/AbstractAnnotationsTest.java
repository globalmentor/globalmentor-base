/*
 * Copyright © 2024-2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static java.util.stream.Collectors.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.*;

import org.jspecify.annotations.*;

import org.junit.jupiter.api.*;

import com.globalmentor.reflect.AnnotatedElementsTest;

/**
 * Abstract base test for consistency in testing implementations of {@link Annotations}. Subclasses must implement {@link #getMethodTestAnnotations(String)} and
 * {@link #getTypeTestAnnotations(String)} to return instances of {@link Annotations} to test.
 * @implNote These tests usually require a class with annotations such as those in parallel to those in {@link AnnotatedElementsTest}.
 * @author Garret Wilson
 * @see AnnotatedElementsTest
 */
public abstract class AbstractAnnotationsTest {

	/** Tests for {@link Annotations#isAnnotationPresent(Class)}. */
	@Test
	void testIsAnnotationPresentOfAnnotatedElement() {
		assertThat("single annotation", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").isAnnotationPresent(TestAnnotation.class), is(true));
		assertThat("annotation with value", getMethodTestAnnotations("methodWithAnnotationHavingValue").isAnnotationPresent(TestAnnotationWithValue.class),
				is(true));
		assertThat("not present", getMethodTestAnnotations("methodWithAnnotationHavingValue").isAnnotationPresent(TestAnnotation.class), is(false));
	}

	/** Tests for {@link Annotations#getWhichAnnotationTypesPresent(Set)}. */
	@Test
	void testGetWhichAnnotationTypesPresent() {
		assertThat("empty query", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").getWhichAnnotationTypesPresent(Set.of()), is(empty()));
		assertThat("not present", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").getWhichAnnotationTypesPresent(Set.of(AnotherTestAnnotation.class)),
				is(empty()));
		assertThat("single match", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").getWhichAnnotationTypesPresent(Set.of(TestAnnotation.class)),
				containsInAnyOrder(TestAnnotation.class));
		assertThat("one of several", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").getWhichAnnotationTypesPresent(
				Set.of(AnotherTestAnnotation.class, TestAnnotation.class, TestAnnotationWithValue.class)), containsInAnyOrder(TestAnnotation.class));

		assertThat("empty query", getMethodTestAnnotations("methodWithAnnotationsHavingNoValue").getWhichAnnotationTypesPresent(Set.of()), is(empty()));
		assertThat("not present",
				getMethodTestAnnotations("methodWithAnnotationsHavingNoValue").getWhichAnnotationTypesPresent(Set.of(TestAnnotationWithValue.class)), is(empty()));
		assertThat("both match", getMethodTestAnnotations("methodWithAnnotationsHavingNoValue").getWhichAnnotationTypesPresent(
				Set.of(TestAnnotation.class, AnotherTestAnnotation.class)), containsInAnyOrder(TestAnnotation.class, AnotherTestAnnotation.class));
		assertThat("two of several",
				getMethodTestAnnotations("methodWithAnnotationsHavingNoValue")
						.getWhichAnnotationTypesPresent(Set.of(AnotherTestAnnotation.class, TestAnnotation.class, TestAnnotationWithValue.class)),
				containsInAnyOrder(TestAnnotation.class, AnotherTestAnnotation.class));

		assertThat("empty query", getMethodTestAnnotations("methodWithAnnotationHavingValue").getWhichAnnotationTypesPresent(Set.of()), is(empty()));
		assertThat("not present", getMethodTestAnnotations("methodWithAnnotationHavingValue").getWhichAnnotationTypesPresent(Set.of(TestAnnotation.class)),
				is(empty()));
		assertThat("single match",
				getMethodTestAnnotations("methodWithAnnotationHavingValue").getWhichAnnotationTypesPresent(Set.of(TestAnnotationWithValue.class)),
				containsInAnyOrder(TestAnnotationWithValue.class));
		assertThat("one of several", getMethodTestAnnotations("methodWithAnnotationHavingValue").getWhichAnnotationTypesPresent(
				Set.of(AnotherTestAnnotation.class, TestAnnotation.class, TestAnnotationWithValue.class)), containsInAnyOrder(TestAnnotationWithValue.class));
	}

	/** Tests for {@link Annotations#findAnnotationValue(Class)}. */
	@Test
	void testFindAnnotationValueOfAnnotatedElement() {
		assertThat("no value element", getMethodTestAnnotations("methodWithAnnotationHavingValue").findAnnotationValue(TestAnnotation.class), is(Optional.empty()));
		assertThat("value element", getMethodTestAnnotations("methodWithAnnotationHavingValue").findAnnotationValue(TestAnnotationWithValue.class),
				isPresentAndIs("value for testing"));
	}

	/** Tests for {@link Annotations#findAnnotation(Class)}. */
	@Test
	void testFindAnnotation() {
		assertThat("present", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").findAnnotation(TestAnnotation.class), isPresent());
		assertThat("not present", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").findAnnotation(AnotherTestAnnotation.class), isEmpty());
		final var foundAnnotationWithValue = getMethodTestAnnotations("methodWithAnnotationHavingValue").findAnnotation(TestAnnotationWithValue.class);
		assertThat("annotation with value is present", foundAnnotationWithValue, isPresent());
		assertThat("value element", foundAnnotationWithValue.orElseThrow().value(), is("value for testing"));
	}

	/** Tests for {@link Annotations#annotationsByType(Class)}. */
	@Test
	void testAnnotationsByType() {
		assertThat("single annotation", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").annotationsByType(TestAnnotation.class).count(), is(1L));
		assertThat("not present", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").annotationsByType(AnotherTestAnnotation.class).count(), is(0L));
		assertThat("repeatable annotations", getMethodTestAnnotations("methodWithRepeatableAnnotations").annotationsByType(Tag.class).count(), is(2L));
		assertThat("repeatable annotation values",
				getMethodTestAnnotations("methodWithRepeatableAnnotations").annotationsByType(Tag.class).map(Tag::value).collect(toList()),
				contains("first", "second"));
	}

	/** Tests for {@link Annotations#findDeclaredAnnotation(Class)}. */
	@Test
	void testFindDeclaredAnnotation() {
		assertThat("directly present", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").findDeclaredAnnotation(TestAnnotation.class), isPresent());
		assertThat("not present", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").findDeclaredAnnotation(AnotherTestAnnotation.class), isEmpty());
		final var foundDeclaredAnnotationWithValue = getMethodTestAnnotations("methodWithAnnotationHavingValue")
				.findDeclaredAnnotation(TestAnnotationWithValue.class);
		assertThat("annotation with value is directly present", foundDeclaredAnnotationWithValue, isPresent());
		assertThat("value element", foundDeclaredAnnotationWithValue.orElseThrow().value(), is("value for testing"));
		assertThat("inherited annotation not declared",
				getTypeTestAnnotations("ChildTestAnnotatedInterface").findDeclaredAnnotation(InheritableTestAnnotation.class), isEmpty());
	}

	/** Tests for {@link Annotations#declaredAnnotationsByType(Class)}. */
	@Test
	void testDeclaredAnnotationsByType() {
		assertThat("single annotation", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").declaredAnnotationsByType(TestAnnotation.class).count(),
				is(1L));
		assertThat("not present", getMethodTestAnnotations("methodWithAnnotationHavingNoValue").declaredAnnotationsByType(AnotherTestAnnotation.class).count(),
				is(0L));
		assertThat("repeatable annotations", getMethodTestAnnotations("methodWithRepeatableAnnotations").declaredAnnotationsByType(Tag.class).count(), is(2L));
		assertThat("repeatable annotation values",
				getMethodTestAnnotations("methodWithRepeatableAnnotations").declaredAnnotationsByType(Tag.class).map(Tag::value).collect(toList()),
				contains("first", "second"));
		assertThat("inherited annotation not declared",
				getTypeTestAnnotations("ChildTestAnnotatedInterface").declaredAnnotationsByType(InheritableTestAnnotation.class).count(), is(0L));
	}

	/**
	 * Abstract strategy method for retrieving an implementation-specific instance of {@link Annotations} of a method for testing.
	 * @param methodName The name of the method for which test annotations should be returned.
	 * @return Method annotations to be tested.
	 */
	protected abstract Annotations getMethodTestAnnotations(@NonNull String methodName);

	/**
	 * Abstract strategy method for retrieving an implementation-specific instance of {@link Annotations} of a type for testing.
	 * @param typeName The simple name of the type for which test annotations should be returned.
	 * @return Type annotations to be tested.
	 */
	protected abstract Annotations getTypeTestAnnotations(@NonNull String typeName);

}
