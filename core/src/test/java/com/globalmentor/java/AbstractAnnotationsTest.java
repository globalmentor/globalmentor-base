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

import static com.github.npathai.hamcrestopt.OptionalMatchers.isPresentAndIs;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.*;

import javax.annotation.*;

import org.junit.jupiter.api.*;

import com.globalmentor.reflect.AnnotatedElementsTest;

/**
 * Abstract base test for consistency in testing implementations of {@link Annotations}. Subclasses must implement {@link #getTestAnnotations()} to return an
 * instance of {@link Annotations} to test.
 * @implNote These tests usually require a class with annotations such as those in parallel those in {@link AnnotatedElementsTest}.
 * @author Garret Wilson
 * @see AnnotatedElementsTest
 */
public abstract class AbstractAnnotationsTest {

	/** Tests {@link Annotations#isAnnotationPresent(Class)}. */
	@Test
	void testIsAnnotationPresentOfAnnotatedElement() {
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingNoValue").isAnnotationPresent(BeforeEach.class), is(true));
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingValue").isAnnotationPresent(Disabled.class), is(true));
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingValue").isAnnotationPresent(BeforeEach.class), is(false));
	}

	/** Tests {@link Annotations#getWhichAnnotationTypesPresent(Class...)}. */
	@Test
	void testGetWhichAnnotationTypesPresent() {
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingNoValue").getWhichAnnotationTypesPresent(Set.of()), is(empty()));
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingNoValue").getWhichAnnotationTypesPresent(Set.of(BeforeAll.class)), is(empty()));
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingNoValue").getWhichAnnotationTypesPresent(Set.of(BeforeEach.class)),
				containsInAnyOrder(BeforeEach.class));
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingNoValue")
				.getWhichAnnotationTypesPresent(Set.of(BeforeAll.class, BeforeEach.class, AfterEach.class, Test.class)), containsInAnyOrder(BeforeEach.class));

		assertThat(getMethodTestAnnotations("methodWithAnnotationsHavingNoValue").getWhichAnnotationTypesPresent(Set.of()), is(empty()));
		assertThat(getMethodTestAnnotations("methodWithAnnotationsHavingNoValue").getWhichAnnotationTypesPresent(Set.of(BeforeAll.class)), is(empty()));
		assertThat(getMethodTestAnnotations("methodWithAnnotationsHavingNoValue").getWhichAnnotationTypesPresent(Set.of(BeforeEach.class, AfterEach.class)),
				containsInAnyOrder(BeforeEach.class, AfterEach.class));
		assertThat(getMethodTestAnnotations("methodWithAnnotationsHavingNoValue").getWhichAnnotationTypesPresent(
				Set.of(BeforeAll.class, BeforeEach.class, AfterEach.class, Test.class)), containsInAnyOrder(BeforeEach.class, AfterEach.class));

		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingValue").getWhichAnnotationTypesPresent(Set.of()), is(empty()));
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingValue").getWhichAnnotationTypesPresent(Set.of(BeforeAll.class)), is(empty()));
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingValue").getWhichAnnotationTypesPresent(Set.of(Disabled.class)),
				containsInAnyOrder(Disabled.class));
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingValue").getWhichAnnotationTypesPresent(
				Set.of(BeforeAll.class, BeforeEach.class, Disabled.class, AfterEach.class, Test.class)), containsInAnyOrder(Disabled.class));
	}

	/** Tests {@link Annotations#findAnnotationValue(Class)}. */
	@Test
	void testFindAnnotationValueOfAnnotatedElement() {
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingValue").findAnnotationValue(BeforeEach.class), is(Optional.empty()));
		assertThat(getMethodTestAnnotations("methodWithAnnotationHavingValue").findAnnotationValue(Disabled.class), isPresentAndIs("value for testing"));
	}

	/**
	 * Abstract strategy method for retrieving an implementation-specific instance of {@link Annotations} of a method for testing.
	 * @param The name of the method for which test annotations should be returned.
	 * @return Method annotations to be tested.
	 */
	protected abstract Annotations getMethodTestAnnotations(@Nonnull String methodName);

}
