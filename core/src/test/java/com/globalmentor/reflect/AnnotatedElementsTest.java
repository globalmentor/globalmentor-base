/*
 * Copyright © 2024 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.reflect;

import static com.github.npathai.hamcrestopt.OptionalMatchers.*;
import static com.globalmentor.reflect.AnnotatedElements.annotationsOf;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.lang.reflect.AnnotatedElement;
import java.util.Optional;

import org.junit.jupiter.api.*;

import com.globalmentor.java.Annotations;

/**
 * Tests of {@link AnnotatedElement} and its implementation of {@link Annotations}.
 * @author Garret Wilson
 */
public class AnnotatedElementsTest {

	/** Tests {@link Annotations#isAnnotationPresent(Class)} of the implementation for {@link AnnotatedElement}. */
	@Test
	void testIsAnnotationPresentOfAnnotatedElement() throws NoSuchMethodException, SecurityException {
		assertThat(annotationsOf(TestAnnotatedInterface.class.getMethod("methodWithAnnotationHavingNoValue")).isAnnotationPresent(BeforeEach.class), is(true));
		assertThat(annotationsOf(TestAnnotatedInterface.class.getMethod("methodWithAnnotationHavingValue")).isAnnotationPresent(Disabled.class), is(true));
		assertThat(annotationsOf(TestAnnotatedInterface.class.getMethod("methodWithAnnotationHavingValue")).isAnnotationPresent(BeforeEach.class), is(false));
	}

	/** Tests {@link Annotations#findAnnotationValue(Class)} of the implementation for {@link AnnotatedElement}. */
	@Test
	void testFindAnnotationValueOfAnnotatedElement() throws NoSuchMethodException, SecurityException {
		assertThat(annotationsOf(TestAnnotatedInterface.class.getMethod("methodWithAnnotationHavingValue")).findAnnotationValue(BeforeEach.class),
				is(Optional.empty()));
		assertThat(annotationsOf(TestAnnotatedInterface.class.getMethod("methodWithAnnotationHavingValue")).findAnnotationValue(Disabled.class),
				isPresentAndIs("value for testing"));
	}

	/**
	 * Interface for testing annotations.
	 * @apiNote The annotations here have no semantic significance; they are only for testing annotation access.
	 */
	interface TestAnnotatedInterface {

		@BeforeEach
		public void methodWithAnnotationHavingNoValue();

		@Disabled("value for testing")
		public void methodWithAnnotationHavingValue();

	}

}
