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

import com.globalmentor.java.*;

/**
 * Tests of {@link AnnotatedElements} and its implementation of {@link Annotations}.
 * @author Garret Wilson
 */
public class AnnotatedElementsTest extends AbstractAnnotationsTest {

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns an {@link Annotations} implementation by delegating to
	 *           {@link AnnotatedElements#annotationsOf(java.lang.reflect.AnnotatedElement)}.
	 */
	@Override
	protected Annotations getMethodTestAnnotations(final String methodName) {
		try {
			return AnnotatedElements.annotationsOf(TestAnnotatedInterface.class.getMethod(methodName));
		} catch(final NoSuchMethodException noSuchMethodException) {
			throw new IllegalArgumentException(
					"Unknown `%s` method referenced in test: %s".formatted(TestAnnotatedInterface.class.getSimpleName(), noSuchMethodException.getMessage()),
					noSuchMethodException);
		}
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns an {@link Annotations} implementation by delegating to
	 *           {@link AnnotatedElements#annotationsOf(java.lang.reflect.AnnotatedElement)}.
	 */
	@Override
	protected Annotations getTypeTestAnnotations(final String typeName) {
		for(final Class<?> declaredClass : TestAnnotatedInterface.class.getDeclaredClasses()) {
			if(declaredClass.getSimpleName().equals(typeName)) {
				return AnnotatedElements.annotationsOf(declaredClass);
			}
		}
		throw new IllegalArgumentException("Unknown `%s` type referenced in test: %s".formatted(TestAnnotatedInterface.class.getSimpleName(), typeName));
	}

	/**
	 * Interface for testing annotations.
	 * @apiNote The annotations here have no semantic significance; they are only for testing annotation access.
	 */
	interface TestAnnotatedInterface {

		@TestAnnotation
		public void methodWithAnnotationHavingNoValue();

		@TestAnnotation
		@AnotherTestAnnotation
		public void methodWithAnnotationsHavingNoValue();

		@TestAnnotationWithValue("value for testing")
		public void methodWithAnnotationHavingValue();

		@Tag("first")
		@Tag("second")
		public void methodWithRepeatableAnnotations();

		@InheritableTestAnnotation
		interface ParentTestAnnotatedInterface {
		}

		interface ChildTestAnnotatedInterface extends ParentTestAnnotatedInterface {
		}

	}

}
