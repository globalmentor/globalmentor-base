/*
 * Copyright © 2023 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.Classes.*;
import static com.globalmentor.java.Conditions.*;
import static java.util.stream.Collectors.*;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.*;
import java.util.stream.Stream;

import javax.annotation.*;

import com.globalmentor.java.Annotations;

/**
 * Utilities for working with annotated elements.
 * @author Garret Wilson
 * @apiNote Definitions of "declared", "present", "associated", etc. can be found in {@link AnnotatedElement}.
 * @see AnnotatedElement
 */
public final class AnnotatedElements {

	private AnnotatedElements() {
	}

	/**
	 * Finds an element's annotation for the specified type.
	 * @implSpec This implementation delegates to {@link AnnotatedElement#getAnnotation(Class)}.
	 * @param <T> The type of the annotation to query for and return if present.
	 * @param annotatedElement The element that may have annotations.
	 * @param annotationClass The Class object corresponding to the annotation type.
	 * @return The element's annotation for the specified annotation type if present on this element.
	 * @throws NullPointerException if the given annotated element and/or annotation class is <code>null</code>.
	 */
	public static <T extends Annotation> Optional<T> findAnnotation(@Nonnull AnnotatedElement annotatedElement, @Nonnull final Class<T> annotationClass) {
		return Optional.ofNullable(annotatedElement.getAnnotation(annotationClass));
	}

	/**
	 * Returns annotations that are <em>present</em> on an element.
	 * @implSpec This implementation delegates to {@link AnnotatedElement#getAnnotations()}.
	 * @param annotatedElement The element that may have annotations.
	 * @return annotations present on this element
	 */
	public static Stream<Annotation> annotations(@Nonnull AnnotatedElement annotatedElement) {
		return Stream.of(annotatedElement.getAnnotations());
	}

	/**
	 * Returns annotations that are <em>associated</em> with an element.
	 * @implSpec This implementation delegates to {@link AnnotatedElement#getAnnotationsByType(Class)}.
	 * @param <T> The type of the annotation to query for and return if present.
	 * @param annotatedElement The element that may have annotations.
	 * @param annotationClass The type of annotations to return.
	 * @return All of the element's annotations for the specified annotation type if associated with the element.
	 * @throws NullPointerException if the given annotated element and/or annotation class is <code>null</code>.
	 */
	public static <T extends Annotation> Stream<T> annotationsByType(@Nonnull AnnotatedElement annotatedElement, @Nonnull final Class<T> annotationClass) {
		return Stream.of(annotatedElement.getAnnotationsByType(annotationClass));
	}

	/**
	 * Returns an element's annotation for the specified type if such an annotation is <em>directly present</em>. This method ignores inherited annotations.
	 * @implSpec This implementation delegates to {@link AnnotatedElement#getDeclaredAnnotation(Class)}.
	 * @param <T> The type of the annotation to query for and return if directly present.
	 * @param annotatedElement The element that may have annotations.
	 * @param annotationClass The type of annotations to return.
	 * @return The element's annotation for the specified annotation type if directly present on the element.
	 * @throws NullPointerException if the given annotated element and/or annotation class is <code>null</code>.
	 */
	public static <T extends Annotation> Optional<T> findDeclaredAnnotation(@Nonnull AnnotatedElement annotatedElement, @Nonnull final Class<T> annotationClass) {
		return Optional.ofNullable(annotatedElement.getDeclaredAnnotation(annotationClass));
	}

	/**
	 * Returns an element's annotation(s) for the specified type if such annotations are either <em>directly present</em> or <em>indirectly present</em>. This
	 * method ignores inherited annotations.
	 * @implSpec This implementation delegates to {@link AnnotatedElement#getDeclaredAnnotationsByType(Class)}.
	 * @param <T> The type of the annotation to query for and return if directly or indirectly present.
	 * @param annotatedElement The element that may have annotations.
	 * @param annotationClass The type of annotations to return.
	 * @return All of the element's annotations for the specified annotation type if directly or indirectly present on the element.
	 * @throws NullPointerException if the given annotated element and/or annotation class class is <code>null</code>.
	 */
	public static <T extends Annotation> Stream<T> declaredAnnotationsByType(@Nonnull AnnotatedElement annotatedElement,
			@Nonnull final Class<T> annotationClass) {
		return Stream.of(annotatedElement.getDeclaredAnnotationsByType(annotationClass));
	}

	/**
	 * Returns annotations that are <em>directly present</em> on an element. This method ignores inherited annotations.
	 * @implSpec This implementation delegates to {@link AnnotatedElement#getDeclaredAnnotations()}.
	 * @param annotatedElement The element that may have annotations.
	 * @return annotations directly present on the element.
	 */
	public static Stream<Annotation> declaredAnnotations(@Nonnull AnnotatedElement annotatedElement) {
		return Stream.of(annotatedElement.getDeclaredAnnotations());
	}

	/**
	 * Creates an abstraction implementation for an annotated element.
	 * @param annotatedElement The annotated element for which an abstraction should be created.
	 * @return An adapter of the annotated element to an annotations abstraction interface.
	 */
	public static Annotations annotationsOf(@Nonnull final AnnotatedElement annotatedElement) {
		return new Annotations() {

			@Override
			public boolean isAnnotationPresent(final Class<? extends Annotation> annotationClass) {
				return annotatedElement.isAnnotationPresent(annotationClass);
			}

			@Override
			public Set<Class<? extends Annotation>> getWhichAnnotationTypesPresent(final Set<Class<? extends Annotation>> annotationClasses) {
				return Stream.of(annotatedElement.getAnnotations()).map(Annotation::annotationType).filter(annotationClasses::contains).collect(toUnmodifiableSet());
			}

			@Override
			public Optional<Object> findAnnotationValue(final Class<? extends Annotation> annotationClass) {
				return findAnnotation(annotatedElement, annotationClass).flatMap(annotation -> {
					return findMethod(annotationClass, VALUE_ELEMENT_NAME).map(valueMethod -> {
						try {
							return valueMethod.invoke(annotation);
						} catch(final IllegalAccessException illegalAccessException) {
							throw unexpected("Found method not expected to be inaccessible.", illegalAccessException);
						} catch(final InvocationTargetException invocationTargetException) {
							final Throwable targetException = invocationTargetException.getTargetException();
							if(targetException instanceof RuntimeException targetRuntimeException) {
								throw targetRuntimeException;
							}
							throw new RuntimeException(targetException);
						}
					});
				});
			}

		};
	}

}
