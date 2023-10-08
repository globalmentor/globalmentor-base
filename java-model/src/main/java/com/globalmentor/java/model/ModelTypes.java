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

package com.globalmentor.java.model;

import static java.util.Objects.*;

import java.util.*;
import java.util.function.Predicate;

import javax.annotation.*;
import javax.lang.model.element.*;
import javax.lang.model.type.*;
import javax.lang.model.util.*;

/**
 * Utilities for working with Java model {@link TypeMirror} and classes.
 * @author Garret Wilson
 * @see Elements
 * @see Types
 * @see ModelElements
 */
public class ModelTypes {

	/**
	 * The simple name of the special <code>value</code> annotation element allowing for shorthand value notation.
	 * @see <a href="https://docs.oracle.com/javase/specs/jls/se21/html/jls-9.html#jls-9.7.3">The Java® Language Specification, Java SE 21 Edition § 9.7.3.
	 *      Single-Element Annotations</a>
	 */
	private static final String ANNOTATION_VALUE_ELEMENT_SIMPLE_NAME = "value";

	/**
	 * Finds the annotation value mapped to the {@value #ANNOTATION_VALUE_ELEMENT_SIMPLE_NAME} element from an annotation mirror.
	 * @apiNote The {@value #ANNOTATION_VALUE_ELEMENT_SIMPLE_NAME} element is the special element which allows the element value to be left out in the source
	 *          file.
	 * @implSpec This implementation delegates to {@link #findAnnotationElementValueBySimpleName(AnnotationMirror, CharSequence)}.
	 * @param annotationMirror The annotation mirror in which to look up an element value.
	 * @return The annotation value if found.
	 * @see <a href="https://docs.oracle.com/javase/specs/jls/se21/html/jls-9.html#jls-9.7.3">The Java® Language Specification, Java SE 21 Edition § 9.7.3.
	 *      Single-Element Annotations</a>
	 */
	public static Optional<? extends AnnotationValue> findAnnotationValueElementValue(@Nonnull AnnotationMirror annotationMirror) {
		return findAnnotationElementValueBySimpleName(annotationMirror, ANNOTATION_VALUE_ELEMENT_SIMPLE_NAME);
	}

	/**
	 * Finds the annotation value mapped to the executable element with the given simple name from an annotation mirror.
	 * @implSpec This implementation calls {@link AnnotationMirror#getElementValues()} and then delegates to
	 *           {@link #findElementValueBySimpleName(Map, CharSequence)}.
	 * @param annotationMirror The annotation mirror in which to look up an element value.
	 * @param simpleName The simple name (i.e. property name) of the value to retrieve.
	 * @return The annotation value if found.
	 */
	public static Optional<? extends AnnotationValue> findAnnotationElementValueBySimpleName(@Nonnull AnnotationMirror annotationMirror,
			@Nonnull final CharSequence simpleName) {
		return findElementValueBySimpleName(annotationMirror.getElementValues(), simpleName);
	}

	/**
	 * Finds the annotation value mapped to the executable element with the given simple name.
	 * @apiNote This is useful for finding a value of an {@link AnnotationMirror} from the map returned by {@link AnnotationMirror#getElementValues()}.
	 * @param elementValues The map of element values associated with their executable elements (e.g. accessor methods of an annotation mirror).
	 * @param simpleName The simple name (i.e. element name) of the value to retrieve.
	 * @return The annotation value if found.
	 * @see <a href="https://area-51.blog/2009/02/13/getting-class-values-from-annotations-in-an-annotationprocessor/">Getting Class values from Annotations in an
	 *      AnnotationProcessor</a>
	 */
	public static Optional<? extends AnnotationValue> findElementValueBySimpleName(
			@Nonnull final Map<? extends ExecutableElement, ? extends AnnotationValue> elementValues, @Nonnull final CharSequence simpleName) {
		requireNonNull(simpleName);
		return elementValues.entrySet().stream().filter(entry -> entry.getKey().getSimpleName().contentEquals(simpleName)).findAny().map(Map.Entry::getValue);
	}

	/**
	 * Tests whether a type is assignable to the type corresponding to the given class (i.e. whether instances of each would have an <code>instanceof</code>
	 * relationship).
	 * @implSpec This implementation uses the predicate from {@link #isTypeAssignableTo(Elements, Types, Class)}.
	 * @param elements The element utilities.
	 * @param types The type utilities.
	 * @param typeMirror The type to test.
	 * @param clazz The class representing the type against which to compare for assignability.
	 * @return <code>true</code> if the type is assignable to the type represented by the class.
	 * @throws IllegalArgumentException if no type could be found for the given class; or given a type for an executable, package, or module is invalid.
	 * @see Types#isAssignable(TypeMirror, TypeMirror)
	 */
	public static boolean isTypeAssignableTo(@Nonnull final Elements elements, @Nonnull final Types types, @Nonnull final TypeMirror typeMirror,
			@Nonnull final Class<?> clazz) {
		return isTypeAssignableTo(elements, types, clazz).test(typeMirror);
	}

	/**
	 * Returns a predicate for testing whether a type is assignable to the type corresponding to the given class (i.e. whether instances of each would have an
	 * <code>instanceof</code> relationship).
	 * @apiNote This factory method is useful for creating the test once and using multiple times.
	 * @implSpec This implementation calls {@link #findDeclaredType(Elements, Types, Class, TypeMirror...)}.
	 * @param elements The element utilities.
	 * @param types The type utilities.
	 * @param clazz The class representing the type against which to compare for assignability.
	 * @return <code>true</code> if the type is assignable to the type represented by the class.
	 * @throws IllegalArgumentException if no type could be found for the given class; or given a type for an executable, package, or module is invalid.
	 * @see Types#isAssignable(TypeMirror, TypeMirror)
	 */
	public static Predicate<TypeMirror> isTypeAssignableTo(@Nonnull final Elements elements, @Nonnull final Types types, @Nonnull final Class<?> clazz) {
		final TypeMirror classType = findDeclaredType(elements, types, clazz)
				.orElseThrow(() -> new IllegalArgumentException("No declared type found for class `%s`.`".formatted(clazz.getName())));
		return type -> types.isAssignable(type, classType);
	}

	/**
	 * Finds a type corresponding to a class type element and actual type arguments, if the type element is uniquely determinable in the environment.
	 * @implSpec This implementation delegates to {@link ModelElements#findTypeElementForClass(Elements, Class)} followed by
	 *           {@link Types#getDeclaredType(TypeElement, TypeMirror...)}.
	 * @param elements The element utilities.
	 * @param types The type utilities.
	 * @param clazz The class for which a type element is to be found.
	 * @param typeArgs The actual type arguments.
	 * @return The type element for the class, which will not be present if no type element can be uniquely determined.
	 * @see javax.annotation.processing.ProcessingEnvironment#getElementUtils()
	 * @see Class#getCanonicalName()
	 * @see Elements#getTypeElement(CharSequence)
	 * @see Types#getDeclaredType(TypeElement, TypeMirror...)
	 */
	public static Optional<DeclaredType> findDeclaredType(@Nonnull final Elements elements, @Nonnull final Types types, @Nonnull final Class<?> clazz,
			@Nonnull final TypeMirror... typeArgs) {
		return ModelElements.findTypeElementForClass(elements, clazz).map(typeElement -> types.getDeclaredType(typeElement, typeArgs));
	}

	/**
	 * Finds a type corresponding to a class type element with an unbounded wildcard type parameter, if the type element is uniquely determinable in the
	 * environment.
	 * @implSpec This implementation delegates to {@link #findDeclaredType(Elements, Types, Class, TypeMirror...)} using the result of
	 *           #getUnboundedWildcardType(Types).
	 * @param elements The element utilities.
	 * @param types The type utilities.
	 * @param clazz The class for which a type element is to be found.
	 * @return The type element for the class, which will not be present if no type element can be uniquely determined.
	 * @see javax.annotation.processing.ProcessingEnvironment#getElementUtils()
	 * @see Class#getCanonicalName()
	 * @see Elements#getTypeElement(CharSequence)
	 * @see Types#getDeclaredType(TypeElement, TypeMirror...)
	 * @see #getUnboundedWildcardType(Types)
	 */
	public static Optional<DeclaredType> findDeclaredTypeWithUnboundedWildcardForClass(@Nonnull final Elements elements, @Nonnull final Types types,
			@Nonnull final Class<?> clazz) {
		return findDeclaredType(elements, types, clazz, getUnboundedWildcardType(types));
	}

	/**
	 * Returns a new unbounded wildcard type ({@code <?>}).
	 * @param types The type utilities.
	 * @implSpec This implementation delegates to {@link Types#getWildcardType(TypeMirror, TypeMirror)}, passing <code>null</code> for both bounds.
	 * @return The new unbounded wildcard type.
	 */
	public static WildcardType getUnboundedWildcardType(@Nonnull final Types types) {
		return types.getWildcardType(null, null);
	}

}
