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

import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Objects.*;
import static java.util.function.Function.*;
import static java.util.stream.Collectors.*;

import java.lang.annotation.Annotation;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

import org.jspecify.annotations.*;
import javax.lang.model.element.*;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.util.*;

import com.globalmentor.java.Annotations;

/**
 * Utilities for working with Java model {@link Element} and related classes.
 * @author Garret Wilson
 * @apiNote Definitions of "declared", "present", "associated", etc. annotations can be found in {@link java.lang.reflect.AnnotatedElement}.
 * @see Elements
 * @see Types
 * @see ModelTypes
 */
public final class ModelElements {

	private ModelElements() {
	}

	/**
	 * Finds a type element from a class if the type element is uniquely determinable in the environment.
	 * @implSpec This implementation delegates to {@link #findTypeElementForCanonicalName(Elements, CharSequence)}.
	 * @param elements The element utilities.
	 * @param clazz The class for which a type element is to be found.
	 * @return The type element for the class, which will not be present if no type element can be uniquely determined.
	 * @see javax.annotation.processing.ProcessingEnvironment#getElementUtils()
	 * @see Class#getCanonicalName()
	 */
	public static Optional<TypeElement> findTypeElementForClass(@NonNull final Elements elements, @NonNull final Class<?> clazz) { //TODO create module-related variations as well
		return findTypeElementForCanonicalName(elements, clazz.getCanonicalName());
	}

	/**
	 * Finds a type element given its canonical name if the type element is uniquely determinable in the environment.
	 * @apiNote This method functions identically to {@link Elements#getTypeElement(CharSequence)} except that it returns an {@link Optional} and never
	 *          <code>null</code>.
	 * @implSpec This implementation delegates to {@link Elements#getTypeElement(CharSequence)}.
	 * @param elements The element utilities.
	 * @param canonicalName The canonical name of the type element to return.
	 * @return The named type element, which will not be present if no type element can be uniquely determined.
	 * @see javax.annotation.processing.ProcessingEnvironment#getElementUtils()
	 */
	public static Optional<TypeElement> findTypeElementForCanonicalName(@NonNull final Elements elements, @NonNull final CharSequence canonicalName) { //TODO create module-related variations as well
		return Optional.ofNullable(elements.getTypeElement(canonicalName));
	}

	//annotations

	/**
	 * Retrieves a <em>present</em> annotation mirror from an element for annotations of a particular type.
	 * @implSpec This implementation does not check for repeated annotations.
	 * @implSpec This implementation calls {@link #elementAnnotationMirrorsForClass(Elements, Element, Class)}
	 * @param elements The element utilities.
	 * @param element The element such as representing a type or method potentially annotated with the specified annotation.
	 * @param annotationClass The type of annotation to find.
	 * @return The mirror for the present annotation annotating the indicated type, if any.
	 * @see Elements#getAllAnnotationMirrors(Element)
	 */
	public static Optional<? extends AnnotationMirror> findElementAnnotationMirrorForClass(@NonNull final Elements elements, @NonNull final Element element,
			@NonNull final Class<? extends Annotation> annotationClass) {
		return elementAnnotationMirrorsForClass(elements, element, annotationClass).findAny();
	}

	/**
	 * Retrieves all the <em>present</em> annotation mirrors from a type element for annotations of a particular type.
	 * @param elements The element utilities.
	 * @param element The element such as representing a type or method potentially annotated with the specified annotation.
	 * @param annotationClass The type of annotation to find.
	 * @return The mirrors for the annotation(s) annotating the indicated type, if any.
	 * @see Elements#getAllAnnotationMirrors(Element)
	 */
	public static Stream<? extends AnnotationMirror> elementAnnotationMirrorsForClass(@NonNull final Elements elements, @NonNull final Element element,
			@NonNull final Class<? extends Annotation> annotationClass) {
		return elementAnnotationMirrorsForClass(element, annotationClass, e -> elements.getAllAnnotationMirrors(e));
	}

	/**
	 * Retrieves a <em>declared</em> annotation mirror from an element for annotations of a particular type.
	 * @implSpec This implementation does not check for repeated annotations.
	 * @implSpec This implementation calls {@link #elementDeclaredAnnotationMirrorsForClass(Element, Class)}
	 * @param element The element such as representing a type or method potentially annotated with the specified annotation.
	 * @param annotationClass The type of annotation to find.
	 * @return The mirror for the declared annotation annotating the indicated type, if any.
	 * @see Element#getAnnotationMirrors()
	 */
	public static Optional<? extends AnnotationMirror> findElementDeclaredAnnotationMirrorForClass(@NonNull final Element element,
			@NonNull final Class<? extends Annotation> annotationClass) {
		return elementDeclaredAnnotationMirrorsForClass(element, annotationClass).findAny();
	}

	/**
	 * Retrieves all the <em>declared</em> annotation mirrors from a type element for annotations of a particular type.
	 * @param element The element such as representing a type or method potentially annotated with the specified annotation.
	 * @param annotationClass The type of annotation to find.
	 * @return The mirrors for the annotation(s) annotating the indicated type, if any.
	 * @see Element#getAnnotationMirrors()
	 */
	public static Stream<? extends AnnotationMirror> elementDeclaredAnnotationMirrorsForClass(@NonNull final Element element,
			@NonNull final Class<? extends Annotation> annotationClass) {
		return elementAnnotationMirrorsForClass(element, annotationClass, Element::getAnnotationMirrors);
	}

	/**
	 * Retrieves the annotation mirrors from a type element for annotations of a particular type, using a supplier of annotation mirrors.
	 * @apiNote This method exists to consolidate code that will work with either <em>present</em> or <em>declared</em> annotations.
	 * @param element The element such as representing a type or method potentially annotated with the specified annotation.
	 * @param annotationClass The type of annotation to find.
	 * @param annotationMirrorsStrategy The strategy for listing the annotation mirrors for the element.
	 * @return The mirrors for the annotation(s) annotating the indicated type, if any.
	 */
	private static Stream<? extends AnnotationMirror> elementAnnotationMirrorsForClass(@NonNull final Element element,
			@NonNull final Class<? extends Annotation> annotationClass,
			@NonNull final Function<Element, ? extends List<? extends AnnotationMirror>> annotationMirrorsStrategy) {
		final String canonicalName = annotationClass.getCanonicalName();
		checkArgument(canonicalName != null, "Annotation class `%s` has no canonical name.", annotationClass.getName()); //check for completeness; not realistically possible: an annotation cannot be defined as an anonymous inner class
		return annotationMirrorsStrategy.apply(element).stream().filter(annotationMirror -> {
			final Element annotationElement = annotationMirror.getAnnotationType().asElement();
			assert annotationElement instanceof TypeElement : "An annotation mirror type's element should always be a `TypeElement`.";
			return ((TypeElement)annotationElement).getQualifiedName().contentEquals(canonicalName);
		});
	}

	/**
	 * Returns all interfaces of a type element annotated with the given annotation. This method finds interfaced with the annotation <em>present</em> (i.e.
	 * inheritance is supported).
	 * @param elements The element utilities.
	 * @param types The type utilities.
	 * @param typeElement The type element representing the type potentially having an interface annotated with the specified annotation.
	 * @param annotationClass The type of annotation to look for.
	 * @return The interfaces of the type element which are in turn annotated with the given annotation.
	 */
	public static Stream<DeclaredType> elementInterfacesAnnotatedWith(@NonNull final Elements elements, @NonNull final Types types,
			@NonNull final TypeElement typeElement, @NonNull final Class<? extends Annotation> annotationClass) {
		return typeElement.getInterfaces().stream().flatMap(asInstances(DeclaredType.class))
				.filter(interfaceType -> findElementAnnotationMirrorForClass(elements, (TypeElement)interfaceType.asElement(), annotationClass).isPresent());
	}

	/**
	 * Creates an abstraction implementation for a model element.
	 * @param elements The element utilities.
	 * @param element The element such as representing a type or method for which an abstraction should be created.
	 * @return An adapter of the type element to an annotations abstraction interface.
	 */
	public static Annotations annotationsOf(@NonNull final Elements elements, @NonNull final Element element) {
		return new Annotations() {

			@Override
			public boolean isAnnotationPresent(final Class<? extends Annotation> annotationClass) {
				return findElementAnnotationMirrorForClass(elements, element, annotationClass).isPresent();
			}

			@Override
			public Set<Class<? extends Annotation>> getWhichAnnotationTypesPresent(final Set<Class<? extends Annotation>> annotationClasses) {
				final Map<String, Class<? extends Annotation>> annotationClassesByCanonicalName = annotationClasses.stream()
						.collect(toUnmodifiableMap(Class::getCanonicalName, identity()));
				//Get the qualified names of the annotation mirrors present on the element.
				return elements.getAllAnnotationMirrors(element).stream().map(annotationMirror -> {
					final Element annotationElement = annotationMirror.getAnnotationType().asElement();
					assert annotationElement instanceof TypeElement : "An annotation mirror type's element should always be a `TypeElement`.";
					return ((TypeElement)annotationElement).getQualifiedName().toString();
				})
						//Look up the annotation class by the annotation element's qualified name.
						//The `Name` interface implies that we can compare string forms;
						//`Name.contentEquals(CharSequence)` seems to be more of a utility
						//method as a parallel to the same method in `String`.
						.map(annotationClassesByCanonicalName::get).filter(Objects::nonNull).collect(toUnmodifiableSet());
			}

			@Override
			public Optional<Object> findAnnotationValue(final Class<? extends Annotation> annotationClass) {
				return findElementAnnotationMirrorForClass(elements, element, annotationClass) //
						.flatMap(ModelTypes::findAnnotationValueElementValue) //
						.map(AnnotationValue::getValue);
			}

		};
	}

}
