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

import static com.globalmentor.collections.Sets.*;
import static java.util.stream.Collectors.*;

import java.lang.annotation.Annotation;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.jspecify.annotations.*;
import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.*;
import javax.lang.model.util.*;

/**
 * A base implementation of an annotation providing convenient access to element and type utilities.
 * @implSpec This processor by default supports the latest supported source version.
 * @author Garret Wilson
 * @see ModelElements
 * @see ModelTypes
 */
public abstract class BaseAnnotationProcessor extends AbstractProcessor {

	/**
	 * The canonical names class names representing annotation type supported by this processor. These will be merged with any annotations indicated by the
	 * {@link SupportedAnnotationTypes} annotation.
	 */
	private final Set<String> supportedAnnotationTypeCanonicalNames;

	/** Constructor specifying no specific annotation types; those may still be specified using the {@link SupportedAnnotationTypes} annotation. */
	public BaseAnnotationProcessor() {
		this(Set.of());
	}

	/**
	 * Supported annotation types constructors.
	 * @param supportedAnnotationTypes The canonical names class names representing annotation type supported by this processor, in addition to any specified by
	 *          the {@link SupportedAnnotationTypes} annotation.
	 */
	public BaseAnnotationProcessor(final Set<Class<? extends Annotation>> supportedAnnotationTypes) {
		this.supportedAnnotationTypeCanonicalNames = supportedAnnotationTypes.stream().map(Class::getCanonicalName).collect(toUnmodifiableSet());
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This processor returns the union of any supported annotation types provided in the constructor, and any specified by the
	 *           {@link SupportedAnnotationTypes} annotation.
	 */
	@Override
	public Set<String> getSupportedAnnotationTypes() {
		if(supportedAnnotationTypeCanonicalNames.isEmpty()) { //if we have no specific supported annotation types, return the default value
			return super.getSupportedAnnotationTypes(); //beyond checking for the `SupportedAnnotationTypes` annotation, the default version provides additional functionality such as appropriate warnings 
		}
		final SupportedAnnotationTypes supportedAnnotationTypesAnnotation = getClass().getAnnotation(SupportedAnnotationTypes.class);
		if(supportedAnnotationTypesAnnotation == null) {
			return supportedAnnotationTypeCanonicalNames;
		}
		assert supportedAnnotationTypesAnnotation != null;
		return toUnion(super.getSupportedAnnotationTypes(), supportedAnnotationTypeCanonicalNames);
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation indicates support for the latest supported source version.
	 * @see SourceVersion#latestSupported()
	 */
	@Override
	public SourceVersion getSupportedSourceVersion() {
		return SourceVersion.latestSupported();
	}

	/**
	 * Returns the processing environment providing by the tool framework.
	 * @apiNote This method provides access to the processing environment using a more modern approach, rather than accessing a protected variable directly from
	 *          {@link AbstractProcessor}.
	 * @return The processing environment providing by the tool framework.
	 */
	protected ProcessingEnvironment getProcessingEnvironment() {
		return processingEnv;
	}

	//## elements

	/**
	 * Finds a type element from a class if the type element is uniquely determinable in the environment.
	 * @implSpec This implementation delegates to {@link ModelElements#findTypeElementForClass(Elements, Class)}
	 * @param clazz The class for which a type element is to be found.
	 * @return The type element for the class, which will not be present if no type element can be uniquely determined.
	 * @see Class#getCanonicalName()
	 */
	public Optional<TypeElement> findTypeElementForClass(@NonNull final Class<?> clazz) { //TODO create module-related variations as well
		return ModelElements.findTypeElementForClass(getProcessingEnvironment().getElementUtils(), clazz);
	}

	/**
	 * Finds a type element given its canonical name if the type element is uniquely determinable in the environment.
	 * @apiNote This method functions identically to {@link Elements#getTypeElement(CharSequence)} except that it returns an {@link Optional} and never
	 *          <code>null</code>.
	 * @implSpec This implementation delegates to {@link ModelElements#findTypeElementForCanonicalName(Elements, CharSequence)}.
	 * @param canonicalName The canonical name of the type element to return.
	 * @return The named type element, which will not be present if no type element can be uniquely determined.
	 */
	public Optional<TypeElement> findTypeElementForCanonicalName(@NonNull final CharSequence canonicalName) { //TODO create module-related variations as well
		return ModelElements.findTypeElementForCanonicalName(getProcessingEnvironment().getElementUtils(), canonicalName);
	}

	/**
	 * Returns all interfaces of a type element annotated with the given annotation. This method finds interfaced with the annotation <em>present</em> (i.e.
	 * inheritance is supported).
	 * @implSpec This implementation delegates to {@link ModelElements#elementInterfacesAnnotatedWith(Elements, Types, TypeElement, Class)}.
	 * @param elements The element utilities.
	 * @param typeElement The type element representing the type potentially having an interface annotated with the specified annotation.
	 * @param annotationClass The type of annotation to look for.
	 * @return The interfaces of the type element which are in turn annotated with the given annotation.
	 */
	public Stream<DeclaredType> elementInterfacesAnnotatedWith(@NonNull final Elements elements, @NonNull final TypeElement typeElement,
			@NonNull final Class<? extends Annotation> annotationClass) {
		return ModelElements.elementInterfacesAnnotatedWith(elements, getProcessingEnvironment().getTypeUtils(), typeElement, annotationClass);
	}

	//## types

	/**
	 * Tests whether a type is assignable to the type corresponding to the given class (i.e. whether instances of each would have an <code>instanceof</code>
	 * relationship).
	 * @implSpec This implementation delegates to {@link ModelTypes#isTypeAssignableTo(Elements, Types, TypeMirror, Class)}.
	 * @param typeMirror The type to test.
	 * @param clazz The class representing the type against which to compare for assignability.
	 * @return <code>true</code> if the type is assignable to the type represented by the class.
	 * @throws IllegalArgumentException if no type could be found for the given class; or given a type for an executable, package, or module is invalid.
	 * @see Types#isAssignable(TypeMirror, TypeMirror)
	 */
	public boolean isTypeAssignableTo(@NonNull final TypeMirror typeMirror, @NonNull final Class<?> clazz) {
		return ModelTypes.isTypeAssignableTo(getProcessingEnvironment().getElementUtils(), getProcessingEnvironment().getTypeUtils(), typeMirror, clazz);
	}

	/**
	 * Returns a predicate for testing whether a type is assignable to the type corresponding to the given class (i.e. whether instances of each would have an
	 * <code>instanceof</code> relationship).
	 * @apiNote This factory method is useful for creating the test once and using multiple times.
	 * @implSpec This implementation delegates to {@link ModelTypes#isTypeAssignableTo(Elements, Types, Class)}.
	 * @param clazz The class representing the type against which to compare for assignability.
	 * @return <code>true</code> if the type is assignable to the type represented by the class.
	 * @throws IllegalArgumentException if no type could be found for the given class; or given a type for an executable, package, or module is invalid.
	 * @see Types#isAssignable(TypeMirror, TypeMirror)
	 */
	public Predicate<TypeMirror> isTypeAssignableTo(@NonNull final Class<?> clazz) {
		return ModelTypes.isTypeAssignableTo(getProcessingEnvironment().getElementUtils(), getProcessingEnvironment().getTypeUtils(), clazz);
	}

	/**
	 * Finds a type corresponding to a class type element and actual type arguments, if the type element is uniquely determinable in the environment.
	 * @implSpec This implementation delegates {@link ModelTypes#findDeclaredType(Elements, Types, Class, TypeMirror...)}.
	 * @param clazz The class for which a type element is to be found.
	 * @param typeArgs The actual type arguments.
	 * @return The type element for the class, which will not be present if no type element can be uniquely determined.
	 * @see Class#getCanonicalName()
	 * @see Elements#getTypeElement(CharSequence)
	 * @see Types#getDeclaredType(TypeElement, TypeMirror...)
	 */
	public Optional<DeclaredType> findDeclaredTypeForClass(@NonNull final Class<?> clazz, @NonNull final TypeMirror... typeArgs) {
		return ModelTypes.findDeclaredType(getProcessingEnvironment().getElementUtils(), getProcessingEnvironment().getTypeUtils(), clazz, typeArgs);
	}

	/**
	 * Finds a type corresponding to a class type element with an unbounded wildcard type parameter, if the type element is uniquely determinable in the
	 * environment.
	 * @implSpec This implementation delegates {@link ModelTypes#findDeclaredTypeWithUnboundedWildcardForClass(Elements, Types, Class)}.
	 * @param clazz The class for which a type element is to be found.
	 * @return The type element for the class, which will not be present if no type element can be uniquely determined.
	 * @see Class#getCanonicalName()
	 * @see Elements#getTypeElement(CharSequence)
	 * @see Types#getDeclaredType(TypeElement, TypeMirror...)
	 * @see #getUnboundedWildcardType()
	 */
	public Optional<DeclaredType> findDeclaredTypeWithUnboundedWildcardForClass(@NonNull final Class<?> clazz) {
		return ModelTypes.findDeclaredTypeWithUnboundedWildcardForClass(getProcessingEnvironment().getElementUtils(), getProcessingEnvironment().getTypeUtils(),
				clazz);
	}

	/**
	 * Returns a new unbounded wildcard type ({@code <?>}).
	 * @implSpec This implementation delegates {@link ModelTypes#getUnboundedWildcardType(Types)}.
	 * @return The new unbounded wildcard type.
	 */
	public WildcardType getUnboundedWildcardType() {
		return ModelTypes.getUnboundedWildcardType(getProcessingEnvironment().getTypeUtils());
	}

}
