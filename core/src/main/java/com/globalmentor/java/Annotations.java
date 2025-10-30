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

package com.globalmentor.java;

import static java.util.stream.Collectors.*;

import java.lang.annotation.Annotation;
import java.util.*;

import org.jspecify.annotations.*;

/**
 * Abstraction for accessing annotations.
 * <p>
 * While {@link java.lang.reflect.AnnotatedElement} provides an abstraction for accessing annotations via <em>reflection</em>, it assumes all related classes
 * are accessible, which may not apply to annotation processing during compilation. Annotation processing instead uses annotation mirrors, which provide less
 * type safety but also make fewer assumptions about available class files, allowing annotation access in terms of structures in the source code.
 * </p>
 * <p>
 * This interface provides a lowest-common-denominator abstraction for accessing basic annotation information that will work for annotated elements or
 * annotation mirrors. It is not appropriate for all annotation access, such as annotations that need only be accessed after compilation.
 * </p>
 * @apiNote As this annotation access is not necessarily performed via reflection, it is not in the <code>reflect</code> package along with
 *          {@link java.lang.reflect.AnnotatedElement}.
 * @apiNote Definitions of "declared", "present", "associated", etc. can be found in {@link java.lang.reflect.AnnotatedElement}.
 * @author Garret Wilson
 * @see java.lang.reflect.AnnotatedElement
 * @see com.globalmentor.reflect.AnnotatedElements
 */
public interface Annotations {

	/**
	 * The simple name of the special <code>value</code> annotation element allowing for shorthand value notation.
	 * @see <a href="https://docs.oracle.com/javase/specs/jls/se21/html/jls-9.html#jls-9.7.3">The Java® Language Specification, Java SE 21 Edition § 9.7.3.
	 *      Single-Element Annotations</a>
	 */
	public static final String VALUE_ELEMENT_NAME = "value";

	/**
	 * Determines whether an annotation of the identified annotation type is <em>present</em>.
	 * @apiNote This method functions identically to {@link java.lang.reflect.AnnotatedElement#isAnnotationPresent(Class)}.
	 * @param annotationClass The class object representing the annotation type.
	 * @return <code>true</code> if an annotation of the specified annotation type is present.
	 */
	public boolean isAnnotationPresent(@NonNull final Class<? extends Annotation> annotationClass);

	/**
	 * Determines which, if any, of the given annotations types are <em>present</em>. Only the exactly queried types will be founded; annotation type hierarchy is
	 * not supported. That is, an annotation type that subclasses one of the given types will not be returned.
	 * @apiNote This method returns actual annotation classes because they are guaranteed to be loaded even in an annotation processing context, all having been
	 *          passed as arguments.
	 * @implSpec The default implementation individually checks in an annotation is present by delegating to {@link #isAnnotationPresent(Class)}, but some
	 *           implementations may find it more efficient first to enumerate the present annotations and check them against those given.
	 * @param annotationClasses The class objects representing the annotation types to check for.
	 * @return The types of annotations of those given that are present.
	 */
	public default Set<Class<? extends Annotation>> getWhichAnnotationTypesPresent(@NonNull Set<Class<? extends Annotation>> annotationClasses) {
		return annotationClasses.stream().filter(this::isAnnotationPresent).collect(toUnmodifiableSet());
	}

	/**
	 * Finds the value of the <code>value</code> element of the identified annotation type if such an annotation is <em>present</em> and has a <code>value</code>
	 * element.
	 * @apiNote This method functions identically to {@link java.lang.reflect.AnnotatedElement#getAnnotation(Class)} except that it returns an {@link Optional}.
	 * @param annotationClass The class object representing the annotation type.
	 * @return The <code>value</code> of the annotation for the specified annotation type, which may not be present.
	 * @see <a href="https://docs.oracle.com/javase/specs/jls/se21/html/jls-9.html#jls-9.7.3">The Java® Language Specification, Java SE 21 Edition § 9.7.3.
	 *      Single-Element Annotations</a>
	 */
	public Optional<Object> findAnnotationValue(@NonNull final Class<? extends Annotation> annotationClass);

}
