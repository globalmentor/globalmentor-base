/*
 * Copyright © 2017 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.io.function;

import java.io.IOException;
import java.util.Objects;

import java.util.function.Predicate;

import javax.annotation.Nonnull;

/**
 * Represents a predicate (boolean-valued function) of one argument.
 *
 * <p>
 * This interface is similar to {@link Predicate} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @param <T> the type of the input to the predicate
 *
 * @author Magno N A Cruz
 * @see Predicate
 */
@FunctionalInterface
public interface IOPredicate<T> {

	/**
	 * <p>
	 * This method is the same as {@link Predicate#test(Object)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param t The input argument.
	 * @return {@code true} if the input argument matches the predicate, {@code false} if not.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	boolean test(T t) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link Predicate#and(Predicate)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param other A predicate that will be logically-ANDed with this predicate.
	 * @return A composed predicate that represents the short-circuiting logical AND of this predicate and the {@code other} predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IOPredicate<T> and(@Nonnull IOPredicate<? super T> other) throws IOException {
		Objects.requireNonNull(other);
		return (t) -> test(t) && other.test(t);
	}

	/**
	 * <p>
	 * This method is the same as {@link Predicate#negate()}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @return A predicate that represents the logical negation of this predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IOPredicate<T> negate() throws IOException {
		return (t) -> !test(t);
	}

	/**
	 * <p>
	 * This method is the same as {@link Predicate#or(Predicate)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param other A predicate that will be logically-ORed with this predicate.
	 * @return A composed predicate that represents the short-circuiting logical OR of this predicate and the {@code other} predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IOPredicate<T> or(@Nonnull IOPredicate<? super T> other) throws IOException {
		Objects.requireNonNull(other);
		return (t) -> test(t) || other.test(t);
	}

	/**
	 * <p>
	 * This method is the same as {@link Predicate#isEqual(Object)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param <T> The type of arguments to the predicate.
	 * @param targetRef The object reference with which to compare for equality, which may be {@code null}.
	 * @return A predicate that tests if two arguments are equal according to {@link Objects#equals(Object, Object)}.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	static <T> IOPredicate<T> isEqual(Object targetRef) throws IOException {
		return (null == targetRef) ? Objects::isNull : object -> targetRef.equals(object);
	}
}
