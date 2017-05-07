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
import java.util.function.BiPredicate;

import javax.annotation.Nonnull;

/**
 * Represents a predicate (boolean-valued function) of two arguments. This is the two-arity specialization of {@link IOPredicate}.
 *
 * <p>
 * This interface is similar to {@link BiPredicate} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @param <T> the type of the first argument to the predicate
 * @param <U> the type of the second argument the predicate
 *
 * @author Magno N A Cruz
 * @see IOPredicate
 */
@FunctionalInterface
public interface IOBiPredicate<T, U> {

	/**
	 * <p>
	 * This method is the same as {@link BiPredicate#test(Object, Object)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param t The first input argument.
	 * @param u The second input argument.
	 * @return {@code true} if the input arguments match the predicate, {@code false} if not.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	boolean test(T t, U u) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link BiPredicate#and(BiPredicate)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param other A predicate that will be logically-ANDed with this predicate.
	 * @return A composed predicate that represents the short-circuiting logical AND of this predicate and the {@code other} predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IOBiPredicate<T, U> and(@Nonnull IOBiPredicate<? super T, ? super U> other) throws IOException {
		Objects.requireNonNull(other);
		return (T t, U u) -> test(t, u) && other.test(t, u);
	}

	/**
	 * <p>
	 * This method is the same as {@link BiPredicate#negate()}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @return A predicate that represents the logical negation of this predicate
	 * @throws IOException if there is an I/O error performing the operation
	 */
	default IOBiPredicate<T, U> negate() throws IOException {
		return (T t, U u) -> !test(t, u);
	}

	/**
	 * <p>
	 * This method is the same as {@link BiPredicate#or(BiPredicate)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param other A predicate that will be logically-ORed with this predicate.
	 * @return A composed predicate that represents the short-circuiting logical OR of this predicate and the {@code other} predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IOBiPredicate<T, U> or(@Nonnull IOBiPredicate<? super T, ? super U> other) throws IOException {
		Objects.requireNonNull(other);
		return (T t, U u) -> test(t, u) || other.test(t, u);
	}
}
