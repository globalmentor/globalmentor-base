/*
 * Copyright © 2017 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.io.function;

import java.io.IOException;
import java.util.Objects;

import java.util.function.IntPredicate;

import javax.annotation.Nonnull;

/**
 * Represents a predicate (boolean-valued function) of one {@code int}-valued argument. This is the {@code int}-consuming primitive type specialization of
 * {@link IOPredicate}.
 *
 * <p>
 * This interface is similar to {@link IntPredicate} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @author Magno N A Cruz
 * @see IOPredicate
 */
@FunctionalInterface
public interface IOIntPredicate {

	/**
	 * <p>
	 * This method is the same as {@link IntPredicate#test(int)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param value The input argument.
	 * @return {@code true} if the input argument matches the predicate, {@code false} if not.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	boolean test(int value) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link IntPredicate#and(IntPredicate)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param other A predicate that will be logically-ANDed with this predicate.
	 * @return A composed predicate that represents the short-circuiting logical AND of this predicate and the {@code other} predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IOIntPredicate and(@Nonnull IOIntPredicate other) throws IOException {
		Objects.requireNonNull(other);
		return (value) -> test(value) && other.test(value);
	}

	/**
	 * <p>
	 * This method is the same as {@link IntPredicate#negate()}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @return A predicate that represents the logical negation of this predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IOIntPredicate negate() throws IOException {
		return (value) -> !test(value);
	}

	/**
	 * <p>
	 * This method is the same as {@link IntPredicate#or(IntPredicate)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param other A predicate that will be logically-ORed with this predicate.
	 * @return A composed predicate that represents the short-circuiting logical OR of this predicate and the {@code other} predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IOIntPredicate or(@Nonnull IOIntPredicate other) throws IOException {
		Objects.requireNonNull(other);
		return (value) -> test(value) || other.test(value);
	}
}
