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

import java.util.function.DoublePredicate;

import javax.annotation.Nonnull;

/**
 * Represents a predicate (boolean-valued function) of one {@code double}-valued argument. This is the {@code double}-consuming primitive type specialization of
 * {@link IOPredicate}.
 *
 * <p>
 * This interface is similar to {@link DoublePredicate} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @author Magno N A Cruz
 * @see IOPredicate
 */
@FunctionalInterface
public interface IODoublePredicate {

	/**
	 * <p>
	 * This method is the same as {@link DoublePredicate#test(double)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param value The input argument.
	 * @return {@code true} if the input argument matches the predicate, {@code false} if not.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	boolean test(double value) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link DoublePredicate#and(DoublePredicate)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param other A predicate that will be logically-ANDed with this predicate.
	 * @return A composed predicate that represents the short-circuiting logical AND of this predicate and the {@code other} predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IODoublePredicate and(@Nonnull IODoublePredicate other) throws IOException {
		Objects.requireNonNull(other);
		return (value) -> test(value) && other.test(value);
	}

	/**
	 * <p>
	 * This method is the same as {@link DoublePredicate#negate()}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @return A predicate that represents the logical negation of this predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IODoublePredicate negate() throws IOException {
		return (value) -> !test(value);
	}

	/**
	 * <p>
	 * This method is the same as {@link DoublePredicate#or(DoublePredicate)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param other A predicate that will be logically-ORed with this predicate.
	 * @return A composed predicate that represents the short-circuiting logical OR of this predicate and the {@code other} predicate.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IODoublePredicate or(IODoublePredicate other) throws IOException {
		Objects.requireNonNull(other);
		return (value) -> test(value) || other.test(value);
	}
}
