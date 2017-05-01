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

import java.util.function.LongPredicate;

/**
 * Represents a predicate (boolean-valued function) of one {@code long}-valued argument. This is the {@code long}-consuming primitive type specialization of
 * {@link IOPredicate}.
 *
 * <p>
 * This interface is similar to {@link LongPredicate} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @author Magno N A Cruz
 * @see IOPredicate
 */
@FunctionalInterface
public interface IOLongPredicate {

	/**
	 * Evaluates this predicate on the given argument.
	 *
	 * @param value the input argument
	 * @return {@code true} if the input argument matches the predicate, otherwise {@code false}
	 * @throws IOException if there is an I/O error performing the operation
	 */
	boolean test(long value) throws IOException;

	/**
	 * Returns a composed predicate that represents a short-circuiting logical AND of this predicate and another. When evaluating the composed predicate, if this
	 * predicate is {@code false}, then the {@code other} predicate is not evaluated.
	 *
	 * <p>
	 * Any exceptions thrown during evaluation of either predicate are relayed to the caller; if evaluation of this predicate throws an exception, the
	 * {@code other} predicate will not be evaluated.
	 *
	 * @param other a predicate that will be logically-ANDed with this predicate
	 * @return a composed predicate that represents the short-circuiting logical AND of this predicate and the {@code other} predicate
	 * @throws NullPointerException if other is null
	 * @throws IOException if there is an I/O error performing the operation
	 */
	default IOLongPredicate and(IOLongPredicate other) throws IOException {
		Objects.requireNonNull(other);
		return (value) -> test(value) && other.test(value);
	}

	/**
	 * Returns a predicate that represents the logical negation of this predicate.
	 *
	 * @return a predicate that represents the logical negation of this predicate
	 * @throws IOException if there is an I/O error performing the operation
	 */
	default IOLongPredicate negate() throws IOException {
		return (value) -> !test(value);
	}

	/**
	 * Returns a composed predicate that represents a short-circuiting logical OR of this predicate and another. When evaluating the composed predicate, if this
	 * predicate is {@code true}, then the {@code other} predicate is not evaluated.
	 *
	 * <p>
	 * Any exceptions thrown during evaluation of either predicate are relayed to the caller; if evaluation of this predicate throws an exception, the
	 * {@code other} predicate will not be evaluated.
	 *
	 * @param other a predicate that will be logically-ORed with this predicate
	 * @return a composed predicate that represents the short-circuiting logical OR of this predicate and the {@code other} predicate
	 * @throws NullPointerException if other is null
	 * @throws IOException if there is an I/O error performing the operation
	 */
	default IOLongPredicate or(IOLongPredicate other) throws IOException {
		Objects.requireNonNull(other);
		return (value) -> test(value) || other.test(value);
	}
}
