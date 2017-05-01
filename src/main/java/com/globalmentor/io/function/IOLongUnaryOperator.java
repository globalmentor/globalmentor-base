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

import java.util.function.LongUnaryOperator;

/**
 * Represents an operation on a single {@code long}-valued operand that produces a {@code long}-valued result. This is the primitive type specialization of
 * {@link IOUnaryOperator} for {@code long}.
 *
 * <p>
 * This interface is similar to {@link LongUnaryOperator} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @author Magno N A Cruz
 * @see IOUnaryOperator
 */
@FunctionalInterface
public interface IOLongUnaryOperator {

	/**
	 * Applies this operator to the given operand.
	 *
	 * @param operand the operand
	 * @return the operator result
	 * @throws IOException if there is an I/O error performing the operation
	 */
	long applyAsLong(long operand) throws IOException;

	/**
	 * Returns a composed operator that first applies the {@code before} operator to its input, and then applies this operator to the result. If evaluation of
	 * either operator throws an exception, it is relayed to the caller of the composed operator.
	 *
	 * @param before the operator to apply before this operator is applied
	 * @return a composed operator that first applies the {@code before} operator and then applies this operator
	 * @throws NullPointerException if before is null
	 * @throws IOException if there is an I/O error performing the operation
	 *
	 * @see #andThen(IOLongUnaryOperator)
	 */
	default IOLongUnaryOperator compose(IOLongUnaryOperator before) throws IOException {
		Objects.requireNonNull(before);
		return (long v) -> applyAsLong(before.applyAsLong(v));
	}

	/**
	 * Returns a composed operator that first applies this operator to its input, and then applies the {@code after} operator to the result. If evaluation of
	 * either operator throws an exception, it is relayed to the caller of the composed operator.
	 *
	 * @param after the operator to apply after this operator is applied
	 * @return a composed operator that first applies this operator and then applies the {@code after} operator
	 * @throws NullPointerException if after is null
	 * @throws IOException if there is an I/O error performing the operation
	 *
	 * @see #compose(IOLongUnaryOperator)
	 */
	default IOLongUnaryOperator andThen(IOLongUnaryOperator after) throws IOException {
		Objects.requireNonNull(after);
		return (long t) -> after.applyAsLong(applyAsLong(t));
	}

	/**
	 * Returns a unary operator that always returns its input argument.
	 *
	 * @return a unary operator that always returns its input argument
	 * @throws IOException if there is an I/O error performing the operation
	 */
	static IOLongUnaryOperator identity() throws IOException {
		return t -> t;
	}
}
