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

import javax.annotation.Nonnull;

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
	 * <p>
	 * This method is the same as {@link LongUnaryOperator#applyAsLong(long)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param operand The operand.
	 * @return The operator result.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	long applyAsLong(long operand) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link LongUnaryOperator#compose(LongUnaryOperator)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param before The operator to apply before this operator is applied.
	 * @return A composed operator that applies the {@code before} operator first and then this operator.
	 * @throws IOException if there is an I/O error performing the operation.
	 *
	 * @see #andThen(IOLongUnaryOperator)
	 */
	default IOLongUnaryOperator compose(@Nonnull IOLongUnaryOperator before) throws IOException {
		Objects.requireNonNull(before);
		return (long v) -> applyAsLong(before.applyAsLong(v));
	}

	/**
	 * <p>
	 * This method is the same as {@link LongUnaryOperator#andThen(LongUnaryOperator)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param after The operator to apply after this operator is applied.
	 * @return A composed operator that applies this operator first and then the {@code after} operator.
	 * @throws IOException if there is an I/O error performing the operation.
	 *
	 * @see #compose(IOLongUnaryOperator)
	 */
	default IOLongUnaryOperator andThen(@Nonnull IOLongUnaryOperator after) throws IOException {
		Objects.requireNonNull(after);
		return (long t) -> after.applyAsLong(applyAsLong(t));
	}

	/**
	 * <p>
	 * This method is the same as {@link LongUnaryOperator#identity()}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @return A unary operator that always returns its input argument.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	static IOLongUnaryOperator identity() throws IOException {
		return t -> t;
	}
}
