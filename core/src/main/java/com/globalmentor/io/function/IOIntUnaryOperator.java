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

import java.util.function.IntUnaryOperator;

import javax.annotation.Nonnull;

/**
 * Represents an operation on a single {@code int}-valued operand that produces an {@code int}-valued result. This is the primitive type specialization of
 * {@link IOUnaryOperator} for {@code int}.
 *
 * <p>
 * This interface is similar to {@link IntUnaryOperator} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @author Magno N A Cruz
 * @see IOUnaryOperator
 */
@FunctionalInterface
public interface IOIntUnaryOperator {

	/**
	 * <p>
	 * This method is the same as {@link IntUnaryOperator#applyAsInt(int)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param operand The operand.
	 * @return The operator result.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	int applyAsInt(int operand) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link IntUnaryOperator#compose(IntUnaryOperator)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param before The operator to apply before this operator is applied.
	 * @return A composed operator that first applies the {@code before} operator and then applies this operator.
	 * @throws IOException if there is an I/O error performing the operation.
	 *
	 * @see #andThen(IOIntUnaryOperator)
	 */
	default IOIntUnaryOperator compose(@Nonnull IOIntUnaryOperator before) throws IOException {
		Objects.requireNonNull(before);
		return (int v) -> applyAsInt(before.applyAsInt(v));
	}

	/**
	 * <p>
	 * This method is the same as {@link IntUnaryOperator#andThen(IntUnaryOperator)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param after The operator to apply after this operator is applied.
	 * @return A composed operator that first applies this operator and then applies the {@code after} operator.
	 * @throws IOException if there is an I/O error performing the operation.
	 *
	 * @see #compose(IOIntUnaryOperator)
	 */
	default IOIntUnaryOperator andThen(@Nonnull IOIntUnaryOperator after) throws IOException {
		Objects.requireNonNull(after);
		return (int t) -> after.applyAsInt(applyAsInt(t));
	}

	/**
	 * <p>
	 * This method is the same as {@link IntUnaryOperator#identity()}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @return A unary operator that always returns its input argument.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	static IOIntUnaryOperator identity() throws IOException {
		return t -> t;
	}
}
