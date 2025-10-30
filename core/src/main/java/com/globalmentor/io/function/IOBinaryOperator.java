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

import java.util.Objects;
import java.util.function.BinaryOperator;

import org.jspecify.annotations.*;

import java.io.IOException;
import java.util.Comparator;

/**
 * Represents an operation upon two operands of the same type, producing a result of the same type as the operands. This is a specialization of
 * {@link IOBiFunction} for the case where the operands and the result are all of the same type.
 *
 * <p>
 * This interface is similar to {@link BinaryOperator} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @param <T> the type of the operands and result of the operator
 *
 * @author Magno N A Cruz
 * @see IOBiFunction
 * @see IOUnaryOperator
 */
@FunctionalInterface
public interface IOBinaryOperator<T> extends IOBiFunction<T, T, T> {

	/**
	 * <p>
	 * This method is the same as {@link BinaryOperator#minBy(Comparator)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param <T> The type of the input arguments of the comparator.
	 * @param comparator A {@code Comparator} for comparing both values.
	 * @return an {@code IOBinaryOperator} which returns the lesser of its operands, according to the supplied {@code Comparator}.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	public static <T> IOBinaryOperator<T> minBy(@NonNull Comparator<? super T> comparator) throws IOException {
		Objects.requireNonNull(comparator);
		return (a, b) -> comparator.compare(a, b) <= 0 ? a : b;
	}

	/**
	 * <p>
	 * This method is the same as {@link BinaryOperator#maxBy(Comparator)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param <T> The type of the input arguments of the comparator.
	 * @param comparator A {@code Comparator} for comparing both values.
	 * @return An {@code IOBinaryOperator} which returns the greater of its operands, according to the supplied {@code Comparator}.
	 * @throws IOException if there is an I/O error performing the operation
	 */
	public static <T> IOBinaryOperator<T> maxBy(@NonNull Comparator<? super T> comparator) throws IOException {
		Objects.requireNonNull(comparator);
		return (a, b) -> comparator.compare(a, b) >= 0 ? a : b;
	}
}
