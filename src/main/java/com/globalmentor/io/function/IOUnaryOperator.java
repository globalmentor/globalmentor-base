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

import java.util.function.UnaryOperator;

/**
 * Represents an operation on a single operand that produces a result of the same type as its operand. This is a specialization of {@code Function} for the case
 * where the operand and result are of the same type.
 *
 * <p>
 * This interface is similar to {@link UnaryOperator} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @param <T> the type of the operand and result of the operator
 *
 * @author Magno N A Cruz
 * @see IOFunction
 */
@FunctionalInterface
public interface IOUnaryOperator<T> extends IOFunction<T, T> {

	/**
	 * <p>
	 * This method is the same as {@link UnaryOperator#identity()}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param <T> The type of the input and output of the operator.
	 * @return A unary operator that always returns its input argument.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	static <T> IOUnaryOperator<T> identity() throws IOException {
		return t -> t;
	}
}
