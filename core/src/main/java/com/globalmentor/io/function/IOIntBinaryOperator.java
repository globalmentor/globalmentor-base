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

import java.util.function.IntBinaryOperator;

/**
 * Represents an operation upon two {@code int}-valued operands and producing an {@code int}-valued result. This is the primitive type specialization of
 * {@link IOBinaryOperator} for {@code int}.
 *
 * <p>
 * This interface is similar to {@link IntBinaryOperator} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @author Magno N A Cruz
 * @see IOBinaryOperator
 * @see IOIntUnaryOperator
 */
@FunctionalInterface
public interface IOIntBinaryOperator {

	/**
	 * <p>
	 * This method is the same as {@link IntBinaryOperator#applyAsInt(int, int)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param left The first operand.
	 * @param right The second operand.
	 * @return The operator result.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	int applyAsInt(int left, int right) throws IOException;
}
