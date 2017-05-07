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

import java.util.function.DoubleBinaryOperator;

/**
 * Represents an operation upon two {@code double}-valued operands and producing a {@code double}-valued result. This is the primitive type specialization of
 * {@link IOBinaryOperator} for {@code double}.
 *
 * <p>
 * This interface is similar to {@link DoubleBinaryOperator} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @author Magno N A Cruz
 * @see IOBinaryOperator
 * @see IODoubleUnaryOperator
 */
@FunctionalInterface
public interface IODoubleBinaryOperator {

	/**
	 * <p>
	 * This method is the same as {@link DoubleBinaryOperator#applyAsDouble(double, double)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param left the first operand
	 * @param right the second operand
	 * @return the operator result
	 * @throws IOException if there is an I/O error performing the operation
	 */
	double applyAsDouble(double left, double right) throws IOException;
}
