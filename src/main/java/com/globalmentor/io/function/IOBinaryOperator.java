/*
 * Copyright (c) 2010, 2013, Oracle and/or its affiliates. All rights reserved.
 * ORACLE PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

package com.globalmentor.io.function;

import java.util.Objects;
import java.util.function.BinaryOperator;
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
	 * Returns a {@link IOBinaryOperator} which returns the lesser of two elements according to the specified {@code Comparator}.
	 *
	 * @param <T> the type of the input arguments of the comparator
	 * @param comparator a {@code Comparator} for comparing the two values
	 * @return a {@code BinaryOperator} which returns the lesser of its operands, according to the supplied {@code Comparator}
	 * @throws NullPointerException if the argument is null
	 * @throws IOException if there is an I/O error performing the operation
	 */
	public static <T> IOBinaryOperator<T> minBy(Comparator<? super T> comparator) throws IOException {
		Objects.requireNonNull(comparator);
		return (a, b) -> comparator.compare(a, b) <= 0 ? a : b;
	}

	/**
	 * Returns a {@link IOBinaryOperator} which returns the greater of two elements according to the specified {@code Comparator}.
	 *
	 * @param <T> the type of the input arguments of the comparator
	 * @param comparator a {@code Comparator} for comparing the two values
	 * @return a {@code BinaryOperator} which returns the greater of its operands, according to the supplied {@code Comparator}
	 * @throws NullPointerException if the argument is null
	 * @throws IOException if there is an I/O error performing the operation
	 */
	public static <T> IOBinaryOperator<T> maxBy(Comparator<? super T> comparator) throws IOException {
		Objects.requireNonNull(comparator);
		return (a, b) -> comparator.compare(a, b) >= 0 ? a : b;
	}
}
