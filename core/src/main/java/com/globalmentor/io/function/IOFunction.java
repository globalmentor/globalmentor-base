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
import java.util.function.Function;

import javax.annotation.Nonnull;

/**
 * Represents a function that accepts one argument and produces a result.
 *
 * <p>
 * This interface is similar to {@link Function} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @param <T> the type of the input to the function
 * @param <R> the type of the result of the function
 *
 * @author Magno N A Cruz
 * @see Function
 */
@FunctionalInterface
public interface IOFunction<T, R> {

	/**
	 * <p>
	 * This method is the same as {@link Function#apply(Object)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param t The function argument.
	 * @return The function result.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	R apply(T t) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link Function#compose(Function)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param <V> The type of input to the {@code before} function, and to the composed function.
	 * @param before The function to apply before this function is applied.
	 * @return A composed function that first applies the {@code before} function and then applies this function.
	 * @throws IOException if there is an I/O error performing the operation.
	 *
	 * @see #andThen(IOFunction)
	 */
	default <V> IOFunction<V, R> compose(@Nonnull IOFunction<? super V, ? extends T> before) throws IOException {
		Objects.requireNonNull(before);
		return (V v) -> apply(before.apply(v));
	}

	/**
	 * <p>
	 * This method is the same as {@link Function#andThen(Function)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param <V> The type of output of the {@code after} function, and of the composed function.
	 * @param after The function to apply after this function is applied.
	 * @return A composed function that first applies this function and then applies the {@code after} function.
	 * @throws IOException if there is an I/O error performing the operation.
	 *
	 * @see #compose(IOFunction)
	 */
	default <V> IOFunction<T, V> andThen(@Nonnull IOFunction<? super R, ? extends V> after) throws IOException {
		Objects.requireNonNull(after);
		return (T t) -> after.apply(apply(t));
	}

	/**
	 * Returns a function that always returns its input argument.
	 *
	 * @param <T> The type of the input and output objects to the function.
	 * @return A function that always returns its input argument.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	static <T> IOFunction<T, T> identity() throws IOException {
		return t -> t;
	}
}
