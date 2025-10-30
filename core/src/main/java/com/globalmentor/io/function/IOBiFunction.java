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
import java.util.function.BiFunction;

import org.jspecify.annotations.*;

/**
 * Represents a function that accepts two arguments and produces a result. This is the two-arity specialization of {@link IOFunction}.
 *
 * <p>
 * This interface is similar to {@link BiFunction} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @param <T> the type of the first argument to the function
 * @param <U> the type of the second argument to the function
 * @param <R> the type of the result of the function
 *
 * @author Magno N A Cruz
 * @see IOFunction
 */
@FunctionalInterface
public interface IOBiFunction<T, U, R> {

	/**
	 * <p>
	 * This method is the same as {@link BiFunction#apply(Object, Object)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param t The first function.
	 * @param u The second function.
	 * @return The function result.
	 * @throws IOException if there is an I/O error performing the operation
	 */
	R apply(T t, U u) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link BiFunction#andThen(java.util.function.Function)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param <V> The type of output of the {@code after} function, and of the composed function.
	 * @param after The function to be applied after this function.
	 * @return A composed function that applies this function first and then the {@code after} function.
	 * @throws IOException if there is an I/O error performing the operation
	 */
	default <V> IOBiFunction<T, U, V> andThen(@NonNull IOFunction<? super R, ? extends V> after) throws IOException {
		Objects.requireNonNull(after);
		return (T t, U u) -> after.apply(apply(t, u));
	}
}
