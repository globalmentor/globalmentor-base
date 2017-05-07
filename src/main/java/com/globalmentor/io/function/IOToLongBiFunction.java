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

import java.util.function.ToLongBiFunction;

/**
 * Represents a function that accepts two arguments and produces a long-valued result. This is the {@code long}-producing primitive specialization for
 * {@link IOBiFunction}.
 *
 * <p>
 * This interface is similar to {@link ToLongBiFunction} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @param <T> the type of the first argument to the function
 * @param <U> the type of the second argument to the function
 *
 * @author Magno N A Cruz
 * @see IOBiFunction
 */
@FunctionalInterface
public interface IOToLongBiFunction<T, U> {

	/**
	 * <p>
	 * This method is the same as {@link ToLongBiFunction#applyAsLong(Object, Object)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param t the first function argument
	 * @param u the second function argument
	 * @return the function result
	 * @throws IOException if there is an I/O error performing the operation
	 */
	long applyAsLong(T t, U u) throws IOException;
}
