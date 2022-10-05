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

import java.util.function.ToIntFunction;

/**
 * Represents a function that produces an int-valued result. This is the {@code int}-producing primitive specialization for {@link IOFunction}.
 *
 * <p>
 * This interface is similar to {@link ToIntFunction} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @param <T> the type of the input to the function
 *
 * @author Magno N A Cruz
 * @see IOFunction
 */
@FunctionalInterface
public interface IOToIntFunction<T> {

	/**
	 * <p>
	 * This method is the same as {@link ToIntFunction#applyAsInt(Object)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param value The function argument.
	 * @return The function result.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	int applyAsInt(T value) throws IOException;
}
