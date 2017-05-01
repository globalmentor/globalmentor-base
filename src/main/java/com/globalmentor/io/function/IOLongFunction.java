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

import java.util.function.LongFunction;

/**
 * Represents a function that accepts a long-valued argument and produces a result. This is the {@code long}-consuming primitive specialization for
 * {@link IOFunction}.
 *
 * <p>
 * This interface is similar to {@link LongFunction} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @param <R> the type of the result of the function
 *
 * @author Magno N A Cruz
 * @see IOFunction
 */
@FunctionalInterface
public interface IOLongFunction<R> {

	/**
	 * Applies this function to the given argument.
	 *
	 * @param value the function argument
	 * @return the function result
	 * @throws IOException if there is an I/O error performing the operation
	 */
	R apply(long value) throws IOException;
}
