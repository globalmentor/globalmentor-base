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

import static java.util.Objects.*;

import java.io.IOException;
import java.util.function.Consumer;

import org.jspecify.annotations.*;

/**
 * Represents an I/O operation that accepts a single input argument and returns no result.
 *
 * <p>
 * This interface is similar to {@link Consumer} except that it is allowed to throw an {@link IOException}.
 * </p>
 *
 * @param <T> the type of the input to the operation.
 * @author Garret Wilson
 * @see Consumer
 */
@FunctionalInterface
public interface IOConsumer<T> {

	/**
	 * <p>
	 * This method is the same as {@link Consumer#accept(Object)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param t the input argument
	 * @throws IOException if there is an I/O error performing the operation
	 */
	public void accept(@NonNull T t) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link Consumer#andThen(Consumer)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param after The operation to perform after this operation.
	 * @return A consumer that performs this operation followed by the given operation in sequence.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	public default IOConsumer<T> andThen(@NonNull final IOConsumer<? super T> after) throws IOException {
		requireNonNull(after);
		return t -> {
			accept(t);
			after.accept(t);
		};
	}
}
