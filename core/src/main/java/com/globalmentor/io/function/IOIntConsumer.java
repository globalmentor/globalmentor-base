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
import java.util.function.Consumer;
import java.util.function.IntConsumer;

import org.jspecify.annotations.*;

/**
 * Represents an operation that accepts a single {@code int}-valued argument and returns no result. This is the primitive type specialization of
 * {@link Consumer} for {@code int}. Unlike most other functional interfaces, {@code IntConsumer} is expected to operate via side-effects.
 *
 * <p>
 * This interface is similar to {@link IntConsumer} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @author Magno N A Cruz
 * @see IOConsumer
 */
@FunctionalInterface
public interface IOIntConsumer {

	/**
	 * <p>
	 * This method is the same as {@link IntConsumer#accept(int)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param value The input argument.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	void accept(int value) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link IntConsumer#andThen(IntConsumer)}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @param after The operation to perform after this operation.
	 * @return A composed {@code IntConsumer} that performs this operation followed by the {@code after} operation in sequence.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	default IOIntConsumer andThen(@NonNull IOIntConsumer after) throws IOException {
		Objects.requireNonNull(after);
		return (int t) -> {
			accept(t);
			after.accept(t);
		};
	}
}
