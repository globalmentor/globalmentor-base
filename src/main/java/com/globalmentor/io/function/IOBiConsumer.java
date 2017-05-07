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
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Represents an operation that accepts two input arguments and returns no result. This is the two-arity specialization of {@link Consumer}. Unlike most other
 * functional interfaces, {@code BiConsumer} is expected to operate via side-effects.
 *
 * <p>
 * This interface is similar to {@link BiConsumer} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @param <T> the type of the first argument to the operation
 * @param <U> the type of the second argument to the operation
 *
 * @author Magno N A Cruz
 * @see IOConsumer
 */
@FunctionalInterface
public interface IOBiConsumer<T, U> {

	/**
	 * <p>
	 * This method is the same as {@link BiConsumer#accept(Object, Object)}, but with a support for {@link IOException}.
	 * </p>
	 *
	 * @param t the first input argument
	 * @param u the second input argument
	 * @throws IOException if there is an I/O error performing the operation
	 */
	void accept(T t, U u) throws IOException;

	/**
	 * <p>
	 * This method is the same as {@link BiConsumer#andThen(BiConsumer)}, but with a support for {@link IOException}.
	 * </p>
	 *
	 * @param after the operation to perform after this operation
	 * @return a composed {@code BiConsumer} that performs in sequence this operation followed by the {@code after} operation
	 * @throws NullPointerException if {@code after} is null
	 * @throws IOException if there is an I/O error performing the operation
	 */
	default IOBiConsumer<T, U> andThen(IOBiConsumer<? super T, ? super U> after) throws IOException {
		Objects.requireNonNull(after);

		return (l, r) -> {
			accept(l, r);
			after.accept(l, r);
		};
	}
}
