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

import java.util.function.LongSupplier;

/**
 * Represents a supplier of {@code long}-valued results. This is the {@code long}-producing primitive specialization of {@link IOSupplier}.
 *
 * <p>
 * This interface is similar to {@link LongSupplier} except that it is allowed to throw an {@link IOException}.
 * </p>
 * 
 * @author Magno N A Cruz
 * @see IOSupplier
 */
@FunctionalInterface
public interface IOLongSupplier {

	/**
	 * <p>
	 * This method is the same as {@link LongSupplier#getAsLong()}, but with a support for {@link IOException}.
	 * </p>
	 * 
	 * @return A result.
	 * @throws IOException if there is an I/O error performing the operation.
	 */
	long getAsLong() throws IOException;
}
