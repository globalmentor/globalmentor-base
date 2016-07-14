/*
 * Copyright © 2016 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.util;

import static java.util.Objects.*;

import java.util.Optional;
import java.util.function.Supplier;

import javax.annotation.*;

/**
 * Utilities to use with {@link Optional}.
 * @author Garret Wilson
 */
public class Optionals {

	/**
	 * Returns another another optional from a supplier if the given optional is not present.
	 * <p>
	 * This method duplicates functionality scheduled to appear in Java 9.
	 * </p>
	 * @param <T> The type of value contained in the optional.
	 * @param optional The optional to check
	 * @param supplier The supplier of an alternative optional if the value of the given optional is not present.
	 * @return The given optional or, if the value is not present, an optional one returned by the given supplier.
	 * @see <a href="https://bugs.openjdk.java.net/browse/JDK-8080418">JDK-8080418</a>
	 */
	public static <T> Optional<T> or(@Nonnull final Optional<T> optional, @Nonnull final Supplier<Optional<T>> supplier) {
		requireNonNull(supplier);
		return optional.isPresent() ? optional : requireNonNull(supplier.get());
	}

}
