/*
 * Copyright © 2016 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.util;

import java.util.Optional;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.OptionalLong;

import javax.annotation.*;

import com.globalmentor.java.Objects;

/**
 * Utilities to use with {@link Optional}.
 * @author Garret Wilson
 */
public class Optionals {

	/**
	 * Convenience method that returns a value if and only if it the given optional is present and is an instance of the given class. This method is equivalent to
	 * {@code optional.filter(instanceClass::isInstance).map(instanceClass::cast)}.
	 * @apiNote Depending on the circumstances, it may be more natural to call {@code optional.flatMap(Objects.asInstance(instanceClass)}, which has the same
	 *          effect and is equivalent to {@code optional.flatMap(object -> Objects.asInstance(object, instanceClass))}.
	 * @implSpec This implementation delegates to {@link Objects#asInstance(Object, Class)}
	 * @param <T> The type of object to check for.
	 * @param optional The optional to examine.
	 * @param instanceClass The class of which the optional value may be an instance.
	 * @return The optional's value if it is present and is an instance of the given class.
	 * @see Objects#asInstance(Object, Class)
	 * @see Objects#asInstance(Class)
	 */
	public static <T> Optional<T> filterAsInstance(final Optional<? super T> optional, final Class<T> instanceClass) {
		return optional.flatMap(object -> Objects.asInstance(object, instanceClass));
	}

	/**
	 * Determines whether an optional value is present and is equal to some other nullable object.
	 * @apiNote Note that the given object with which to compare the optional object can be <code>null</code>, as per the parameter of
	 *          {@link Object#equals(Object)}.
	 * @implSpec This method is equivalent to calling {@link Optional#isPresent()} and if the result is <code>true</code>, calling {@link Object#equals(Object)}
	 *           on the contained object.
	 * @param optional The optional to check
	 * @param object The object to compare for equality with the optional value.
	 * @return <code>true</code> if the given optional value is present and is equal to the given object.
	 * @throws NullPointerException if the given optional is <code>null</code>.
	 * @see Optional#isPresent()
	 * @see Optional#get()
	 * @see Optional#equals(Object)
	 */
	public static boolean isPresentAndEquals(@Nonnull final Optional<?> optional, @Nullable final Object object) {
		return optional.isPresent() && optional.get().equals(object);
	}

	/**
	 * Converts an optional wrapper {@link Double} instance to a primitive containing {@link OptionalDouble} instance.
	 * @param optional The {@link Optional} instance to convert.
	 * @return The equivalent primitive optional wrapper.
	 */
	public static OptionalDouble toOptionalDouble(@Nonnull final Optional<Double> optional) {
		return optional.isPresent() ? OptionalDouble.of(optional.get().doubleValue()) : OptionalDouble.empty();
	}

	/**
	 * Converts an optional wrapper {@link Integer} instance to a primitive containing {@link OptionalInt} instance.
	 * @param optional The {@link Optional} instance to convert.
	 * @return The equivalent primitive optional wrapper.
	 */
	public static OptionalInt toOptionalInt(@Nonnull final Optional<Integer> optional) {
		return optional.isPresent() ? OptionalInt.of(optional.get().intValue()) : OptionalInt.empty();
	}

	/**
	 * Converts an optional wrapper {@link Long} instance to a primitive containing {@link OptionalLong} instance.
	 * @param optional The {@link Optional} instance to convert.
	 * @return The equivalent primitive optional wrapper.
	 */
	public static OptionalLong toOptionalLong(@Nonnull final Optional<Long> optional) {
		return optional.isPresent() ? OptionalLong.of(optional.get().longValue()) : OptionalLong.empty();
	}

}
