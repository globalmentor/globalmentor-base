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

import static java.util.Objects.*;

import java.util.*;
import java.util.function.*;

import javax.annotation.*;

import com.globalmentor.java.Objects;

/**
 * Utilities to use with {@link Optional}.
 * @author Garret Wilson
 */
public final class Optionals {

	private Optionals() {
	}

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
	 * Performs a combining operation on the values of two <code>Optional</code>s if both are present, or the one that is present if any. This is a higher-order
	 * functional <dfn>fold</dfn>.
	 * @apiNote This operation is equivalent to converting each <code>Optional</code> to a stream using {@link Optional#stream()} and then performing a
	 *          {@link java.util.stream.Stream#reduce(Object, BinaryOperator)} operation.
	 * @apiNote For folding more than two values, be aware that there exists the choice of a "left fold" or a "right fold", differing in the order of pair-wise
	 *          association of combining operations. The {@link java.util.stream.Stream#reduce(Object, BinaryOperator)} method performs a left fold; Java streams
	 *          do not have a right fold operation as of Java 24. Either operation may be performed by calling this method in the correct associative order for
	 *          each pair of values.
	 * @param <T> The common type of optional value.
	 * @param optional1 The first optional value.
	 * @param optional2 The second optional value.
	 * @param combiner The combining function.
	 * @return The values combined with the combining function, if both are present; otherwise the value present, if any.
	 * @see <a href="https://en.wikipedia.org/wiki/Fold_(higher-order_function)">Fold (higher-order function)</a>
	 * @see <a href="https://stackoverflow.com/q/79679814">Map two Java `Optional`s or produce the one with a value</a>
	 */
	public static <T> Optional<T> fold(@Nonnull final Optional<? extends T> optional1, @Nonnull final Optional<? extends T> optional2,
			@Nonnull final BinaryOperator<T> combiner) {
		@SuppressWarnings("unchecked") //`Optional<>` is immutable, so it can hold a subclass of `T`; see source code of `Optional.or(java.util.function.Supplier)`
		final Optional<T> result = optional1.isPresent()
				? optional2.isPresent() ? Optional.of(combiner.apply(optional1.orElseThrow(), optional2.orElseThrow())) : (Optional<T>)optional1
				: (Optional<T>)optional2;
		return result;
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
	 * Returns another a second optional if the first optional is not present.
	 * @apiNote This method is similar to {@link Optional#or(java.util.function.Supplier)} and performs the same logic, but this method is useful in cases in
	 *          which the other optional is already available and no supplier is needed.
	 * @param <T> The type of value contained in the optional.
	 * @param optional The optional to check.
	 * @param otherOptional The alternative optional.
	 * @return The given optional or, if the value is not present, the other optional.
	 * @throws NullPointerException if either argument is <code>null</code>.
	 * @see Optional#or(java.util.function.Supplier)
	 * @see <a href="https://stackoverflow.com/q/24599996">Get value from one Optional or another</a>
	 */
	public static <T> Optional<T> or(@Nonnull final Optional<? extends T> optional, @Nonnull Optional<? extends T> otherOptional) {
		requireNonNull(otherOptional);
		@SuppressWarnings("unchecked") //`Optional<>` is immutable, so it can hold a subclass of `T`; see source code of `Optional.or(java.util.function.Supplier)`
		final Optional<T> result = optional.isPresent() ? (Optional<T>)optional : (Optional<T>)otherOptional;
		return result;
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
