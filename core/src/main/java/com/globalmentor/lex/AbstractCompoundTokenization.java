/*
 * Copyright © 2019-2023 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.lex;

import static com.globalmentor.java.Conditions.*;
import static java.util.Objects.*;

import java.util.*;
import java.util.function.BiFunction;

import javax.annotation.*;

/**
 * A base compound tokenization implementation.
 * @implNote This implementation provides a generalized algorithm in {@link #join(Iterable)} for joining components, which can be customized by subclasses by
 *           overriding {@link #appendJoinDelimiter(StringBuilder, int)} and passing a transformation function to the constructor to transform each component
 *           before being joined.
 * @author Garret Wilson
 */
public abstract class AbstractCompoundTokenization implements CompoundTokenization {

	/** @return a component transformation function that performs no changes on a component before joining. */
	protected static BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> noJoinComponentTransformation() {
		return (i, component) -> component;
	}

	private final String name;

	@Override
	public String getName() {
		return name;
	}

	private final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> joinComponentTransformation;

	/**
	 * Join component transformation constructor.
	 * @param name The name to use for the compound tokenization.
	 * @param joinComponentTransformation The function to be applied to each component before joining with {@link #join(Iterable)}. The first function parameter
	 *          is the index of the component being joined. The second function parameter is the non-empty component being joined.
	 */
	protected AbstractCompoundTokenization(@Nonnull final String name,
			final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> joinComponentTransformation) {
		this.name = requireNonNull(name);
		this.joinComponentTransformation = requireNonNull(joinComponentTransformation);
	}

	/**
	 * Returns a composed join component transformation function that will first perform the existing join component transformation of this compound tokenization,
	 * and then apply the given function to that result.
	 * @apiNote This method is meant to facilitate the implementation of TODO
	 * @param after The function to apply during joining after the join transformation function of this compound tokenization is applied. The first function
	 *          parameter is the index of the component being joined. The second function parameter is the non-empty component being joined.
	 * @return A composed function to be applied to components before they are joined, that first applies this compound tokenization's transformation and then
	 *         applies the given function with the result.
	 * @throws NullPointerException if the given function is <code>null</code>.
	 * @see #join(Iterable)
	 */
	protected BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> addSubsequentJoinComponentTransformation(
			@Nonnull final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> after) {
		requireNonNull(after);
		return (componentIndex, component) -> after.apply(componentIndex, this.joinComponentTransformation.apply(componentIndex, component));
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation invokes the internal join component transformation to transform each component as needed, and calls
	 *           {@link #appendJoinDelimiter(StringBuilder, int)} between components to add any delimiter if needed.
	 * @implNote This implementation performs joining manually rather than calling {@link String#join(CharSequence, Iterable)} for efficiency and to check each
	 *           component.
	 * @throws IllegalArgumentException if one of the components is empty.
	 */
	@Override
	public String join(final Iterable<? extends CharSequence> components) {
		final Iterator<? extends CharSequence> componentIterator = components.iterator();
		boolean hasNext = componentIterator.hasNext();
		checkArgument(hasNext, "Cannot create compound tokenization with no components to join.");
		int componentIndex = -1;
		final StringBuilder stringBuilder = new StringBuilder();
		do { //we know there is at least one component
			componentIndex++;
			final CharSequence component = componentIterator.next();
			validateJoinComponent(componentIndex, component);
			stringBuilder.append(joinComponentTransformation.apply(componentIndex, component));
			hasNext = componentIterator.hasNext();
			if(hasNext) {
				appendJoinDelimiter(stringBuilder, componentIndex);
			}
		} while(hasNext);
		return stringBuilder.toString();
	}

	/**
	 * Validates the component to use before joining.
	 * @apiNote Subclasses should always first call the super class version.
	 * @implSpec The default implementation validates that the component is not empty.
	 * @param componentIndex The index of the component being joined.
	 * @param component The non-empty component being joined.
	 * @throws NullPointerException if the component is <code>null</code>.
	 * @throws IllegalArgumentException if the component is the empty string.
	 */
	protected void validateJoinComponent(final int componentIndex, @Nonnull final CharSequence component) {
		checkArgument(component.length() != 0, "Compound token component cannot be empty.");
	}

	/**
	 * Appends any necessary delimiter to the given string builder when joining.
	 * @apiNote Subclasses should first call the super class version.
	 * @implSpec The default implementation does nothing.
	 * @param stringBuilder The string builder representing the components being joined.
	 * @param delimiterIndex The index of the delimiter, which will be equal to the index of the component just added.
	 * @return The given string builder.
	 */
	protected StringBuilder appendJoinDelimiter(@Nonnull StringBuilder stringBuilder, final int delimiterIndex) {
		return stringBuilder;
	}

}
