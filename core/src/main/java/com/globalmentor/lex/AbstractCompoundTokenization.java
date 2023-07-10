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

import java.util.*;

import javax.annotation.*;

/**
 * A base compound tokenization implementation.
 * @implNote This implementation provides a generalized algorithm in {@link #join(Iterable)} for joining components, which can be customized by subclasses by
 *           overriding {@link #transformJoinComponent(int, CharSequence)} and {@link #appendJoinDelimiter(StringBuilder, int)}.
 * @author Garret Wilson
 */
public abstract class AbstractCompoundTokenization implements CompoundTokenization {

	/** This class may be subclassed and instantiated from other classes in the package. */
	protected AbstractCompoundTokenization() {
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation calls {@link #transformJoinComponent(int, CharSequence)} to transform each component as needed, and calls
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
			stringBuilder.append(transformJoinComponent(componentIndex, componentIterator.next()));
			hasNext = componentIterator.hasNext();
			if(hasNext) {
				appendJoinDelimiter(stringBuilder, componentIndex);
			}
		} while(hasNext);
		return stringBuilder.toString();
	}

	/**
	 * Determines the component to use before joining.
	 * @implSpec The default implementation merely validates that the component is not empty and returns the same component with no transformation.
	 * @param componentIndex The index of the component being joined.
	 * @param component The non-empty component being joined.
	 * @return The component to use for joining.
	 * @throws NullPointerException if the component is <code>null</code>.
	 * @throws IllegalArgumentException if the component is the empty string.
	 */
	protected CharSequence transformJoinComponent(final int componentIndex, @Nonnull final CharSequence component) {
		checkArgument(component.length() != 0, "Compound token component cannot be empty.");
		return component;
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
