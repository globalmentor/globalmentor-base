/*
 * Copyright © 2019 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Conditions.*;

import java.util.*;
import java.util.function.BiFunction;

import javax.annotation.*;

import com.globalmentor.java.Characters;

/**
 * A base compound tokenization implementation that relies on a delimiter character between components.
 * @author Garret Wilson
 */
public abstract class AbstractDelimiterCompoundTokenization extends AbstractCompoundTokenization {

	private final Characters delimiterCharacters;

	private final char delimiter;

	/** @return The delimiter used by this tokenization. */
	public char getDelimiter() {
		return delimiter;
	}

	/**
	 * Delimiter constructor.
	 * @param name The name to use for the compound tokenization.
	 * @param delimiter The delimiter for splitting and joining a component token.
	 */
	public AbstractDelimiterCompoundTokenization(@Nonnull final String name, final char delimiter) {
		this(name, delimiter, noJoinComponentTransformation());
	}

	/**
	 * Delimiter and join component transformation constructor.
	 * @param name The name to use for the compound tokenization.
	 * @param delimiter The delimiter for splitting and joining a component token.
	 * @param joinComponentTransformation The function to be applied to each component before joining with {@link #join(Iterable)}. The first function parameter
	 *          is the index of the component being joined. The second function parameter is the non-empty component being joined.
	 */
	protected AbstractDelimiterCompoundTokenization(@Nonnull final String name, final char delimiter,
			final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> joinComponentTransformation) {
		super(name, joinComponentTransformation);
		delimiterCharacters = Characters.of(delimiter);
		this.delimiter = delimiter;
	}

	/**
	 * Produces a new compound tokenization that the same functionality as this one, with the given <em>additional</em> transformation to be applied to each
	 * component before joining.
	 * @param name The name to use for the new compound tokenization.
	 * @param after The function to apply during joining after the join transformation function of this compound tokenization is applied. The first function
	 *          parameter is the index of the component being joined. The second function parameter is the non-empty component being joined.
	 * @return a composed function that first applies this function and then applies the {@code after} function
	 * @throws NullPointerException if the given name and/or function is <code>null</code>.
	 * @see #getName()
	 * @see #join(Iterable)
	 */
	public CompoundTokenization withJoinComponentTransformation(@Nonnull final String name,
			@Nonnull final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> after) {
		return new AbstractDelimiterCompoundTokenization(name, getDelimiter(), addSubsequentJoinComponentTransformation(after)) {}; //TODO promote anonymous class
	}

	@Override
	public List<String> split(final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		return delimiterCharacters.split(token);
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version validates that the component does not contain the delimiter.
	 * @throws IllegalArgumentException if one of the components already contains the tokenization delimiter.
	 * @see #getDelimiter()
	 */
	@Override
	protected void validateJoinComponent(final int componentIndex, final CharSequence component) {
		super.validateJoinComponent(componentIndex, component);
		checkArgument(!contains(component, delimiter), "Component %s cannot contain the delimiter %s.", component, getDelimiter());
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation appends the delimiter returned by {@link #getDelimiter()}.
	 * @see #getDelimiter()
	 */
	@Override
	protected StringBuilder appendJoinDelimiter(@Nonnull StringBuilder stringBuilder, final int delimiterIndex) {
		return super.appendJoinDelimiter(stringBuilder, delimiterIndex).append(getDelimiter()); //the call to super is just for completeness; it does nothing
	}

}
