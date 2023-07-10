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

import javax.annotation.*;

import com.globalmentor.java.Characters;

/**
 * A base compound tokenization implementation that relies on a delimiter between components.
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
	 * Delimiters constructor.
	 * @param delimiter The delimiter for splitting and joining a component token.
	 */
	public AbstractDelimiterCompoundTokenization(final char delimiter) {
		delimiterCharacters = Characters.of(delimiter);
		this.delimiter = delimiter;
	}

	@Override
	public List<String> split(final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		return delimiterCharacters.split(token);
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version merely validates that the component does not contain the delimiter returns the same component with no transformation.
	 * @throws IllegalArgumentException if one of the components already contains the tokenization delimiter.
	 * @see #getDelimiter()
	 */
	protected CharSequence transformJoinComponent(final int componentIndex, @Nonnull CharSequence component) {
		component = super.transformJoinComponent(componentIndex, component); //do the default transformation (which actually only does validation)
		checkArgument(!contains(component, delimiter), "Component %s cannot contain the delimiter %s.", component, getDelimiter());
		return component;
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
