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

import com.globalmentor.java.Characters;

/**
 * A base compound tokenization strategy that relies on a delimiter between components.
 * @author Garret Wilson
 */
public abstract class AbstractDelimiterCompoundTokenization implements CompoundTokenization {

	private final Characters delimiterCharacters;

	private final String delimiterString;

	/** @return The delimiter used by this tokenization. */
	public char getDelimiter() {
		return delimiterString.charAt(0);
	}

	/**
	 * Delimiters constructor.
	 * @param delimiter The delimiter for splitting and joining a component token.
	 */
	public AbstractDelimiterCompoundTokenization(final char delimiter) {
		delimiterCharacters = Characters.of(delimiter);
		delimiterString = String.valueOf(delimiter);
	}

	@Override
	public List<String> split(final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		return delimiterCharacters.split(token);
	}

	/**
	 * {@inheritDoc}
	 * @implNote This implementation performs joining manually rather than calling {@link String#join(CharSequence, Iterable)} for efficiency and to check each
	 *           component.
	 * @throws IllegalArgumentException if one of the components already contains the tokenization delimiter.
	 */
	@Override
	public String join(final Iterable<? extends CharSequence> components) {
		final Iterator<? extends CharSequence> componentIterator = components.iterator();
		boolean hasNext = componentIterator.hasNext();
		checkArgument(hasNext, "Cannot create compound tokenization with no components to join.");
		final char delimiter = getDelimiter();
		final StringBuilder stringBuilder = new StringBuilder();
		do { //we know there is at least one component
			final CharSequence component = componentIterator.next();
			checkArgument(component.length() != 0, "Compound token component cannot be empty.");
			checkArgument(!contains(component, delimiter), "Component %s cannot contain the delimiter %s.", component, delimiterString);
			stringBuilder.append(component);
			hasNext = componentIterator.hasNext();
			if(hasNext) {
				stringBuilder.append(delimiter);
			}
		} while(hasNext);
		return stringBuilder.toString();
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation does a simple delimiter replacement, which is more efficient than splitting and joining.
	 */
	@Override
	public String toKebabCase(final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		return token.toString().replace(getDelimiter(), KEBAB_CASE_DELIMITER);
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation does a simple delimiter replacement, which is more efficient than splitting and joining.
	 */
	@Override
	public String toSnakeCase(CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		return token.toString().replace(getDelimiter(), SNAKE_CASE_DELIMITER);
	}

}
