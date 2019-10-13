/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.lex;

import static com.globalmentor.java.Conditions.*;

import java.util.List;

import javax.annotation.*;

/**
 * A strategy for dealing with compound tokens, such as <code>fooBar</code> or <code>foo-bar</code>.
 * @author Garret Wilson
 */
public interface CompoundTokenization {

	/**
	 * The delimiter for <code>kebab-case</code>.
	 * @see <a href="https://stackoverflow.com/q/11273282/421049">What's the name for hyphen-separated case?</a>
	 */
	public static final char KEBAB_CASE_DELIMITER = '-';

	/**
	 * The delimiter for <code>kebab_case</code>.
	 * @see <a href="https://en.wikipedia.org/wiki/Snake_case">Snake case</a>
	 */
	public static final char SNAKE_CASE_DELIMITER = '_';

	/**
	 * Splits a compound token into its component parts.
	 * @param charSequence The character sequence to split.
	 * @return The components of the compound token.
	 */
	public List<String> split(@Nonnull final CharSequence charSequence);

	/**
	 * Joins components into a compound token.
	 * @param components The components to join.
	 * @return The compound token resulting from joining the given components.
	 * @throws NullPointerException if one of the components is <code>null</code>.
	 * @throws IllegalArgumentException if one of the components is the empty string.
	 */
	public String join(@Nonnull final Iterable<? extends CharSequence> components);

	/**
	 * Converts a token from one tokenization to <code>kebab-case</code>.
	 * @implSpec The default implementation splits the compound token and then then joins the components using {@link #KEBAB_CASE}.
	 * @param token The compound token.
	 * @return The same compound token using the <code>kebab-case</code> tokenization.
	 * @see <a href="https://stackoverflow.com/q/11273282/421049">What's the name for hyphen-separated case?</a>
	 */
	public default String toKebabCase(@Nonnull final String token) {
		return KEBAB_CASE.join(split(token));
	}

	/**
	 * Converts a token from one tokenization to <code>snake_case</code>.
	 * @implSpec The default implementation splits the compound token and then then joins the components using {@link #SNAKE_CASE}.
	 * @param token The compound token.
	 * @return The same compound token using the <code>snake_case</code> tokenization.
	 * @see <a href="https://en.wikipedia.org/wiki/Snake_case">Snake case</a>
	 */
	public default String toSnakeCase(@Nonnull final String token) {
		return SNAKE_CASE.join(split(token));
	}

	//TODO document

	public CompoundTokenization CAMEL_CASE = new AbstractUppercaseDelimitedCompoundTokenization() {
		@Override
		protected CharSequence transformJoinComponent(final int componentIndex, final CharSequence component) {
			if(componentIndex == 0) { //the first component should not be capitalized
				checkArgument(component.length() != 0, "Compound token component cannot be empty.");
				return component;
			}
			return super.transformJoinComponent(componentIndex, component);
		};
	};

	public CompoundTokenization PASCAL_CASE = new AbstractUppercaseDelimitedCompoundTokenization() {};

	public CompoundTokenization KEBAB_CASE = new AbstractDelimiterCompoundTokenization(KEBAB_CASE_DELIMITER) {};

	public CompoundTokenization SNAKE_CASE = new AbstractDelimiterCompoundTokenization(SNAKE_CASE_DELIMITER) {};

}
