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

import static com.globalmentor.java.Conditions.*;

import java.util.List;

import javax.annotation.*;

import com.globalmentor.model.Named;

/**
 * A strategy for dealing with compound tokens, such as <code>fooBar</code> or <code>foo-bar</code>.
 * @implNote The default implementations work best if acronyms are formatted the same as non-acronyms. For example in {@link #CAMEL_CASE}, use
 *           <code>oldUrlMapper</code> instead of <code>oldURLMapper</code>.
 * @author Garret Wilson
 */
public interface CompoundTokenization extends Named<String> {

	/**
	 * The delimiter for <code>kebab-case</code>.
	 * @see <a href="https://stackoverflow.com/q/11273282">What's the name for hyphen-separated case?</a>
	 */
	public static final char KEBAB_CASE_DELIMITER = '-';

	/**
	 * The delimiter for <code>snake_case</code>.
	 * @see <a href="https://en.wikipedia.org/wiki/Snake_case">Snake case</a>
	 */
	public static final char SNAKE_CASE_DELIMITER = '_';

	/**
	 * Splits a compound token into its component parts.
	 * @param token The compound token to split.
	 * @return The components of the compound token.
	 * @throws IllegalArgumentException if the token is empty.
	 */
	public List<String> split(@Nonnull final CharSequence token);

	/**
	 * Joins components into a compound token.
	 * @param components The components to join.
	 * @return The compound token resulting from joining the given components.
	 * @throws NullPointerException if one of the components is <code>null</code>.
	 * @throws IllegalArgumentException if there are no components.
	 * @throws IllegalArgumentException if one of the components is the empty string.
	 * @throws IllegalArgumentException if one of the components is not valid for this tokenization.
	 */
	public String join(@Nonnull final Iterable<? extends CharSequence> components);

	/**
	 * Converts a string from this compound tokenization to another compound tokenization. If both compound tokenizations are the same instance, the token is not
	 * modified.
	 * @apiNote This transformation can only be used for round-trip conversion if neither this compound tokenization performs additional component transformations
	 *          when splitting, nor the other compound tokenization performs additional transformations when joining.
	 * @implSpec The default implementation splits the compound token using the other compound tokenization's {@link #split(CharSequence)}, and then joins the
	 *           components using this compound tokenization's {@link #join(Iterable)}.
	 * @param otherCompoundTokenization The other compound tokenization to convert to.
	 * @param token The compound token.
	 * @return The same compound token using the other tokenization.
	 * @throws IllegalArgumentException if the token is empty.
	 */
	public default String to(@Nonnull final CompoundTokenization otherCompoundTokenization, @Nonnull final CharSequence token) {
		if(otherCompoundTokenization == this) { //if the other compound tokenization is the same, assume the token will not change
			checkArgument(token.length() > 0, "Token cannot be empty.");
			return token.toString();
		}
		return otherCompoundTokenization.join(split(token));
	}

	/**
	 * Converts a token from one tokenization to <code>camelCase</code>, leaving the case of the first component unchanged.
	 * @apiNote This is a convenience method which is equivalent to calling {@link #to(CompoundTokenization, CharSequence)} using {@link #CAMEL_CASE}.
	 * @param token The compound token.
	 * @return The same compound token using the <code>camelCase</code> tokenization.
	 * @throws IllegalArgumentException if the token is empty.
	 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
	 */
	public default String toCamelCase(@Nonnull final CharSequence token) {
		return to(CAMEL_CASE, token);
	}

	/**
	 * Converts a token from one tokenization to <code>kebab-case</code>.
	 * @apiNote This is a convenience method which is equivalent to calling {@link #to(CompoundTokenization, CharSequence)} using {@link #KEBAB_CASE}.
	 * @param token The compound token.
	 * @return The same compound token using the <code>kebab-case</code> tokenization.
	 * @throws IllegalArgumentException if the token is empty.
	 * @see <a href="https://stackoverflow.com/q/11273282/421049">What's the name for hyphen-separated case?</a>
	 */
	public default String toKebabCase(@Nonnull final CharSequence token) {
		return to(KEBAB_CASE, token);
	}

	/**
	 * Converts a token from one tokenization to <code>snake_case</code>.
	 * @apiNote This is a convenience method which is equivalent to calling {@link #to(CompoundTokenization, CharSequence)} using {@link #SNAKE_CASE}.
	 * @param token The compound token.
	 * @return The same compound token using the <code>snake_case</code> tokenization.
	 * @throws IllegalArgumentException if the token is empty.
	 * @see <a href="https://en.wikipedia.org/wiki/Snake_case">Snake case</a>
	 */
	public default String toSnakeCase(@Nonnull final CharSequence token) {
		return to(SNAKE_CASE, token);
	}

	/**
	 * A general case-based compound tokenization, supporting both <code>dromedaryCase</code> and <code>PascalCase</code> variations.
	 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
	 */
	public CamelCase CAMEL_CASE = new CamelCase();

	/**
	 * A delimiter-based compound tokenization using {@value #KEBAB_CASE_DELIMITER}.
	 * @see #KEBAB_CASE_DELIMITER
	 * @see <a href="https://stackoverflow.com/q/11273282/421049">What's the name for hyphen-separated case?</a>
	 */
	public CompoundTokenization KEBAB_CASE = new AbstractDelimiterCompoundTokenization(KEBAB_CASE_DELIMITER) {
		@Override
		public String getName() {
			return "kebab-case";
		}
	};

	/**
	 * A delimiter-based compound tokenization using {@value #SNAKE_CASE_DELIMITER}.
	 * @see #SNAKE_CASE_DELIMITER
	 * @see <a href="https://en.wikipedia.org/wiki/Snake_case">Snake case</a>
	 */
	public CompoundTokenization SNAKE_CASE = new AbstractDelimiterCompoundTokenization(SNAKE_CASE_DELIMITER) {
		@Override
		public String getName() {
			return "snake_case";
		}
	};

}
