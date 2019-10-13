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

import com.globalmentor.model.Named;

/**
 * A strategy for dealing with compound tokens, such as <code>fooBar</code> or <code>foo-bar</code>.
 * @implNote The if a token may be either in {@link #PASCAL_CASE} or in {@link #CAMEL_CASE}, use {@link #CAMEL_CASE} to convert it to another tokenization if
 *           the capitalization of the first component should be maintained, as {@link #PASCAL_CASE} will normalize the case of the first component.
 * @implNote The default implementations work best if acronyms are formatted the same as non-acronyms. For example in {@link #PASCAL_CASE}, using
 *           <code>DefaultUrlMapper</code> instead of <code>DefaultURLMapper</code>.
 * @author Garret Wilson
 */
public interface CompoundTokenization extends Named<String> {

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
	 * @throws IllegalArgumentException if one of the components is not valid for this tokenization.
	 */
	public String join(@Nonnull final Iterable<? extends CharSequence> components);

	/**
	 * Converts a token from one tokenization to <code>camelCase</code>, leaving the case of the first component unchanged.
	 * @apiNote This conversion is agnostic to the case of the first component, and can thus be used for round-trip conversions.
	 * @implSpec The default implementation splits the compound token and then joins the components using {@link #CAMEL_CASE}.
	 * @param token The compound token.
	 * @return The same compound token using the <code>camelCase</code> tokenization.
	 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
	 */
	public default String toCamelCase(@Nonnull final CharSequence token) {
		return CAMEL_CASE.join(split(token));
	}

	/**
	 * Converts a token from one tokenization to <code>PascalCase</code>.
	 * @apiNote Unlike {@link #toCamelCase(CharSequence)}, this method will change the case of the first component, and should thus not be used for round-trip
	 *          conversions.
	 * @implSpec The default implementation splits the compound token and then joins the components using {@link #PASCAL_CASE}.
	 * @param token The compound token.
	 * @return The same compound token using the <code>PascalCase</code> tokenization.
	 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
	 */
	public default String toPascalCase(@Nonnull final CharSequence token) {
		return PASCAL_CASE.join(split(token));
	}

	/**
	 * Converts a token from one tokenization to <code>kebab-case</code>.
	 * @implSpec The default implementation splits the compound token and then joins the components using {@link #KEBAB_CASE}.
	 * @param token The compound token.
	 * @return The same compound token using the <code>kebab-case</code> tokenization.
	 * @see <a href="https://stackoverflow.com/q/11273282/421049">What's the name for hyphen-separated case?</a>
	 */
	public default String toKebabCase(@Nonnull final CharSequence token) {
		return KEBAB_CASE.join(split(token));
	}

	/**
	 * Converts a token from one tokenization to <code>snake_case</code>.
	 * @implSpec The default implementation splits the compound token and then joins the components using {@link #SNAKE_CASE}.
	 * @param token The compound token.
	 * @return The same compound token using the <code>snake_case</code> tokenization.
	 * @see <a href="https://en.wikipedia.org/wiki/Snake_case">Snake case</a>
	 */
	public default String toSnakeCase(@Nonnull final CharSequence token) {
		return SNAKE_CASE.join(split(token));
	}

	/**
	 * A general case-based compound tokenization supporting both <code>camelCase</code> and <code>PascalCase</code>. That is, this tokenization is agnostic to
	 * whether the first component is capitalized, and thus supports round-trip split+join.
	 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
	 */
	public CompoundTokenization CAMEL_CASE = new AbstractUppercaseDelimitedCompoundTokenization() {
		@Override
		public String getName() {
			return "camelCase";
		}

		@Override
		protected CharSequence transformSplitComponent(final int componentIndex, final CharSequence component) {
			if(componentIndex == 0) { //the first component should not be decapitalized
				checkArgument(component.length() != 0, "Compound token component cannot be empty.");
				return component;
			}
			return super.transformSplitComponent(componentIndex, component);
		};

		@Override
		protected CharSequence transformJoinComponent(final int componentIndex, final CharSequence component) {
			if(componentIndex == 0) { //the first component should not be capitalized
				checkArgument(component.length() != 0, "Compound token component cannot be empty.");
				return component;
			}
			return super.transformJoinComponent(componentIndex, component);
		};

		@Override
		public String toCamelCase(final CharSequence token) {
			return token.toString();
		}
	};

	/**
	 * A special case of {@link #CAMEL_CASE} that decapitalizes the first component when splitting and capitalizes the first component when joining.
	 * @implNote Warning: This implementation does <em>not</em> support round-trip split+join.
	 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
	 */
	public CompoundTokenization PASCAL_CASE = new AbstractUppercaseDelimitedCompoundTokenization() {
		@Override
		public String getName() {
			return "PascalCase";
		}

		@Override
		public String toPascalCase(final CharSequence token) {
			if(token.length() == 0) {
				return "";
			}
			final char firstChar = token.charAt(0);
			if(Character.isUpperCase(token.charAt(0))) {
				return token.toString();
			}
			final StringBuilder stringBuilder = new StringBuilder(token);
			stringBuilder.setCharAt(0, Character.toUpperCase(firstChar));
			return stringBuilder.toString();
		}
	};

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

		@Override
		public String toKebabCase(final CharSequence token) {
			return token.toString();
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

		@Override
		public String toSnakeCase(final CharSequence token) {
			return token.toString();
		}
	};

}
