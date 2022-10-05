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
import static java.util.Collections.*;

import java.beans.Introspector;
import java.util.*;

import javax.annotation.*;

/**
 * A compound tokenization strategy that relies a change from non-uppercase to uppercase to delimit tokens.
 * @apiNote This tokenization supports both <code>dromedaryCase</code> and <code>PascalCase</code> variations. That is, this tokenization is agnostic to whether
 *          the first component is capitalized, and thus supports round-trip split+join.
 * @apiNote This class normally need not be instantiated. Instead use the constant singleton instance {@link CompoundTokenization#CAMEL_CASE}.
 * @implSpec This implementation does not recognize delimiters whose code points lie outside the BMP (i.e. that depend on surrogate pairs).
 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
 * @author Garret Wilson
 */
public class CamelCase implements CompoundTokenization {

	/** This class cannot be publicly instantiated, but may be subclassed and instantiated from other classes in the package. */
	protected CamelCase() {
	}

	@Override
	public String getName() {
		return "camelCase";
	}

	@Override
	public List<String> split(final CharSequence token) {
		ArrayList<String> components = null; //we'll only create this if we have to
		int componentIndex = 0;
		final int length = token.length();
		checkArgument(length > 0, "Token cannot be empty.");
		int start = 0;
		while(start < length) {
			boolean wasUppercase = Character.isUpperCase(token.charAt(start));
			for(int end = start + 1; end <= length; end++) { //go clear to the end (one past the last character)
				final boolean isEnd = end == length;
				final boolean isUppercase = !isEnd && Character.isUpperCase(token.charAt(end));
				if((isUppercase && !wasUppercase) || isEnd) { //if we switched from non-uppercase to uppercase (or reached the end)
					final String component = transformSplitComponent(componentIndex, token.subSequence(start, end)).toString();
					if(components == null) { //if there are no components yet
						if(isEnd) { //only one component
							return singletonList(component);
						}
						components = new ArrayList<>();
					}
					components.add(component);
					componentIndex++;
					start = end;
					break;
				}
				wasUppercase = isUppercase;
			}
		}
		return components != null ? components : emptyList();
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation calls {@link #transformJoinComponent(int, CharSequence)} to transform each component as needed.
	 */
	@Override
	public String join(final Iterable<? extends CharSequence> components) {
		final Iterator<? extends CharSequence> componentIterator = components.iterator();
		boolean hasNext = componentIterator.hasNext();
		checkArgument(hasNext, "Cannot create compound tokenization with no components to join.");
		int componentIndex = 0;
		final StringBuilder stringBuilder = new StringBuilder();
		do { //we know there is at least one component
			stringBuilder.append(transformJoinComponent(componentIndex, componentIterator.next()));
			hasNext = componentIterator.hasNext();
			componentIndex++;
		} while(hasNext);
		return stringBuilder.toString();
	}

	/**
	 * Determines the component to use after splitting. The first component is unchanged, as the capitalization of the first component is irrelevant to the
	 * compound tokenization.
	 * @implSpec The default implementation changes the first character to lowercase if the first character is uppercase but not followed by another uppercase
	 *           character.
	 * @param componentIndex The index of the component being split.
	 * @param component The non-empty component being split.
	 * @return The component after splitting.
	 * @throws NullPointerException if the component is <code>null</code>.
	 * @throws IllegalArgumentException if the component is the empty string.
	 * @see Introspector#decapitalize(String)
	 */
	protected CharSequence transformSplitComponent(final int componentIndex, @Nonnull final CharSequence component) {
		checkArgument(component.length() != 0, "Compound token component cannot be empty.");
		if(componentIndex != 0) {
			final char firstChar = component.charAt(0);
			//if the first character is uppercase, only decapitalize if it is not followed by another capital letter
			if(Character.isUpperCase(firstChar) && (component.length() == 1 || !Character.isUpperCase(component.charAt(1)))) {
				final StringBuilder stringBuilder = new StringBuilder(component);
				stringBuilder.setCharAt(0, Character.toLowerCase(firstChar));
				return stringBuilder;
			}
		}
		return component;
	}

	/**
	 * Determines the component to use before joining. The first component is unchanged, as the capitalization of the first component is irrelevant to the
	 * compound tokenization.
	 * @implSpec The default implementation changes the first character to uppercase.
	 * @param componentIndex The index of the component being joined.
	 * @param component The non-empty component being joined.
	 * @return The component to use for joining.
	 * @throws NullPointerException if the component is <code>null</code>.
	 * @throws IllegalArgumentException if the component is the empty string.
	 */
	protected CharSequence transformJoinComponent(final int componentIndex, @Nonnull final CharSequence component) {
		checkArgument(component.length() != 0, "Compound token component cannot be empty.");
		if(componentIndex == 0) {
			return component;
		}
		final char firstChar = component.charAt(0);
		if(Character.isUpperCase(firstChar)) { //if the first character is already in uppercase
			return component; //no changes need to be made
		}
		final StringBuilder stringBuilder = new StringBuilder(component);
		stringBuilder.setCharAt(0, Character.toUpperCase(firstChar));
		return stringBuilder;
	}

	@Override
	public String toCamelCase(final CharSequence token) {
		return token.toString();
	}

	/**
	 * Indicates whether a token is in <code>dromedaryCase</code> (lower <code>camelCase</code>); this, whether its first letter is in lowercase.
	 * @apiNote It is possible for a token in <code>camelCase</code> to be neither in <code>dromedaryCase</code> nor in <code>PascalCase</code>, e.g. if the first
	 *          character is a symbol.
	 * @param token The compound token in <code>camelCase</code>.
	 * @return <code>true</code> if the first character is in lowercase.
	 * @throws IllegalArgumentException if the token is empty.
	 */
	public boolean isDromedaryCase(@Nonnull final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		return Character.isLowerCase(token.charAt(0));
	}

	/**
	 * Converts a token from <code>camelCase</code> to <code>dromedaryCase</code> (lower <code>camelCase</code>) by decapitalizing the initial letter.
	 * @apiNote The resulting token will not necessarily be in <code>dromedaryCase</code> (that is, {@link #isDromedaryCase(CharSequence)} may not return
	 *          <code>true</code> for the resulting token), e.g. if the first character is a symbol.
	 * @apiNote This is a one-way conversion; it will not be possible to return to the previous tokenization unless the previous capitalization was known.
	 * @param token The compound token in <code>camelCase</code>.
	 * @return The same compound token using the <code>PascalCase</code> tokenization.
	 * @throws IllegalArgumentException if the token is empty.
	 */
	public String toDromedaryCase(@Nonnull final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		final char firstChar = token.charAt(0);
		if(!Character.isUpperCase(firstChar)) {
			return token.toString();
		}
		final StringBuilder stringBuilder = new StringBuilder(token);
		stringBuilder.setCharAt(0, Character.toLowerCase(firstChar));
		return stringBuilder.toString();
	}

	/**
	 * Indicates whether a token is in <code>PascalCase</code> (upper <code>camelCase</code>); this, whether its first letter is in uppercase.
	 * @apiNote It is possible for a token in <code>camelCase</code> to be neither in <code>dromedaryCase</code> nor in <code>PascalCase</code>, e.g. if the first
	 *          character is a symbol.
	 * @param token The compound token in <code>camelCase</code>.
	 * @return <code>true</code> if the first character is in uppercase.
	 * @throws IllegalArgumentException if the token is empty.
	 */
	public boolean isPascalCase(@Nonnull final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		return Character.isUpperCase(token.charAt(0));
	}

	/**
	 * Converts a token from <code>camelCase</code> to <code>PascalCase</code> (upper <code>camelCase</code>) by capitalizing the initial letter.
	 * @apiNote The resulting token will not necessarily be in <code>PascalCase</code> (that is, {@link #isPascalCase(CharSequence)} may not return
	 *          <code>true</code> for the resulting token), e.g. if the first character is a symbol.
	 * @apiNote This is a one-way conversion; it will not be possible to return to the previous tokenization unless the previous capitalization was known.
	 * @param token The compound token in <code>camelCase</code>.
	 * @return The same compound token using the <code>PascalCase</code> tokenization.
	 * @throws IllegalArgumentException if the token is empty.
	 */
	public String toPascalCase(@Nonnull final CharSequence token) {
		checkArgument(token.length() > 0, "Token cannot be empty.");
		final char firstChar = token.charAt(0);
		if(Character.isUpperCase(firstChar)) {
			return token.toString();
		}
		final StringBuilder stringBuilder = new StringBuilder(token);
		stringBuilder.setCharAt(0, Character.toUpperCase(firstChar));
		return stringBuilder.toString();
	}

}
