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
import static java.util.Collections.*;

import java.beans.Introspector;
import java.util.*;

import javax.annotation.*;

/**
 * A base compound tokenization strategy that relies a change from non-uppercase to uppercase to delimit tokens.
 * @implSpec This implementation does not recognize delimiters whose code points lie outside the BMP (i.e. that depend on surrogate pairs).
 * @author Garret Wilson
 */
public abstract class AbstractUppercaseDelimitedCompoundTokenization implements CompoundTokenization {

	@Override
	public List<String> split(final CharSequence charSequence) {
		ArrayList<String> components = null; //we'll only create this if we have to
		int componentIndex = 0;
		final int length = charSequence.length();
		int start = 0;
		while(start < length) {
			boolean wasUppercase = Character.isUpperCase(charSequence.charAt(start));
			for(int end = start + 1; end <= length; end++) { //go clear to the end (one past the last character)
				final boolean isEnd = end == length;
				final boolean isUppercase = !isEnd && Character.isUpperCase(charSequence.charAt(end));
				if((isUppercase && !wasUppercase) || isEnd) { //if we switched from non-uppercase to uppercase (or reached the end)
					final String component = transformSplitComponent(componentIndex, charSequence.subSequence(start, end)).toString();
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
		if(!hasNext) { //no need to build a string if there are no components
			return "";
		}
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
	 * Determines the component to use after splitting.
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
		final char firstChar = component.charAt(0);
		//if the first character is uppercase, only decapitalize if it is not followed by another capital letter
		if(Character.isUpperCase(firstChar) && (component.length() == 1 || !Character.isUpperCase(component.charAt(1)))) {
			final StringBuilder stringBuilder = new StringBuilder(component);
			stringBuilder.setCharAt(0, Character.toLowerCase(firstChar));
			return stringBuilder;
		}
		return component;
	}

	/**
	 * Determines the component to use before joining.
	 * @implSpec The default implementation changes the first character to uppercase.
	 * @param componentIndex The index of the component being joined.
	 * @param component The non-empty component being joined.
	 * @return The component to use for joining.
	 * @throws NullPointerException if the component is <code>null</code>.
	 * @throws IllegalArgumentException if the component is the empty string.
	 */
	protected CharSequence transformJoinComponent(final int componentIndex, @Nonnull final CharSequence component) {
		checkArgument(component.length() != 0, "Compound token component cannot be empty.");
		final char firstChar = component.charAt(0);
		if(Character.isUpperCase(firstChar)) { //if the first character is already in uppercase
			return component; //no changes need to be made
		}
		final StringBuilder stringBuilder = new StringBuilder(component);
		stringBuilder.setCharAt(0, Character.toUpperCase(firstChar));
		return stringBuilder;
	}

}
