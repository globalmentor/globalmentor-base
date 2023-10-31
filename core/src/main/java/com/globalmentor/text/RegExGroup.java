/*
 * Copyright © 2023 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.text;

import java.util.Optional;
import java.util.regex.*;

import javax.annotation.*;

import com.globalmentor.java.EnumLike;

/**
 * A utility which facilitates regular expression {@link Pattern} compilation as well as {@link Matcher} lookup by encapsulating matching groups. Each instance
 * is a group that can be matched.
 * @apiNote The following is the envisioned use: <pre>{@code
 *
 * enum FooBarRegExGroup implements RegExGroup.ByNumber {
 *   FOO, BAR;
 *
 *   public static final Pattern PATTERN = Pattern.compile("(foo)(bar)?");
 * }
 * …
 * Matcher matcher = FooBarRegExGroup.PATTERN.matcher("foo");
 * assert matcher.matches();
 * Optional<String> foundFoo = FooBarRegExGroup.FOO.findIn(matcher); //`Optional.of("foo")`
 * Optional<String> foundBar = FooBarRegExGroup.BAR.findIn(matcher); //`Optional.empty()`
 * }</pre>
 * @author Garret Wilson
 */
public interface RegExGroup extends EnumLike {

	/**
	 * Finds the input subsequence captured by this group in the given matcher. The way this group identifies the captured group in the match depends on the
	 * implementation of this class.
	 * @param matcher The matcher to check.
	 * @return The subsequence captured by this group in the match, which will be empty if this group did not capture part of the input.
	 * @throws IllegalStateException If no match has yet been attempted, or if the previous match operation failed.
	 */
	public Optional<String> findIn(@Nonnull final Matcher matcher);

	/** A regular expression group definition that finds captured groups based upon the group's order in the pattern. */
	public interface ByNumber extends RegExGroup {

		/**
		 * {@inheritDoc}
		 * @implSpec This implementation finds the captured group using the one-based equivalent of this group's ordinal.
		 * @see #ordinal()
		 * @see Matcher#group(int)
		 * @throws IndexOutOfBoundsException If there is no capturing group in the pattern with the one-based equivalent of this group's ordinal.
		 */
		@Override
		default Optional<String> findIn(@Nonnull final Matcher matcher) {
			return Optional.ofNullable(matcher.group(ordinal() + 1));
		}

	}

	/** A regular expression group definition that finds captured groups based upon the group's name in the pattern. */
	public interface ByName extends RegExGroup {

		/**
		 * {@inheritDoc}
		 * @implSpec This implementation finds the captured group using the this group's name.
		 * @see #name()
		 * @see Matcher#group(String)
		 * @throws IndexOutOfBoundsException If there is no capturing group in the pattern with the this group's name.
		 */
		@Override
		default Optional<String> findIn(@Nonnull final Matcher matcher) {
			return Optional.ofNullable(matcher.group(ordinal() + 1));
		}

	}

}
