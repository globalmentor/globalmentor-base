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

import static com.globalmentor.io.function.Functions.*;
import static com.globalmentor.java.Conditions.*;

import java.util.List;
import java.util.function.*;

import javax.annotation.*;

import com.globalmentor.model.Named;

/**
 * A generalization for dealing with compound tokens, such as <code>fooBar</code> or <code>foo-bar</code>. Each individual component of a compound token is
 * referred to as a <dfn>segment</dfn>. By default "transformation" refers to transformation of segments before joining via {@link #join(Iterable)} unless
 * indicated otherwise.
 * @implNote The default implementations work best if acronyms are formatted the same as non-acronyms. For example in {@link #CAMEL_CASE}, use
 *           <code>oldUrlMapper</code> instead of <code>oldURLMapper</code>.
 * @author Garret Wilson
 */
public interface CompoundTokenization extends Named<String> {

	/**
	 * The delimiter for <code>kebab-case</code>.
	 * @deprecated to be removed in favor of calling {@link CharacterDelimitedCompoundTokenization#getDelimiter()} on {@link #KEBAB_CASE}.
	 */
	@Deprecated
	public static final char KEBAB_CASE_DELIMITER = '-';

	/**
	 * The delimiter for <code>snake_case</code>.
	 * @deprecated to be removed in favor of calling {@link CharacterDelimitedCompoundTokenization#getDelimiter()} on {@link #SNAKE_CASE}.
	 */
	@Deprecated
	public static final char SNAKE_CASE_DELIMITER = '_';

	/**
	 * Splits a compound token into its component segments.
	 * @param token The compound token to split.
	 * @return The segments of the compound token.
	 * @throws IllegalArgumentException if the token is empty.
	 */
	public List<String> split(@Nonnull final CharSequence token);

	/**
	 * Joins segments into a compound token.
	 * @param segments The segments to join.
	 * @return The compound token resulting from joining the given segments.
	 * @throws NullPointerException if one of the segments is <code>null</code>.
	 * @throws IllegalArgumentException if there are no segments.
	 * @throws IllegalArgumentException if one of the segments is the empty string.
	 * @throws IllegalArgumentException if one of the segments is not valid for this tokenization.
	 */
	public String join(@Nonnull final Iterable<? extends CharSequence> segments);

	/**
	 * Factory method to create a character-delimited compound tokenization with no segment transformation.
	 * @implSpec This implementation delegates to {@link #namedDelimitedByWithSegmentTransformation(String, char, BiFunction)}.
	 * @param name The name to use for the compound tokenization.
	 * @param delimiter The delimiter for splitting and joining a segment token.
	 * @return A new compound tokenization with the given name and using the given delimiter to {@link #split(CharSequence)} and {@link #join(Iterable)}.
	 * @see #getName()
	 */
	public static CharacterDelimitedCompoundTokenization namedDelimitedBy(@Nonnull final String name, final char delimiter) {
		return namedDelimitedByWithSegmentTransformation(name, delimiter, CharacterDelimitedCompoundTokenization.noSegmentTransformation());
	}

	/**
	 * Factory method to create a character-delimited compound tokenization with a segment transformation.
	 * @apiNote This is a utility method to work with functions that accept {@link String}. If your function can work with {@link CharSequence}, it is better to
	 *          use {@link #namedDelimitedByWithSegmentTransformation(String, char, Function)} or
	 *          {@link #namedDelimitedByWithSegmentTransformation(String, char, BiFunction)}, which may be more efficient because of not requiring the
	 *          intermediate conversion to {@link String}.
	 * @implSpec This implementation delegates to {@link #namedDelimitedByWithSegmentTransformation(String, char, Function)}.
	 * @param name The name to use for the compound tokenization.
	 * @param delimiter The delimiter for splitting and joining a segment token.
	 * @param segmentTransformation The function to be applied to each segment before joining with {@link #join(Iterable)}. The function parameter is the
	 *          non-empty segment being joined.
	 * @return A new compound tokenization with the given name, using the given delimiter to {@link #split(CharSequence)} and {@link #join(Iterable)}, using the
	 *         given function for transforming segments.
	 * @see #getName()
	 * @see #join(Iterable)
	 */
	public static CharacterDelimitedCompoundTokenization namedDelimitedByWithSegmentStringTransformation(@Nonnull final String name, final char delimiter,
			final Function<? super String, ? extends CharSequence> segmentTransformation) {
		return namedDelimitedByWithSegmentTransformation(name, delimiter, segmentTransformation.compose(CharSequence::toString));
	}

	/**
	 * Factory method to create a character-delimited compound tokenization with a segment transformation.
	 * @implSpec This implementation delegates to {@link #namedDelimitedByWithSegmentTransformation(String, char, BiFunction)}.
	 * @param name The name to use for the compound tokenization.
	 * @param delimiter The delimiter for splitting and joining a segment token.
	 * @param segmentTransformation The function to be applied to each segment before joining with {@link #join(Iterable)}. The function parameter is the
	 *          non-empty segment being joined.
	 * @return A new compound tokenization with the given name, using the given delimiter to {@link #split(CharSequence)} and {@link #join(Iterable)}, using the
	 *         given function for transforming segments.
	 * @see #getName()
	 * @see #join(Iterable)
	 */
	public static CharacterDelimitedCompoundTokenization namedDelimitedByWithSegmentTransformation(@Nonnull final String name, final char delimiter,
			final Function<? super CharSequence, ? extends CharSequence> segmentTransformation) {
		return namedDelimitedByWithSegmentTransformation(name, delimiter, toBiFunctionU(segmentTransformation));
	}

	/**
	 * Factory method to create a character-delimited compound tokenization with a segment transformation.
	 * @param name The name to use for the compound tokenization.
	 * @param delimiter The delimiter for splitting and joining a segment token.
	 * @param segmentTransformation The function to be applied to each segment before joining with {@link #join(Iterable)}. The first function parameter is the
	 *          index of the segment being joined. The second function parameter is the non-empty segment being joined.
	 * @return A new compound tokenization with the given name, using the given delimiter to {@link #split(CharSequence)} and {@link #join(Iterable)}, using the
	 *         given function for transforming segments.
	 * @see #getName()
	 * @see #join(Iterable)
	 */
	public static CharacterDelimitedCompoundTokenization namedDelimitedByWithSegmentTransformation(@Nonnull final String name, final char delimiter,
			final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> segmentTransformation) {
		return new CharacterDelimitedCompoundTokenization(name, delimiter, segmentTransformation);
	}

	/**
	 * Produces a new compound tokenization that the same functionality as this one, with the given <em>additional</em> transformation to be applied to each
	 * segment before joining.
	 * @apiNote This is a utility method to work with functions that accept {@link String}. If your function can work with {@link CharSequence}, it is better to
	 *          use {@link #namedWithAddedSegmentTransformation(String, Function)} or {@link #namedWithAddedSegmentTransformation(String, BiFunction)}, which may
	 *          be more efficient because of not requiring the intermediate conversion to {@link String}.
	 * @implSpec The default implementation delegates to {@link #namedWithAddedSegmentTransformation(String, Function)}.
	 * @param name The name to use for the new compound tokenization.
	 * @param segmentTransformation The function to apply during joining after the transformation function of this compound tokenization is applied. The function
	 *          parameter is the non-empty segment being joined.
	 * @return A new version of this compound tokenization that performs the specified additional transformation before joining with {@link #join(Iterable)}.
	 * @throws NullPointerException if the given name and/or function is <code>null</code>.
	 * @see #getName()
	 * @see #join(Iterable)
	 */
	public default CompoundTokenization namedWithAddedSegmentStringTransformation(@Nonnull final String name,
			@Nonnull final Function<? super String, ? extends CharSequence> segmentTransformation) {
		return namedWithAddedSegmentTransformation(name, segmentTransformation.compose(CharSequence::toString));
	}

	/**
	 * Produces a new compound tokenization that the same functionality as this one, with the given <em>additional</em> transformation to be applied to each
	 * segment before joining.
	 * @implSpec The default implementation delegates to {@link #namedWithAddedSegmentTransformation(String, BiFunction)}.
	 * @param name The name to use for the new compound tokenization.
	 * @param segmentTransformation The function to apply during joining after the transformation function of this compound tokenization is applied. The function
	 *          parameter is the non-empty segment being joined.
	 * @return A new version of this compound tokenization that performs the specified additional transformation before joining with {@link #join(Iterable)}.
	 * @throws NullPointerException if the given name and/or function is <code>null</code>.
	 * @see #getName()
	 * @see #join(Iterable)
	 */
	public default CompoundTokenization namedWithAddedSegmentTransformation(@Nonnull final String name,
			@Nonnull final Function<? super CharSequence, ? extends CharSequence> segmentTransformation) {
		return namedWithAddedSegmentTransformation(name, toBiFunctionU(segmentTransformation));
	}

	/**
	 * Produces a new compound tokenization that the same functionality as this one, with the given <em>additional</em> transformation to be applied to each
	 * segment before joining.
	 * @param name The name to use for the new compound tokenization.
	 * @param segmentTransformation The function to apply during joining after the transformation function of this compound tokenization is applied. The first
	 *          function parameter is the index of the segment being joined. The second function parameter is the non-empty segment being joined.
	 * @return A new version of this compound tokenization that performs the specified additional transformation before joining with {@link #join(Iterable)}.
	 * @throws NullPointerException if the given name and/or function is <code>null</code>.
	 * @see #getName()
	 * @see #join(Iterable)
	 */
	public CompoundTokenization namedWithAddedSegmentTransformation(@Nonnull final String name,
			@Nonnull final BiFunction<? super Integer, ? super CharSequence, ? extends CharSequence> segmentTransformation);

	/**
	 * Converts a string from this compound tokenization to another compound tokenization. If both compound tokenizations are the same instance, the token is not
	 * modified.
	 * @apiNote This transformation can only be used for round-trip conversion if neither this compound tokenization performs additional segment transformations
	 *          when splitting, nor the other compound tokenization performs additional transformations when joining.
	 * @implSpec The default implementation splits the compound token using the other compound tokenization's {@link #split(CharSequence)}, and then joins the
	 *           segments using this compound tokenization's {@link #join(Iterable)}.
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
	 * Converts a token from one tokenization to <code>camelCase</code>, leaving the case of the first segment unchanged.
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
	 * A general case-based compound tokenization, supporting both <code>dromedaryCase</code> and <code>PascalCase</code> variations. Its conversion is agnostic
	 * to whether the result is <code>dromedaryCase</code> or <code>PascalCase</code> (i.e. it depends on the existing case of the segments being joined).
	 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
	 * @see #DROMEDARY_CASE
	 * @see #PASCAL_CASE
	 */
	public CamelCase CAMEL_CASE = new CamelCase("camelCase");

	/**
	 * The <code>dromedaryCase</code> variation of <code>camelCase</code>.
	 * @apiNote This tokenization uses a one-way conversion; it will not be possible to return to the previous tokenization unless the previous capitalization was
	 *          known.
	 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
	 */
	public CamelCase DROMEDARY_CASE = CAMEL_CASE.namedWithAddedSegmentTransformation("dromedaryCase", CamelCase::transformCamelCaseSegmentToDromedaryCase);

	/**
	 * The <code>PascalCase</code> variation of <code>camelCase</code>.
	 * @apiNote This tokenization uses a one-way conversion; it will not be possible to return to the previous tokenization unless the previous capitalization was
	 *          known.
	 * @see <a href="https://en.wikipedia.org/wiki/Camel_case">Camel case</a>
	 */
	public CamelCase PASCAL_CASE = CAMEL_CASE.namedWithAddedSegmentTransformation("PascalCase", CamelCase::transformCamelCaseSegmentToPascalCase);

	/**
	 * A delimiter-based compound tokenization using <code>.</code> as a delimiter.
	 * @see <a href="https://stackoverflow.com/q/49263762">Is there a name for dot-separated case?</a>
	 */
	public CharacterDelimitedCompoundTokenization DOT_CASE = namedDelimitedBy("dot.case", '.');

	/**
	 * A delimiter-based compound tokenization using <code>-</code> as a delimiter.
	 * @see <a href="https://stackoverflow.com/q/11273282/421049">What's the name for hyphen-separated case?</a>
	 */
	public CharacterDelimitedCompoundTokenization KEBAB_CASE = namedDelimitedBy("kebab-case", '-');

	/**
	 * A delimiter-based compound tokenization using <code>_</code> as a delimiter.
	 * @see <a href="https://en.wikipedia.org/wiki/Snake_case">Snake case</a>
	 */
	public CharacterDelimitedCompoundTokenization SNAKE_CASE = namedDelimitedBy("snake_case", '_');

}
