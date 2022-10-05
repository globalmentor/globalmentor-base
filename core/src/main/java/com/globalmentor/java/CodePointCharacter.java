/*
 * Copyright © 2017 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.java;

import static com.globalmentor.java.Conditions.*;

import java.util.stream.Stream;

import javax.annotation.*;

/**
 * Value class representing a Unicode code point.
 * 
 * <p>
 * The Java <code>char</code> type and {@link Character} wrapper class only support 16 bits. They cannot represent the entire range of Unicode code points, such
 * as any supplementary character appearing outside the Basic Multilingual Plane (BMP). This class represents <em>any</em> Unicode code point and additionally
 * provides utilities for converting to and from Java characters.
 * </p>
 * @author Garret Wilson
 */
public final class CodePointCharacter {

	private final int codePoint;

	/** @return The Unicode code point represented. */
	public int getCodePoint() {
		return codePoint;
	}

	/**
	 * Code point constructor.
	 * 
	 * @param codePoint The Unicode code point to represent.
	 * @throws IllegalArgumentException if the given code point is not a valid Unicode code point.
	 * @see Character#isValidCodePoint(int)
	 */
	private CodePointCharacter(final int codePoint) {
		checkArgument(Character.isValidCodePoint(codePoint), "The value %d does not represent a valid code point.", codePoint);
		this.codePoint = codePoint;
	}

	/**
	 * Creates a code point character from a Java primitive <code>char</code> type value.
	 * 
	 * @param c The character to represent.
	 * @return A new code point character representing the given character.
	 */
	public static CodePointCharacter of(final char c) {
		return of((int)c);
	}

	/**
	 * Creates a code point character from a code point.
	 * 
	 * @param codePoint The Unicode code point to represent.
	 * @return A new code point character representing the given code point.
	 * @throws IllegalArgumentException if the given code point is not a valid Unicode code point.
	 * @see Character#isValidCodePoint(int)
	 */
	public static CodePointCharacter of(final int codePoint) {
		return new CodePointCharacter(codePoint);
	}

	/**
	 * Creates a code point character from a surrogate pair.
	 * 
	 * @param high The high-surrogate code unit.
	 * @param low The low-surrogate code unit.
	 * @return A new code point character representing the supplementary code point composed from the specified surrogate pair.
	 * @throws IllegalArgumentException if the given characters do not comprise a surrogate pair.
	 * @see Character#isSurrogatePair(char, char)
	 */
	public static CodePointCharacter fromSurrogatePair(final char high, final char low) {
		checkArgument(Character.isSurrogatePair(high, low), "The values %d and %d do not comprise a code point surrogate pair.", high, low);
		return of(Character.toCodePoint(high, low));
	}

	/**
	 * Produces a code point character from a sequence of characters. If the character sequence has a single character, that will be the character represented. If
	 * the character sequence has two characters, the two characters must be the high and low characters of a surrogate pair; the resulting code point will be
	 * returned. The character sequence must not be empty or contain more than two characters.
	 * @param charSequence The low-surrogate code unit.
	 * @return A new code point character representing one or two characters in the given character sequence.
	 * @throws IllegalArgumentException if the given character sequence does not represent a single character.
	 */
	public static CodePointCharacter fromCharSequence(@Nonnull final CharSequence charSequence) {
		switch(charSequence.length()) {
			case 1:
				return CodePointCharacter.of(charSequence.charAt(0));
			case 2:
				return CodePointCharacter.fromSurrogatePair(charSequence.charAt(0), charSequence.charAt(1));
			default:
				throw new IllegalArgumentException(String.format("The character sequence %s does not represent a single code point.", charSequence));
		}
	}

	/**
	 * Convenience method to create a stream producing {@link CodePointCharacter} instances from the code points in the given character sequence. The
	 * {@link CodePointCharacter} instances are only created when they are needed in stream processing.
	 * @param charSequence The character sequence the code points of which should be processed.
	 * @return A stream of {@link CodePointCharacter} instances representing the code points of the given character sequence.
	 */
	public static Stream<CodePointCharacter> streamOf(@Nonnull final CharSequence charSequence) {
		return charSequence.codePoints().mapToObj(CodePointCharacter::of);
	}

	/**
	 * Determines whether the code point is in the Basic Multilingual Plane (BMP) and can thus be represented by a single <code>char</code>.
	 * @return <code>true</code> if the code point is in the BMP.
	 * @see #isBmpCodePoint()
	 */
	public boolean isBmpCodePoint() {
		return Character.isBmpCodePoint(codePoint);
	}

	/**
	 * Returns the primitive <code>char</code> represented by this code point. <strong>This method must only be called if the represented code point is in the
	 * Basic Multilingual Plane (BMP).</strong>
	 * @return A <code>char</code> containing this code point.
	 * @throws UnsupportedOperationException if the represented code point is not in the BMP.
	 * @see #isBmpCodePoint()
	 */
	public char toChar() throws UnsupportedOperationException {
		checkSupportedOperation(isBmpCodePoint(),
				"The code point value %d cannot be converted to a char because it falls outside the Basic Multilingual Plane (BMP).", codePoint);
		return (char)codePoint;
	}

	/**
	 * Returns a {@link Character} representing this this code point. <strong>This method must only be called if the represented code point is in the Basic
	 * Multilingual Plane (BMP).</strong>
	 * @return A {@link Character} containing this code point.
	 * @throws UnsupportedOperationException if the represented code point is not in the BMP.
	 * @see #isBmpCodePoint()
	 */
	public char toCharacter() throws UnsupportedOperationException {
		return Character.valueOf(toChar());
	}

	@Override
	public int hashCode() {
		return codePoint;
	}

	@Override
	public boolean equals(@Nonnull final Object object) {
		if(this == object) {
			return true;
		}
		if(!(object instanceof CodePointCharacter)) {
			return false;
		}
		return codePoint == ((CodePointCharacter)object).codePoint;
	}

	/**
	 * {@inheritDoc} This version return a string containing the character(s) represented by the given code point, which may be a surrogate pair of characters.
	 */
	@Override
	public String toString() {
		return isBmpCodePoint() ? Character.toString((char)codePoint) : new String(Character.toChars(codePoint));
	}
}
