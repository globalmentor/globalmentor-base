/*
 * Copyright © 1996-2017 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text;

import com.globalmentor.java.Characters;

/**
 * Definitions for augmented BNF as defined by <a href="http://www.ietf.org/rfc/rfc2234.txt"><cite>RFC 2234: Augmented BNF for Syntax Specifications:
 * ABNF</cite></a>.
 * @author Garret Wilson
 * @see <a href="http://www.ietf.org/rfc/rfc2234.txt">RFC 2234</a>
 */
public class ABNF {

	/** Alphabetic characters: 0x41-5A / 0x61-7A (A-Z / a-z). */
	public static final Characters ALPHA_CHARACTERS = Characters.ofRange((char)0x41, (char)0x5A).addRange((char)0x61, (char)0x7A);

	/** Character representing binary bits: "0" / "1". */
	public static final Characters BIT_CHARACTERS = Characters.of('0', '1');

	/** Any 7-bit US-ASCII characters, excluding NUL: 0x01-7F. */
	public static final Characters CHAR_CHARACTERS = Characters.ofRange((char)0x01, (char)0x7F);

	/** A carriage return character. */
	public static final char CR = 0x0D;

	/** A linefeed character. */
	public static final char LF = 0x0A;

	/** Internet standard newline. */
	public static final String CRLF = "" + CR + LF;

	/** Control characters: 0x00-1F / 0x7F. */
	public static final Characters CTL_CHARACTERS = Characters.ofRange((char)0x00, (char)0x1F).add((char)0x7F);

	/** Digit characters: 0x30-39 (0-9). */
	public static final Characters DIGIT_CHARACTERS = Characters.ofRange((char)0x30, (char)0x39);

	/** A double quote character. */
	public static final char DQUOTE = 0x22;

	/**
	 * Hexadecimal digits.
	 * <p>
	 * Note that this definition differs from {@link ASCII#HEX_CHARACTERS} in that this definition, following <cite>RFC 2234</cite>, does not include lowercase
	 * letters.
	 * </p>
	 */
	public static final Characters HEXDIG_CHARACTERS = DIGIT_CHARACTERS.addRange('A', 'F');

	/** A horizontal tab character. */
	public static final char HTAB = 0x09;

	/** A space character. */
	public static final char SP = 0x20;

	/** White space characters. */
	public static final Characters WSP_CHARACTERS = Characters.of(SP, HTAB);

	/** Linear whitespace (WSP / CRLF WSP). */
	public static final Characters LWSP_CHARACTERS = WSP_CHARACTERS.add(CRLF);

	/** Characters taking up 8 bits of data: 0x00-FF. */
	public static final Characters OCTET_CHARACTERS = Characters.ofRange((char)0x00, (char)0xff);

	/** Visible (printing) characters: 0x21-7E. */
	public static final Characters VCHAR_CHARACTERS = Characters.ofRange((char)0x21, (char)0x7E);

}
