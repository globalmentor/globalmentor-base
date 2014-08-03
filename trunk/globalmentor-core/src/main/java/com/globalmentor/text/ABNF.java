/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Strings.*;

/**
 * Definitions for augmented BNF as defined by <a href="http://www.ietf.org/rfc/rfc2234.txt">RFC 2234</a>, "Augmented BNF for Syntax Specifications: ABNF".
 * @author Garret Wilson
 */
public class ABNF {

	/** Alphabetic characters: 0x41-5A / 0x61-7A (A-Z / a-z). */
	public final static String ALPHA_CHARS = createString((char)0x41, (char)0x5A) + createString((char)0x61, (char)0x7A);

	/** Character representing binary bits: "0" / "1". */
	public final static String BIT_CHARS = "01";

	/** Any 7-bit US-ASCII characters, excluding NUL: 0x01-7F. */
	public final static String CHAR_CHARS = createString((char)0x01, (char)0x7F);

	/** A carriage return character. */
	public final static char CR = 0x0D;

	/** A linefeed character. */
	public final static char LF = 0x0A;

	/** Internet standard newline. */
	public final static String CRLF = "" + CR + LF;

	/** Control characters: 0x00-1F / 0x7F. */
	public final static String CTL_CHARS = createString((char)0x00, (char)0x1F) + (char)0x7F;

	/** Digit characters: 0x30-39 (0-9). */
	public final static String DIGIT_CHARS = createString((char)0x30, (char)0x39);

	/** A double quote character. */
	public final static char DQUOTE = 0x22;

	/** Hexadecimal digits. */
	public final static String HEXDIG_CHARS = DIGIT_CHARS + "ABCDEF";

	/** A horizontal tab character. */
	public final static char HTAB = 0x09;

	/** A space character. */
	public final static char SP = 0x20;

	/** White space characters. */
	public final static String WSP_CHARS = "" + SP + HTAB;

	/** Linear whitespace (WSP / CRLF WSP). */
	public final static String LWSP_CHARS = WSP_CHARS + CRLF;

	/** Characters taking up 8 bits of data: 0x00-FF. */
	public final static String OCTET_CHARS = createString((char)0x00, (char)0xff);

	/** Visible (printing) characters: 0x21-7E. */
	public final static String VCHAR_CHARS = createString((char)0x21, (char)0x7E);

}
