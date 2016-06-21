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

import com.globalmentor.java.Integers;

/**
 * General information and utilities for working with Unicode.
 * @author Garret Wilson
 */
public class Unicode {

	/**
	 * A map of Unicode characters stored at Adobe Symbol font indexes. Conversion from the symbol font to Unicode is be performed using:
	 * <code>unicodeChar=SYMBOL_FONT_TO_UNICODE_TABLE[symbolChar];</code>
	 * 
	 * <p>
	 * Implemented by referencing the "Encoding Symbol Font" table provided by Oscar van Vlijmen (<a href="mailto:o.van.vlijmen@tip.nl">o.van.vlijmen@tip.nl</a>).
	 * </p>
	 * <p>
	 * Note that several characters are not completely encoded; their complete representation, as given by Oscar van Vlijmen's table, are as follows:
	 * </p>
	 * <ul>
	 * <li>0xE6: 0x0028+0xF870</li>
	 * <li>0xE7: 0x0028+0xF871</li>
	 * <li>0xE8: 0x0028+0xF872</li>
	 * <li>0xE9: 0x005B+0xF870</li>
	 * <li>0xEA: 0x005B+0xF871</li>
	 * <li>0xEB: 0x005B+0xF872</li>
	 * <li>0xEC: 0x007B+0xF870</li>
	 * <li>0xED: 0x007B+0xF871</li>
	 * <li>0xEE: 0x007B+0xF872</li>
	 * <li>0xF4: 0x222B+0xF871</li>
	 * <li>0xF6: 0x0029+0xF870</li>
	 * <li>0xF7: 0x0029+0xF871</li>
	 * <li>0xF8: 0x0029+0xF872</li>
	 * <li>0xF9: 0x005D+0xF870</li>
	 * <li>0xFA: 0x005D+0xF871</li>
	 * <li>0xFB: 0x005D+0xF872</li>
	 * <li>0xFC: 0x007D+0xF870</li>
	 * <li>0xFD: 0x007D+0xF871</li>
	 * <li>0xFE: 0x007D+0xF872</li>
	 * </ul>
	 * @see <a href="http://www.tip.nl/users/o.van.vlijmen/charsets.html">Encoding Symbol Font</a>
	 */
	public static final char[] SYMBOL_FONT_TO_UNICODE_TABLE = { //TODO convert to something immutable or hide
			//  0       1       2       3       4       5       6       7       8       9       A       B       C       D       E       F
			0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, //000X
			0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017, 0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F, //001X
			0x0020, 0x0021, 0x2200, 0x0023, 0x2203, 0x0025, 0x0026, 0x220D, 0x0028, 0x0029, 0x2217, 0x002B, 0x002C, 0x2212, 0x002E, 0x002F, //002X
			0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, 0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F, //003X
			0x2245, 0x0391, 0x0392, 0x03A7, 0x0394, 0x0395, 0x03A6, 0x0393, 0x0397, 0x0399, 0x03D1, 0x039A, 0x039B, 0x039C, 0x039D, 0x039F, //004X
			0x03A0, 0x0398, 0x03A1, 0x03A3, 0x03A4, 0x03A5, 0x03C2, 0x03A9, 0x039E, 0x03A8, 0x0396, 0x005B, 0x2234, 0x005D, 0x22A5, 0x005F, //005X
			0xF8E5, 0x03B1, 0x03B2, 0x03C7, 0x03B4, 0x03B5, 0x03C6, 0x03B3, 0x03B7, 0x03B9, 0x03D5, 0x03B1, 0x03BB, 0x03BC, 0x03BD, 0x03BF, //006X
			0x03C0, 0x03B8, 0x03C1, 0x03C3, 0x03C4, 0x03C5, 0x03D6, 0x03C9, 0x03BE, 0x03C8, 0x03B6, 0x007B, 0x007C, 0x007D, 0x223C, 0x007F, //007X
			0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E, 0x008F, //008X
			0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097, 0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E, 0x009F, //009X
			0x20AC, 0x03D2, 0x2032, 0x2264, 0x2044, 0x221E, 0x0192, 0x2663, 0x2666, 0x2665, 0x2660, 0x2192, 0x2190, 0x2191, 0x2192, 0x2193, //00AX
			0x00B0, 0x00B1, 0x2033, 0x2265, 0x00D7, 0x221D, 0x2202, 0x2022, 0x00F7, 0x2260, 0x2261, 0x2248, 0x2026, 0xF8E6, 0xF8E7, 0x21B5, //00BX
			0x2135, 0x2111, 0x211C, 0x2118, 0x2297, 0x2295, 0x2205, 0x2229, 0x222A, 0x2283, 0x2287, 0x2284, 0x2282, 0x2286, 0x2208, 0x2209, //00CX
			0x2220, 0x2207, 0x00AE, 0x00A9, 0x2122, 0x220F, 0x221A, 0x22C5, 0x00AC, 0x2227, 0x2228, 0x21D4, 0x21D0, 0x21D1, 0x21D2, 0x21D3, //00DX
			0x22C4, 0x3008, 0x00AE, 0x00A9, 0x2122, 0x2211, 0x0028, 0x0028, 0x0028, 0x005B, 0x005B, 0x005B, 0x007B, 0x007B, 0x007B, 0xF8F4, //00EX
			0xF8FF, 0x3009, 0x222B, 0x2320, 0x222B, 0x2321, 0x0029, 0x0029, 0x0029, 0x005D, 0x005D, 0x005D, 0x007D, 0x007D, 0x007D, 0x00FF //00FX
	};

	/**
	 * Creates a string representation of a given Unicode code point in the form "U+XXXX[XX]".
	 * @param codeValue The Unicode code point to represent.
	 * @return A string representation of the Unicode code point in the form "U+XXXX[XX]"
	 */
	public static String getCodePointString(final int codeValue) {
		final StringBuilder stringBuilder = new StringBuilder("U+"); //create a string buffer
		//append the code value, using six digits if needed
		stringBuilder.append(ASCII.toUpperCase(Integers.toHexString(codeValue, codeValue <= 0xFFFF ? 4 : 6)));
		return stringBuilder.toString(); //return the string we constructed		
	}

}