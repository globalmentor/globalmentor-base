/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.util;

import java.util.*;

/**
 * Various utilities for working with a string tokenizer.
 * @author Garret Wilson
 */
public final class StringTokenizers {

	private StringTokenizers() {
	}

	/**
	 * Retrieves all remaining tokens from a string tokenizer and returns them in an array of strings.
	 * @param stringTokenizer The string tokenizer from which to retrieve tokens.
	 * @return An array of all remaining tokens in the string tokenizer.
	 */
	public static String[] getTokens(final StringTokenizer stringTokenizer) {
		final String[] tokens = new String[stringTokenizer.countTokens()]; //create an array in which to place the tokens
		for(int i = 0; i < tokens.length; ++i) { //fill at each position in the array
			tokens[i] = stringTokenizer.nextToken(); //fill this position in the array with the next token
		}
		return tokens; //return the tokens we extracted from the string tokenizer
	}
}
