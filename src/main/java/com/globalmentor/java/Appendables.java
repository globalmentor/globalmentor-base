/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

import java.io.IOException;

/**
 * Utilities for working with {@link Appendable} objects.
 * 
 * @author Garret Wilson
 * 
 * @see Appendable
 */
public class Appendables {

	private Appendables() {
	}

	/**
	 * Appends a given repetition of characters to an appendable.
	 * @param appendable The appendable to which the characters should be appended.
	 * @param character The character to append.
	 * @param count The number of repetitions of the character.
	 * @return The appendable with the appended repetitions of the character.
	 * @throws NullPointerException if the given appendable is <code>null</code>.
	 * @throws IOException if there is an error appending to the appendable.
	 */
	public static <A extends Appendable> A append(final A appendable, final char character, int count) throws IOException {
		for(; count > 0; --count) {
			appendable.append(character);
		}
		return appendable;
	}

}
