/*
 * Copyright © 2007-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.iso.datetime;

/**
 * The base for all ISO 8601 types.
 * @author Garret Wilson
 */
public interface ISOTemporal {

	/**
	 * Appends the canonical lexical representation of this temporal.
	 * @param stringBuild The string builder to which the lexical representation will be appended.
	 * @return The string builder.
	 */
	public StringBuilder append(final StringBuilder stringBuilder);

	/**
	 * Returns the canonical lexical representation of this temporal
	 * @return The canonical lexical representation of this temporal.
	 */
	public String toString();

}
