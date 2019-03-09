/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.model;

/**
 * Utilities for working with human-readable representations of objects.
 * 
 * @author Garret Wilson
 * 
 */
public class Labels {

	/**
	 * Determines a human-readable representation of a given object.
	 * <p>
	 * If the object is {@link Labeled}, {@link Labeled#getLabel()} is returned. Otherwise, if the object is a {@link CharSequence} it is returned unchanged; if
	 * not, {@link Object#toString()} is returned.
	 * </p>
	 * @param object The object for which a label should be determined.
	 * @return A label meant for human consumption identifying the object.
	 * @throws NullPointerException if the given object is <code>null</code>.
	 * @see Labeled#getLabel()
	 */
	public static CharSequence getLabel(final Object object) {
		if(object instanceof Labeled) {
			return ((Labeled)object).getLabel();
		} else if(object instanceof CharSequence) {
			return (CharSequence)object;
		} else {
			return object.toString();
		}
	}

}
