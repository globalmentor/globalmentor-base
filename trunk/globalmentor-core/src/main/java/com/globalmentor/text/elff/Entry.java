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

package com.globalmentor.text.elff;

import java.util.*;

/**
 * An entry of the Extended Log File Format (ELFF).
 * @author Garret Wilson
 * @see <a href="file:///D:/reference/W3C/ELFF/WD-logfile.html">W3C Extended Log File Format</a>
 */
public class Entry {

	/** The map of values keyed to fields. */
	private final Map<Field<?>, Object> fieldValueMap = new HashMap<Field<?>, Object>();

	/**
	 * Retrieves a value from a field.
	 * @param <T> The type of value stored in the field.
	 * @param field The field from which the value should be retrieved.
	 * @return The value stored in the field, or <code>null</code> if no value is stored in the field.
	 */
	@SuppressWarnings("unchecked")
	//we only allow correct types to be stored in the map, so we expect the returned type to be correct
	public <T> T getFieldValue(final Field<T> field) {
		return (T)fieldValueMap.get(field); //retrieve the value from the field		
	}

	/**
	 * Stores a value in a field.
	 * @param <T> The type of value to store in the field; must be appropriate for the given field.
	 * @param field The field to which a value should be assigned.
	 * @param value The value to assign to the field, or <code>null</code> if no value should be stored in the field.
	 * @return The previous value stored in the field, or <code>null</code> if no value was stored in the field.
	 */
	@SuppressWarnings("unchecked")
	//we only allow correct types to be stored in the map, so we expect the returned type to be correct
	public <T> T setFieldValue(final Field<T> field, final T value) {
		if(value != null) { //if a value was given
			return (T)fieldValueMap.put(field, value); //store the value in the field
		} else { //if no value was given
			return (T)fieldValueMap.remove(field); //remove the value from the field
		}
	}
}
