/*
 * Copyright © 2007-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
 * Converts an object from one type to another.
 * 
 * @author Garret Wilson
 */
public interface TypeConverter {

	/**
	 * Converts an object from one type to another. If the object is already of the correct type, no action occurs.
	 * @param T The type of object required.
	 * @param object The object to convert.
	 * @param type The class representing required type of the object.
	 * @return The object as the required type, or <code>null</code> if the object cannot be converted to the required type.
	 * @throws NullPointerException if the given object is <code>null</code>.
	 * @throws IllegalArgumentException if the given object should be able to be converted to the required type but something about its state, format, or contents
	 *           prevented the conversion.
	 */
	public <T> T convert(final Object object, final Class<T> type) throws IllegalArgumentException;

}
