/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text.directory;

import java.util.*;

import com.globalmentor.model.NameValuePair;

/**
 * Abstract profile for predefined types of a <code>text/directory</code> as defined in <a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
 * "A MIME Content-Type for Directory Information".
 * @author Garret Wilson
 */
public abstract class AbstractProfile implements Profile {

	/** A map of value type strings keyed to supported type name strings. */
	private final Map<String, String> typeNameValueTypeMap = new HashMap<String, String>();

	/**
	 * Registers a value type keyed to the lowercase version of a type name.
	 * @param typeName The type name for which a value type should be retrieved.
	 * @param valueType The value type to associate with this type name.
	 */
	protected void registerValueType(final String typeName, final String valueType) {
		typeNameValueTypeMap.put(typeName.toLowerCase(), valueType); //put the value type in the map, keyed to the lowercase version of the type name		
	}

	/**
	 * Returns a value type keyed to the lowercase version of a type name.
	 * @param typeName The type name for which a value type should be associated.
	 * @return The value type associated with this type name, or <code>null</code> if no value type has been registered with the type name.
	 */
	protected String getValueType(final String typeName) {
		return (String)typeNameValueTypeMap.get(typeName.toLowerCase()); //get whatever value type we have associated with this type name, if any
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This version determines the value type by the value name.
	 * </p>
	 */
	public String getValueType(final String profile, final String group, final String name, final List<NameValuePair<String, String>> paramList) {
		return getValueType(name); //return whatever value type we have associated with this type name, if any
	}
}
