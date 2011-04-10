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

import java.io.*;
import java.util.*;

import com.globalmentor.model.NameValuePair;

/**
 * Class that knows how to serialize values provided in the lines of a <code>text/directory</code> as defined in <a
 * href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>, "A MIME Content-Type for Directory Information".
 * @author Garret Wilson
 */
public interface ValueSerializer
{

	/**
	 * Serializes a line's value.
	 * <p>
	 * Only the value will be serialized, not any previous or subsequent parts of the line or delimiters.
	 * </p>
	 * @param profile The profile of this content line, or <code>null</code> if there is no profile.
	 * @param group The group specification, or <code>null</code> if there is no group.
	 * @param name The name of the information.
	 * @param paramList The list of parameters, each item of which is a {@link NameValuePair} with a name of type {@link String} and a value of type
	 *          {@link String}.
	 * @param value The value to serialize.
	 * @param valueType The type of value, or <code>null</code> if the type of value is unknown.
	 * @param writer The writer to which the directory information should be written.
	 * @return <code>true</code> if the operation was successful, else <code>false</code> if this class does not support writing the given value.
	 * @exception IOException Thrown if there is an error writing to the directory.
	 * @see NameValuePair
	 */
	public boolean serializeValue(final String profile, final String group, final String name, final List<NameValuePair<String, String>> paramList,
			final Object value, final String valueType, final Writer writer) throws IOException;

}
