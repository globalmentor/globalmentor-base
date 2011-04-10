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

import com.globalmentor.io.*;
import com.globalmentor.model.NameValuePair;

/**
 * Class that knows how to create objects to represent values provided in the lines of a <code>text/directory</code> as defined in <a
 * href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>, "A MIME Content-Type for Directory Information".
 * @author Garret Wilson
 */
public interface ValueFactory
{

	/**
	 * Processes the textual representation of a line's value and returns one or more object representing the value(s).
	 * <p>
	 * Whatever delimiter ended the value will be left in the reader.
	 * </p>
	 * @param profile The profile of this content line, or <code>null</code> if there is no profile.
	 * @param group The group specification, or <code>null</code> if there is no group.
	 * @param name The name of the information.
	 * @param paramList The list of parameters.
	 * @param valueType The type of value, or <code>null</code> if the type of value is unknown.
	 * @param reader The reader that contains the lines of the directory.
	 * @return An array of objects represent the value string, or <code>null</code> if the type of value cannot be determined by the given line information, in
	 *         which case no information is removed from the reader.
	 * @exception IOException Thrown if there is an error reading the directory.
	 * @exception ParseIOException Thrown if there is a an error interpreting the directory.
	 * @see NameValuePair
	 */
	public Object[] createValues(final String profile, final String group, final String name, final List<NameValuePair<String, String>> paramList,
			final String valueType, final Reader reader) throws IOException, ParseIOException;

}
