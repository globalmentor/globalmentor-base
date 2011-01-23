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

package com.globalmentor.model;

import java.net.URI;
import java.util.UUID;

import static com.globalmentor.java.StringBuilders.*;
import static com.globalmentor.net.URIs.*;

/**Utilities for manipulating a universally unique identifier (UUID).
@author Garret Wilson 
*/
public class UUIDs
{
	/**The UUID URN namespace identifier "uuid".*/
	public final static String UUID_URN_NAMESPACE="uuid";

	/**Constructs a string of hexadecimal characters equivalent to the return value of {@link UUID#toString()} with all non-digits removed.
	@param uuid The UUID from which to construct a hex string.
	@return A pure hex string representing the UUID.
	*/
	public static String toHexString(final UUID uuid)
	{
		final StringBuilder hexStringBuilder=new StringBuilder(uuid.toString());	//create a string from the UUID
		return removeEvery(hexStringBuilder, '-').toString();	//remove all the hyphens and return the resulting string
	}

	/**Creates a URI from the UUID in the form <code>urn:uuid:<var>uuid</var></code>.
	@see <a href="http://www.ietf.org/rfc/rfc4122.txt">RFC 4122: A Universally Unique IDentifier (UUID) URN Namespace</a>  
	@param uuid The UUID.
	@return A URI representing the UUID.
	*/
	public static URI toURI(final UUID uuid)
	{
		return createURN(UUID_URN_NAMESPACE, uuid.toString());	//construct an return the URN
	}

}
