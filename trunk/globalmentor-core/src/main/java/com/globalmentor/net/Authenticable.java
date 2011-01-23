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

package com.globalmentor.net;

import java.net.PasswordAuthentication;
import java.net.URI;

/**Presents a method for retrieving user authentication credentials.
@author Garret Wilson
*/
public interface Authenticable
{

	/**Determines user and password information.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
//TODO fix	public PasswordAuthentication getPasswordAuthentication();

	/**Determines password information for a given user.
	The user must not be allowed to change the username.
	@param username The user for which password information should be gathered.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
//TODO fix	public PasswordAuthentication getPasswordAuthentication(final String username);

	/**Determines password information in relation to a given description.
	@param prompt A description of the authentication.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
	public PasswordAuthentication getPasswordAuthentication(final String prompt);

	/**Determines password information in relation to a given URI and description.
	@param uri The URI for which authentication is requested, or <code>null</code> if there is no relevant URI.
	@param prompt A description of the authentication.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
	public PasswordAuthentication getPasswordAuthentication(final URI uri, final String prompt);

	/**Determines password information for a given user in relation to a given URI and description.
	The user must not be allowed to change the username, if one is provided.
	@param uri The URI for which authentication is requested, or <code>null</code> if there is no relevant URI.
	@param prompt A description of the authentication.
	@param username The user for which password information should be gathered, or <code>null</code> if the username is not restricted.
	@return The password authentication collected from the user, or <code>null</code> if none is provided.
	*/
	public PasswordAuthentication getPasswordAuthentication(final URI uri, final String prompt, final String username);

}
