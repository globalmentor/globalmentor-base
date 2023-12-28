/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.model;

import java.security.Principal;

/**
 * A representation of an individual.
 * @param <I> The type of user ID.
 * @author Garret Wilson
 */
public interface User<I> extends IDed<I>, Principal {

	/**
	 * Returns the first name of the user.
	 * @return The first name of the user.
	 */
	public String getFirstName();

	/**
	 * Returns the last name of the user.
	 * @return The last name of the user.
	 */
	public String getLastName();

	/**
	 * Returns the composite name of the user.
	 * @return The composite name of the user.
	 */
	public String getFullName();

	/**
	 * Returns the password of the user.
	 * @return The password of the user.
	 */
	public char[] getPassword();

}
