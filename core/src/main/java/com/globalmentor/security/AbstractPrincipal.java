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

package com.globalmentor.security;

import java.security.Principal;

import com.globalmentor.model.DefaultIDed;

/**
 * An abstract implementation of a principal.
 * @author Garret Wilson
 */
public abstract class AbstractPrincipal extends DefaultIDed<String> implements Principal {

	/**
	 * @return The name of user as a principal.
	 * @see #getID()
	 */
	public String getName() {
		return getID();
	}

	/**
	 * Constructor specifying the principal name.
	 * @param name The name of the principal.
	 */
	public AbstractPrincipal(final String name) {
		super(name); //create the parent class with the name
	}

	/**
	 * Determines if the object is another principal with the same ID.
	 * @param object The object with which to compare this object.
	 * @return <code>true</code> if the object is a principal with the same ID.
	 */
	public boolean equals(Object object) {
		return object instanceof Principal && super.equals(object); //see if the object is a principal and it passes the default tests
	}
}
