/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.net;

import static java.util.Objects.*;

import java.util.AbstractMap;

import com.globalmentor.model.Named;

/**
 * A URI query parameter name/value pair. The name cannot be <code>null</code>, but the value is allowed to be <code>null</code> indicating that no value should
 * be provided for this parameter in the URI query.
 * @author Garret Wilson
 */
public class URIQueryParameter extends AbstractMap.SimpleImmutableEntry<String, String> implements Named<String> {

	private static final long serialVersionUID = 1L;

	/**
	 * Constructor specifying the name and value.
	 * @param name The parameter name.
	 * @param value The parameter value.
	 * @throws NullPointerException if the given name is <code>null</code>.
	 */
	public URIQueryParameter(final String name, final String value) {
		super(requireNonNull(name, "URI query parameter name cannot be null."), value); //construct the parent class
	}

	/**
	 * {@inheritDoc}
	 * @apiNote This method is a convenience method that delegates to {@link #getKey()}.
	 */
	@Override
	public final String getName() {
		return getKey();
	}

}
