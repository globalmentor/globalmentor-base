/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Objects.checkInstance;

import com.globalmentor.model.NameValuePair;

/**
 * A URI query parameter name/value pair. The name cannot be <code>null</code>, but the value is allowed to be <code>null</code> indicating that no value should
 * be provided for this parameter in the URI query.
 * @author Garret Wilson
 */
public class URIQueryParameter extends NameValuePair<String, String>
{
	/**
	 * Constructor specifying the name and value.
	 * @param name The parameter name.
	 * @param value The parameter value.
	 * @exception NullPointerException if the given name is <code>null</code>.
	 */
	public URIQueryParameter(final String name, final String value)
	{
		super(checkInstance(name, "URI query parameter name cannot be null."), value); //construct the parent class
	}

}
