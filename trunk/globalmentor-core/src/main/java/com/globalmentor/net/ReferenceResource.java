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

import java.net.URI;

import com.globalmentor.java.Objects;

/**
 * A class that represents a reference to another resource via its URI.
 * @author Garret Wilson
 */
public class ReferenceResource extends AbstractResource
{

	private final URI uri;

	/** {@inheritDoc} */
	public URI getURI()
	{
		return uri;
	}

	/**
	 * URI constructor.
	 * @param uri The resource identifier URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 */
	public ReferenceResource(final URI uri)
	{
		this.uri = Objects.checkInstance(uri, "URI cannot be null.");
	}

}