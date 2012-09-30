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

package org.urframework;

import java.net.URI;
import java.util.List;

import com.globalmentor.model.NameValuePair;
import com.globalmentor.net.ReferenceResource;

/**
 * Represents an assertion in URF.
 * 
 * <p>
 * For efficiency the object will be either a value object (such as a primitive class instance, a string, or an immutable object instance) or a
 * {@link ReferenceResource}, the URI of which will indicate the referent object.
 * </p>
 * 
 * @author Garret Wilson
 * 
 */
public interface URFAssertion
{
	/** @return The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value. */
	public List<NameValuePair<URI, Object>> getScopeChain();

	/** @return The URI of the assertion subject. */
	public URI getSubjectURI();

	/** @return The URI of the assertion predicate. */
	public URI getPredicateURI();

	/** @return The assertion object; either a value object or a {@link ReferenceResource}. */
	public Object getObject();
}
