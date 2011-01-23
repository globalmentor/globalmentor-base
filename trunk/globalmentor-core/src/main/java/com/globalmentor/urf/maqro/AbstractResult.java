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

package com.globalmentor.urf.maqro;

import java.net.URI;

import com.globalmentor.urf.AbstractClassTypedURFResource;
import static com.globalmentor.urf.maqro.MAQRO.*;

/**An abstract implementation of a result of an outcome evaluation.
@author Garret Wilson
@see Outcome
*/
public abstract class AbstractResult extends AbstractClassTypedURFResource implements Result
{

	/**URI constructor with a type namespace of {@value MAQRO#MAQRO_NAMESPACE_URI}.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	*/
	public AbstractResult(final URI uri)
	{
		this(uri, MAQRO_NAMESPACE_URI);	//construct the parent class
	}

	/**URI and type namespace URI constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeNamespaceURI The namespace URI of the URI of the type to be added.
	@exception NullPointerException if the given type type namespace URI is <code>null</code>.
	*/
	public AbstractResult(final URI uri, final URI typeNamespaceURI)
	{
		super(uri, typeNamespaceURI);	//construct the parent class
	}

}
