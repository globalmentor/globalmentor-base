/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Classes.*;
import static org.urframework.URF.*;

/**An URF resource that automatically adds an <code>urf:type</code> property of the local name of its Java class.
@author Garret Wilson
*/
public abstract class AbstractClassTypedURFResource extends DefaultURFResource
{

	/**URI and type namespace URI constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeNamespaceURI The namespace URI of the URI of the type to be added.
	@exception NullPointerException if the given type type namespace URI is <code>null</code>.
	*/
	public AbstractClassTypedURFResource(final URI uri, final URI typeNamespaceURI)
	{
		super(uri);  //construct the parent class
		addType(new DefaultURFResource(createResourceURI(typeNamespaceURI, getLocalName(getClass()))));	//add the default type based upon the given type namespace URI and the local name of the class
	}

}