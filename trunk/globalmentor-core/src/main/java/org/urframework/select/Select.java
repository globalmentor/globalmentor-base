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

package org.urframework.select;

import java.net.URI;

import static org.urframework.URF.*;

/**The URF select ontology.
@author Garret Wilson
*/
public class Select
{

	/**The URI to the URF select namespace.*/
	public final static URI SELECT_NAMESPACE_URI=URI.create("http://urf.name/select/");

		//properties
	/**Specifies the selected class of a class selector.*/
	public final static URI SELECT_CLASS_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectClass");
	/**Specifies the selected object of an object selector.*/
	public final static URI SELECT_OBJECT_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectObject");
	/**Specifies the selected property of a property selector.*/
	public final static URI SELECT_PROPERTY_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectProperty");
	/**Specifies the name of a selected property of an object property selector.*/
	public final static URI SELECT_PROPERTY_NAME_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectPropertyName");
	/**Specifies the selected resource of a resource selector.*/
	public final static URI SELECT_RESOURCE_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectResource");
	/**Specifies the selected URI of a URI selector.*/
	public final static URI SELECT_URI_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectURI");
	/**Specifies the selector to select one or more resources.*/
	public final static URI SELECTOR_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selector");

}
