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

import com.globalmentor.java.Objects;
import com.globalmentor.net.Resource;

import static org.urframework.URF.*;
import static org.urframework.select.Select.*;

/**A selector that selects a resource based upon its URI.
@author Garret Wilson
*/
public class URISelector extends AbstractSelector
{

	/**Default constructor.*/
	public URISelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public URISelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Returns the URI identified by this selector.
	@return This selector's URI designation, or <code>null</code> if this selector has no <code>selectURI</code> property with a URI value.
	@see Select#SELECT_URI_PROPERTY_URI
	*/
	public URI getSelectURI()
	{
		return asURI(getPropertyValue(SELECT_URI_PROPERTY_URI));	//get the selectURI property as a URI
	}

	/**Sets the URI identified by this selector.
	@param selectURI The URI to be selected.
	@see Select#SELECT_URI_PROPERTY_URI
	*/
	public void setSelectURI(final URI selectURI)
	{
		setPropertyValue(SELECT_URI_PROPERTY_URI, selectURI);	//set the given select URI
	}

	/**Determines if this selector selects a given object.
	A selector with no select URI will match only anonymous resources.
	This version returns <code>true</code> if the object is a {@link Resource} and the URI of the given resource matches the URI specified by this selector, if any.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@see #getSelectURI()
	*/
	public boolean selects(final Object object)
	{
		return object instanceof Resource && Objects.equals(getSelectURI(), ((Resource)object).getURI());	//if the object is a resource, compare the resource's URI with the select URI, if any
	}
}