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

package org.urframework.maqro;

import java.net.URI;

import org.urframework.*;


import static com.globalmentor.java.Objects.*;
import static com.globalmentor.java.Numbers.*;
import static org.urframework.URF.*;
import static org.urframework.maqro.MAQRO.*;

/**A selector of MAQRO group selection criteria.
@author Garret Wilson
*/
public class Selector extends AbstractClassTypedURFResource
{

	/**Default constructor.*/
	public Selector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public Selector(final URI uri)
	{
		super(uri, MAQRO_NAMESPACE_URI);  //construct the parent class
	}

	/**@return The number of interactions to include, or -1 if the question count is not indicated.*/
	public long getCount()
	{
		return asIntegralValue(asInstance(asObject(getPropertyValue(COUNT_PROPERTY_URI)), Number.class));	//get the integer question count
	}

	/**Sets the number of interactions to include.
	@param count The number of interactions to include.
	*/
	public void setCount(final long interactionCount)
	{
		setPropertyValue(COUNT_PROPERTY_URI, interactionCount);	//set the interaction count
	}

	/**@return The list of filters for this selection, or <code>null</code> if there is no list of filters or the value is not a list.*/
/*TODO fix
	public RDFListResource getFilters()
	{
		return RDFResources.asListResource(getPropertyValue(MAQRO_NAMESPACE_URI, FILTERS_PROPERTY_NAME));	//get the maqro:filters property value as a list	
	}
*/

	/**Sets the list of filters.
	@param selectors The list of filters.
	*/
/*TODO fix
	public void setFilters(final RDFListResource selectors)
	{
		setProperty(MAQRO_NAMESPACE_URI, FILTERS_PROPERTY_NAME, selectors);	//set the filters
	}
*/

}