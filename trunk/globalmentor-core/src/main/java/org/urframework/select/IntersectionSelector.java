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

/**A selector that selects an object based upon the intersection of subselectors.
@author Garret Wilson
*/
public class IntersectionSelector extends AbstractOperatorSelector
{

	/**Default constructor.*/
	public IntersectionSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public IntersectionSelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Determines if this selector selects a given object.
	This version returns the logical intersection of its subselectors, or <code>false</code> if there are no subselectors.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@see #getSelectors()
	*/
	public boolean selects(final Object object)
	{
		boolean selects=false;	//there has to be at least one subselector before this selector can select anything
		for(final Selector selector:getSelectors())	//for each subselector
		{
			selects=selector.selects(object);	//see if the subselector selects the object
			if(!selects)	//if any subselector doesn't select the object
			{
				break;	//one negative spoils an entire intersection
			}
		}
		return selects;	//return whether or not the intersection was successful
	}
}