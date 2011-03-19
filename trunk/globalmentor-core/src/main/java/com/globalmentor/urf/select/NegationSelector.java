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

package com.globalmentor.urf.select;

import java.net.URI;

/**A selector that selects an object based upon the negation of a subselector.
@author Garret Wilson
*/
public class NegationSelector extends AbstractOperatorSelector
{

	/**Default constructor.*/
	public NegationSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public NegationSelector(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Determines if this selector selects a given object.
	This version returns the logical negation of its subselector, or <code>false</code> if there is no subselector.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@see #getSelector()
	*/
	public boolean selects(final Object object)
	{
		final Selector selector=getSelector();	//get the selector
		if(selector!=null)	//if there is a selector
		{
			return !selector.selects(object);	//return the negation of its selection
		}
		return false;	//if there is no subselector, there is no selection
	}
}