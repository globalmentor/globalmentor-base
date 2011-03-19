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

/**A selector that selects no objects.
@author Garret Wilson
*/
public class EmptySelector extends AbstractSelector
{

	/**Default constructor.*/
	public EmptySelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The uri for the new resource.
	*/
	public EmptySelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Determines if this selector selects a given object.
	This version always returns <code>false</code>.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>false</code>
	*/
	public boolean selects(final Object object)
	{
		return false;	//the empty selector selects nothing
	}
}