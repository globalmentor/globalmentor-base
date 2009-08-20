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

package com.globalmentor.collections;

import java.util.List;

/**Utilities to be used with lists.
@author Garret Wilson
*/
public class Lists
{

	/**Replaces the first ocurrence of the given object with a new object.
		If the object does not appear in the list, no action is taken.
	@param list The list in which the object is included.
	@param oldObject The object to replace.
	@param newObject The object to take the place of the old object.
	*/
	public final static <E> void replace(final List<E> list, final E oldObject, final E newObject)
	{
		final int index=list.indexOf(oldObject);  //get the index of the old object
		if(index>=0)  //if the old item exists
		{
			list.set(index, newObject); //replace the item at that index
		}
	}

}