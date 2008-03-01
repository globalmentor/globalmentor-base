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

package com.globalmentor.util;

import java.util.List;

/**Represents a list that is a subset of some larger list.
@author Garret Wilson
*/
public interface SubList<E> extends List<E>
{

	/**@return The size of the superlist of which this list is a sublist.*/
	public int getSuperListSize();

	/**@return The index of the superlist at which this list starts.*/
	public int getStartIndex();

}