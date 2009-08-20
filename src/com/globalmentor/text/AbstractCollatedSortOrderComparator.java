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

package com.globalmentor.text;

import java.text.Collator;

import com.globalmentor.collections.comparators.AbstractSortOrderComparator;
import com.globalmentor.collections.comparators.SortOrder;
import com.globalmentor.util.*;
import static com.globalmentor.java.Objects.*;

/**A comparator that uses a collator for text sorting.
@param <T> The type of objects that may be compared by this comparator.
@author Garret Wilson
*/
public abstract class AbstractCollatedSortOrderComparator<T> extends AbstractSortOrderComparator<T> implements CollatorFactory
{

	/**The source of collators.*/
	private final CollatorFactory collatorFactory;

	/**Retrieves an instance of a collator.
	@return An instance of a collator.
	*/
	public Collator getCollatorInstance()
	{
		return collatorFactory.getCollatorInstance();	//delegate to the internal collator factory
	}

	/**Collator factory and sort order constructor.
	@param collatorFactory The source of collators.
	@param sortOrder The order in which to perform comparisons.
	@throws NullPointerException if the given collator factory and/or sort order is <code>null</code>.
	*/
	public AbstractCollatedSortOrderComparator(final CollatorFactory collatorFactory, final SortOrder sortOrder)
	{
		super(sortOrder);	//construct the parent class
		this.collatorFactory=checkInstance(collatorFactory, "Collator factory cannot be null.");
	}

}
