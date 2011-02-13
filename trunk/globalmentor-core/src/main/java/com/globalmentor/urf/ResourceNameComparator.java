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

package com.globalmentor.urf;

import java.net.*;

import com.globalmentor.collections.comparators.SerialDelegateComparator;
import com.globalmentor.collections.comparators.SortOrder;
import com.globalmentor.net.*;
import static com.globalmentor.net.URIs.*;
import com.globalmentor.text.*;
import com.globalmentor.util.*;

/**Sorts resources based upon name---the unencoded last component of the resource URI.
This comparator does not sort unambiguously, and should be used as a delegate comparator of a {@link SerialDelegateComparator}.
@author Garret Wilson
@see URIs#getName(URI)
@see SerialDelegateComparator
*/
public class ResourceNameComparator extends AbstractCollatedSortOrderComparator<URFResource>
{

	/**Collator factory constructor sorting in ascending order.
	@param collatorFactory The source of collators.
	@throws NullPointerException if the given collator factory is <code>null</code>.
	*/
	public ResourceNameComparator(final CollatorFactory collatorFactory)
	{
		this(collatorFactory, SortOrder.ASCENDING);	//construct the class using ascending order
	}

	/**Collator factory and sort order constructor.
	@param collatorFactory The source of collators.
	@param sortOrder The order in which to perform comparisons.
	@throws NullPointerException if the given collator factory and/or sort order is <code>null</code>.
	*/
	public ResourceNameComparator(final CollatorFactory collatorFactory, final SortOrder sortOrder)
	{
		super(collatorFactory, sortOrder);	//construct the parent class
	}

	/**Compares two resources for order.
	Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	Identical resources are always considered equal.
	<p>This version compares resources based upon their decoded URI name.
	Resources without a name are considered less than those with a name property.</p>
	@param resource1 The first resource to be compared.
	@param resource2 The second resource to be compared.
	@return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	@throws ClassCastException if the arguments' types prevent them from being compared by this comparator.
	@see URIs#getName(URI)
	*/
	public int compare(final URFResource resource1, final URFResource resource2)
	{
		if(resource1==resource2)	//if the resources are identical
		{
			return 0;	//identical resources are always equal
		}
		final URI resource1URI=resource1.getURI();		
		final URI resource2URI=resource2.getURI();
		final String resource1Name=resource1URI!=null ? getName(resource1URI) : null;
		final String resource2Name=resource2URI!=null ? getName(resource2URI) : null;
		return Text.compare(resource1Name, resource2Name, getCollatorInstance(), getSortOrder());	//compare resource names using the correct sort order
	}

}
