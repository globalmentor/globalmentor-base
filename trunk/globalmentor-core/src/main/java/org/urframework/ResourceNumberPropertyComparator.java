/*
 * Copyright © 2008-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package org.urframework;

import java.net.URI;

import com.globalmentor.collections.comparators.SerialDelegateComparator;
import com.globalmentor.collections.comparators.SortOrder;
import com.globalmentor.java.Numbers;

import static org.urframework.URF.*;

/**
 * Comparator that sorts resources based upon the numeric value of some identified property. This comparator does not sort unambiguously, and should be used as
 * a delegate comparator of a {@link SerialDelegateComparator}.
 * @author Garret Wilson
 * @see Numbers#sort(Object, Object, boolean)
 * @see SerialDelegateComparator
 */
public class ResourceNumberPropertyComparator extends AbstractResourcePropertyComparator
{

	/**
	 * Property URI constructor sorting in ascending order.
	 * @param propertyURI The property URI the value on which comparison will be made.
	 * @throws NullPointerException if the given property URI is <code>null</code>.
	 */
	public ResourceNumberPropertyComparator(final URI propertyURI)
	{
		this(propertyURI, SortOrder.ASCENDING); //construct the class using ascending order
	}

	/**
	 * Property and sort order constructor.
	 * @param propertyURI The property URI the value on which comparison will be made.
	 * @param sortOrder The order in which to perform comparisons.
	 * @throws NullPointerException if the given property URI and/or sort order is <code>null</code>.
	 */
	protected ResourceNumberPropertyComparator(final URI propertyURI, final SortOrder sortOrder)
	{
		super(propertyURI, sortOrder); //construct the parent class
	}

	/**
	 * {@inheritDoc} This version compares resources based upon their the values of their properties specified by {@link #getPropertyURI()}, but only if each value
	 * is a {@link Number}. Resources with no such property or with non-number property values are sorted before those with number property values.
	 * @see #getPropertyURI()
	 * @see Numbers#sort(Object, Object, boolean)
	 */
	@Override
	public int compareImpl(final URFResource resource1, final URFResource resource2)
	{
		final URI propertyURI = getPropertyURI();
		final Object propertyValue1 = asNumber(resource1.getPropertyValue(propertyURI)); //retrieve the property values, but only if they are numbers
		final Object propertyValue2 = asNumber(resource2.getPropertyValue(propertyURI)); //retrieve the property values, but only if they are numbers
		return Numbers.sort(propertyValue1, propertyValue2, true); //compare ambiguously; allow two non-numbers to be considered equivalent 
	}

}
