/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

import java.util.Comparator;

import com.globalmentor.collections.comparators.SortOrder;

/**
 * Utilities for working with comparables.
 * @author Garret Wilson
 * @see Comparable
 */
public class Comparables
{

	/**
	 * Compares two comparables for order in ascending order. Returns a negative integer, zero, or a positive integer as the first argument is less than, equal
	 * to, or greater than the second. Identical comparables are always considered equal. This method functions exactly as if the two comparables were compared
	 * using their {@link Comparable#compareTo(Object)} argument, except:
	 * <ul>
	 * <li>Identical comparables are recognized as such without delegating to the actual {@link Comparable#compareTo(Object)} method.</li>
	 * <li>This method allows <code>null</code> arguments, considering a <code>null</code> comparable to be lower than a non-<code>null</code> comparable.</li>
	 * </ul>
	 * This method matches the semantics of {@link Comparator#compare(Object, Object)}, except that this method allows <code>null</code> arguments.
	 * @param <T> the type of comparables being compared.
	 * @param comparable1 The first comparable to be compared, or <code>null</code> if the comparable is not available.
	 * @param comparable2 The second comparable to be compared, or <code>null</code> if the comparable is not available.
	 * @return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	 * @throws ClassCastException if the arguments' types prevent them from being compared.
	 * @see Comparable#compareTo(Object)
	 */
	public static <T extends Comparable<? super T>> int compare(final T comparable1, final T comparable2)
	{
		return compare(comparable1, comparable2, SortOrder.ASCENDING); //compare in ascending order
	}

	/**
	 * Compares two comparables for order using the specified sort order. Returns a negative integer, zero, or a positive integer as the first argument is less
	 * than, equal to, or greater than the second. Identical comparables are always considered equal. This method functions exactly as if the two comparables were
	 * compared using their {@link Comparable#compareTo(Object)} argument, except:
	 * <ul>
	 * <li>Identical comparables are recognized as such without delegating to the actual {@link Comparable#compareTo(Object)} method.</li>
	 * <li>This method allows <code>null</code> arguments, considering a <code>null</code> comparable to be lower than a non-<code>null</code> comparable.</li>
	 * </ul>
	 * @param <T> the type of comparables being compared.
	 * @param comparable1 The first comparable to be compared, or <code>null</code> if the comparable is not available.
	 * @param comparable2 The second comparable to be compared, or <code>null</code> if the comparable is not available.
	 * @param sortOrder The order in which to perform comparisons.
	 * @return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	 * @throws NullPointerException if the given sort order is <code>null</code>.
	 * @throws ClassCastException if the arguments' types prevent them from being compared.
	 * @see Comparable#compareTo(Object)
	 */
	public static <T extends Comparable<? super T>> int compare(final T comparable1, final T comparable2, final SortOrder sortOrder)
	{
		if(comparable1 == comparable2) //if the comparables are identical
		{
			return 0; //identical comparables are always equal
		}
		if(comparable1 != null) //if the first comparable is not null
		{
			if(comparable2 != null) //if the second comparable is not null
			{
				return sortOrder == SortOrder.ASCENDING ? comparable1.compareTo(comparable2) : comparable2.compareTo(comparable1); //compare in the requested order
			}
			else
			//if only the first comparable is not null
			{
				return sortOrder == SortOrder.ASCENDING ? 1 : -1; //null comparables should be sorted lower
			}
		}
		else
		//if the first comparable is null
		{
			assert comparable2 != null : "Both comparables cannot be null, because we already checked for identity.";
			return sortOrder == SortOrder.ASCENDING ? -1 : 1; //null comparables should be sorted lower
		}
	}

}
