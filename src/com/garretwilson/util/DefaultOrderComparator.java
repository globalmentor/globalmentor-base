package com.garretwilson.util;

import java.util.*;

/**A comparator that compares object according to their default order,
	as specified in a list.
<p>Derived classes may use this comparison algorithm as a default fallback.
@author Garret Wilson
*/
public class DefaultOrderComparator<T> implements Comparator<T>
{
		
	/**The list that determines the default order of the objects.*/ 
	private final List<T> defaultOrderList;

		/**@return The list that determines the default order of the objects.*/ 
		protected List<T> getDefaultOrderList() {return defaultOrderList;}
		
	/**Constructs a comparator to compare objects based upon the order in which
		they appear in a given list.
	@param defaultOrderList The list that determines the default order of the
		objects. This must not a different list than any list being sorted.
	*/
	public DefaultOrderComparator(final List<T> defaultOrderList)
	{
		this.defaultOrderList=defaultOrderList;	//save the default order list
	}

	/**Compares actions by the order in which the appear in the default order list.
	@param object1 The first object to be compared.
	@param object2 The second object to be compared.
	@return A negative integer, zero, or a positive integer as the
		first argument is less than, equal to, or greater than the second. 
	@throws ClassCastException Thrown if the arguments' types prevent them from
		being compared by this comparator.
	@throws IllegalArgumentException Thrown if one of the objects is not in
		the default order list.
	@see #getDefaultOrderList()
	*/
	public int compare(final T object1, final T object2)
	{
		final int index1=getDefaultOrderList().indexOf(object1);	//get the first index
		if(index1<0)	//if the object is not in the list
		{
			throw new IllegalArgumentException("Object "+object1+" has no default order.");
		}
		final int index2=getDefaultOrderList().indexOf(object2);	//get the second index
		if(index2<0)	//if the object is not in the list
		{
			throw new IllegalArgumentException("Object "+object2+" has no default order.");
		}
		return index1-index2;	//compare the indexes in the default order list
	}
}
