package com.garretwilson.util;

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