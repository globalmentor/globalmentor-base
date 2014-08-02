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

import java.util.*;

/**Various vector manipulating functions.
@author Garret Wilson
@see Comparable
*/
public class Vectors
{

	/**Randomizes the elements of the vector.
	@param vector The vector to be randomized.
	@return The same vector, randomized.
	*/
	public static <E> Vector<E> randomize(final Vector<E> vector)
	{
		Random random=new Random();	//create a randomizing object
		return randomize(vector, random);	//randomize the vector and return it
	}

	/**Sorts a vector containing {@link Comparable} objects using a quicksort algorithm.
	@param vector The vector to be sorted.
	@throws ClassCastException if an element in the vector does not implement {@link Comparable}.
	@see #quickSort(Vector, int, int)
	@see Comparable
	*/
	public static <E> void sort(final Vector<E> vector)
	{
		quickSort(vector, 0, vector.size()-1);	//start the quicksort process on the entire vector
	}

	/**Sorts a vector containing {@link Comparable} objects using a quicksort algorithm.
	Modified from the QSortAlgorithm.java 1.3 program by James Gosling Copyright (c) 1994-1996 Sun Microsystems, Inc.
	@param vector The vector to be sorted.
	@param low Left boundary of vector partition.
	@param high Right boundary of vector partition.
	@throws ClassCastException if an element in the vector does not implement {@link Comparable}.
	@author Garret Wilson
	@author James Gosling
	@author Kevin A. Smith
	@see Comparable
	*/
	@SuppressWarnings("unchecked")
	private static <E> void quickSort(final Vector<E> vector, final int low, final int high)
	{
		int loIndex=low, hiIndex=high;	//start out with a low and high index
		E midElement;
		if(high>low)	//if the original high index is lower than the original low index
		{
			midElement=vector.elementAt((low+high)/2);	//we'll use the middle element as the partition element
			while(loIndex<=hiIndex)	//loop through the array until indices cross
			{
				while((loIndex<high) && (((Comparable<E>)vector.elementAt(loIndex)).compareTo(midElement)<0))	//find the first element that is greater than or equal to the partition element starting from the left index
					++loIndex;	//keep looking
				while((hiIndex>low) && (((Comparable<E>)vector.elementAt(hiIndex)).compareTo(midElement)>0))	//find an element that is smaller than or equal to the partition element starting from the right index
					--hiIndex;	//keep looking
				if(loIndex<=hiIndex)	//if the indexes have not crossed, swap the objects
				{
					final E tempObject=vector.elementAt(loIndex);	//get a reference to the low element
					vector.setElementAt(vector.elementAt(hiIndex), loIndex);	//copy the high element and put it in the low element's place
					vector.setElementAt(tempObject, hiIndex);	//put the low element in the high element's place
					++loIndex;	//increment the low index
					--hiIndex;	//decrement the high index
				}
			}
			if(low<hiIndex)	//if the right index has not reached the left side of array
				quickSort(vector, low, hiIndex);	//sort the left partition
			if(loIndex<high)	//if the left index has not reached the right side of array
				quickSort(vector, loIndex, high);	//sort the right partition
		}
	}

	/**Randomizes the elements of the vector with the specified random number generator.
	@param <E> The type of element stored in the vector.
	@param vector The vector to be randomized.
	@param random The random number generator to use.
	@return The same vector, randomized.
	*/
	public static <E> Vector<E> randomize(final Vector<E> vector, final Random random)
	{
		for(int i=0; i<vector.size()-1; ++i)	//start at the first element and go to the second to last element
		{
			int fromIndex=((int)(random.nextFloat()*(vector.size()-i)))+i;	//get a random number (from zero to the number of elements left minus one), and put it into the range at our current slot
			if(fromIndex!=i)	//if we should move our current object
			{
				final E tempObject=vector.elementAt(i);	//get a reference to the element to be replaced
				vector.setElementAt(vector.elementAt(fromIndex), i);	//copy the specified element from its index into the current slot
				vector.setElementAt(tempObject, fromIndex);	//put the element that used to be in this slot where the other one came from
			}
		}
		return vector;	//return the vector, which is now randomized
	}

}
