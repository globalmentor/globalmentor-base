package com.garretwilson.util;

import java.util.ArrayList;

/**Represents a list that is a subset of some larger list, based upon an array.
@author Garret Wilson
*/
public class ArraySubList extends ArrayList implements SubList
{

	/**The size of the superlist of which this list is a sublist.*/
	private int superListSize=0;

		/**@return The size of the superlist of which this list is a sublist.*/
		public int getSuperListSize() {return superListSize;}

		/**Sest the size of the superlist of which this list is a sublist.
		@param newSuperListSize The size of the superlist.
		*/
		public void setSuperListSize(final int newSuperListSize) {superListSize=newSuperListSize;}

	/**The index of the superlist at which this list starts.*/
	private int startIndex=0;

		/**@return The index of the superlist at which this list starts.*/
		public int getStartIndex() {return startIndex;}

		/**Sets The index of the superlist at which this list starts.
		@param newStartIndex The new starting index.
		*/
		public void setStartIndex(final int newStartIndex) {startIndex=newStartIndex;}

}