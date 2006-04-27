package com.garretwilson.util;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.text.CharacterConstants.INFINITY_CHAR;

import com.garretwilson.lang.ObjectUtilities;

/**Represents a range of comparable values.
@author Garret Wilson
@param <T> The type of value supported by the range.
*/
public class Range<T extends Comparable<T>> implements Comparable<Range<T>>
{
	/**The lower bound of the range, inclusive, or <code>null</code> for infinity.*/
	private final T lowerBound;

		/**@return The lower bound of the range, inclusive, or <code>null</code> for infinity.*/
		public T getLowerBound() {return lowerBound;}

	/**The upper bound of the range, inclusive, or <code>null</code> for infinity.*/
	private final T upperBound;

		/**@return The upper bound of the range, inclusive, or <code>null</code> for infinity.*/
		public T getUpperBound() {return upperBound;}

	/**Creates a range with a lower and upper bound, inclusive.
	@param lowerBound The lower bound of the range, inclusive, or <code>null</code> for infinity.
	@param upperBound The upper bound of the range, inclusive, or <code>null</code> for infinity.
	*/
	public Range(final T lowerBound, final T upperBound)
	{
		this.lowerBound=lowerBound;
		this.upperBound=upperBound;
	}

	/**Determines if this range contains the given value.
	@param value The value to check.
	@return <code>true</code> if the given value is within this range, inclusive.
	@exception NullPointerException if the given value is <code>null</code>.
	 */
	public boolean contains(final T value)
	{
		return compare(value, getLowerBound(), -1)>=0 && compare(value, getUpperBound(), 1)<=0;	//compare the value with the two bounds, and make sure the value comes between or equals the two bounds
	}

	/**@return A unique hash code for the bounds of this range.*/
	public int hashCode()
	{
		return ObjectUtilities.hashCode(getLowerBound(), getUpperBound());	//return a hash code composed of the bounds, if any
	}

	/**Determins if this object is equal to another object.
	This method find equality if the object is another range and the lower and upper bounds are equal (including matching <code>null</code>s).
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof Range)	//if the object is a range
		{
			final Range<?> range=(Range<?>)object;	//get the object as a range
			return ObjectUtilities.equals(getLowerBound(), range.getLowerBound()) && ObjectUtilities.equals(getUpperBound(), range.getUpperBound());	//compare bounds
		}
		else	//if the object is not a range
		{
			return false;	//the object is not equivalent
		}
	}

	/**Compares this object with the specified object for order.
	This implementation determines order based upon the lower bounds and, only if the lower bounds are equal, the upper bound.
	Infinity is considered to be a lower value for a lower bound and a higher value for an upper bound than a given value for a bound.
	@param range The object to be compared.
	@return A negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	*/
	public int compareTo(final Range<T> range)
	{
		final int lowerBoundResult=compare(getLowerBound(), range.getLowerBound(), -1);	//compare lower bounds, with null considered a lesser value
		return lowerBoundResult!=0 ? lowerBoundResult : compare(getUpperBound(), range.getUpperBound(), 1);	//if the lower bounds are equal, compare upper bounds, with null considered a greater value
	}

	/**@return A string in the form "(X,X)" representing this range.*/
	public String toString()
	{
		final StringBuilder stringBuilder=new StringBuilder();
		stringBuilder.append('(');
		if(lowerBound!=null)
		{
			stringBuilder.append(lowerBound);
		}
		else
		{
			stringBuilder.append(INFINITY_CHAR);
		}
		stringBuilder.append(',');
		if(upperBound!=null)
		{
			stringBuilder.append(upperBound);
		}
		else
		{
			stringBuilder.append(INFINITY_CHAR);
		}
		stringBuilder.append(')');
		return stringBuilder.toString();
	}
}