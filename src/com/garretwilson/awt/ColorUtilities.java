package com.garretwilson.awt;

import java.awt.Color;

/**Various utilities for manipulating colors.
@author Garret Wilson
*/
public class ColorUtilities
{

	/**Compares one color with another, based on their alpha, red, green, and blue
		values, taking into account that one or both colors may be <code>null</code>.
		If one color is <code>null</code> but the other is not, the <code>null</code>
		color is considered to be less than the non-<code>null</code> color
	@param color1 The first colorto compare.
	@param color2 The second color to compare.
	@return A negative integer, zero, or a positive integer if the first object
		is less than, equal to, or greater than the specified annotation,
		respectively, with a <code>null</code> considered less than a
		non-<code>null</code> value.
	@see Comparable#compareTo
	*/
	public final static int compareTo(final Color color1, final Color color2)
	{
		if(color1!=null && color2!=null) //if both colors are non-null
			return color1.getRGB()-color2.getRGB(); //subtract and return their RGB values
		else if(color1==color2)  //if both colors are null (we know at this point that one color is null, so if the color are equal then both are null)
			return 0; //the colors are equal
		else  //if one color is null and the other isn't
			return color1==null ? -1 : 1;  //the null color is lower
	}

}