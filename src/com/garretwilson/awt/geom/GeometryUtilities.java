package com.garretwilson.awt.geom;

import java.awt.*;
import java.awt.geom.*;

/**Utilities that work with geometrical coordinates.
@author Garret Wilson
*/

public class GeometryUtilities
{

	/**This class cannot be publicly instantiated.*/
	private GeometryUtilities()
	{
	}

	/**Calculates the center point of the shape.
	@param shape A shape the center of which to return.
	@return The point representing the center of the rectangular bounding area of
		the shape.
	@see Shape#getBounds
	*/
	public static Point getCenter(final Shape shape)
	{
		final Rectangle bounds=shape.getBounds();  //get the bounds of the shape
		return getCenter(bounds.x, bounds.y, bounds.width, bounds.height);  //return the center of the bounding rectangle
	}

	/**Calculates the center point of the bounding coordinates.
	@param x The horizontal location of the bounding area.
	@param y The vertical location of the bounding area.
	@param width The width of the bounding area.
	@param height The height of the bounding area.
	@return The point representing the center of the bounding area.
	*/
	public static Point getCenter(final int x, final int y, final int width, final int height)
	{
		return new Point(x+(width/2), y+(height/2));  //create a point in the center of the bounding area
	}
}
