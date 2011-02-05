/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.awt.geom;

import java.awt.*;
import java.awt.geom.Dimension2D;

/**
 * Utilities that work with geometrical coordinates.
 * @author Garret Wilson
 */
public class Geometry
{

	/** This class cannot be publicly instantiated. */
	private Geometry()
	{
	}

	/**
	 * Constrains the given inner dimension within the given outer dimension by scaling the inner dimension so that no
	 * part lies outside the outer dimension.
	 * <p>
	 * This implementation returns an immutable dimension.
	 * </p>
	 * @param dimension The inner dimension to be constrained.
	 * @param constrainingDimension The outer constraining dimension.
	 * @return A dimension representing the constrained dimension.
	 */
	public static Dimension2D constrain(final Dimension2D dimension, final Dimension2D constrainingDimension)
	{
		final double width = dimension.getWidth();
		final double height = dimension.getHeight();
		final double constrainingWidth = constrainingDimension.getWidth();
		final double constrainingHeight = constrainingDimension.getHeight();
		if (width <= constrainingWidth && height <= constrainingHeight) //if nothing needs to be constrained
		{
			return dimension; //return the dimension unchanged
		}
		final double relation = (double) width / height; //determine the relationship of the sides
		double newWidth;
		double newHeight = constrainingWidth / relation; //get the matching height for a constrained width
		if (newHeight <= constrainingHeight) //if the height has been constrained
		{
			newWidth = constrainingWidth; //constrain the width to the edges
		}
		else
		//if the height needs to be constrained
		{
			newWidth = constrainingHeight * relation; //get the matching width for a constrained height
			newHeight = constrainingHeight; //constrain the height to the edges
		}
		return new ImmutableDimension2D(newWidth, newHeight); //return the new constrained dimensions
	}

	/**
	 * Calculates the center point of the shape.
	 * @param shape A shape the center of which to return.
	 * @return The point representing the center of the rectangular bounding area of the shape.
	 * @see Shape#getBounds
	 */
	public static Point getCenter(final Shape shape)
	{
		final Rectangle bounds = shape.getBounds(); //get the bounds of the shape
		return getCenter(bounds.x, bounds.y, bounds.width, bounds.height); //return the center of the bounding rectangle
	}

	/**
	 * Calculates the center point of the bounding coordinates.
	 * @param x The horizontal location of the bounding area.
	 * @param y The vertical location of the bounding area.
	 * @param width The width of the bounding area.
	 * @param height The height of the bounding area.
	 * @return The point representing the center of the bounding area.
	 */
	public static Point getCenter(final int x, final int y, final int width, final int height)
	{
		return new Point(x + (width / 2), y + (height / 2)); //create a point in the center of the bounding area
	}

}
