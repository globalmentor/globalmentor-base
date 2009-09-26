/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.garretwilson.awt;

import java.awt.*;
import java.util.*;

import javax.swing.Box;

import static com.garretwilson.awt.Containers.*;

/**Grid bag layout class that has additional convenience functionality,
	including box layout and border layout functionality.
<p>This layout can be used in place of {@link BorderLayout} either by using
	the {@link BorderLayout} constraints or, preferably, by using one of
	the <code>createBorderConstraints()</code> in this class. Border layout
	functionality is enhanced by allowing multiple components in each border,
	the total number in both directions together limited by
	{@link GridBagLayout#MAXGRIDSIZE}.</p>
<p>A component with a basic grid bag layout can be used in place of a horizontal
	or vertical {@link Box}, with the added benefit that weights
	can be assigned to each component, using <code>createNextBoxConstraints()</code>
	for layout constraints when adding components. For example, a vertical layout
	might add a {@link Box#createGlue()} at the end using constraints of
	<code>createNextBoxConstraints(BasicGridBagLayout.X_AXIS, 1.0)</code>.</p>  
@author Garret Wilson
@see Box
*/
public class BasicGridBagLayout extends GridBagLayout
{

	/**The origin (i.e. the center cell position) both horizontally and vertically
		to use when impersonating a border layout.
	*/
	protected final static int ORIGIN=MAXGRIDSIZE/2;

	/**Specifies that components should be laid out left to right.*/
	public final static int X_AXIS=0;
    
	/**Specifies that components should be laid out top to bottom.*/
	public static final int Y_AXIS=1;

	/**Determines which layout constraints are associated with the given child component
	@param component The child component with which constraints are associated.
	@return The constraints associated with the component, or <code>null</code>
		if there are no constraints associated with the component.
	*/
	public GridBagConstraints getConstraints(final Component component)
	{
		return (GridBagConstraints)comptable.get(component);	//get any constraints associated with the component 
	}

	/**Determines the largest x or y coordinate of all components that were added
		using a <code>GridBagConstraint</code>.
	@param axis The axis on which to determine the maximum coordinate, either
		<code>X_AXIS</code> or <code>Y_AXIS</code>
	@return The largest coordinate on the given axis, or <code>-1</code> if
		no components were added using a <code>GridBagConstraint</code>.
	@exception IllegalArgumentException Thrown if the axis value is unrecognized
	@see #X_AXIS
	@see #Y_AXIS
	*/
	protected int getMaxGrid(final int axis) throws IllegalArgumentException	//TODO probably replace this with getGridBounds() 
	{
		int max=-1;	//start out not finding any coordinate
		final Iterator constraintsIterator=comptable.values().iterator();	//get an iterator to all the constraints
		while(constraintsIterator.hasNext())	//while there are more constraints
		{
			final GridBagConstraints constraints=(GridBagConstraints)constraintsIterator.next();	//get the next layout constraints
			switch(axis)	//see which axis we're looking at
			{
				case X_AXIS:
					max=Math.max(max, constraints.gridx);	//see if we need to update the largest x coordinate
					break;
				case Y_AXIS:
					max=Math.max(max, constraints.gridy);	//see if we need to update the largest y coordinate
					break;
				default:	//if the axis value isn't recognized
					throw new IllegalArgumentException("Unrecognized axis value");	//TODO i18n
			}
		}
		return max;	//return whatever max value we found
	}

	/**Determines the largest and smallest coordinates of all constraints.
	@return A rectangle representing the upper left x and y coordinates as well
		as the extent of the grid.
	*/
	public Rectangle getGridBounds() 
	{
		int minX=0, minY=0, maxX=0, maxY=0;	//the minimum and maximum coordinates
		final Iterator constraintsIterator=comptable.values().iterator();	//get an iterator to all the constraints
		while(constraintsIterator.hasNext())	//while there are more constraints
		{
			final GridBagConstraints constraints=(GridBagConstraints)constraintsIterator.next();	//get the next layout constraints
			minX=Math.min(minX, constraints.gridx);	//see if we need to update the smallest x coordinate
			maxX=Math.max(maxX, constraints.gridx);	//see if we need to update the largest x coordinate
			minY=Math.min(minY, constraints.gridy);	//see if we need to update the smallest y coordinate
			maxY=Math.max(maxY, constraints.gridy);	//see if we need to update the largest y coordinate
		}
		return new Rectangle(minX, minY, maxX-minX+1, maxY-minY+1);	//create a rectangle representing the absolute origin and size of the grid 
	}

	/**Creates constraints appropriate for laying out components in a row in a
		single column or row on the horizontal or vertial axis.
	<p>The constraints will have a weight of <code>0.0</code>.</p>
	@param axis The axis along which components are being laid out, either
		<code>X_AXIS</code> or <code>Y_AXIS</code>
	@return A grid bag constraint object for adding a new component in single
		file along the horizontal or vertical axis.
	@exception IllegalArgumentException Thrown if the axis value is unrecognized
	@see #X_AXIS
	@see #Y_AXIS
	*/
	public GridBagConstraints createNextBoxConstraints(final int axis) throws IllegalArgumentException 
	{
		return createNextBoxConstraints(axis, 0.0);	//return box constraints with no weight
	}

	/**Creates constraints appropriate for laying out components in a row in a
		single column or row on the horizontal or vertial axis.
	<p>This version uses a default anchor of <code>GridBagConstraints.CENTER</code>
		and a default fill of <code>GridBagConstraints.BOTH</code>. No insets are
		used.</p>
	@param axis The axis along which components are being laid out, either
		<code>X_AXIS</code> or <code>Y_AXIS</code>
	@param weight An amount specifying how to distribute the extra space along the
		axis.
	@return A grid bag constraint object for adding a new component in single
		file along the horizontal or vertical axis.
	@exception IllegalArgumentException Thrown if the axis value is unrecognized
	@see #X_AXIS
	@see #Y_AXIS
	*/
	public GridBagConstraints createNextBoxConstraints(final int axis, double weight) throws IllegalArgumentException 
	{
		return createNextBoxConstraints(axis, weight, GridBagConstraints.CENTER, GridBagConstraints.BOTH);	//create box constraints anchoring in the center and filling both axes
	}

	/**Creates constraints appropriate for laying out components in a row in a
		single column or row on the horizontal or vertial axis.
	<p>This version uses a default weight of <code>0.0</code>.</p>
	@param axis The axis along which components are being laid out, either
		<code>X_AXIS</code> or <code>Y_AXIS</code>
	@param anchor	The corner to which anchor the anchor component.
	@param fill	How the component should fill the grid cell.
	@return A grid bag constraint object for adding a new component in single
		file along the horizontal or vertical axis.
	@see #X_AXIS
	@see #Y_AXIS
	@exception IllegalArgumentException Thrown if the axis value is unrecognized
	*/
	public GridBagConstraints createNextBoxConstraints(final int axis, final int anchor, final int fill) throws IllegalArgumentException 
	{
		return createNextBoxConstraints(axis, 0.0, anchor, fill);	//create constraints with no weight		
	}

	/**Creates constraints appropriate for laying out components in a row in a
		single column or row on the horizontal or vertial axis.
	@param axis The axis along which components are being laid out, either
		<code>X_AXIS</code> or <code>Y_AXIS</code>
	@param weight An amount specifying how to distribute the extra space along the
		axis.
	@param anchor	The corner to which anchor the anchor component.
	@param fill	How the component should fill the grid cell.
	@return A grid bag constraint object for adding a new component in single
		file along the horizontal or vertical axis.
	@exception IllegalArgumentException Thrown if the axis value is unrecognized
	@see #X_AXIS
	@see #Y_AXIS
	*/
	public GridBagConstraints createNextBoxConstraints(final int axis, double weight, final int anchor, final int fill) throws IllegalArgumentException 
	{
		final int nextGrid=getMaxGrid(axis)+1;	//determine the next coordinate on the grid
		switch(axis)	//see which axis we're dealing with
		{
			case X_AXIS:
				return new GridBagConstraints(nextGrid, 0, 1, MAXGRIDSIZE, weight, 1.0, anchor, fill, NO_INSETS, 0, 0);
			case Y_AXIS:
				return new GridBagConstraints(0, nextGrid, MAXGRIDSIZE, 1, 1.0, weight, anchor, fill, NO_INSETS, 0, 0);
			default:	//if the axis value isn't recognized
				throw new IllegalArgumentException("Unrecognized axis value");	//TODO i18n
		}
	}


	/**Determines the grid horizontal position to use to place
	@param position The position (center or border) in which to place the
		component; one of the <code>BorderLayout</code> position constants.
	@param displacement The zero-based displacement from the center, allowing
		multiple borders away from the center component.
	@return A grid bag constraint object for adding a new component in the center
		or border of this panel.
	@see BorderLayout
	*/
//TODO fix	public static GridBagConstraints createBorderConstraints(final String position, final int displacement) 



	/**Creates constraints appropriate for placing a component in a the center
		or in a border of the panel. This mimics the functionality of
		<code>BorderLayout</code>.
	@param position The position (center or border) in which to place the
		component; one of the <code>BorderLayout</code> position constants.
	@param displacement The zero-based displacement from the center, allowing
		multiple borders away from the center component.
	@return A grid bag constraint object for adding a new component in the center
		or border of this panel.
	@see BorderLayout
	*/
/*TODO fix
	public static GridBagConstraints createBorderConstraints(final String position, final int displacement) 
	{
		final int nextGrid=getMaxGrid(axis)+1;	//determine the next coordinate on the grid
		return new GridBagConstraints(
				axis==BoxLayout.X_AXIS ? nextGrid : 0,	//use the next coordinate for the appropriate axis
				axis==BoxLayout.Y_AXIS ? nextGrid : 0,
				1,
				1,
				axis==BoxLayout.X_AXIS ? weight : 1.0,	//use the weight for the appropriate axis
				axis==BoxLayout.Y_AXIS ? weight : 1.0,
				anchor, fill, NO_INSETS, 0, 0);
	}
*/


	/**Determines the next available index for a component in the given border.
	<p>A constraint of <code>BorderLayout.CENTER</code> always returns 0.</p>
	@param border The border in which to place the component, one of the 
		<code>BorderLayout</code> constants such as <code>Border.NORTH</code>.
	@return The next available zero-based index suitable for placing a component
		in the given border, or -1 if no next index is available.
	@exception IllegalArgumentException Thrown if constraints is not a
		valid <code>BorderLayout</code> constraint.
	*/
	protected int getNextBorderIndex(String border) throws IllegalArgumentException 
	{
		if(BorderLayout.CENTER.equals(border))	//if the center is being requested
			return 0;	//always return an index of zero for the center
		border=getCanonicalBorderConstraint(border);	//make sure we have one of the canonical border constraints
		final int axis;	//see which axis we're looking at
		final int direction;	//see if we're looking in the forward (+1) or negative (-1) direction along the axis
		if(BorderLayout.NORTH.equals(border))	//north
		{
			axis=Y_AXIS;
			direction=-1;
		}
		else if(BorderLayout.SOUTH.equals(border))	//south
		{
			axis=Y_AXIS;
			direction=+1;
		}
		else if(BorderLayout.WEST.equals(border))	//west
		{
			axis=X_AXIS;
			direction=-1;
		}
		else if(BorderLayout.EAST.equals(border))	//east
		{
			axis=X_AXIS;
			direction=-1;
		}
		else	//if we didn't recognize the border
		{
			throw new IllegalArgumentException("Unknown border: "+border);	//throw an exception
		}
		int index=0;	//start looking at the first index
		do	//this is a slow way to look for the next index, but in border layouts there shouldn't be very many child components and this takes up less memory than a once-through array-based method
		{
			boolean isIndexUsed=false;	//we'll see if this index has been used
			final Iterator constraintsIterator=comptable.values().iterator();	//get an iterator to all the constraints
			while(constraintsIterator.hasNext())	//while there are more constraints
			{
				final GridBagConstraints constraints=(GridBagConstraints)constraintsIterator.next();	//get the next layout constraints
				final int position=axis==X_AXIS ? constraints.gridx : constraints.gridy;	//get the value we're interested in
				final int usedIndex=direction<0 ? position : MAXGRIDSIZE-1-position;	//if we're going down, use the index; going up, subtract the position from the size of the grid 
				if(usedIndex==index)	//if we've used this index
				{
					isIndexUsed=true;	//show that we've already used this index
					break;	//we can't use this index, so stop looking at constraints matching this index
				} 
			}
			if(!isIndexUsed)	//if we haven't used this index
			{
				return index;	//report that this index is available
			}
			else	//if we have used this index
			{ 
				++index;	//see if the the next index has been used
			} 			
		}
		while(index<ORIGIN-1);	//keep looking until we reach the origin cell (allowing for even-celled grids, which means one direction has less indexes to work with)
		return -1;	//show that all available indexes have been used
	}

	/**Creates constraints appropriate for laying out a component in one of the
		four borders or the center. This method considers the center to be cell
		(<code>MAXGRIDSIZE</code>/2, <code>MAXGRIDSIZE</code>/2).
		Components in the borders are placed at their farthest position
		<em>away</em> from the center, so that higher indexes move towards the
		center. This method uses the first free index available in the given
		border. 
	<p>Components in north and south borders will fill multiple cells, so
		that components in west and east borders will always be between components
		in the north and south borders.</p>
	@param border The border in which to place the component, one of the 
		<code>BorderLayout</code> constants such as <code>Border.NORTH</code>.
	@return A grid bag constraint object for adding a new component in one of
		the borders.
	@exception IllegalArgumentException Thrown if constraints is not a
		valid <code>BorderLayout</code> constraint.
	@see #getNextBorderIndex(String)
	*/
	public GridBagConstraints createBorderConstraints(final String border) throws IllegalArgumentException 
	{
		return createBorderConstraints(border, getNextBorderIndex(border));	//return border constraints for the next available border index for this border
	}

	/**Creates constraints appropriate for laying out a component in one of the
		four borders or the center. The center is considered to be cell
		(<code>MAXGRIDSIZE</code>/2, <code>MAXGRIDSIZE</code>/2).
		Components in the borders are placed at their farthest position
		<em>away</em> from the center, so that higher indexes move towards the
		center.
	<p>Components in north and south borders will fill multiple cells, so
		that components in west and east borders will always be between components
		in the north and south borders.</p>
	@param border The border in which to place the component, one of the 
		<code>BorderLayout</code> constants such as <code>Border.NORTH</code>.
	@param index The zero-based index toward from the center; the true
		index taking into account positive and negative directions will be
		correctly calculated. Every non-negative index value is ignored for
		a border of <code>BorderLayout.CENTER</code>. 
	@return A grid bag constraint object for adding a new component in one of
		the borders.
	@exception IllegalArgumentException Thrown if constraints is not a
		valid <code>BorderLayout</code> constraint or the given index is negative.
	*/
	public GridBagConstraints createBorderConstraints(String border, final int index) throws IllegalArgumentException 
	{
		if(index<0)	//if the index isn't valid
		{
			throw new IllegalArgumentException("Invalid index "+index+" for border "+border);	//throw the error
		}
		if(BorderLayout.CENTER.equals(border))	//check for the center
		{
			return new GridBagConstraints(ORIGIN, ORIGIN, 1, 1, 1.0, 1.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, NO_INSETS, 0, 0); 
		}
		border=getCanonicalBorderConstraint(border);	//make sure we have one of the canonical border constraints
			//create grid bag constraints for the appropriate border
		if(BorderLayout.NORTH.equals(border))	//north
		{
			return new GridBagConstraints(0, index, MAXGRIDSIZE, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, NO_INSETS, 0, 0); 
		}
		else if(BorderLayout.SOUTH.equals(border))	//south
		{
			return new GridBagConstraints(0, MAXGRIDSIZE-1-index, MAXGRIDSIZE, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, NO_INSETS, 0, 0); 
		}
		else if(BorderLayout.WEST.equals(border))	//west
		{
			return new GridBagConstraints(index, ORIGIN, 1, 1, 0.0, 1.0, GridBagConstraints.CENTER, GridBagConstraints.VERTICAL, NO_INSETS, 0, 0); 
		}
		else if(BorderLayout.EAST.equals(border))	//east
		{
			return new GridBagConstraints(MAXGRIDSIZE-1-index, ORIGIN, 1, 1, 0.0, 1.0, GridBagConstraints.CENTER, GridBagConstraints.VERTICAL, NO_INSETS, 0, 0); 
		}
		else	//if we didn't recognize the border
		{
			throw new IllegalArgumentException("Unknown border: "+border);	//throw an exception
		}
	}

	/**Converts the given border constrataint to one of the absolute canonical
		border constraints.
	<p>Internationalization border constraints such as 
		<code>BorderLayout.LINE_START</code> will be converted to absolute border
		constraints such as <code>BorderLayout.WEST</code> assuming left-to-right
		top-to-bottom component orientation. (All absolute values become
		relative internationalized values when laid out using this class, however.)</p>
	@param border The border in which to place the component, one of the 
		<code>BorderLayout</code> constants such as <code>Border.PAGE_START</code>
		or <code>Border.NORTH</code>.
	@return A canonical absolute border constraint, one of
		<code>Border.CENTER</code>, <code>Border.NORTH</code>,
		<code>Border.EAST</code>, <code>Border.SOUTH</code>, and
		<code>Border.WEST</code>.
	@exception IllegalArgumentException Thrown if constraints is not a
		valid <code>BorderLayout</code> constraint.
	*/
	public static String getCanonicalBorderConstraint(final String border) throws IllegalArgumentException
	{
			//convert the relative i18n values to absolute border values
		if(BorderLayout.LINE_START.equals(border) || BorderLayout.BEFORE_LINE_BEGINS.equals(border))	//line-start
		{
			return BorderLayout.WEST;
		}
		else if(BorderLayout.LINE_END.equals(border) || BorderLayout.AFTER_LINE_ENDS.equals(border))	//line-end
		{
			return BorderLayout.EAST;
		}
		else if(BorderLayout.PAGE_START.equals(border) || BorderLayout.BEFORE_FIRST_LINE.equals(border))	//page-start
		{
			return BorderLayout.NORTH;
		}
		else if(BorderLayout.PAGE_END.equals(border) || BorderLayout.AFTER_LAST_LINE.equals(border))	//page-end
		{
			return BorderLayout.SOUTH;
		}
		return border;	//if the border isn't one of the non-canonical forms, assume it is one of the canonical forms TODO make sure this is one of the canonical borders
	} 

	/**Adds the specified component to the layout, using the specified constraints.
	<p>If a <code>String</code> is passed representing a valid
		<code>BorderLayout</code> constraint, that constraint is converted to 
		the appropriate <code>GridBagConstraint</code>.</p>
	@param component The component to be added.
	@param constraints An object that determines how the component is added to
		the layout.
	@exception IllegalArgumentException Thrown if constraints is not a
		<code>GridBagConstraint</code> or a valid <code>BorderLayout</code>.
	@see #createBorderConstraints(String)
	*/
	public void addLayoutComponent(Component comp, Object constraints)
	{
		if(constraints instanceof String)	//if this seems to be a border constraint
			super.addLayoutComponent(comp, createBorderConstraints((String)constraints));	//create grid bag constraints from the border layout constraints
		else	//if this is not a string, it's probably a grid bag constraint
			super.addLayoutComponent(comp, constraints);	//add the layout component normally
	}
	
	/**Default constructor.*/
	public BasicGridBagLayout()
	{
		super();	//create the parent class
	}

}
