package com.garretwilson.awt;

import java.awt.*;
import java.util.*;

/**Grid bag layout class that has additional convenience functionality,
	including box layout and border layout functionality.
<p>This layout can be used in place of <code>BorderLayout</code> by using

</p>
<p>A component with a basic grid bag layout can be used in place of a horizontal
	or vertical <code>javax.swing.Box</code>, with the added benefit that weights
	can be assigned to each component, using <code>createNextBoxConstraints()</code>
	for layout constraints when adding components. For example, a vertical layout
	might add a <code>Box.createGlue()</code> at the end using constraints of
	<code>createNextBoxConstraints(BasicGridBagLayout.X_AXIS, 1.0)</code>.</p>  
@author Garret Wilson
@see javax.swing.Box
*/
public class BasicGridBagLayout extends GridBagLayout implements ContainerConstants
{

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
	@see #X_AXIS
	@see #Y_AXIS
	*/
	protected int getMaxGrid(final int axis) 
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
			}
		}
		return max;	//return whatever max value we found
	}

	/**Creates constraints appropriate for laying out components in a row in a
		single column or row on the horizontal or vertial axis.
	<p>The constraints will have a weight of <code>0.0</code>.</p>
	@param axis The axis along which components are being laid out, either
		<code>X_AXIS</code> or <code>Y_AXIS</code>
	@return A grid bag constraint object for adding a new component in single
		file along the horizontal or vertical axis.
	@see #X_AXIS
	@see #Y_AXIS
	*/
	public GridBagConstraints createNextBoxConstraints(final int axis) 
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
	@see #X_AXIS
	@see #Y_AXIS
	*/
	public GridBagConstraints createNextBoxConstraints(final int axis, double weight) 
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
	*/
	public GridBagConstraints createNextBoxConstraints(final int axis, final int anchor, final int fill) 
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
	@see #X_AXIS
	@see #Y_AXIS
	*/
	public GridBagConstraints createNextBoxConstraints(final int axis, double weight, final int anchor, final int fill) 
	{
		final int nextGrid=getMaxGrid(axis)+1;	//determine the next coordinate on the grid
		return new GridBagConstraints(
				axis==X_AXIS ? nextGrid : 0,	//use the next coordinate for the appropriate axis
				axis==Y_AXIS ? nextGrid : 0,
				1,
				1,
				axis==X_AXIS ? weight : 1.0,	//use the weight for the appropriate axis
				axis==Y_AXIS ? weight : 1.0,
				anchor, fill, NO_INSETS, 0, 0);
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
//G***fix	public static GridBagConstraints createBorderConstraints(final String position, final int displacement) 



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
/*G***fix
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

	/**Creates constraints appropriate for laying out a component in one of the
		four borders or the center. This method considers the center to be cell
		(0,0), and components in the north, east, south, and west borders are
		considered to lie in a cell one unit away on the -y, x, y, and -x axes,
		respectively.
	<p>Components in north and south borders will fill multiple cells, so
		that components in west and east borders will always be between components
		in the north and south borders.</p>
	@param border The border in which to place the component, one of the 
		<code>Border</code> constants such as <code>Border.NORTH</code>.
	@return A grid bag constraint object for adding a new component in one of
		the borders.
	*/
	public GridBagConstraints createBorderConstraints(final String border) 
	{
		return createBorderConstraints(border, 1);	//return border constraints for the border one unit from the center
	}

	/**Creates constraints appropriate for laying out a component in one of the
		four borders or the center. The center is considered to be cell (0,0),
		and components in the north, east, south, and west borders are considered
		to lie in cells one or more units on the -y, x, y, and -x axes,
		respectively.
	<p>Components in north and south borders will fill multiple cells, so
		that components in west and east borders will always be between components
		in the north and south borders.</p>
	@param border The border in which to place the component, one of the 
		<code>Border</code> constants such as <code>Border.NORTH</code>.
	@param index The <em>positive</em> index away from the center; the true
		index taking into account positive and negative directions will be
		correctly calculated. Any non-negative index value is ignored for
		a border of <code>BorderLayout.CENTER</code>. 
	@return A grid bag constraint object for adding a new component in one of
		the borders.
	*/
	public GridBagConstraints createBorderConstraints(String border, final int index) 
	{
		if(index<=0)	//if the index isn't valid
		{
			if(index<0 || !BorderLayout.CENTER.equals(border))	//a zero index is allowed for the center; otherwise, it's an error
			{
				throw new IllegalArgumentException("Invalid index "+index+" for border "+border);	//throw the error
			}
		}
		if(BorderLayout.CENTER.equals(border))	//check for the center
		{
			return new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, NO_INSETS, 0, 0); 
		}
			//convert the relative i18n values to absolute border values TODO fix this so they update automatically when the component orientation is changed
		if(BorderLayout.LINE_START.equals(border) || BorderLayout.BEFORE_LINE_BEGINS.equals(border))	//line-start
		{
			border=BorderLayout.WEST;
		}
		else if(BorderLayout.LINE_END.equals(border) || BorderLayout.AFTER_LINE_ENDS.equals(border))	//line-end
		{
			border=BorderLayout.EAST;
		}
		else if(BorderLayout.PAGE_START.equals(border) || BorderLayout.BEFORE_FIRST_LINE.equals(border))	//page-start
		{
			border=BorderLayout.NORTH;
		}
		else if(BorderLayout.PAGE_END.equals(border) || BorderLayout.AFTER_LAST_LINE.equals(border))	//page-end
		{
			border=BorderLayout.SOUTH;
		}
			//create grid bag constraints for the appropriate border
		if(BorderLayout.NORTH.equals(border))	//north
		{
			return new GridBagConstraints(Integer.MIN_VALUE, -index, Integer.MAX_VALUE, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, NO_INSETS, 0, 0); 
		}
		else if(BorderLayout.SOUTH.equals(border))	//south
		{
			return new GridBagConstraints(Integer.MIN_VALUE, +index, Integer.MAX_VALUE, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, NO_INSETS, 0, 0); 
		}
		else if(BorderLayout.WEST.equals(border))	//west
		{
			return new GridBagConstraints(-index, 0, 1, 1, 0.0, 1.0, GridBagConstraints.CENTER, GridBagConstraints.VERTICAL, NO_INSETS, 0, 0); 
		}
		else if(BorderLayout.EAST.equals(border))	//east
		{
			return new GridBagConstraints(+index, 0, 1, 1, 0.0, 1.0, GridBagConstraints.CENTER, GridBagConstraints.VERTICAL, NO_INSETS, 0, 0); 
		}
		else	//if we didn't recognize the border
		{
			throw new IllegalArgumentException("Unknown border: "+border);	//throw an exception
		}
	}
	
	/**Default constructor.*/
	public BasicGridBagLayout()
	{
		super();	//create the parent class
	}

}
