package com.garretwilson.awt;

import java.awt.*;

/**Various utility methods for manipulating windows.
@author Garret Wilson
*/
public class WindowUtilities
{

	/**This class cannot be publicly instantiated.*/
	private WindowUtilities() {}


	/**Centers a window in its graphics configuration, which is usually the
		physical screen although a window may be located in a virtual display.
	@param window The window to center.
	*/
	public static void center(final Window window)  //G***this needs to be updated to consider centering points and virtual screens
	{
		final GraphicsConfiguration graphicsConfiguration=window.getGraphicsConfiguration();  //get the window's graphics configuration
		final Rectangle gcBounds=graphicsConfiguration.getBounds(); //get the bounds of the graphics configuration
		final int centerX=(gcBounds.width-window.getWidth())/2; //find out how far to move the window over horizontally
		final int centerY=(gcBounds.height-window.getHeight())/2; //find out how far to move the window down vertically
		window.setLocation(gcBounds.x+centerX, gcBounds.y+centerY); //center the window in the graphics configuration
	}

	/**Maximizes a window in its graphics configuration, which is usually the
		physical screen although a window may be located in a virtual display.
	@param window The window to maximize.
	*/
	public static void maximize(final Window window)
	{
		final GraphicsConfiguration graphicsConfiguration=window.getGraphicsConfiguration();  //get the window's graphics configuration
		final Rectangle gcBounds=graphicsConfiguration.getBounds(); //get the bounds of the graphics configuration
		gcBounds.grow(4, 4);  //grow the rectangle by four units in all four directions
		window.setBounds(gcBounds); //G***testing
//G***bring back if  needed window.validate();	//update our size changes G***do we need this?
	}

	/**Finds and packs the window of which the given component is a descendant.
	@param component The component contained in a window.
	*/
	public static void packWindow(final Component component)
	{
		final Window window=ComponentUtilities.getParentWindow(component); //get this component's window
		if(window!=null)  //if we found a window
		{
			window.pack();    //pack the window

			//G***see if the window goes outside the viewport, or if it needs expanded and/or centered;  maybe validate
		}
	}

}