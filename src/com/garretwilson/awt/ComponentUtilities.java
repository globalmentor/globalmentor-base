package com.garretwilson.awt;

import java.awt.*;

/**Helper functions to work with AWT components and/or their Swing descendants.
@author Garret Wilson
*/
public class ComponentUtilities
{

	/**This class cannot be publicly instantiated.*/
	private ComponentUtilities()
	{
	}

	/**Finds the frame of which the given component is a child.
	@param component The component contained in a frame
	@return The ancestor frame, or <code>null</code> if there is no ancestor
		frame.
	*/
	public static Frame getParentFrame(final Component component)
	{
		Component frameComponent=component;  //we'll search for the frame component using this variable
		while(frameComponent!=null && !(frameComponent instanceof Frame))  //keep looking for a frame until we've ran out of components
		  frameComponent=frameComponent.getParent();  //try the component's parent
		return (Frame)frameComponent; //return whatever we found -- the window component or null
	}

	/**Finds the window of which the given component is a child.
	@param component The component contained in a window.
	@return The ancestor window, or <code>null</code> if there is no ancestor
		window.
	*/
	public static Window getParentWindow(final Component component)
	{
		Component windowComponent=component;  //we'll search for the window component using this variable
		while(windowComponent!=null && !(windowComponent instanceof Window))  //keep looking for a window until we've ran out of components
		  windowComponent=windowComponent.getParent();  //try the component's parent
		return (Window)windowComponent; //return whatever we found -- the window component or null
	}

}