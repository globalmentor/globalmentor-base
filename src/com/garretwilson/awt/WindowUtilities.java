package com.garretwilson.awt;

import java.awt.*;
import javax.swing.*;

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

	/**If the window contains a scroll pane as its single
		child component, and one extent of the window is larger than the
		current graphics configuration bounds yet the area of the window
		is smaller than the graphics configuration bounds, the window is sized
		to a rectangle with the preferred area proportional to the graphics
		configuration bounds.
	<p>This is useful for packing windows containing text components, which
		report a very large preferred width and a very small preferred height.</p>
	@param window The window to resize if needed.
	@see JScrollPane
	*/ 
	public static void constrainSize(final Window window)
	{
/*G***fix or del; an option pane doesn't have a single scroll pane, so maybe look only at center border layouts
		boolean hasSingleScrollPaneDesendant=false;	//we'll see if there descendant line between the window and any scroll pane has no siblings
		Container container;	//see which container to start looking at
		if(window instanceof JFrame)	//if the window is a frame
		{
			container=((JFrame)window).getContentPane();	//start with the content pane
		}
		else if(window instanceof JDialog)	//if the window is a dialog
		{
			container=((JDialog)window).getContentPane();	//start with the content pane
		}
		else	//if this doesn't look like a special type of window
		{
			container=window;	//start looking at the window itself
		}
		while(container.getComponentCount()==1)	//if this container has just one child
		{
			final Component component=container.getComponent(0);	//get the sole child component
			if(component instanceof JScrollPane)	//if the sole child is a scroll pane
			{
				hasSingleScrollPaneDesendant=true;	//we found the single scroll pane descendant
				break;	//stop looking for a scroll pane descendant
			}
			else if(component instanceof Container)	//if the component is a container
			{
				container=(Container)component;	//look inside the container
			}
			else	//if the sole child is not a scroll pane or a container
			{
				break;	//there's nothing more to look for
			}
		}
*/
//G***fix		if(hasSingleScrollPaneDesendant)	//if the window has just one scroll pane descendant
		{
			final GraphicsConfiguration graphicsConfiguration=window.getGraphicsConfiguration();  //get the window's graphics configuration
			final Rectangle gcBounds=graphicsConfiguration.getBounds(); //get the bounds of the graphics configuration
			final int gcArea=gcBounds.width*gcBounds.height;	//find the area of the graphics configuration
			final Dimension preferredSize=window.getPreferredSize();	//get the preferred size of the window
			final int preferredArea=preferredSize.width*preferredSize.height;	//get the area of the window at its preferred size
				//if the preferred size is wider or taller than what we allow, yet we have enough area to contain the window if it were sized proportionally
			if((preferredSize.width>gcBounds.width || preferredSize.height>gcBounds.height) && preferredArea<=gcArea)
			{
				final float ratio=(float)gcBounds.width/(float)gcBounds.height;	//get the ratio of the graphics configuration
				final float proportionalHeight=(float)Math.sqrt(preferredArea/ratio);	//make the height of the window proportional  
				final float proportionalWidth=preferredArea/proportionalHeight;	//find the width from the height
	//G**fix			final int proportionalWidth=(int)Math.ceil(Math.sqrt(preferredArea));	//make the sides of the window approximately equal  
				window.setSize((int)Math.ceil(proportionalWidth), (int)Math.ceil(proportionalWidth));	//change the size of the window to our new approximately square size
	//G***fix			window.setSize(gcBounds.width*2/3, gcBounds.height*2/3);
				window.validate();	//validate the new bounds
			}
		}
	}

}