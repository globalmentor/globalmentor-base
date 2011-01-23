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

package com.globalmentor.awt;

import java.awt.*;

/**Helper functions to work with AWT components and/or their Swing descendants.
@author Garret Wilson
*/
public class Components
{

	/**This class cannot be publicly instantiated.*/
	private Components()
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