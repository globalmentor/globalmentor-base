package com.garretwilson.awt;

import java.awt.*;

/**Indicates the the implementing class can indicate which component should
	get the default focus. 
@author Garret Wilson
*/
public interface DefaultFocusable
{
	
	/**@return The component that should get the default focus, or
		<code>null</code> if no component should get the default focus or it is
		unknown which component should get the default focus.
	*/
	public Component getDefaultFocusComponent();
	
}
