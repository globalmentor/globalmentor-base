package com.garretwilson.awt;

import java.awt.*;

/**Indicates the the implementing class can indicate which component should
	get the initial focus 
@author Garret Wilson
*/
public interface InitialFocusable
{
	
	/**@return The component that should get the initial focus.*/
	public Component getInitialFocusComponent();
	
}
