package com.garretwilson.awt;

import java.awt.*;

/**Indicates the the implementing class can request the focus for its
	default focus component. 
@author Garret Wilson
*/
public interface DefaultFocusable
{
	
	/**@return The component that should get the default focus, or
		<code>null</code> if no component should get the default focus or it is
		unknown which component should get the default focus.
	*/
	public Component getDefaultFocusComponent();

//TODO add a requestInitialFocus() method, and use that in OptionPane

	/**Requests that the default focus component should get the default.
	If the default focus comonent is itself <code>DefaultFocusable</code>, that
		component is asked to request focus for its default focus component, and
		so on.
	@return <code>false</code> if the focus change request is guaranteed to
		fail; <code>true</code> if it is likely to succeed.
	@see Component#requestFocusInWindow
	*/
	public boolean requestDefaultFocusComponentFocus();
	
}
