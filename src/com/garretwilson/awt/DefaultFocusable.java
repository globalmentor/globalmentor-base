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
