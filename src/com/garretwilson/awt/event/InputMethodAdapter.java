package com.garretwilson.awt.event;

import java.awt.event.InputMethodEvent;
import java.awt.event.InputMethodListener;

/**An object that implements the <code>InputMethodListener</code> interface
	with no functionality.
@author Garret Wilson
@see InputMethodListener
*/
public class InputMethodAdapter implements InputMethodListener
{

	/**Invoked when the text entered through an input method has changed.
	@param event The input method event.
	*/
	public void inputMethodTextChanged(final InputMethodEvent event) {}

	/**Invoked when the caret within composed text has changed.
	@param event The input method event.
	*/
	public void caretPositionChanged(final InputMethodEvent event) {}

}
