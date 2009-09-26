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

package com.garretwilson.awt.event;

import java.awt.event.InputMethodEvent;
import java.awt.event.InputMethodListener;

/**An object that implements the {@link InputMethodListener} interface
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
