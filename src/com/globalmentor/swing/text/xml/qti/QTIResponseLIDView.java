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

package com.globalmentor.swing.text.xml.qti;

import javax.swing.ButtonGroup;
import javax.swing.text.*;

import com.globalmentor.swing.text.xml.*;

/**Represents a QTI logical response group.
@author Garret Wilson
*/
public class QTIResponseLIDView extends XMLBlockView	//TODO delete class
{

	/**The button group for the logical ID responses.*/
	private final ButtonGroup buttonGroup=new ButtonGroup();

		/**The button group for the logical ID responses.*/
		public ButtonGroup getButtonGroup() {return buttonGroup;}

	/**Constructs a logical ID response view.
	@param element The element this view is responsible for.
	@param axis The tiling axis, either <code>View.X_AXIS</code> or
		<code>View.Y_AXIS</code>.
	*/
	public QTIResponseLIDView(final Element element, final int axis)
	{
		super(element, axis, true, true); //construct the parent, allowing expansion in both direction
	}

}