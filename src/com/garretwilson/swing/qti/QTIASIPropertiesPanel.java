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

package com.garretwilson.swing.qti;

import java.awt.*;
import javax.swing.*;

/**Provides a visual editing environment for properties for a QTI assessment,
	section, or item.
@author Garret Wilson
*/
public class QTIASIPropertiesPanel extends JPanel
{
  GridBagLayout gridBagLayout = new GridBagLayout();
  JLabel identLabel = new JLabel();
  JTextField identTextField = new JTextField();
  JLabel titleLabel = new JLabel();
  JTextField titleTextField = new JTextField();

	/**Default constructor.*/
	public QTIASIPropertiesPanel()
	{
    try
    {
      jbInit();
    }
    catch(Exception e)
    {
      e.printStackTrace();
		}
	}

	/**Setup the user interface.*/
  private void jbInit() throws Exception
  {
    this.setLayout(gridBagLayout);
    identLabel.setText("ID");
    identTextField.setText("identTextField");
    identTextField.setColumns(16);
    titleLabel.setText("Title");
    titleTextField.setText("titleTextField");
    this.add(identLabel,      new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
    this.add(identTextField,           new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
    this.add(titleLabel,  new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(titleTextField,  new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
  }

}