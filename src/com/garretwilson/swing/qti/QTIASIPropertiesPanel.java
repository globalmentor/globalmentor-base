package com.garretwilson.swing.qti;

import java.awt.*;
import javax.swing.*;
import java.util.*;
import com.garretwilson.text.xml.qti.*;

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