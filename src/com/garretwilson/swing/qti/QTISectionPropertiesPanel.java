package com.garretwilson.swing.qti;

import java.awt.*;
import javax.swing.*;
import java.util.*;
import com.garretwilson.swing.*;

/**Provides a visual editing environment for a QTI section properties.
@author Garret Wilson
*/
public class QTISectionPropertiesPanel extends JPanel
{
  GridBagLayout gridBagLayout = new GridBagLayout();
  JScrollPane scrollPane = new JScrollPane();
  ActionList itemList = new ActionList();

	/**Default constructor.*/
	public QTISectionPropertiesPanel()
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

	/**Initializes the user interface.*/
  private void jbInit() throws Exception
  {
    this.setLayout(gridBagLayout);
//G***del    scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    this.add(scrollPane,   new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0
            ,GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
    scrollPane.getViewport().add(itemList, null);
  }
}