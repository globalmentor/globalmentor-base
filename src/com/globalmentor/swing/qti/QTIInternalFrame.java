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

package com.globalmentor.swing.qti;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import javax.swing.*;


import com.globalmentor.log.Log;
import com.globalmentor.mentoract.qti.QTI;
import com.globalmentor.model.Modifiable;
import com.globalmentor.swing.IconResources;

/**An internal frame for editing lists of questions. Contains the internal
	item list data model.
//TODO del @see QTIItemListModel
@author Garret Wilson
*/
public class QTIInternalFrame extends JInternalFrame implements Modifiable
{

	/**The file in which the QTI data is stored.*/
//TODO del if not needed	private File file=null;

		/**@return The file in which the QTI data is stored, or <code>null</code>
		  if the QTI is not stored in a file.*/
//TODO del if not needed		public File getFile() {return file;}

		/**Sets the file in which the QTI data should be stored.
		@param newFile The file for storing QTI data.
		*/
//TODO del if not needed		public void setFile(final File newFile) {file=newFile;}

	/**The object containing the QTI information; the model for this frame.*/
//TODO del if not needed	private QTI qti=new QTI();  //TODO don't initialize this, and instead do this in the default constructor, with another constructor for initialization with QTI data

	/**@return The QTI data model.*/ //TODO do we want to do this and allow it to be edited elsewhere?
	public QTI getQTI() {return explorePanel.getQTI();}

	/**@return Whether the QTI data model has been modified.*/
	public boolean isModified() {return explorePanel.isModified();}

	/**Sets whether the QTI data model has been modified.
	@param newModified The new modification status.
	*/
	public void setModified(final boolean newModified) {explorePanel.setModified(newModified);}

	/**@return The action for adding a new section.*/
	public Action getAddSectionAction() {return explorePanel.getAddSectionAction();}

	/**@return The action for adding a new item.*/
	public Action getAddItemAction() {return explorePanel.getAddItemAction();}

	/**@return The action for removing an item.*/
	public Action getRemoveItemAction() {return explorePanel.getDeleteAction();}

	/**The action for editing an item.*/
//TODO fix	final Action editItemAction=new EditItemAction();

	/**@return The action for editing an item.*/  //TODO probably rename these to getEditAction() and getDeleteAction()
	public Action getEditItemAction() {return explorePanel.getEditAction();}

	JToolBar toolBar = new JToolBar();
  BorderLayout borderLayout = new BorderLayout();
	QTIExplorePanel explorePanel=new QTIExplorePanel();
//TODO del  JTabbedPane sectionTabbedPane = new JTabbedPane();
//TODO fix  ActionList itemList = new ActionList();

	/**Default constructor.*/
	public QTIInternalFrame()
	{
		super("[Question Items]", true, true, true, true);  //create the default options TODO i18n
		jbInit();
	}

	/**Constructs a QTI item list internal frame with an existing list data model.
	@param listModel The data model for the item list
	*/
/*TODO fix
	public QTIItemListInternalFrame(final QTIItemListModel listModel)
	{
		super("[Question Items]", true, true, true, true);  //create the default options TODO i18n
//TODO del		  final JInternalFrame internalFrame=new JInternalFrame("Test internal frame", true, true, true, true);

Log.trace("saving item list model with size: ", listModel.size()); //TODO del
	  itemListModel=listModel;  //save the given list model
			//create the actions
		deleteAction=new DeleteAction();
		editAction=new EditAction();
		newAction=new NewAction();
    try
    {
      jbInit();
    }
    catch(Exception e)
    {
      e.printStackTrace();
    }
		updateActions();  //update the state of the actions
	}
*/

	/**Initializes the user interface.*/
  private void jbInit()
  {
		this.setBackground(Color.lightGray);  //TODO testing
		//setup the toolbar
		toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);	//TODO use constant here, fix
//TODO fix		toolBar.add(newAction);	//new
//TODO fix		toolBar.add(deleteAction);	//delete
//TODO fix		toolBar.addSeparator();	//--
//TODO fix		toolBar.add(editAction);	//edit
//TODO del Log.trace("ready to set list model with size: ", itemListModel.size()); //TODO del
    this.getContentPane().setLayout(borderLayout);
			//forward all explore panel "modified" property changes to anyone listening to our property changes
		explorePanel.addPropertyChangeListener(MODIFIED_PROPERTY, new java.beans.PropertyChangeListener()
    {
      public void propertyChange(final PropertyChangeEvent propertyChangeEvent) //if the "modified" property changes in the explore panel
      {
					//forward the "modified" property change event
				firePropertyChange(propertyChangeEvent.getPropertyName(), propertyChangeEvent.getOldValue(), propertyChangeEvent.getNewValue());
      }
    });
    this.getContentPane().add(toolBar, BorderLayout.NORTH);
    this.getContentPane().add(explorePanel, BorderLayout.CENTER);
//TODO del    this.getContentPane().add(sectionTabbedPane, BorderLayout.CENTER);
//TODO fix    this.getContentPane().add(itemList, BorderLayout.CENTER);
		this.setSize(100, 100); //TODO testing
  }

	/**Sets the QTI information for the frame, in essence loading the data.
	@param newQTI The QTI data.
	*/
	public void setQTI(final QTI newQTI)
	{
Log.trace("Setting QTI"); //TODO del
//TODO del		qti=newQTI; //save the QTI data
		explorePanel.setQTI(newQTI); //set the QTI data model in the explore panel
	}

	/**Action for creating a new section.*/
	class NewSectionAction extends AbstractAction
	{
		/**Default constructor.*/
		public NewSectionAction()
		{
			super("New Section...");	//create the base class TODO i18n
			putValue(SHORT_DESCRIPTION, "new section");	//set the short description TODO i18n
			putValue(LONG_DESCRIPTION, "Create a new section.");	//set the long description TODO i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_S));  //set the mnemonic key TODO i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.FOLDER_NEW_ICON_FILENAME)); //load the correct icon
//TODO del when works			new ImageIcon(ReaderFrame.class.getResource("book_open.gif")));	//load the correct icon
//TODO del when works			putValue(SMALL_ICON, new ImageIcon(ReaderFrame.class.getResource("book_open.gif")));	//load the correct icon
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{
/*TODO fix
			QTIItemListPanel itemListPanel=(QTIItemListPanel)sectionTabbedPane.getSelectedComponent(); //get the section panel selected in the tabbed pane
			if(itemListPanel==null)  //if there is no section panel selected, assume there are no section panels
			{
				itemListPanel=addItemListPanel(qti.getItemList()); //create a section UI for the items outside the section
			}
			final ItemListModel itemListModel=(ItemListModel)itemListPanel.itemList.getModel(); //get the list model being used in the section
			final Item item=askNewItem(); //ask for a new item
			if(item!=null)  //if an item was created
				itemListModel.addItem(item);  //add the item to the list model
*/
		}
	}


}