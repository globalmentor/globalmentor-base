package com.garretwilson.swing.qti;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.swing.ActionList;
import com.globalmentor.java.Booleans;
import com.globalmentor.mentoract.qti.Render;
import com.globalmentor.mentoract.qti.ResponseLabel;
import com.globalmentor.util.*;

/**Provides a visual editing environment for a type of QTI rendering.
@author Garret Wilson
*/
public abstract class QTIRenderPanel extends JPanel implements Modifiable
{

	/**The action for adding a response label.*/
	private Action addResponseLabelAction;

		/**@return The action for adding a response label.*/
		public Action getAddResponseLabelAction() {return addResponseLabelAction;}

	/**The action for removing a response label.*/
	private Action removeResponseLabelAction;

		/**@return The action for removing a response label.*/
		public Action getRemoveResponseLabelAction() {return removeResponseLabelAction;}

	/**The action for editing a response label.*/
	private Action editResponseLabelAction;

		/**@return The action for editing a response label.*/
		public Action getEditResponseLabelAction() {return editResponseLabelAction;}

	/**Whether the data has been modified; the default is not modified.*/
	private boolean modified=false;

		/**@return Whether the datahas been modified.*/
		public boolean isModified() {return modified;}

		/**Sets whether the data has been modified.
		  This is a bound property.
		@param newModified The new modification status.
		*/
		public void setModified(final boolean newModified)
		{
			final boolean oldModified=modified; //get the old modified value
			if(oldModified!=newModified)  //if the value is really changing
			{
			  modified=newModified; //update the value
				  //show that the modified property has changed
        firePropertyChange(MODIFIED_PROPERTY, Boolean.valueOf(oldModified), Boolean.valueOf(newModified));
			}
		}

	/**The panel that holds the presentation material currently	being edited for
		the item, so that the relevant image(s) can be retrieved.
	*/
	protected QTIMaterialPanel materialPanel=null;


	/**@return The action for adding a response label.*/
//G***del	public abstract Action getAddResponseLabelAction();

	/**@return The action for removing a response label.*/
//G***del	public abstract Action getRemoveResponseLabelAction();

	/**@return The action for editing a response label.*/
//G***del	public abstract Action getEditResponseLabelAction();

  GridBagLayout gridBagLayout = new GridBagLayout();
  JLabel label = new JLabel();
	JScrollPane scrollPane=new JScrollPane();
  ActionList choiceList = new ActionList();
  JToolBar toolBar = new JToolBar();

	/**Default constructor.*/
	public QTIRenderPanel()
	{
		createActions();  //create the actions we'll use
		jbInit();
		updateActions(); //update the actions for the first time
	}

	/**Initializes the user interface.*/
  private void jbInit()
  {
    this.setLayout(gridBagLayout);
		choiceList.setModel(new DefaultListModel());  //use a default list model in the choice list
//G***del    choiceList.addListSelectionListener(this);  //show that we want to listent to changes in the list
    choiceList.addListSelectionListener(new ListSelectionListener()
    {
      public void valueChanged(ListSelectionEvent e)
      {
				updateActions();  //update the state of the actions when the list changes
      }
    });
		choiceList.addActionListener(editResponseLabelAction); //call the edit action when the item is selected
    label.setText("Choices");
//G***del    scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
//G***del    jButton1.setText("jButton1");
    scrollPane.setMinimumSize(new Dimension(50, 50));
    this.add(label,     new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
/*G***fix
    this.add(newChoiceButton,     new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
*/
    this.add(scrollPane,     new GridBagConstraints(0, 1, 2, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
    this.add(toolBar,  new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    scrollPane.getViewport().add(choiceList, null);
		toolBar.add(getAddResponseLabelAction());
		toolBar.add(getRemoveResponseLabelAction());
		toolBar.addSeparator();
		toolBar.add(getEditResponseLabelAction());
//G***del    scrollPane.getViewport().add(responseLabelsPanel, null);
  }

	/**Creates a new render of the correct type (e.g. choice or hotspot) and
		initializes it with the appropriate values. Its response labels are not
		added.
	@return An empty initialized render object of the correct type.
	*/
	protected abstract Render createRender();

	/**@return The rendering object being edited in the panel.*/
	public Render getRender()
	{
		final Render render=createRender(); //create the correct type of rendering
//G***del		final RenderChoice renderChoice=new RenderChoice();  //create a new choice rendering
		final ListModel choiceListModel=choiceList.getModel();  //get the list model of choices
		for(int i=0; i<choiceListModel.getSize(); ++i)  //look at each of the choices
		{
			final ResponseLabel responseLabel=(ResponseLabel)choiceListModel.getElementAt(i); //get this response label
//G**del Debug.trace("Getting response label: ", responseLabel); //G***del
			render.getResponseLabelList().add(responseLabel); //add this response label to our list
		}
		return render;  //return the rendering
	}

	/**Sets the rendering object that appears in the panel.
	@param qtiMaterialPanel The panel that holds the presentation material currently
		being edited for the item, so that the relevant image(s) can be retrieved.
	@param render The rendering that should be represented by the panel.
	*/
	public void setRender(final QTIMaterialPanel qtiMaterialPanel, final Render render)
	{
	  materialPanel=qtiMaterialPanel; //set the material panel G***is this the best way to do this?
//G***del		final DefaultListModel choiceListModel=new DefaultListModel();  //create a new list model
//G***del		choiceList.setModel(choiceListModel);  //assign the default list model to the choice list
		final DefaultListModel choiceListModel=(DefaultListModel)choiceList.getModel();  //get the list model of choices
		choiceListModel.clear();  //clear all the values in the list model
		final Iterator renderChoiceIterator=render.getResponseLabelList().iterator(); //get an iterator to the choices
		while(renderChoiceIterator.hasNext()) //while there are more choices
		{
			choiceListModel.addElement(renderChoiceIterator.next());  //add the next choice to the list model
		}
	}

	/**Called when the selected choice changes.
	@param listSelectionEvent The event that gives information about the list
		selection.
	@see #updateActions
	*/
/*G***del
  public void valueChanged(final ListSelectionEvent listSelectionEvent)
  {
		updateActions();  //update the actions
  }
*/

	/**Creates the actions that this component will use.*/
	public void createActions()
	{
		addResponseLabelAction=new AddResponseLabelAction();
		removeResponseLabelAction=new RemoveResponseLabelAction();
		editResponseLabelAction=new EditResponseLabelAction();
	}

	/**Updates the state of the actions based upon the selection.*/
  protected void updateActions()
  {
		if(choiceList.getSelectedIndex()>=0) //if a choice is selected
		{
			removeResponseLabelAction.setEnabled(true);  //enable the remove action
			editResponseLabelAction.setEnabled(true);  //enable the edit action
		}
		else  //if no choice is selected
		{
			removeResponseLabelAction.setEnabled(false);  //disable the remove action
			editResponseLabelAction.setEnabled(false);  //disable the edit action
		}
  }

	/**Creates a new response label.
	@return The new default response label.
	*/
	protected ResponseLabel createResponseLabel()
	{
		return new ResponseLabel();  //create a new response label and return it
	}

	/**Brings up a dialog for editing the response label.
	@param responseLabel The response label to edit.
	@return The new, modified response label or <code>null</code> if editing was
		cancelled.
	*/
	protected abstract ResponseLabel editResponseLabel(final ResponseLabel responseLabel);

	/**A custom renderer for the choices in the list.*/
/*G***fix
	protected static class ChoiceListCellRenderer extends StringListCellRenderer
	{
*/
		/**@return The correct text for this choice.
		@param list The list component.
		@param value The value of this list item.
		@param index The index of this list item.
		@param cellHasFocus Whether the list item has the focus.
		*/
/*G***fix
		protected String getListCellRendererString(final Object value)
		{
			final ChoiceLabel choiceLabel=(ChoiceLabel)value; //get the choice label
			return "["+choiceLabel.getIdent()+"] "+choiceLabel.getMaterial(); //return "[id] material"
		}
	}
*/

	/**Action for adding a new response label.*/
	protected class AddResponseLabelAction extends AbstractAction
	{
		/**Default constructor.*/
		public AddResponseLabelAction()
		{
			super("Add Response Label...");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Add response label.");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Add a new response label.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_A));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.ADD_ICON_FILENAME)); //load the correct icon
//G***del when works			new ImageIcon(ReaderFrame.class.getResource("book_open.gif")));	//load the correct icon
//G***del when works			putValue(SMALL_ICON, new ImageIcon(ReaderFrame.class.getResource("book_open.gif")));	//load the correct icon
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{
			final ResponseLabel responseLabel=createResponseLabel();  //create a new response label
			final ResponseLabel editedResponseLabel=editResponseLabel(responseLabel); //edit the new response label
			if(editedResponseLabel!=null)  //if the user accepted the changes
			{
				final Render render=getRender();  //get the render object being represented
				render.getResponseLabelList().add(editedResponseLabel); //add this response to our choice rendering
				setRender(materialPanel, render);  //set the new choice rendering back in our panel
				setModified(true);  //show that we've been modified
			}
		}
	}

	/**Action for deleting a response label.*/
	protected class RemoveResponseLabelAction extends AbstractAction
	{
		/**Default constructor.*/
		public RemoveResponseLabelAction()
		{
			super("Remove Response Label...");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Remove response label.");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Remove the selected response label.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_R));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.SUBTRACT_ICON_FILENAME)); //load the correct icon
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{

			final ResponseLabel responseLabel=(ResponseLabel)choiceList.getSelectedValue(); //get the selected choice
			if(responseLabel!=null) //if a choice is selected
		  {
					//if they really want to remove the item
				if(JOptionPane.showConfirmDialog(QTIRenderPanel.this, "Are you sure you want to remove response label with ID "+responseLabel.getIdent()+"?", "Remove Response Label", JOptionPane.OK_CANCEL_OPTION)==JOptionPane.OK_OPTION)
				{
				  final DefaultListModel choiceListModel=(DefaultListModel)choiceList.getModel();  //get the list model of choices
					choiceListModel.removeElement(responseLabel); //remove the response label from the list
					updateActions(); //update the actions after removing the choice
					setModified(true);  //show that we've been modified
				}
			}
		}
	}

	/**Action for editing a response label.*/
	protected class EditResponseLabelAction extends AbstractAction
	{
		/**Default constructor.*/
		public EditResponseLabelAction()
		{
			super("Edit Response Label...");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Edit response label.");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Edit the selected response label.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_E));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.EDIT_ICON_FILENAME)); //load the correct icon
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{
			final ResponseLabel oldResponseLabel=(ResponseLabel)choiceList.getSelectedValue(); //get the selected choice
			if(oldResponseLabel!=null) //if a choice is selected
		  {
				final ResponseLabel newResponseLabel=editResponseLabel(oldResponseLabel); //edit the response label
				if(newResponseLabel!=null)  //if the user accepted the changes
				{
				  final DefaultListModel choiceListModel=(DefaultListModel)choiceList.getModel();  //get the list model of choices
					final int index=choiceListModel.indexOf(oldResponseLabel);  //find out where in the list the old response label is
					choiceListModel.setElementAt(newResponseLabel, index);  //replace the old response label with the new
					setModified(true);  //show that we've been modified
				}
		  }
		}
	}

}
