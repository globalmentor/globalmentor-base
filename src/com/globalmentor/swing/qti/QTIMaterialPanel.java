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
import java.io.*;
import java.net.*;
import java.util.*;
import javax.swing.*;

import com.globalmentor.mentoract.qti.*;
import com.globalmentor.swing.*;

import static com.globalmentor.net.URIs.*;

/**Provides a visual editing environment for QTI material.
@author Garret Wilson
*/
public class QTIMaterialPanel extends JPanel
{

	/**The action that sets an image for the material.*/
	private final Action setImageAction=new SetImageAction();

		/**@return The action that sets an image for the material.*/
		public Action getSetImageAction() {return setImageAction;}

	/**The action that previews the image associated with the material.*/
	private final Action previewImageAction=new PreviewImageAction();

		/**@return The action that previews the image associated with the material.*/
		public Action getPreviewImageAction() {return previewImageAction;}

	/**The action that deletes the image associated with the material.*/
	private final Action deleteImageAction=new DeleteImageAction();

		/**@return The action that deletes the image associated with the material.*/
		public Action getDeleteImageAction() {return deleteImageAction;}

	/**The file of the image associated with this material.*/
	private File imageFile=null;

		/**@return The file of the image associated with this material, or
			<code>null</code> if there is no image associated with this material.*/
		public File getImageFile() {return imageFile;}

		/**Sets the image associated with the material.
		  The canonical form of the file is used.
		@param newImageFile The file of the image to associate with the material,
			or <code>null</code> to remove any image association.*/
		public void setImageFile(final File newImageFile)
		{
			try
			{
			  imageFile=newImageFile!=null ? newImageFile.getCanonicalFile() : null; //update the image file
			}
			catch(IOException ioException)
			{
				throw new AssertionError(ioException); //we don't expect to get this error
			}
			updateActions();  //update the actions, now that the image has changed
		}

  GridBagLayout gridBagLayout = new GridBagLayout();
  JScrollPane scrollPane = new JScrollPane();
  JTextArea textArea = new JTextArea();

	/**Default constructor.*/
	public QTIMaterialPanel()
	{
		jbInit();
		updateActions();  //update the state of the actions
	}

	/**Material constructor.
	@param material The material to edit.
	*/
	public QTIMaterialPanel(final Material material)
	{
		this(); //do the default construction
		setMaterial(material);  //set the material to edit
	}

	/**Initializes the user interface.*/
  private void jbInit()
  {
    this.setLayout(gridBagLayout);
//TODO del    scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		textArea.setColumns(32);
		textArea.setLineWrap(true);
    textArea.setWrapStyleWord(true);
    this.add(scrollPane,    new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
            ,GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
    scrollPane.getViewport().add(textArea, null);
  }

	/**@return The material being edited in the panel.*/
	public Material getMaterial()
	{
		final Material material=new Material(); //create new material
		if(textArea.getText().length()>0)  //if text has been entered for the material
		{
			final MaterialText materialText=new MaterialText(); //create new material text
			materialText.setText(textArea.getText()); //get the text from the text field
			material.getMaterialContentList().add(materialText);  //add the material text to the material
		}
		final String imageURI;  //we'll determine the image being stored for the material
		if(getImageFile()!=null)  //if there is an associated image
		{
			try
			{
				material.setImageURI(getImageFile().toURL().toString());  //convert the image to a URL and store it in the material
			}
			catch(MalformedURLException malformedURLException)
			{
				imageURI=null;  //show that we couldn't get an image
				throw new AssertionError(malformedURLException); //we don't expect to get this error
			}
		}
		else  //if there  is no associated image
			imageURI=null;  //there should be no reference to an image
		return material;  //return the material
	}

	/**Sets the material that appears in the panel. Currently on the the first
		material text is recognized.
	@param material The material that should be represented by the panel.
	*/
	public void setMaterial(final Material material)
	{
		textArea.setText(""); //clear the text in the text area
		setImageFile(null); //remove any associated image
		final Iterator materialContentIterator=material.getMaterialContentList().iterator();  //get an iterator to the material content
		while(materialContentIterator.hasNext())  //while there is more content
		{
			final MaterialContent materialContent=(MaterialContent)materialContentIterator.next();  //get the next material content
		  if(materialContent instanceof MaterialText) //if this is material text
			{
				textArea.append((((MaterialText)materialContent).getText()));  //append the text to the text field
//TODO del				break;  //stop looking for text TODO what about adding multiple material text?
			}
		}
		if(material.getImageURI()!=null)  //if there is an image
		{
			//TODO decide what to do if the image is a relative URI
			try
			{
				final URL imageURL=new URL(material.getImageURI()); //create a URL from the image reference
				if(FILE_SCHEME.equals(imageURL.getProtocol())) //if this URL contains a file
				{
					setImageFile(new File(imageURL.getPath())); //set the file to thta contained in the URL
				}
				else
	;			//TODO decide what to do if the image is not a file
			}
			catch(MalformedURLException malformedURLException)
			{
				throw new AssertionError(malformedURLException); //TODO fix
			}
		}
	}

	/**Updates the state of the actions based upon the selection.*/
  public void updateActions()
  {
		final boolean hasImage=getImageFile()!=null;  //see if there is an image associated with the material
		getPreviewImageAction().setEnabled(hasImage); //we can only preview an image if there is an image
		getDeleteImageAction().setEnabled(hasImage); //we can only delete an image if there is an image
  }

	/**Action for setting the image.*/
	class SetImageAction extends AbstractAction
	{
		/**Default constructor.*/
		public SetImageAction()
		{
			super("Set Image...");	//create the base class TODO i18n
			putValue(SHORT_DESCRIPTION, "Set image.");	//set the short description TODO i18n
			putValue(LONG_DESCRIPTION, "Set the image to use with the material.");	//set the long description TODO i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_I));  //set the mnemonic key TODO i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.IMAGE_ICON_FILENAME)); //load the correct icon
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{
//TODO fix			try
			{
				final JFileChooser fileChooser=new JFileChooser();	//create a new dialog for listing files
	//TODO fix			fileChooser.setCurrentDirectory(getReaderConfig().getFileLocations().getCurrentDirectory());	//change the file chooser directory to the reader's current directory
				final int option=fileChooser.showOpenDialog(QTIMaterialPanel.this);	//show the open dialog
				if(option==JFileChooser.APPROVE_OPTION)	//if they chose a file
				{
	//TODO fix				getReaderConfig().getFileLocations().setCurrentDirectory(fileChooser.getCurrentDirectory());	//save the new directory they changed to
					final File selectedFile=fileChooser.getSelectedFile();	//get the file they chose
					if(selectedFile!=null)	//if they chose a file
					{
						setImageFile(selectedFile); //set the file to whatever they chose
					}
				}
			}
//TODO fix			catch(Exception ex)
			{
//TODO fix				Log.error("Error: "+ex);	//TODO fix
			}
		}
	}

	/**Action for previewing the associated image.*/
	class PreviewImageAction extends AbstractAction
	{
		/**Default constructor.*/
		public PreviewImageAction()
		{
			super("Preview Image...");	//create the base class TODO i18n
			putValue(SHORT_DESCRIPTION, "Preview image.");	//set the short description TODO i18n
			putValue(LONG_DESCRIPTION, "Preview the image assocciated with the material.");	//set the long description TODO i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_P));  //set the mnemonic key TODO i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.IMAGE_PREVIEW_ICON_FILENAME)); //load the correct icon
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{
			final File imageFile=getImageFile();  //get the file of the image
			if(imageFile!=null) //if there is an image file
			{
				final Toolkit toolkit=Toolkit.getDefaultToolkit();	//get the default toolkit
				final Image image=toolkit.getImage(imageFile.toString()); //load the image from the file
				final ImagePanel imagePanel=new ImagePanel(image); //create a panel to view the image
//TODO del if doesn't work				final Frame frame=ComponentUtilities.getParentFrame(QTIMaterialPanel.this); //get the frame in which this panel is embedded, if possible

//TODO fix				JOptionPane.showConfirmDialog(QTIMaterialPanel.this, imagePanel, "Item", JOptionPane.OK_CANCEL_OPTION); //TODO testing

				final Dimension screenSize=Toolkit.getDefaultToolkit().getScreenSize(); //find the size of the screen
				imagePanel.setPreferredSize(new Dimension(screenSize.width/2, screenSize.height/2));  //set the panel prefer to be 1/4 the screen size
				JOptionPane.showMessageDialog(QTIMaterialPanel.this, imagePanel, imageFile.toString(), JOptionPane.PLAIN_MESSAGE); //TODO testing

/*TODO fix
				final JFrame frame=new JFrame(imageFile.toString()); //TODO testing
				frame.setContentPane(imagePanel); //TODO testing
				final Dimension screenSize=Toolkit.getDefaultToolkit().getScreenSize(); //find the size of the screen
				frame.setSize(screenSize.width/2, screenSize.height/2);  //set the dialog to be 1/4 the screen size
//TODO fix				frame.setResizable(true);  //TODO testing
	//TODO fix				dialog.validate();  //TODO comment
	//TODO fix				dialog.pack();  //pack the contents of the dialog
				frame.setVisible(true);  //show the dialog
*/
/*TODO fix
				  //TODO check about closing and disposing; see JOptionPane
				final JDialog dialog=new JDialog((Frame)null, imageFile.toString(), true); //create a new modal dialog in which to show the image
				dialog.setDefaultCloseOperation(dialog.DISPOSE_ON_CLOSE); //TODO comment; fix for other dialogs; create utility function
				dialog.setContentPane(imagePanel);  //put the image panel in the dialog
				final Dimension screenSize=Toolkit.getDefaultToolkit().getScreenSize(); //find the size of the screen
				dialog.setSize(screenSize.width/2, screenSize.height/2);  //set the dialog to be 1/4 the screen size
				dialog.setResizable(true);  //TODO testing
	//TODO fix				dialog.validate();  //TODO comment
	//TODO fix				dialog.pack();  //pack the contents of the dialog
				dialog.setVisible(true);  //show the dialog
*/
			}
		}
	}

	/**Action for deleting the associated image.*/
	class DeleteImageAction extends AbstractAction
	{
		/**Default constructor.*/
		public DeleteImageAction()
		{
			super("Delete Image...");	//create the base class TODO i18n
			putValue(SHORT_DESCRIPTION, "Delete image.");	//set the short description TODO i18n
			putValue(LONG_DESCRIPTION, "Delete the image currently assocciated with the material.");	//set the long description TODO i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_D));  //set the mnemonic key TODO i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.IMAGE_DELETE_ICON_FILENAME)); //load the correct icon
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{
		  //TODO ask for confirmation
			setImageFile(null); //remove the image
		}
	}

}