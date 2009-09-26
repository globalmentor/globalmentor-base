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
import java.io.*;
import javax.swing.*;

import com.garretwilson.awt.ImageUtilities;
import com.garretwilson.swing.*;
import com.garretwilson.swing.draw.*;

import com.globalmentor.log.Log;
import com.globalmentor.mentoract.qti.*;

/**Provides a visual editing environment for a QTI hotspot rendering.
@author Garret Wilson
*/
public class QTIRenderHotspotPanel extends QTIRenderPanel
{

	/**Default constructor.*/
	public QTIRenderHotspotPanel()
	{
		super();  //construct the parent
		jbInit();
	}

	/**Initializes the user interface.*/
  private void jbInit()
  {
    label.setText("Hotspots");  //TODO fix
  }

	/**@return An empty hotspot render object.*/
	protected Render createRender()
	{
		return new RenderHotspot();  //create a choice render object
	}

	/**Creates the actions that this component will use.*/
	public void createActions()
	{
		super.createActions();  //create the default actions
		  //update the add action properties to be specific to the type of rendering
		getAddResponseLabelAction().putValue(Action.NAME, "Add Hotspot...");
		getAddResponseLabelAction().putValue(Action.SHORT_DESCRIPTION, "Add hotspot");
		getAddResponseLabelAction().putValue(Action.LONG_DESCRIPTION, "Add a new hotspot.");
//TODO fix putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_P));  //set the mnemonic key TODO i18n
		  //update the remove action properties to be specific to the type of rendering
		getRemoveResponseLabelAction().putValue(Action.NAME, "Remove Hotspot...");
		getRemoveResponseLabelAction().putValue(Action.SHORT_DESCRIPTION, "Remove hotspot");
		getRemoveResponseLabelAction().putValue(Action.LONG_DESCRIPTION, "Remove the selected hotspot.");
//TODO fix putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_P));  //set the mnemonic key TODO i18n
		  //update the edit action properties to be specific to the type of rendering
		getEditResponseLabelAction().putValue(Action.NAME, "Edit Hotspot...");
		getEditResponseLabelAction().putValue(Action.SHORT_DESCRIPTION, "Edit hotspot");
		getEditResponseLabelAction().putValue(Action.LONG_DESCRIPTION, "Edit the selected hotspot.");
//TODO fix putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_P));  //set the mnemonic key TODO i18n
	}

	/**Creates a new response label.
	@return The new default response label.
	*/
	protected ResponseLabel createResponseLabel()
	{
		final ResponseLabel responseLabel=super.createResponseLabel();  //create the default response label
		final Render render=getRender();  //get the render object being represented
		responseLabel.setIdent(String.valueOf((char)('A'+render.getResponseLabelList().size()))); //set the ident for the new choice TODO fix to some standard method TODO do we want to do this?
		responseLabel.setArea(new Rectangle()); //set the area to a default rectangle
		return responseLabel;  //return the default response label we constructed
	}

	/**Brings up a dialog for editing the response label.
	@param responseLabel The response label to edit.
	@return The new, modified response label or <code>null</code> if editing was
		cancelled.
	*/
	protected ResponseLabel editResponseLabel(final ResponseLabel responseLabel)
	{
/*TODO fix
		final QTIResponseLabelPanel responseLabelPanel=new QTIResponseLabelPanel();  //create a new panel to edit the response label
		responseLabelPanel.setResponseLabel(responseLabel); //set the response label
		responseLabelPanel.setPreferredSize(new Dimension(300, 200));  //TODO fix preferred size QTI
		  //show the response label panel
		final int result=JOptionPane.showConfirmDialog(this, responseLabelPanel, "Response Label", JOptionPane.OK_CANCEL_OPTION);  //TODO i18n; comment
		return result==JOptionPane.OK_OPTION ? responseLabelPanel.getResponseLabel() : null; //return their response, or null if they cancelled
*/
			//TODO put all of this in ResponseLabel
		if(materialPanel!=null) //if we know the material panel TODO we always should
		{
Log.trace("have material"); //TODO del
			final File imageFile=materialPanel.getImageFile();  //get the file of the image
			if(imageFile!=null) //if there is an image file
			{
Log.trace("found image"); //TODO del
				final Toolkit toolkit=Toolkit.getDefaultToolkit();	//get the default toolkit
				final Image image=toolkit.getImage(imageFile.toString()); //load the image from the file
/*TODO del when works
//TODO del when works				  ImageUtilities.loadImage(image);  //load the image so that we will know its size when we display it
				final DrawRectangle drawRectangle=new DrawRectangle(Color.black);  //TODO testing
				final HotspotComponent hotspotComponent=new HotspotComponent(image, drawRectangle);  //TODO fix
				final JScrollPane hotspotScrollPane=new JScrollPane(hotspotComponent);  //create a scroll pane to scroll the image
				JOptionPane.showConfirmDialog(QTIRenderHotspotPanel.this, hotspotScrollPane, "Edit Hotspot", JOptionPane.OK_CANCEL_OPTION);  //TODO testing
*/

				final QTIResponseLabelPanel responseLabelPanel=new QTIResponseLabelPanel();  //create a new panel to edit the response label
				responseLabelPanel.setResponseLabel(responseLabel, image); //set the response label
//TODO del				responseLabelPanel.setPreferredSize(new Dimension(300, 200));  //TODO fix preferred size QTI
					//show the response label panel
				final int result=BasicOptionPane.showConfirmDialog(this, responseLabelPanel, "Response Label", JOptionPane.OK_CANCEL_OPTION);  //TODO i18n; comment
				return result==JOptionPane.OK_OPTION ? responseLabelPanel.getResponseLabel() : null; //return their response, or null if they cancelled


//TODO del if doesn't work				final Frame frame=ComponentUtilities.getParentFrame(QTIMaterialPanel.this); //get the frame in which this panel is embedded, if possible

//TODO fix				JOptionPane.showConfirmDialog(QTIMaterialPanel.this, imagePanel, "Item", JOptionPane.OK_CANCEL_OPTION); //TODO testing
/*G**del
				final Dimension screenSize=Toolkit.getDefaultToolkit().getScreenSize(); //find the size of the screen
				imagePanel.setPreferredSize(new Dimension(screenSize.width/2, screenSize.height/2));  //set the panel prefer to be 1/4 the screen size
				JOptionPane.showMessageDialog(QTIMaterialPanel.this, imagePanel, imageFile.toString(), JOptionPane.PLAIN_MESSAGE); //TODO testing
*/
			}
		}
		return null;  //show that editing the response label was cancelled
	}

	/**The component for editing hotspots.*/  //TODO put this inside ResponseLabelPanel
	protected static class HotspotComponent extends ImageComponent
	{

		/**The strategy used to actually draw the shape.*/
		final DrawShapeStrategy drawShapeStrategy;

		/**Creates a component for editing hotspots.
		@param image The image on which the hotspots will be drawn.
		@param drawShape The shape to be drawn.
		*/
		public HotspotComponent(final Image image, final DrawShape drawShape)
		{
			super(image); //construct the parent class with the image
			ImageUtilities.loadImage(image);  //load the image so that we will know its size when we display it
			drawShapeStrategy=new DrawShapeStrategy(this, drawShape);  //create the strategy for drawing the shape
			addMouseListener(drawShapeStrategy); //allow the strategy to listen to mouse events
			addMouseMotionListener(drawShapeStrategy); //allow the strategy to listen to mouse motion events
		}

		/**Paints the image. Overridden to paint the shapes.
		@param graphics The graphics context used for painting.
		*/
		public void paintComponent(final Graphics graphics)
		{
			super.paintComponent(graphics); //paint the parent normally
			drawShapeStrategy.paint(graphics);  //TODO testing
		}
	}

}
