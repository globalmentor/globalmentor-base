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
import java.awt.geom.Ellipse2D;
import javax.swing.*;
import com.globalmentor.awt.*;
import com.globalmentor.mentoract.qti.ResponseLabel;
import com.globalmentor.model.Verifiable;
import com.globalmentor.swing.*;
import com.globalmentor.swing.draw.*;

/**Provides a visual editing environment for a QTI response label.
@author Garret Wilson
*/
public class QTIResponseLabelPanel extends JPanel implements Verifiable
{
	/**The material panel, if the response label has material.*/
	protected QTIMaterialPanel materialPanel=null;
	/**The hotspot component, if the response label has an area.*/
	protected HotspotComponent hotspotComponent=null;

	/**@return The object being drawn, or <code>null</code> if no shape is being
		drawn.
	@see HotspotComponent#getDrawShape
	*/
	protected DrawShape getDrawShape()
	{
		return hotspotComponent!=null ? hotspotComponent.getDrawShape() : null; //return the hotspot component's drawing shape, if there is a hotspot component
	}

	/**Sets the object being drawn.
	@param newDrawShape The new shape to draw, or <code>null</code> if no shape
		is to be drawn.
	@see HotspotComponent#setDrawShape
	*/
	protected void setDrawShape(final DrawShape newDrawShape)
	{
		if(hotspotComponent!=null)  //if we have a hotspot component
			hotspotComponent.setDrawShape(newDrawShape); //set the hotspot component's drawing shape
	}

	/**@return the location and dimensions of the object being drawn, or
		<code>null</code> if no shape is being drawn.
	*/
	protected Shape getArea()
	{
	  final DrawShape drawShape=getDrawShape(); //get the drawing shape
		return drawShape!=null ? drawShape.getShape() : null; //return the drawing shape's shape if there is a drawing shape
	}

	/**Sets the area type being used in the hotspot editor.
	@param area The shape to use in the drawing component.
	*/
	protected void setArea(final Shape area)
	{
		if(area instanceof Ellipse2D) //if the area is an ellipse
		{
/*TODO fix
			final Rectangle rectangle=(Rectangle)area;  //cast the area to a rectangle
			stringBuffer.append(' ').append(RECTANGLE_AREA).append(':').append(' ');  //append "rectangle: "
			stringBuffer.append('('); //append '('
				//append the coordinates
			stringBuffer.append(rectangle.x).append(',').append(rectangle.y).append(',').append(rectangle.width).append(',').append(rectangle.height);
			stringBuffer.append(')'); //append ')'
*/
		}
		else if(area instanceof Rectangle) //if the area is a rectangle
		{
			setDrawShape(new DrawRectangle(Color.black, (Rectangle)area));  //create a draw rectangle and set it as the edited object
		}
		else if(area instanceof Polygon) //if the area is a polygon
		{
/*TODO fix
			final Rectangle rectangle=(Rectangle)area;  //cast the area to a rectangle
			stringBuffer.append(' ').append(RECTANGLE_AREA).append(':').append(' ');  //append "rectangle: "
			stringBuffer.append('('); //append '('
				//append the coordinates
			stringBuffer.append(rectangle.x).append(',').append(rectangle.y).append(',').append(rectangle.width).append(',').append(rectangle.height);
			stringBuffer.append(')'); //append ')'
*/
		}
	}

  GridBagLayout gridBagLayout = new GridBagLayout();
  JTextField identTextField = new JTextField();
  JLabel idLabel = new JLabel();
  JLabel contentLabel = new JLabel();
	JPanel contentContainerPanel=new JPanel();
  BorderLayout contentBorderLayout = new BorderLayout();
  JToolBar toolBar = new JToolBar();

	/**Default constructor.*/
	public QTIResponseLabelPanel()
	{
		jbInit();
	}

	/**Initializes the user interface.*/
  private void jbInit()
  {
    identTextField.setColumns(4);
    this.setLayout(gridBagLayout);
    idLabel.setText("ID");
    contentLabel.setText("Content");
    contentContainerPanel.setLayout(contentBorderLayout);
    this.add(identTextField,       new GridBagConstraints(0, 1, 1, 1, 0.1, 0.0
            ,GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 4), 0, 0));
    this.add(idLabel,    new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(contentLabel,   new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(contentContainerPanel,         new GridBagConstraints(1, 1, 2, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
    this.add(toolBar,  new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
  }

	/**@return The response label being edited in the panel.*/
	public ResponseLabel getResponseLabel()
	{
		final ResponseLabel responseLabel=new ResponseLabel();  //create a new response label
		responseLabel.setIdent(identTextField.getText()); //get the ident
		if(materialPanel!=null) //if material is being edited
		{
			responseLabel.setMaterial(materialPanel.getMaterial()); //get the material
		}
		else if(hotspotComponent!=null) //if we were editing a hotspot
		{
			responseLabel.setArea(getArea()); //set the area shape of the response label
		}
		//G**update the other properties
		return responseLabel;  //return the response label
	}

	/**Sets the response label that appears in the panel.
	@param responseLabel The response label that should be represented by the panel.
	@param image The image for a hotspot, or <code>null</code> if no image is
		available or an image is not applicable.
	*/
	public void setResponseLabel(final ResponseLabel responseLabel, final Image image)
	{
		identTextField.setText(responseLabel.getIdent()); //set the ident text
		contentContainerPanel.removeAll();  //remove everything from the content container panel
		final Shape area=responseLabel.getArea(); //see if the response label has an area
		if(area!=null)  //if the response label has an area
		{
			toolBar.setVisible(false); //hide the toolbar, because we won't need it
	    contentLabel.setText("Hotspot");  //TODO i18n
			materialPanel=null; //show that we don't have a material panel
			hotspotComponent=new HotspotComponent(image); //create a hotspot component
			setArea(area);  //set the area being used
			final JScrollPane hotspotScrollPane=new JScrollPane(hotspotComponent);  //create a scroll pane to scroll the image
	    contentContainerPanel.add(hotspotScrollPane, BorderLayout.CENTER);  //add the hotpot panel to the content container
		}
		else if(responseLabel.getMaterial()!=null)  //if there is no area, but there is material
		{
	    contentLabel.setText("Material"); //TODO i18n
			hotspotComponent=null;  //show that we don't have a hotspot component
				//create a material panel with the material
		  materialPanel=new QTIMaterialPanel(responseLabel.getMaterial());
			toolBar.setVisible(true); //show the toolbar, which allows images to be added
			toolBar.removeAll();  //remove all the components from the toolbar
			toolBar.add(materialPanel.getSetImageAction()); //add the image editing functionality to the toolbar
			toolBar.add(materialPanel.getPreviewImageAction());
			toolBar.add(materialPanel.getDeleteImageAction());
	    contentContainerPanel.add(materialPanel, BorderLayout.CENTER);  //add the material panel to the content container
		}
	}

	/**Verifies the component.
	@return <code>true</code> if the component contents are valid, <code>false</code>
		if not.
	*/
	public boolean verify()
	{
		if(identTextField.getText().length()==0)  //if there is no ID
		{
			JOptionPane.showMessageDialog(this, "Each response label must have a unique identifier.", "Missing ident", JOptionPane.ERROR_MESSAGE);	//TODO i18n
			identTextField.requestFocus(); //focus on the ID text field
			return false; //show that verification failed
		}
		return true;  //if we couldn't find any problems, verification succeeded
//TODO change this panel to inherit from BasicPanel, and verify the parent class (removing the implements Verifiable)		return super.verify();  //if we couldn't find any problems, verify the parent class
	}

//TODO probably add an option for showing the labels or not

	/**The component for editing hotspots.*/  //TODO maybe refactor this into a separate public class
	protected static class HotspotComponent extends ImageComponent
	{

		/**The strategy used to actually draw the shape.*/
		private final DrawShapeStrategy drawShapeStrategy;

		/**@return The object being drawn, or <code>null</code> if no shape is being
		  drawn.
		@see DrawShapeStrategy#getDrawShape
		*/
		public DrawShape getDrawShape() {return drawShapeStrategy.getDrawShape();}

		/**Sets the object being drawn.
		@param newDrawShape The new shape to draw, or <code>null</code> if no shape
			is to be drawn.
		@see DrawShapeStrategy#setDrawShape
		*/
		public void setDrawShape(final DrawShape newDrawShape) {drawShapeStrategy.setDrawShape(newDrawShape);}


		/**Creates a component for editing hotspots.
		@param image The image on which the hotspots will be drawn.
		@param drawShape The shape to be drawn.
		*/
		public HotspotComponent(final Image image, final DrawShape drawShape)
		{
			super(image); //construct the parent class with the image
			Images.loadImage(image);  //load the image so that we will know its size when we display it
			drawShapeStrategy=new DrawShapeStrategy(this, drawShape);  //create the strategy for drawing the shape
			addMouseListener(drawShapeStrategy); //allow the strategy to listen to mouse events
			addMouseMotionListener(drawShapeStrategy); //allow the strategy to listen to mouse motion events
		}

		/**Creates a component for editing hotspots, with no initial shape specified.
		@param image The image on which the hotspots will be drawn.
		*/
		public HotspotComponent(final Image image)
		{
			this(image, null);  //do the default construction with no shape specified
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