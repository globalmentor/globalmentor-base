package com.garretwilson.swing.qti;

import java.awt.*;
import javax.swing.*;
import java.util.*;
import com.globalmentor.mentoract.qti.*;
import com.globalmentor.util.Debug;

/**Provides a visual editing environment for a QTI logical ID response.
@author Garret Wilson
*/
public class QTIResponseLIDPanel extends JPanel
{

	/**The panel that holds the presentation material currently	being edited for
		the item, so that the relevant image(s) can be retrieved.
	*/
	protected QTIMaterialPanel materialPanel=null;

  GridBagLayout gridBagLayout = new GridBagLayout();
  JComboBox cardinalityComboBox = new JComboBox(new Object[]{Response.SINGLE_CARDINALITY, Response.MULTIPLE_CARDINALITY, Response.ORDERED_CARDINALITY});
  JCheckBox timingCheckBox = new JCheckBox();
  JPanel renderContainerPanel = new JPanel();
  JLabel cardinalityLabel = new JLabel();
  GridBagLayout renderGridBagLayout = new GridBagLayout();
  JLabel responseLIDLabel = new JLabel();

	/**Default constructor.*/
	public QTIResponseLIDPanel()
	{
		jbInit();
	}

	/**Constructs a panel from a logical ID response.
	@param qtiMaterialPanel The panel that holds the presentation material currently
		being edited for the item, so that the relevant image(s) can be retrieved.
	@param responseLID The logical ID response that should be represented by the panel.
	*/
	public QTIResponseLIDPanel(final QTIMaterialPanel qtiMaterialPanel, final ResponseLID responseLID)
	{
		this(); //do the default constructing
		setResponseLID(qtiMaterialPanel, responseLID);  //set the logical ID response
	}

	/**Initializes the user interface.*/
  private void jbInit()
  {
    this.setLayout(gridBagLayout);
    timingCheckBox.setToolTipText("");
    timingCheckBox.setActionCommand("timedCheckBox");
    timingCheckBox.setText("Timed");
    cardinalityLabel.setText("Cardinality");
    renderContainerPanel.setLayout(renderGridBagLayout);
    responseLIDLabel.setText("Logical ID");
    this.add(cardinalityComboBox,      new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(timingCheckBox,      new GridBagConstraints(3, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(renderContainerPanel,          new GridBagConstraints(0, 1, 4, 1, 1.0, 1.0
            ,GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 4, 0, 0), 0, 0));
    this.add(cardinalityLabel,          new GridBagConstraints(1, 0, 1, 1, 1.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(responseLIDLabel,   new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
  }

	/**Retrieves the render panel representing the indexed rendering.
	@param renderIndex The index of the rendering the panel of which to retrieve.
	@return The panel used to represent the appropriate rendering.
	@exception IndexOutOfBoundsException Thrown if there is no QTI render panel to
		represent a rendering at the given index.
	*/
	QTIRenderPanel getRenderPanel(int renderIndex) throws IndexOutOfBoundsException
	{
		final Component[] renderComponents=renderContainerPanel.getComponents(); //get the components in the render panel
		for(int componentIndex=0; ; ++componentIndex) //look at each component, allowing an index out of bounds exception to be thrown if we run out of render panels
		{
			final Component component=renderComponents[componentIndex];  //get the current component
		  if(component instanceof QTIRenderPanel) //if this is a render panel
			{
				if(renderIndex==0)  //if we've found the rendering at the correct index
				{
					return (QTIRenderPanel)component; //return this render panel
				}
				else  //if we have more renderings to go
				{
					--renderIndex;  //maybe the next rendering will be the correct one
				}
			}
		}
	}

	/**@return The logical ID response being edited in the panel.*/
	public ResponseLID getResponseLID()
	{
		final ResponseLID responseLID=new ResponseLID();  //create a new logical ID response
		responseLID.setCardinality((String)cardinalityComboBox.getSelectedItem());  //get the cardinality
		responseLID.setTiming(timingCheckBox.isSelected()); //get the timing
		if(renderContainerPanel.getComponents().length>0)  //if there is at least one rendering G***fix for multiple renderings
		{
/*G***fix for multiple renderings
		final Component[] renderComponents=renderContainerPanel.getComponents(); //get the components in the render panel
		if(renderComponents.length>0) //if there is at least one rendering G***fix for multiple renderings
		{
			final Component component=renderComponents[0];  //get the component
			Debug.assert(component instanceof QTIRenderPanel, "Component is not a render panel.");
			final QTIRenderPanel renderPanel=(QTIRenderPanel)component; //cast the component to a render panel
*/
			final QTIRenderPanel renderPanel=getRenderPanel(0); //get the first render panel
			final Render render=renderPanel.getRender();  //get the render from the panel
//G***del Debug.trace("Getting render choice with first response label: ", renderChoice.getResponseLabelList().get(0)); //G***del
			responseLID.getRenderList().add(render);  //add the rendering to our renderings
/*G***del when works
			if(component instanceof QTIRenderChoicePanel) //if this is a choice rendering
			{
				final QTIRenderChoicePanel renderChoicePanel=(QTIRenderChoicePanel)component; //cast the component to a render choice panel
				final RenderChoice renderChoice=renderChoicePanel.getRenderChoice();  //get the render choice from the panel
//G***del Debug.trace("Getting render choice with first response label: ", renderChoice.getResponseLabelList().get(0)); //G***del
				responseLID.getRenderList().add(renderChoice);  //add the choice rendering to our renderings
			}
*/

		}
		//G**update the other properties
		return responseLID;  //return the logical ID response
	}

	/**Sets the response that appears in the panel. Currently only the first render
		type is recognized.
	@param qtiMaterialPanel The panel that holds the presentation material currently
		being edited for the item, so that the relevant image(s) can be retrieved.
	@param responseLID The logical ID response that should be represented by the panel.
	*/
	public void setResponseLID(final QTIMaterialPanel qtiMaterialPanel, final ResponseLID responseLID)
	{
		materialPanel=qtiMaterialPanel; //save the material panel so that we can access it in the hotspot panel, for example
		cardinalityComboBox.setSelectedItem(responseLID.getCardinality());  //set the selected cardinality
		timingCheckBox.setSelected(responseLID.isTiming());  //set the timing checkbox
		if(responseLID.getRenderList().size()>0)  //if there is at least one render type
		{
			final Render render=(Render)responseLID.getRenderList().get(0); //get the first render type G***assert there is at least one
		  final QTIRenderPanel renderPanel; //we'll construct the correct type of panel here
			if(render instanceof RenderChoice)  //if this is a render choice
			{
//G***del when works				final RenderChoice renderChoice=(RenderChoice)render; //cast the render to a render choice
				renderPanel=new QTIRenderChoicePanel();  //create a panel for the render choice
/*G***del when works
				final QTIRenderChoicePanel renderChoicePanel=new QTIRenderChoicePanel();  //create a panel for the render choice
				renderChoicePanel.setRenderChoice(renderChoice);  //set the choice being shown in the panel
				renderPanel.removeAll();  //remove all components from the render panel
	//G***del		  renderPanel.add(renderChoicePanel, BorderLayout.CENTER);  //add the render choice panel to the render panel G***do we want to just replace the render choice panel, or perhaps use a borderlayout?
	//G***del			renderPanel.add(renderChoicePanel); //add the render choice panel to the render panel G***do we want to just replace the render choice panel, or perhaps use a borderlayout?
						//add the render choice panel to the render panel G***do we want to just replace the render choice panel, or perhaps use a borderlayout?
				renderPanel.add(renderChoicePanel, new GridBagConstraints(0, 0, 1, 1, 1.0, 0.0,
					GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
*/
			}
			else if(render instanceof RenderHotspot)  //if this is a render hotspot
			{
//G***del when works				final RenderHotspot renderHotspot=(RenderHotspot)render; //cast the render to a render hotspot
				renderPanel=new QTIRenderHotspotPanel();  //create a panel for the render hotspot
/*G***del when works
				final QTIRenderHotspotPanel renderHotspotPanel=new QTIRenderHotspotPanel();  //create a panel for the render hotspot
				renderHotspotPanel.setRenderHotspot(materialPanel, renderHotspot);  //set the hotspots being shown in the panel
				renderPanel.removeAll();  //remove all components from the render panel G***should we combine all this for all the render types?
				renderPanel.add(renderHotspotPanel, new GridBagConstraints(0, 0, 1, 1, 1.0, 0.0,
					GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
*/
			}
			else  //if we don't recognize the rendering type
			{
				Debug.error("Unrecognized rendering type"); //G***fix
				return; //G***fix
			}
			renderPanel.setRender(materialPanel, render);  //set the rendering being shown in the panel
			renderContainerPanel.removeAll();  //remove all components from the render container panel
					//add the render panel to the render container panel G***do we want to just replace the render choice panel, or perhaps use a borderlayout?
			renderContainerPanel.add(renderPanel, new GridBagConstraints(0, 0, 1, 1, 1.0, 0.0,
				GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
		}
	}

}