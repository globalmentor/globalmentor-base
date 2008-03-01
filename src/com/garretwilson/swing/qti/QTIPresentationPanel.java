package com.garretwilson.swing.qti;

import java.awt.*;
import java.awt.event.ActionEvent;
import javax.swing.*;
import java.util.*;
import com.garretwilson.awt.WindowUtilities;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.assess.qti.*;
import com.globalmentor.util.Debug;

/**Provides a visual editing environment for QTI presentation.
@author Garret Wilson
*/
public class QTIPresentationPanel extends JPanel
{
  GridBagLayout gridBagLayout = new GridBagLayout();
  JPanel responsePanel = new JPanel();
  JLabel materialLabel = new JLabel();
  QTIMaterialPanel materialPanel = new QTIMaterialPanel();
  JLabel responseLabel = new JLabel();
//G***del  JButton newResourceButton = new JButton(new NewResponseAction());
  GridBagLayout responseGridBagLayout = new GridBagLayout();
  JToolBar toolBar = new JToolBar();

	/**Default constructor.*/
	public QTIPresentationPanel()
	{
		jbInit();
	}

	/**Initializes the user interface.*/
  private void jbInit()
  {
    this.setLayout(gridBagLayout);
    materialLabel.setText("Material");
    responseLabel.setText("Responses");
//G***del    newResourceButton.setText("jButton1");
    responsePanel.setLayout(responseGridBagLayout);
    this.add(responsePanel,           new GridBagConstraints(0, 3, 2, 1, 1.0, 0.0
            ,GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
    this.add(materialLabel,      new GridBagConstraints(0, 0, 2, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(materialPanel,        new GridBagConstraints(0, 1, 2, 1, 1.0, 0.2
            ,GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
    this.add(responseLabel,    new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    this.add(toolBar,  new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
		toolBar.add(materialPanel.getSetImageAction());
		toolBar.add(materialPanel.getPreviewImageAction());
		toolBar.add(materialPanel.getDeleteImageAction());
//G***del when not needed    this.add(newResourceButton,   new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0
//G***del when not needed            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
  }

	/**@return The presentation being edited in the panel.*/
	public Presentation getPresentation()
	{
		final Presentation presentation=new Presentation();  //create new presentation
		presentation.setMaterial(materialPanel.getMaterial()); //get the material
//G***fix; we need to gather the responses
		presentation.getResponseList().addAll(getResponses());  //get the responses and add them to the response list
		//G**update the other properties
		return presentation;  //return the presentation
	}

	/**Sets the presentation that appears in the panel. Currently only the first
		response type is recognized.
	@param presentation The QTI presentation that should be represented by the panel.
	*/
	public void setPresentation(final Presentation presentation)
	{
		final Material material=presentation.getMaterial(); //get the material
		if(material!=null)  //if there is material
		  materialPanel.setMaterial(material);  //set the material
		responsePanel.removeAll();  //remove all components from the response panel
		final Iterator responseIterator=presentation.getResponseList().iterator();  //get an iterator to look through the responses
		while(responseIterator.hasNext()) //while there are more responses
		{
			final Response response=(Response)responseIterator.next(); //get the next response type
		  addResponse(response);  //add this response to the panel
		}
	}

	/**Adds a response to the response panel at the appropriate index. Does not
		pack the window or validate the components.
	@param response The response to add.
	*/
	protected void addResponse(final Response response)
	{
		if(response instanceof ResponseLID)  //if this is a logical ID response
		{
//G***del when works				final ResponseLID responseLID=(ResponseLID)response; //cast the response to a logical ID response
//G***del Debug.trace("adding response, material panel is: ", materialPanel); //G***del
			final QTIResponseLIDPanel responseLIDPanel=new QTIResponseLIDPanel(materialPanel, (ResponseLID)response);  //create a panel for the logical ID response
//G***del				responseLIDPanel.setResponseLID(responseLID); //set the response being shown in the panel
		  final int responseCount=responsePanel.getComponentCount();  //find out how many components are present already
				//add the response to the response panel at the appropriate location
			responsePanel.add(responseLIDPanel,  new GridBagConstraints(0, responseCount, 1, 1, 1.0, 1.0,
					GridBagConstraints.NORTH, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
		}
		//G***fix for other response types such as ResponseXY, ResponseStr, etc.
	}

	/**@return A list of responses, created from the current information being
		edited.
	*/
	protected java.util.List getResponses()
	{
		final Component[] responseComponents=responsePanel.getComponents(); //get an array of components being used to edit responses
		final java.util.List responseList=new ArrayList(responseComponents.length);  //create a list in which to store the responses, reserving enough room for all the responses
		for(int i=0; i<responseComponents.length; ++i)  //look at each of the components
		{
			final Component component=responseComponents[i];  //get a reference to this component
			if(component instanceof QTIResponseLIDPanel) //if this is a panel for a logical ID response
			{
				final QTIResponseLIDPanel responseLIDPanel=(QTIResponseLIDPanel)component;  //cast the component to a logical ID response panel
//G***del Debug.trace("Getting response lid with render choice with first response label: ", ((RenderChoice)responseLIDPanel.getResponseLID().getRenderList().get(0)).getResponseLabelList().get(0)); //G***del
				responseList.add(responseLIDPanel.getResponseLID());  //get the logical ID response and add it to the list
			}
			//G***add other response types
		}
		return responseList;  //return the list of responses
	}

	/**Retrieves the response panel representing the indexed response.
	@param responseIndex The index of the response the panel of which to retrieve.
	@return The panel used to represent the appropriate response.
	@exception IndexOutOfBoundsException Thrown if there is no QTI response panel to
		represent a response at the given index.
	*/
	JPanel getResponsePanel(int responseIndex) throws IndexOutOfBoundsException //G***maybe later add an abstract parent QTIResponsePanel to QTIResponseLIDPanel
	{
		final Component[] renderComponents=responsePanel.getComponents(); //get the components in the response panel
		for(int componentIndex=0; ; ++componentIndex) //look at each component, allowing an index out of bounds exception to be thrown if we run out of response panels
		{
			final Component component=renderComponents[componentIndex];  //get the current component
		  if(component instanceof JPanel) //if this is a response panel
			{
				if(responseIndex==0)  //if we've found the response at the correct index
				{
					return (JPanel)component; //return this response panel
				}
				else  //if we have more responses to go
				{
					--responseIndex;  //maybe the next response will be the correct one
				}
			}
		}
	}

}