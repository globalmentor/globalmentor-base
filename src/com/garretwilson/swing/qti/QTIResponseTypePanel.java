package com.garretwilson.swing.qti;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.assess.qti.*;
import com.garretwilson.util.*;
import com.globalmentor.java.CharSequences;
import com.globalmentor.java.Strings;

/**Allows a response type to be selected.
@author Garret Wilson
*/
public class QTIResponseTypePanel extends JPanel implements Verifiable
{
		//response category
	/**Indicates a template response category was selected.*/
	public final static int TEMPLATE_CATEGORY=0;
	/**Indicates a general response category was selected.*/
	public final static int GENERAL_CATEGORY=1;

		//response types -- template
	/**Indicates that the true/false template type was selected.*/
	public final static String TF_TEMPLATE_RESPONSE_TYPE="tfTemplateType";
	/**Indicates that the multiple choice template type was selected.*/
	public final static String MC_TEMPLATE_RESPONSE_TYPE="mcTemplateType";
	/**Indicates that the hotspot choice template type was selected.*/
	public final static String HOTSPOT_CHOICE_TEMPLATE_RESPONSE_TYPE="hotspotChoiceTemplateType";
		//response types -- general
	/**Indicates that the logical ID response type was selected.*/
	public final static String LID_RESPONSE_TYPE="response_lid";
	/**Indicates that the XY response type was selected.*/
	public final static String XY_RESPONSE_TYPE="response_xy";
	/**Indicates that the string response type was selected.*/
	public final static String STRING_RESPONSE_TYPE="response_str";
	/**Indicates that the number response type was selected.*/
	public final static String NUMBER_RESPONSE_TYPE="response_num";
	/**Indicates that the group response type was selected.*/
	public final static String GROUP_RESPONSE_TYPE="response_grp";

		//render types
	/**Indicates that the choice render type was selected.*/
	public final static String CHOICE_RENDER_TYPE="render_choice";
	/**Indicates that the hotspot render type was selected.*/
	public final static String HOTSPOT_RENDER_TYPE="render_hotspot";
	/**Indicates that the slider render type was selected.*/
	public final static String SLIDER_RENDER_TYPE="render_slider";
	/**Indicates that the fill-in-the-blank render type was selected.*/
	public final static String FIB_RENDER_TYPE="render_fib";

	/**An action listener just for updating the display.*/
	private final ActionListener updateDisplayActionListener=new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        updateDisplay();
      }
    };
  BorderLayout backBorderLayout = new BorderLayout();
  JTabbedPane tabbedPane = new JTabbedPane();
  JPanel templatePanel = new JPanel();
  JPanel generalPanel = new JPanel();
  GridBagLayout templateGridBagLayout = new GridBagLayout();
  JRadioButton tfRadioButton = new JRadioButton();
  JRadioButton mcRadioButton = new JRadioButton();
  JRadioButton hotspotChoiceRadioButton = new JRadioButton();
  GridBagLayout generalGridBagLayout = new GridBagLayout();
  JLabel responseTypeLabel = new JLabel();
  JLabel renderTypeLabel = new JLabel();
  ButtonGroup responseTypeButtonGroup = new ButtonGroup();
  JRadioButton lidRadioButton = new JRadioButton();
  JRadioButton xyRadioButton = new JRadioButton();
  JRadioButton stringRadioButton = new JRadioButton();
  JRadioButton numberRadioButton = new JRadioButton();
  JRadioButton groupRadioButton = new JRadioButton();
  ButtonGroup renderTypeButtonGroup = new ButtonGroup();
  ButtonGroup templateButtonGroup = new ButtonGroup();
  JRadioButton choiceRadioButton = new JRadioButton();
  JRadioButton hotspotRadioButton = new JRadioButton();
  JRadioButton sliderRadioButton = new JRadioButton();
  JRadioButton fibRadioButton = new JRadioButton();
  JButton fibButton = new JButton();
  JButton sliderButton = new JButton();
  JButton hotspotButton = new JButton();
  JButton choiceButton = new JButton();
  JButton lidButton = new JButton();
  JButton xyButton = new JButton();
  JButton stringButton = new JButton();
  JButton numberButton = new JButton();
  JButton groupButton = new JButton();
  JButton tfButton = new JButton();
  JButton mcButton = new JButton();
  JButton hotspotChoiceButton = new JButton();
  JTextField numChoicesTextField = new JTextField();
  JLabel numChoicesLabel = new JLabel();

	/**Default constructor.*/
	public QTIResponseTypePanel()
	{
		jbInit();
		updateDisplay();  //update the user interface
	}

	/**Initializes the panel.*/
  private void jbInit()
  {
    this.setLayout(backBorderLayout);
    templatePanel.setLayout(templateGridBagLayout);
    tfRadioButton.setActionCommand(TF_TEMPLATE_RESPONSE_TYPE);
    tfRadioButton.setText("True/False");
		tfButton.setBorderPainted(false);
    tfButton.setIcon(IconResources.getIcon(IconResources.BOOLEAN_ICON_FILENAME));
		tfButton.setModel(tfRadioButton.getModel());
    mcRadioButton.setActionCommand(MC_TEMPLATE_RESPONSE_TYPE);
    mcRadioButton.setText("Multiple Choice");
		mcButton.setBorderPainted(false);
    mcButton.setIcon(IconResources.getIcon(IconResources.CHOICE_ICON_FILENAME));
		mcButton.setModel(mcRadioButton.getModel());
    hotspotChoiceRadioButton.setActionCommand(HOTSPOT_CHOICE_TEMPLATE_RESPONSE_TYPE);
    hotspotChoiceRadioButton.setText("Hotspot Choice");
		hotspotChoiceButton.setBorderPainted(false);
    hotspotChoiceButton.setIcon(IconResources.getIcon(IconResources.HOT_SPOT_ICON_FILENAME));
		hotspotChoiceButton.setModel(hotspotChoiceRadioButton.getModel());
    generalPanel.setLayout(generalGridBagLayout);
    responseTypeLabel.setFont(new java.awt.Font("Dialog", 1, 12));
    responseTypeLabel.setText("Response Type");
    renderTypeLabel.setFont(new java.awt.Font("Dialog", 1, 12));
    renderTypeLabel.setText("Render Type");
    lidRadioButton.setActionCommand(LID_RESPONSE_TYPE);
    lidRadioButton.setText("Logical ID");
    lidRadioButton.addActionListener(updateDisplayActionListener);
		lidButton.setBorderPainted(false);
    lidButton.setIcon(IconResources.getIcon(IconResources.PROPERTY_ICON_FILENAME));
		lidButton.setModel(lidRadioButton.getModel());
    xyRadioButton.setActionCommand(XY_RESPONSE_TYPE);
    xyRadioButton.setText("XY");
    xyRadioButton.addActionListener(updateDisplayActionListener);
		xyButton.setBorderPainted(false);
    xyButton.setIcon(IconResources.getIcon(IconResources.GRAPH_POINTS_ICON_FILENAME));
		xyButton.setModel(xyRadioButton.getModel());
    stringRadioButton.setActionCommand(STRING_RESPONSE_TYPE);
    stringRadioButton.setText("String");
    stringRadioButton.addActionListener(updateDisplayActionListener);
		stringButton.setBorderPainted(false);
    stringButton.setIcon(IconResources.getIcon(IconResources.STRING_ICON_FILENAME));
		stringButton.setModel(stringRadioButton.getModel());
    numberRadioButton.setActionCommand(NUMBER_RESPONSE_TYPE);
    numberRadioButton.setText("Number");
    numberRadioButton.addActionListener(updateDisplayActionListener);
		numberButton.setBorderPainted(false);
    numberButton.setIcon(IconResources.getIcon(IconResources.NUMBER_ICON_FILENAME));
		numberButton.setModel(numberRadioButton.getModel());
    groupRadioButton.setActionCommand(GROUP_RESPONSE_TYPE);
    groupRadioButton.setText("Group");
    groupRadioButton.addActionListener(updateDisplayActionListener);
		groupButton.setBorderPainted(false);
    groupButton.setIcon(IconResources.getIcon(IconResources.GROUP_ICON_FILENAME));
		groupButton.setModel(groupRadioButton.getModel());
    choiceRadioButton.setActionCommand(CHOICE_RENDER_TYPE);
	  choiceRadioButton.setText("Choice");
    choiceRadioButton.addActionListener(updateDisplayActionListener);
		choiceButton.setBorderPainted(false);
    choiceButton.setIcon(IconResources.getIcon(IconResources.CHOICE_ICON_FILENAME));
		choiceButton.setModel(choiceRadioButton.getModel());
    hotspotRadioButton.setActionCommand(HOTSPOT_RENDER_TYPE);
    hotspotRadioButton.setText("Hotspot");
    hotspotRadioButton.addActionListener(updateDisplayActionListener);
		hotspotButton.setBorderPainted(false);
    hotspotButton.setIcon(IconResources.getIcon(IconResources.HOT_SPOT_ICON_FILENAME));
		hotspotButton.setModel(hotspotRadioButton.getModel());
    sliderRadioButton.setActionCommand(SLIDER_RENDER_TYPE);
    sliderRadioButton.setText("Slider");
    sliderRadioButton.addActionListener(updateDisplayActionListener);
		sliderButton.setBorderPainted(false);
    sliderButton.setIcon(IconResources.getIcon(IconResources.SLIDER_ICON_FILENAME));
		sliderButton.setModel(sliderRadioButton.getModel());
    fibRadioButton.setActionCommand(FIB_RENDER_TYPE);
    fibRadioButton.setText("Fill-in-the-blank");
    fibRadioButton.addActionListener(updateDisplayActionListener);
		fibButton.setBorderPainted(false);
    fibButton.setIcon(IconResources.getIcon(IconResources.STRING_EDIT_ICON_FILENAME));
		fibButton.setModel(fibRadioButton.getModel());
    numChoicesTextField.setText("4");
    numChoicesTextField.setColumns(3);
    numChoicesLabel.setText("Number of Choices:");
    this.add(tabbedPane, BorderLayout.CENTER);
    tabbedPane.add(templatePanel,    "Templates");
    tabbedPane.add(generalPanel,   "General");
    templatePanel.add(tfRadioButton,    new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    templatePanel.add(mcRadioButton,    new GridBagConstraints(1, 1, 2, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    templatePanel.add(tfButton,  new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(responseTypeLabel,     new GridBagConstraints(0, 0, 2, 1, 0.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 32, 0));
    generalPanel.add(renderTypeLabel,     new GridBagConstraints(2, 0, 2, 1, 0.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(lidRadioButton,   new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    responseTypeButtonGroup.add(lidRadioButton);
    responseTypeButtonGroup.add(xyRadioButton);
    responseTypeButtonGroup.add(stringRadioButton);
    responseTypeButtonGroup.add(numberRadioButton);
    responseTypeButtonGroup.add(groupRadioButton);
    generalPanel.add(xyRadioButton,   new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(stringRadioButton,    new GridBagConstraints(1, 3, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(numberRadioButton,    new GridBagConstraints(1, 4, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(groupRadioButton,    new GridBagConstraints(1, 5, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(choiceRadioButton,   new GridBagConstraints(3, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(hotspotRadioButton,   new GridBagConstraints(3, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(sliderRadioButton,    new GridBagConstraints(3, 3, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(fibRadioButton,   new GridBagConstraints(3, 4, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(fibButton,  new GridBagConstraints(2, 4, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(sliderButton,  new GridBagConstraints(2, 3, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    templateButtonGroup.add(tfRadioButton);
    templateButtonGroup.add(mcRadioButton);
    templateButtonGroup.add(hotspotChoiceRadioButton);
    renderTypeButtonGroup.add(fibRadioButton);
    renderTypeButtonGroup.add(sliderRadioButton);
    renderTypeButtonGroup.add(hotspotRadioButton);
    renderTypeButtonGroup.add(choiceRadioButton);
    generalPanel.add(hotspotButton,  new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(choiceButton,  new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(lidButton,   new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(xyButton,   new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(stringButton,   new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(numberButton,   new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    generalPanel.add(groupButton,   new GridBagConstraints(0, 5, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    templatePanel.add(mcButton,  new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    templatePanel.add(numChoicesTextField,       new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 0), 0, 0));
    templatePanel.add(numChoicesLabel,    new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 16, 0, 0), 0, 0));
    templatePanel.add(hotspotChoiceRadioButton,   new GridBagConstraints(1, 3, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
    templatePanel.add(hotspotChoiceButton,  new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0
            ,GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
  }

	/**Updates the state of the user interface components.*/
	protected void updateDisplay()
	{
			//see what type of response is selected
		if(lidRadioButton.isSelected()) //if the logical ID response type is selected
		{
			choiceRadioButton.setEnabled(true); //enable choice rendering
			hotspotRadioButton.setEnabled(true); //enable hotspot rendering
			sliderRadioButton.setEnabled(false); //disable slider rendering
			sliderRadioButton.setSelected(false);
			fibRadioButton.setEnabled(false); //disable fill-in-the-blank rendering
			fibRadioButton.setSelected(false);
		}
		else  //G***fix other response types
		{
			choiceRadioButton.setEnabled(false); //disable choice rendering
			choiceRadioButton.setSelected(false);
			hotspotRadioButton.setEnabled(false); //disable hotspot rendering
			hotspotRadioButton.setSelected(false);
			sliderRadioButton.setEnabled(false); //disable choice rendering
			sliderRadioButton.setSelected(false);
			fibRadioButton.setEnabled(false); //disable fill-in-the-blank rendering
			fibRadioButton.setSelected(false);
		}
	}

	/**@return The category of response type chosen; one of the
		<code>XXX_CATEGORY</code> constants.
	*/
	public int getResponseTypeCategory()
	{
		return tabbedPane.getSelectedIndex(); //return the selected pane index
	}

	/**@return The response type chosen; one of the
		<code>XXX_TEMPLATE_RESPONSE_TYPE</code> constants, one of the
		<code>XXX_RESPONSE_TYPE</code> constants, or <code>null</code> if no type
		was selected.
	*/
	public String getResponseType()
	{
		final ButtonModel selectedButtonModel;  //we'll get the selected button model based upon whether a template or general type was selected
		switch(getResponseTypeCategory()) //see which response category was chosen
		{
			case TEMPLATE_CATEGORY: //if one of the template categories was selected
				selectedButtonModel=templateButtonGroup.getSelection(); //get the selected template type
				break;
			case GENERAL_CATEGORY: //if one of the general categories were selected
				selectedButtonModel=responseTypeButtonGroup.getSelection(); //get the selected general type
				break;
			default:  //if neither category was selected (which should never happen)
				selectedButtonModel=null; //show that nothing was selected
				break;
		}
		return selectedButtonModel!=null ? selectedButtonModel.getActionCommand() : null; //return the action command or null if none is selected
	}

	/**@return The render type chosen; one of the
		<code>XXX_RENDER_TYPE</code> constants, or <code>null</code> if no render
		type was selected.
	*/
	public String getRenderType()
	{
		final ButtonModel selectedButtonModel=renderTypeButtonGroup.getSelection(); //get the selected render type
		return selectedButtonModel!=null ? selectedButtonModel.getActionCommand() : null; //return the action command or null if none is selected
	}

	/**@return The number of choices requested for a multiple choice question.
	@exception NumberFormatException Thrown if the multiple choice number of
		choices text field does not contain a parsable integer.
	*/
	public int getNumChoices() throws NumberFormatException
	{
		return Integer.parseInt(numChoicesTextField.getText()); //convert the number to a string and return it
	}

	/**Verifies the component.
	@return <code>true</code> if the component contents are valid, <code>false</code>
		if not.
	*/
	public boolean verify()
	{
		final String responseType=getResponseType();  //get the response type
		if(responseType==null)  //if there is no response type
		{
			JOptionPane.showMessageDialog(this, "A response type must be specified.", "Missing response type", JOptionPane.ERROR_MESSAGE);	//G***i18n
			return false;
		}
		final int responseTypeCategory=getResponseTypeCategory(); //get the category of response type
		final String renderType=getRenderType();  //get the render type
		switch(responseTypeCategory) //see which response category was chosen
		{
			case TEMPLATE_CATEGORY: //if one of the template categories was selected
				if(MC_TEMPLATE_RESPONSE_TYPE.equals(responseType))  //if multiple choice was selected
				{
							//make sure a valid number of choices is listed
					if(numChoicesTextField.getText().length()==0 || !CharSequences.isLatinDigits(numChoicesTextField.getText()))
					{
		  			JOptionPane.showMessageDialog(this, "A valid number of choices must be specified.", "Invalid number of choices", JOptionPane.ERROR_MESSAGE);	//G***i18n
						numChoicesTextField.requestFocus(); //focus on the number of choices text field
						return false;
					}
				}
				break;
			case GENERAL_CATEGORY: //if one of the general categories were selected
				if(renderType==null)  //if there is no render type
				{
					JOptionPane.showMessageDialog(this, "A rendering type must be specified.", "Missing render type", JOptionPane.ERROR_MESSAGE);	//G***i18n
					return false;
				}
				break;
			default:  //if neither category was selected (which should never happen)
				return false;
		}
		return true;  //if we couldn't find any problems, verification succeeded
//	TODO change this panel to inherit from BasicPanel, and verify the parent class (removing the implements Verifiable)		return super.verify();  //if we couldn't find any problems, verify the parent class
	}

	/**Creates and returns the appropriate default response object from the given
		information in the panel.
	@return The default response based upon the information in the panel.
	*/
	public Response createResponse()  //G**this would ideally go elsewhere, maybe even in a utilities class
	{
			//G***we probably want to make sure we set default idents for responses and such
//G***del		final QTIResponseTypePanel responseTypePanel=new QTIResponseTypePanel();  //create a new response type panel
		final Response response;  //we'll create the appropriate response and store it in this variable
		final String responseType=getResponseType();  //see what type of response type is requested
		final String renderType=getRenderType();  //see what type of rendering was requested
		if(TF_TEMPLATE_RESPONSE_TYPE.equals(responseType))  //true/false
		{
			final RenderChoice renderChoice=new RenderChoice(); //create a render choice
				//create a response label for "false"
			renderChoice.getResponseLabelList().add(new ResponseLabel(String.valueOf(Boolean.FALSE), "False"));  //G***i18n
				//create a response label for "true"
			renderChoice.getResponseLabelList().add(new ResponseLabel(String.valueOf(Boolean.TRUE), "True"));  //G***i18n
			final ResponseLID responseLID=new ResponseLID(renderChoice); //create a new logical ID response with the choice rendering we created
			response=responseLID;  //show which response we created
		}
		else if(MC_TEMPLATE_RESPONSE_TYPE.equals(responseType))  //multiple choice
		{
			final RenderChoice renderChoice=new RenderChoice(); //create a render choice
			final int numChoices=getNumChoices(); //find out how many choices are requested
			for(int i=0; i<numChoices; ++i) //create each choice
			{
				renderChoice.getResponseLabelList().add(new ResponseLabel(String.valueOf((char)('A'+i)), ""));  //create this choice G***i18n
			}
			final ResponseLID responseLID=new ResponseLID(renderChoice); //create a new logical ID response with the choice rendering we created
			response=responseLID;  //show which response we created
		}
		else if(HOTSPOT_CHOICE_TEMPLATE_RESPONSE_TYPE.equals(responseType))  //hotspot choice
		{
			final RenderHotspot renderHotspot=new RenderHotspot(); //create a hotspot rendering
/*G***del if not needed
			final int numChoices=getNumChoices(); //find out how many choices are requested
			for(int i=0; i<numChoices; ++i) //create each choice
			{
				renderChoice.getResponseLabelList().add(new ResponseLabel(String.valueOf((char)('A'+i)), ""));  //create this choice G***i18n
			}
*/
			final ResponseLID responseLID=new ResponseLID(renderHotspot); //create a new logical ID response with the hotspot rendering we created
			response=responseLID;  //show which response we created
		}
		else if(LID_RESPONSE_TYPE.equals(responseType))  //logical ID
		{
			final Render render;  //we'll create a rendering and store it here, if we can
			if(CHOICE_RENDER_TYPE.equals(renderType)) //choice rendering
			{
				render=new RenderChoice(); //create a render choice
			}
			else if(HOTSPOT_RENDER_TYPE.equals(renderType)) //hotspot rendering
			{
				render=new RenderHotspot(); //create a render hotspot
			}
/*G***fix
			else if(SLIDER_RENDER_TYPE.equals(renderType)) //slider rendering
			{
				render=new RenderSlider(); //create a render slider
			}
*/
			else  //if we don't recognize the render type
				render=null;  //don't create a render
					//create a new logical ID response with our rendering, if we created a rendering
			response=render!=null ? new ResponseLID(render) :  null;
		}
		else  //if we don't recognize the response type
		{
			throw new AssertionError("Unrecognized response type"); //this should never happen once we add conditions for all the choices
		}
		return response;  //return the type of response requested
	}

  void lidRadioButton_actionPerformed(ActionEvent e)
  {
		updateDisplay();  //update the user interface
  }

}