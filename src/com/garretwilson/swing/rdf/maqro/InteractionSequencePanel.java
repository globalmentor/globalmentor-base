package com.garretwilson.swing.rdf.maqro;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.swing.*;

/**A component that displays a sequence of MAQRO interactions and allows them
	optionally to be edited.
@author Garret Wilson
*/
public class InteractionSequencePanel extends AbstractListComponentSequencePanel
{

	/**Default constructor.*/
	public InteractionSequencePanel()
	{
		this(new ArrayList());
	}

	/**List constructor.
	@param list The list the items of which the components in this sequence panel
		represent.
	*/
	public InteractionSequencePanel(final List list)
	{
		this(list, true); //construct and initialize the panel
	}

	/**Constructor that allows optional initialization.
	@param list The list the items of which the components in this sequence panel
		represent.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public InteractionSequencePanel(final List list, final boolean initialize)
	{
		this(list, true, true, initialize);	//consruct the panel with a toolbar and statusbar
	}

	/**Constructor that allows optional initialization.
	@param list The list the items of which the components in this sequence panel
		represent.
	@param hasToolBar Whether this panel should have a toolbar.
	@param hasStatusBar Whether this panel should have a status bar.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public InteractionSequencePanel(final List list, final boolean hasToolBar, final boolean hasStatusBar, final boolean initialize)
	{
		super(list, hasToolBar, hasStatusBar, false);	//construct the panel, but don't initialize
//TODO do something or initialize directly
		if(initialize)  //if we should initialize the panel
			initialize();   //initialize everything		
	}

	/**Initializes the user interface.*/
/*G***del if not needed
	protected void initializeUI()
	{
		super.initializeUI();	//do the default initialization
		setContentComponent(getFirstComponent());	//start with the first component in the sequence
	}
*/

	/**Returns a component appropriate for representing the given object from
		the list.
	@param object An object in the list.
	@return A component appropriate for representing the object.
	*/
	protected Component getComponent(final Object object)
	{
		if(object instanceof Question)	//if the object is a question
		{
			final QuestionModel questionModel=new QuestionModel((Question)object);	//create a new model for the question TODO add the base URI and URIInputStreamable
			return new QuestionPanel(questionModel);	//create a new panel for the question
		}
		return new JLabel("Unrecognized interaction");	//TODO fix unrecognized interaction
	}
}
