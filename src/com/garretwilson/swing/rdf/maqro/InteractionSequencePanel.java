package com.garretwilson.swing.rdf.maqro;

import java.awt.*;
import javax.swing.*;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.swing.*;

/**A component that displays a sequence of MAQRO interactions and allows them
	optionally to be edited.
@author Garret Wilson
*/
public class InteractionSequencePanel extends AbstractListModelComponentSequencePanel
{

	/**Default constructor.*/
	public InteractionSequencePanel()
	{
		this(null);	//initialize without a list of interactions
	}

	/**List constructor.
	@param listModel The list the items of which the components in this
		sequence panel represent, or <code>null</code> for no list
	*/
	public InteractionSequencePanel(final ListModel listModel)
	{
		this(listModel, true); //construct and initialize the panel
	}

	/**Constructor that allows optional initialization.
	@param listModel The list the items of which the components in this
		sequence panel represent, or <code>null</code> for no list
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public InteractionSequencePanel(final ListModel listModel, final boolean initialize)
	{
		this(listModel, true, true, initialize);	//consruct the panel with a toolbar and statusbar
	}

	/**Constructor that allows optional initialization.
	@param listModel The list the items of which the components in this
		sequence panel represent, or <code>null</code> for no list
	@param hasToolBar Whether this panel should have a toolbar.
	@param hasStatusBar Whether this panel should have a status bar.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public InteractionSequencePanel(final ListModel listModel, final boolean hasToolBar, final boolean hasStatusBar, final boolean initialize)
	{
		super(listModel, hasToolBar, hasStatusBar, false);	//construct the panel, but don't initialize
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

	/**Sent after the indices in the index0, index1
		interval have been inserted in the data model.
		The new interval includes both index0 and index1.
	@param index0 The lower index, inclusive, of the range.
	@param index1 The upper index, inclusive, of the range.
	*/
	protected void onIntervalAdded(final int index0, final int index1)
	{
		go(index0);	//go to the indicated index, which will update our status
	}

}
