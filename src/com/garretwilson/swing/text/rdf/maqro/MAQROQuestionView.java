package com.garretwilson.swing.text.rdf.maqro;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.swing.text.rdf.RDFStyleUtilities.*;

import java.util.*;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.text.*;

import com.garretwilson.rdf.RDFResource;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.swing.text.xml.*;

/**View representing a MAQRO question.
@author Garret Wilson
*/
public class MAQROQuestionView extends XMLBlockView implements Outcomable
{

	/**The button group for the choices, if any.*/
	private final ButtonGroup buttonGroup=new ButtonGroup();

		/**@return The button group for the choices.*/
		public ButtonGroup getButtonGroup() {return buttonGroup;}

	/**The set of views representing choices.*/
	protected final Set<View> choiceViews=new HashSet<View>();
	
		/**Adds a view representing a choice of this question.*/
		public void addChoiceView(final View view) {choiceViews.add(view);}

	/**@return The question stored in a element's attributes, or <code>null</code> if no question could be found.*/ 
	public Question getQuestion()
	{
		return asInstance(getRDFResource(getElement().getAttributes()), Question.class);	//get the question from the attributes 
	}

	/**Constructs a question rendering view.
	@param element The element this view is responsible for.
	@param axis The tiling axis, either <code>View.X_AXIS</code> or <code>View.Y_AXIS</code>.
	*/
	public MAQROQuestionView(final Element element, final int axis)
	{
		super(element, axis, true, true); //construct the parent, allowing expansion in both direction
	}

	/**Retrieves user response information for the associated interaction and returns the outcome information.
	@return An object representing the current state of user responses and outcome for the current interaction,
		or <code>null</code> if no results are available for the associated interaction.
	*/
	public Outcome getOutcome()
	{
		final Outcome result=new Outcome(getQuestion());	//create a result for the question
		for(final View view:choiceViews)	//for all of our choices, if any
		{
			if(view instanceof MAQRODialogueChoiceView)	//if this is a MAQRO dialogue choice view TODO make this more general for non-dialogue choices
			{
				final MAQRODialogueChoiceView choiceView=(MAQRODialogueChoiceView)view;	//get the view as a choice view
				final RDFResource choice=choiceView.getChoice();	//get the associated choice
				final AbstractButton button=choiceView.getToggleButton();	//get the button
				if(button.isSelected())	//if this button is selected
				{
					result.addResponse(choice);	//add this choice as a response to the result
				}				
			}
		}
		return result;	//return the result
	}

}