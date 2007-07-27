package com.garretwilson.swing.text.rdf.maqro;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.swing.text.rdf.RDFStyleUtilities.*;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.*;

import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.text.*;

import com.garretwilson.io.URIAccessible;
import com.garretwilson.rdf.RDFObject;
import com.garretwilson.rdf.RDFResource;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.swing.text.ViewComponentManager;
import com.garretwilson.swing.text.xml.*;
import com.globalmentor.mentoract.activity.maqro.MAQROActivityPanel;

/**View representing a MAQRO question.
@author Garret Wilson
*/
public class MAQROQuestionView extends XMLComponentBlockView implements Outcomable
{

	/**The button group for the choices, if any.*/
	private final ButtonGroup buttonGroup=new ButtonGroup();

		/**@return The button group for the choices.*/
		public ButtonGroup getButtonGroup() {return buttonGroup;}

	/**The set of views representing choices.*/
	protected final Set<View> choiceViews=new HashSet<View>();
	
		/**Adds a view representing a choice of this question.*/
		public void addChoiceView(final View view) {choiceViews.add(view);}

	/**The button interface to the hint action, or <code>null</code> if there is no hint button.*/
	private final AbstractButton hintButton;

		/**@return The button interface to the hint action, or <code>null</code> if there is no hint button.*/
		protected AbstractButton getSubmitButton() {return hintButton;}

	/**@return The question stored in a element's attributes, or <code>null</code> if no question could be found.*/ 
	public Question getQuestion()
	{
		return asInstance(getRDFResource(getElement().getAttributes()), Question.class);	//get the question from the attributes 
	}

	/**Constructs a question rendering view.
	@param element The element this view is responsible for.
	@param axis The tiling axis, either <code>View.X_AXIS</code> or <code>View.Y_AXIS</code>.
	@param activityView The activityview with which this question is associated, or <code>null</code> if there is no associated activity rendering view.
	@exception IllegalArgumentException if the given element does not represent a question.
	*/
	public MAQROQuestionView(final Element element, final int axis, final MAQROActivityView activityView)
	{
		super(element, axis, false); //construct the parent class, but don't compensate for the hint button
		final Question question=getQuestion();	//get a reference to the associated question
		if(question==null)
		{
			throw new IllegalArgumentException("Element does not represent a question");
		}
		final List<RDFObject> hintList=question.getHints();	//get the question hints
		if(hintList!=null && hintList.size()>0	//if there are hints
				&& (activityView==null || activityView.getActivity().isAllowHint()))	//and if there is no associated activity, or the activity allows hints
		{
			hintButton=new JButton(new HintAction());	//create a button for a new hint action
			hintButton.setText("");	//TODO fix better; fix compensation for size of button and update MAQROActivityView
			hintButton.setBorderPainted(false);
			hintButton.setContentAreaFilled(false); //don't paint the background---let the button be transparent
//TODO del if not needed			hintButton.setOpaque(false);
			//TODO add a rollover border
			hintButton.setFocusable(false);	//TODO fix component manager focus traversal
				//place the button in the near inset of the tile axis and in the far inset of the perpendicular axis
			final ViewComponentManager.AxisLocation.Region regionX=axis==Y_AXIS ? ViewComponentManager.AxisLocation.Region.AFTER : ViewComponentManager.AxisLocation.Region.BEFORE; 
			final ViewComponentManager.AxisLocation.Region regionY=axis==X_AXIS ? ViewComponentManager.AxisLocation.Region.AFTER : ViewComponentManager.AxisLocation.Region.BEFORE; 
			getComponentManager().add(hintButton, regionX, 0.5f, regionY, 0.5f); //add the button to the question view
//TODO del when works			getComponentManager().add(hintButton, axis==Y_AXIS ? ViewComponentManager.Border.LINE_END : ViewComponentManager.Border.PAGE_END); //add the button to the question view
		}
		else	//if there are no hints
		{
			hintButton=null;	//don't create a hint button
		}
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

	/**Action for showing a hint.*/
	protected class HintAction extends AbstractAction
	{
		/**Default constructor.*/
		public HintAction()
		{
			super("Hint");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Show hint");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Show a hint.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_H));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.QUESTION_ICON_FILENAME)); //load the correct icon
		}

		/**Called when the action should be performed.
		@param actionEvent The event causing the action.
		*/
		public void actionPerformed(final ActionEvent actionEvent)
		{
			final Container container=getContainer();	//get the container in which this view is embedded
			final Document document=getDocument();	//get the document the view represents
			if(document instanceof URIAccessible)	//if the document allows access to streams TODO do better checks here, maybe using a default URIAccessible, or throwing an exception if the document is not URIAccessible
			{
				MAQROActivityPanel.showHints(getQuestion(), container, (URIAccessible)document);	//show the question hints
			}
		}
	}

}