package com.garretwilson.swing.rdf.maqro;

import java.awt.*;
import java.awt.event.*;
import java.net.*;
import javax.swing.*;
import com.garretwilson.model.Resource;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.rdf.xmlschema.BooleanLiteral;
import com.garretwilson.swing.*;
import com.garretwilson.text.xml.schema.XMLSchemaConstants;
import com.garretwilson.util.Debug;

/**Panel for editing the answer of a MAQRO question.
@author Garret Wilson
@see QuestionPanel
*/
public class AnswerPanel extends ContentPanel
{

	/**Example expected types for the combo box.*/
	protected final static RDFResource[] TYPE_EXAMPLES=new RDFResource[]
			{new DefaultRDFResource(XMLSchemaConstants.BOOLEAN_DATATYPE_URI)};

	final JLabel typeLabel;
	final JComboBox typeComboBox;
	final JLabel valueLabel;

	/**Sets whether or not this component is enabled.
		This implementation enables or disables child components appropriately.
	@param enabled <code>true</code> if this component should be enabled,
		<code>false</code> otherwise.
	*/
	public void setEnabled(final boolean enabled)
	{
		super.setEnabled(enabled);	//do the default enabling
		typeComboBox.setEnabled(enabled);	//enable or disable the type combo box
		getContentComponent().setEnabled(enabled);	//enable or disable the content component, representing the answer
	}

	/**Default constructor.*/
	public AnswerPanel()
	{
		super(new JPanel(), new GridBagConstraints(1, 1, 1, 1, 1.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, NO_INSETS, 0, 0), false);	//construct the parent class but don't initialize it
		typeLabel=new JLabel();
		typeComboBox=new JComboBox();
		valueLabel=new JLabel();
		initialize();   //initialize the panel
	}

	/**Initialize the user interface.*/
	protected void initializeUI()
	{
		super.initializeUI(); //do the default UI initialization
		typeLabel.setText("Type");	//G***i18n
		typeComboBox.setEditable(true);
		typeComboBox.setModel(new DefaultComboBoxModel(TYPE_EXAMPLES));	//set up the example type resources
//G***del		typeComboBox.setPrototypeDisplayValue(TYPE_EXAMPLES[0]);
		typeComboBox.addItemListener(getModifyItemListener());
		typeComboBox.addActionListener(new ActionListener()
				{
					public void actionPerformed(final ActionEvent actionEvent)	//if a different type is selected
					{
						final Object typeObject=typeComboBox.getSelectedItem();	//get the new selected item
						final RDFResource typeResource;	//well determine a resource for this type
						if(typeObject!=null && typeObject.toString().trim().length()>0)	//if something that wasn't nothing was selected
						{
							try
							{
								typeResource=getTypeRDFResource(typeObject);	//turn the type into an RDF resource
							}
							catch(final URISyntaxException uriSyntaxException)	//if the user entered a syntactically incorrect URI
							{
								SwingApplication.displayApplicationError(AnswerPanel.this, uriSyntaxException);	//show the error
								typeComboBox.requestFocusInWindow();	//put the focus back on the combo box
								return;	//do no further processing
							}
						}
						else	//if there is nothing in the type combo box
						{
							typeResource=null;	//use no type at all
						}
						updateContentComponent(typeResource);	//update the content component to reflect the new selected type
					}
				});
		valueLabel.setText("Value");	//G***i18n
		add(typeLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		add(typeComboBox, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, NO_INSETS, 0, 0));
		add(valueLabel, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
	}

	/**Updates the states of the actions, including enabled/disabled status,
		proxied actions, etc.
	*/
/*G***del if not needed
	protected void updateStatus()
	{
		super.updateStatus();	//do the default updating
		choiceList.setEnabled(choicesRadioButton.isSelected());	//only enable the choice list if the choices radio button is selected
//	G***fix for expectation		choiceList.setEnabled(choicesRadioButton.isSelected());	//only enable the choice list if the choices radio button is selected
	}
*/

	/**Sets the expected answer type.
	@param expectedType A resource that indicates the expected type, such as a
		resource with the reference URI
		<code>http://www.w3.org/2001/XMLSchema#boolean</code>, or <code>null</code>
		if there is no expected type.
	*/
	public void setExpectation(final Resource expectedType)
	{
		typeComboBox.setSelectedItem(expectedType);	//show the expected type in the combo box
	}

	/**@return A resource the reference URI of which indicates the expected
		answer type.
	*/
	public RDFResource getExpectation()
	{
		final Object typeObject=typeComboBox.getSelectedItem();	//get the selected item
		if(typeObject!=null && typeObject.toString().trim().length()>0)	//if something that wasn't nothing was selected
		{
			try
			{
				return getTypeRDFResource(typeObject);	//turn the type into an RDF resource
			}
			catch(final URISyntaxException uriSyntaxException)	//if the user entered a syntactically incorrect URI
			{
				Debug.warn(uriSyntaxException);	//we shouldn't get an error here if the panel was already verified, as the user interface should do
			}
		}
		return null;	//return no expected resource
	}

	/**Sets the correct answer, if there is one.
	<p>The expected type should already have been set to the type identical to
		that of the <code>rdf:value</code> of the answer.</p>
	@param answer The dialogue the <code>rdf:value</code> of which indicates
		the correct answer, or <code>null</code> if there is no answer.
	@see #getExpectation()
	*/
	public void setAnswer(final Dialogue answer)
	{
		final RDFResource expectation=getExpectation();	//see what we expect
		if(expectation!=null && answer!=null)	//if we expect a type
		{
			final RDFLiteral answerValue=answer.getValue();	//get the value
			if(answerValue instanceof RDFTypedLiteral)	//if the answer value is a typed literal
			{
				final RDFTypedLiteral answerTypedLiteral=(RDFTypedLiteral)answerValue;	//get the typed literal form of the answer value
				final URI datatypeURI=answerTypedLiteral.getDatatypeURI();	//get the typed literal datatype
				if(datatypeURI.equals(expectation.getReferenceURI()))	//if this answer has a datatype that we expect
				{
					if(answerTypedLiteral instanceof BooleanLiteral)	//if this is a boolean literal
					{
						((BooleanPanel)getContentComponent()).setValue(((BooleanLiteral)answerTypedLiteral).getBoolean());	//set the value of the boolean panel
					}
				}
				//G***fix for unexpected types else--put the lexical form in a generic text field
			}
		}
/*G***fix or del
		else if(answer!=null)	//if we don't expect a type but an answer is given
		{
			
		}
			//assert that the expectation already matches the datatype G***should we throw an illegal
		assert answer==null || !(answer.getValue() instanceof RDFTypedLiteral) || getExpectation()==null || ((RDFTypedLiteral)answer.getValue()).getDatatypeURI().equals(getExpectation().getReferenceURI());
*/
	}

	/**@return A resource the <code>rdf:value</code> of which indicates the
		answer provided by the user, or <code>null</code> if no answer is indicated.
	*/
	public Dialogue getAnswer()
	{
		final RDFLiteral value;	//we'll determine the literal value to use, if any
		final Component answerComponent=getContentComponent();	//get the answer component
		if(answerComponent instanceof BooleanPanel)	//if this is a boolean panel
		{
			final Boolean booleanValue=((BooleanPanel)answerComponent).getValue();	//get the boolean value from the panel
			value=booleanValue!=null ? new BooleanLiteral(booleanValue.booleanValue()) : null;	//create a boolean literal with the given answer, if there is an answer
		}
		else	//if we don't recognize the answer component
		{
			value=null;	//we can't have an answer
		}
		if(value!=null)	//if we have an answer value
		{
			final Dialogue answer=new Dialogue();	//create a new answer
			answer.setValue(value);	//set the value of the answer
			return answer;	//return the answer
		}
		else	//if there is no answer value
		{
			return null;	//show that there is no answer
		}
	}

	/**Determines a resource appropriate for representing the indicated type object.
	@param typeObject A resource that indicates the  type, such as a resource
		with the reference URI <code>http://www.w3.org/2001/XMLSchema#boolean</code>,
		or a <code>String</code> or <code>URI</code> indicating the reference URI
		of the expected type.
	@return The RDF resource, if one was provided, or an RDF resource created
		from the given resource reference URI or reference URI string.
	@exception URISyntaxException Thrown if the URI given for the type is not
		syntactically correct.
	@see RDFResource
	@see Resource
	@see URI
	@see String
	*/
	protected RDFResource getTypeRDFResource(final Object typeObject) throws URISyntaxException
	{
		if(typeObject instanceof RDFResource)	//if an RDF resource was passed
		{
			return ((RDFResource)typeObject);	//return the resource
		}
		else if(typeObject instanceof Resource)	//if a normal resource was passed
		{
			return getTypeRDFResource(((Resource)typeObject).getReferenceURI());	//get the type resource from the resource reference URI
		}
		else if(typeObject instanceof URI)	//if a URI was passed
		{
			return new DefaultRDFResource((URI)typeObject);	//return a default resource from the URI
		}
		else if(typeObject instanceof String)	//if a string was passed, assume it's representing a URI
		{
			return getTypeRDFResource(new URI((String)typeObject));	//create a URI from the string and create a resource from that
		}
		throw new IllegalArgumentException("Unrecognized type object: "+typeObject.getClass());	//show that we don't understand the object representing the type
	}

	/**Updates the content component to allow entry for the given expected type.
	@param expectedType A resource that indicates the expected type, such as a
		resource with the reference URI
		<code>http://www.w3.org/2001/XMLSchema#boolean</code>, or <code>null</code>
		if there is no expected type.
	*/
	protected void updateContentComponent(final Resource expectedType)
	{
		setContentComponent(getComponent(expectedType));	//update the content component using a component specific to the given type
	}

	/**Gets a component to represent the given type.
	@param type A resource that indicates the type, such as a resource with the
		reference URI <code>http://www.w3.org/2001/XMLSchema#boolean</code>, or
		<code>null</code> if there is no type.
	@return The component to represent the given type.
	*/
	protected Component getComponent(final Resource type)
	{
		if(type!=null)	//if a type was given
		{
			if(XMLSchemaConstants.BOOLEAN_DATATYPE_URI.equals(type.getReferenceURI()))	//if this is the boolean type
			{
				return new BooleanPanel(BooleanPanel.HORIZONTAL);	//use a boolean panel for the content component
			}
		}
		return new JPanel();	//if we don't recognize the expected type, or there was no type given, return a default panel G***fix
	}

	/**Verifies the component.
	@return <code>true</code> if the component contents are valid, <code>false</code>
		if not.
	*/
	public boolean verify()
	{
		final Object typeObject=typeComboBox.getSelectedItem();	//get the selected item
		if(typeObject!=null && typeObject.toString().trim().length()>0)	//if something that wasn't nothing was selected
		{
			try
			{
				getTypeRDFResource(typeObject);	//turn the type into an RDF resource
			}
			catch(final URISyntaxException uriSyntaxException)	//if the user entered a syntactically incorrect URI
			{
				SwingApplication.displayApplicationError(AnswerPanel.this, uriSyntaxException);	//show the error
				typeComboBox.requestFocusInWindow();	//put the focus back on the combo box
				return false;	//show that the URI was incorrect
			}
		}
		return super.verify();  //if we couldn't find any problems, verify the parent class
	}

}
