package com.garretwilson.swing.rdf.maqro;

import java.awt.GridBagConstraints;
import java.io.IOException;
import javax.swing.*;
import com.garretwilson.awt.BasicGridBagLayout;
import com.garretwilson.rdf.maqro.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.swing.*;
import com.garretwilson.swing.border.BorderUtilities;
import com.garretwilson.text.xml.XMLDocumentFragmentModel;
import com.garretwilson.text.xml.xhtml.XHTMLConstants;

/**Panel for editing a MAQRO question.
@author Garret Wilson
*/
public class QuestionPanel extends TabbedViewPanel
{
	/**The view in which the query and choices and/or answers are shown.*/
	public final static int QUERY_MODEL_VIEW=-1;

	/**The default model views supported by this panel.*/
	private final int[] DEFAULT_SUPPORTED_MODEL_VIEWS=new int[]{QUERY_MODEL_VIEW};

	/**The default default model view of this panel.*/
	private final int DEFAULT_DEFAULT_MODEL_VIEW=QUERY_MODEL_VIEW;

	/**The tab in which the query and choices and/or answers are shown.*/
	private final BasicPanel queryPanel;

		/**@return The tab in which the query and choices and/or answers are shown.*/
		private BasicPanel getQueryPanel() {return queryPanel;}

	private final JLabel queryLabel;
	private final XMLPanel queryXMLPanel;

	/**@return The data model for which this component provides a view.
	@see ModelViewablePanel#getModel()
	*/
	public QuestionModel getQuestionModel() {return (QuestionModel)getModel();}

	/**Sets the data model.
	@param model The data model for which this component provides a view.
	@see ModelViewablePanel#setModel(Model)
	*/
	public void setQuestionModel(final QuestionModel model) {setModel(model);}

	/**Model constructor.
	@param model The data model for which this component provides a view.
	*/
	public QuestionPanel(final QuestionModel model)
	{
		this(model, true);	//construct and initialize the panel
	}

	/**Model constructor with optional initialization.
	@param model The data model for which this component provides a view.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public QuestionPanel(final QuestionModel model, final boolean initialize)
	{
		super(model, false);	//construct the parent class without initializing the panel
		setSupportedModelViews(DEFAULT_SUPPORTED_MODEL_VIEWS);	//set the model views we support
		setDefaultDataView(DEFAULT_DEFAULT_MODEL_VIEW);	//set the default data view
		queryPanel=new BasicPanel(new BasicGridBagLayout());	//create the query panel
		queryLabel=new JLabel();
		queryXMLPanel=new XMLPanel(new XMLDocumentFragmentModel(), XHTMLConstants.XHTML_CONTENT_TYPE);	//TODO set the base URI and URIInputStreamable 
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}

	/**Initialize the user interface.*/
	protected void initializeUI()
	{
		setBorder(BorderUtilities.createDefaultTitledBorder());	//set a titled border
		setTitle("Question");	//G***i18n
		addView(QUERY_MODEL_VIEW, "Query and Response", IconResources.getIcon(IconResources.QUESTION_ICON_FILENAME), queryPanel);	//add the query view G***i18n
		super.initializeUI(); //do the default UI initialization
		getTabbedPane().setTabPlacement(JTabbedPane.TOP);	//put the tabs on the top
		queryLabel.setText("Query");	//G***i18n
		queryPanel.add(queryLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.SOUTHWEST, GridBagConstraints.NONE, NO_INSETS, 0, 0));
		queryPanel.add(queryXMLPanel, new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0, GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, NO_INSETS, 0, 0));
	}

	/**Loads the data from the model to the view, if necessary.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void loadModel() throws IOException
	{
		super.loadModel();	//do the default loading
		final QuestionModel model=getQuestionModel();	//get the data model
		switch(getModelView())	//see which view of data we should load
		{
			case QUERY_MODEL_VIEW:	//if we're changing to the query view
				break;
		}
	}

	/**Stores the current data being edited to the model, if necessary.
	@exception IOException Thrown if there was an error loading the model.
	*/
	protected void saveModel() throws IOException
	{
		super.saveModel();	//do the default saving
		final QuestionModel model=getQuestionModel();	//get the data model
		switch(getModelView())	//see which view of data we have, in order to get the current RDF
		{
			case QUERY_MODEL_VIEW:	//if we should store the query
				break;
		}
	}

	/**Indicates that the view of the data has changed.
	@param oldView The view before the change.
	@param newView The new view of the data
	*/
/*G***del if not needed
	protected void onModelViewChange(final int oldView, final int newView)
	{
		super.onModelViewChange(oldView, newView);	//perform the default functionality
		switch(oldView)	//see which view we're changing from
		{
			case SOURCE_MODEL_VIEW:	//if we're changing from the source view
				getSourceTextPane().getDocument().removeDocumentListener(getModifyDocumentListener());	//don't listen for changes to the source text pane any more
				getSourceTextPane().setDocument(getSourceTextPane().getEditorKit().createDefaultDocument());	//remove the content from the source text pane by installing a new document
				break;
		}
		switch(newView)	//see which view we're changing to
		{
			case SOURCE_MODEL_VIEW:	//if we're changing to the source view
				getSourceTextPane().getDocument().addDocumentListener(getModifyDocumentListener());	//add ourselves as a document listener to see if the source pane is modified
				break;
		}
	}
*/
}
