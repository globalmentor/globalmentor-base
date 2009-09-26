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

package com.garretwilson.swing.qti;

import java.beans.*;
import java.io.*;
import javax.swing.*;
import com.garretwilson.swing.*;
import com.globalmentor.mentoract.qti.Assessment;
import com.globalmentor.mentoract.qti.QTI;
import com.globalmentor.mentoract.qti.QTIXMLifier;
import com.globalmentor.model.Modifiable;
import com.globalmentor.text.xml.XMLSerializer;

import org.w3c.dom.*;

/**A panel that contains a toolbar and a status. The panel uses a border layout,
	and content can be added by a component being placed in the panel center.
@author Garret Wilson
*/
public class QTIDocumentPanel extends AbstractMDIDocumentPanel implements Modifiable
{

	/**The main QTI exploration panel.*/
	private final QTIExplorePanel qtiExplorePanel;

		/**@return The QTI data model.*/ //TODO do we want to do this and allow it to be edited elsewhere?
		public QTI getQTI() {return qtiExplorePanel.getQTI();}

		/**Sets the QTI information for the frame, in essence loading the data.
		@param newQTI The QTI data.
		*/
		public void setQTI(final QTI newQTI)
		{
			qtiExplorePanel.setQTI(newQTI); //set the QTI data model in the explore panel
		}

		/**@return The assessment stored in the panel, if this panel is only
		  holding an assessment, or <code>null</code> if QTI in general or another
			object is being explored.
		*/
		public Assessment getAssessment() {return qtiExplorePanel.getAssessment();}

		/**Sets the assessment displayed in the panel.
		@param newAssessment The assessment to explore.
		*/
		public void setAssessment(final Assessment newAssessment) {qtiExplorePanel.setAssessment(newAssessment);}

	/**@return Whether the QTI data model has been modified.*/
	public boolean isModified() {return qtiExplorePanel.isModified();}

	/**Sets whether the QTI data model has been modified.
	@param newModified The new modification status.
	*/
	public void setModified(final boolean newModified) {qtiExplorePanel.setModified(newModified);}

	/**@return The action for adding a new section.*/
	public Action getAddSectionAction() {return qtiExplorePanel.getAddSectionAction();}

	/**@return The action for adding a new item.*/
	public Action getAddItemAction() {return qtiExplorePanel.getAddItemAction();}

	/**@return The action for removing an item.*/
	public Action getRemoveItemAction() {return qtiExplorePanel.getDeleteAction();}

	/**The action for editing an item.*/
//TODO fix	final Action editItemAction=new EditItemAction();

	/**@return The action for editing an item.*/  //TODO probably rename these to getEditAction() and getDeleteAction()
	public Action getEditItemAction() {return qtiExplorePanel.getEditAction();}

	/**Default constructor.*/
	public QTIDocumentPanel()
	{
		super(true, true, false); //construct the panel without initializing
		qtiExplorePanel=new QTIExplorePanel();  //create the QTI exploring panel
//TODO del		super(qtiExplorePanel=new QTIExplorePanel());  //construct the parent, creating the QTI exploring panel
		initialize(); //initialize the panel
	}

	/**QTI constructor.
	@param qti The QTI document model to display.
	*/
	public QTIDocumentPanel(final QTI qti)
	{
		this();  //do the default construction
		setQTI(qti);  //set the QTI
	}

	/**Assessment constructor.
	@param assessment The assessment to display.
	*/
	public QTIDocumentPanel(final Assessment assessment)
	{
		this();  //do the default construction
		setAssessment(assessment);  //set the assessment
	}

	/**Creates any application objects and initializes data.
		Any class that overrides this method must call this version.
	*/
/*TODO del when works
	protected void initializeData()
	{
		super.initializeData(); //do the default initialization
		qtiExplorePanel=new QTIExplorePanel();  //create the QTI exploring panel
	}
*/

	/**Creates the default resource used for describing the document.
	@return A resource for describing the document.
	*/
/*TODO del when works
	protected RDFResource createDefaultResource()
	{
		final RDFResource resource=super.createDefaultResource(); //create the default resource
		XPackageUtilities.addContentType(getRDF(), resource, new MediaType("application", "x-qti-assessment")); //TODO testing; use constants
		return resource;  //return the default resource with our new properties added
	}
*/

	/**Initializes the user interface.
		Any derived class that overrides this method should call this version.
	*/
  protected void initializeUI()
  {
		super.initializeUI(); //do the default initialization
			//forward all explore panel "modified" property changes to anyone listening to our property changes
		qtiExplorePanel.addPropertyChangeListener(MODIFIED_PROPERTY, new java.beans.PropertyChangeListener()
    {
      public void propertyChange(final PropertyChangeEvent propertyChangeEvent) //if the "modified" property changes in the explore panel
      {
					//forward the "modified" property change event
				firePropertyChange(propertyChangeEvent.getPropertyName(), propertyChangeEvent.getOldValue(), propertyChangeEvent.getNewValue());
      }
    });
		setContentComponent(qtiExplorePanel); //show the QTI exploring panel as our application component
				//TODO should this all go in a special initializeToolBar()?
		getToolBar().add(qtiExplorePanel.getAddItemAction());  //TODO testing; use intermediate methods
		getToolBar().add(qtiExplorePanel.getDeleteAction());  //TODO testing; use intermediate methods
		getToolBar().add(qtiExplorePanel.getEditAction());  //TODO testing; use intermediate methods
  }

	/**Loads the document by reading the contents of the document from the given
		input stream.
	@param inputStream The source of the content.
	@param description The object describing the input stream.
	@exception IOException Thrown if there is an error reading the contents.
	*/ 
	public void read(final InputStream inputStream, final Object description) throws IOException
	{
//TODO fix
	}

	/**Saves the document by writing the contents of the document to the given
		output stream.
	@param outputStream The destination of the content.
	@exception IOException Thrown if there is an error writing the contents.
	*/
	public void write(final OutputStream outputStream) throws IOException
	{
		final QTI qti;  //we'll get or create QTI
		if(getQTI()!=null)  //if we have QTI
		{
			qti=getQTI(); //store the QTI
		}
		else  //if we don't have QTI, we must have an assessment TODO fix; this will eventually allow other things, such as sections and such
		{
			qti=new QTI();  //create qti
			qti.getAssessmentList().add(getAssessment()); //add the assessment
		}
		final Document document=QTIXMLifier.createDocument(qti);  //create an XML document from the QTI data model
		final XMLSerializer xmlSerializer=new XMLSerializer(true);  //create a formatted XML serializer
		xmlSerializer.serialize(document, outputStream); //serialize the XML to the output string
	}

}
