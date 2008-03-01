package com.garretwilson.swing.qti;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.io.*; //G***move
import java.util.*;
import javax.mail.internet.ContentType;
import javax.swing.*;
import javax.swing.event.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.swing.*;
import com.garretwilson.swing.text.xml.XMLEditorKit;
import com.garretwilson.text.CharacterEncoding; //G***move
import com.garretwilson.assess.qti.*;
import com.garretwilson.text.xml.stylesheets.css.XMLCSSProcessor; //G***this goes somewhere else
import com.globalmentor.util.*;

/**An internal frame for editing lists of questions. Contains the internal
	item list data model.
//G***del @see QTIItemListModel
@author Garret Wilson
*/
public class QTIInternalFrame extends JInternalFrame implements Modifiable
{

	/**The file in which the QTI data is stored.*/
//G***del if not needed	private File file=null;

		/**@return The file in which the QTI data is stored, or <code>null</code>
		  if the QTI is not stored in a file.*/
//G***del if not needed		public File getFile() {return file;}

		/**Sets the file in which the QTI data should be stored.
		@param newFile The file for storing QTI data.
		*/
//G***del if not needed		public void setFile(final File newFile) {file=newFile;}

	/**The object containing the QTI information; the model for this frame.*/
//G***del if not needed	private QTI qti=new QTI();  //G***don't initialize this, and instead do this in the default constructor, with another constructor for initialization with QTI data

	/**@return The QTI data model.*/ //G***do we want to do this and allow it to be edited elsewhere?
	public QTI getQTI() {return explorePanel.getQTI();}

	/**@return Whether the QTI data model has been modified.*/
	public boolean isModified() {return explorePanel.isModified();}

	/**Sets whether the QTI data model has been modified.
	@param newModified The new modification status.
	*/
	public void setModified(final boolean newModified) {explorePanel.setModified(newModified);}

	/**@return The action for adding a new section.*/
	public Action getAddSectionAction() {return explorePanel.getAddSectionAction();}

	/**@return The action for adding a new item.*/
	public Action getAddItemAction() {return explorePanel.getAddItemAction();}

	/**@return The action for removing an item.*/
	public Action getRemoveItemAction() {return explorePanel.getDeleteAction();}

	/**The action for editing an item.*/
//G***fix	final Action editItemAction=new EditItemAction();

	/**@return The action for editing an item.*/  //G***probably rename these to getEditAction() and getDeleteAction()
	public Action getEditItemAction() {return explorePanel.getEditAction();}

	JToolBar toolBar = new JToolBar();
  BorderLayout borderLayout = new BorderLayout();
	QTIExplorePanel explorePanel=new QTIExplorePanel();
//G***del  JTabbedPane sectionTabbedPane = new JTabbedPane();
//G***fix  ActionList itemList = new ActionList();

	/**Default constructor.*/
	public QTIInternalFrame()
	{
		super("[Question Items]", true, true, true, true);  //create the default options G***i18n
		jbInit();
	}

	/**Constructs a QTI item list internal frame with an existing list data model.
	@param listModel The data model for the item list
	*/
/*G***fix
	public QTIItemListInternalFrame(final QTIItemListModel listModel)
	{
		super("[Question Items]", true, true, true, true);  //create the default options G***i18n
//G***del		  final JInternalFrame internalFrame=new JInternalFrame("Test internal frame", true, true, true, true);

Debug.trace("saving item list model with size: ", listModel.size()); //G***del
	  itemListModel=listModel;  //save the given list model
			//create the actions
		deleteAction=new DeleteAction();
		editAction=new EditAction();
		newAction=new NewAction();
    try
    {
      jbInit();
    }
    catch(Exception e)
    {
      e.printStackTrace();
    }
		updateActions();  //update the state of the actions
	}
*/

	/**Initializes the user interface.*/
  private void jbInit()
  {
		this.setBackground(Color.lightGray);  //G***testing
		//setup the toolbar
		toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);	//G***use constant here, fix
//G***fix		toolBar.add(newAction);	//new
//G***fix		toolBar.add(deleteAction);	//delete
//G***fix		toolBar.addSeparator();	//--
//G***fix		toolBar.add(editAction);	//edit
//G***del Debug.trace("ready to set list model with size: ", itemListModel.size()); //G***del
    this.getContentPane().setLayout(borderLayout);
			//forward all explore panel "modified" property changes to anyone listening to our property changes
		explorePanel.addPropertyChangeListener(MODIFIED_PROPERTY, new java.beans.PropertyChangeListener()
    {
      public void propertyChange(final PropertyChangeEvent propertyChangeEvent) //if the "modified" property changes in the explore panel
      {
					//forward the "modified" property change event
				firePropertyChange(propertyChangeEvent.getPropertyName(), propertyChangeEvent.getOldValue(), propertyChangeEvent.getNewValue());
      }
    });
    this.getContentPane().add(toolBar, BorderLayout.NORTH);
    this.getContentPane().add(explorePanel, BorderLayout.CENTER);
//G***del    this.getContentPane().add(sectionTabbedPane, BorderLayout.CENTER);
//G***fix    this.getContentPane().add(itemList, BorderLayout.CENTER);
		this.setSize(100, 100); //G***testing
  }

	/**Sets the QTI information for the frame, in essence loading the data.
	@param newQTI The QTI data.
	*/
	public void setQTI(final QTI newQTI)
	{
Debug.trace("Setting QTI"); //G***del
//G***del		qti=newQTI; //save the QTI data
		explorePanel.setQTI(newQTI); //set the QTI data model in the explore panel
	}

	/**Action for creating a new section.*/
	class NewSectionAction extends AbstractAction
	{
		/**Default constructor.*/
		public NewSectionAction()
		{
			super("New Section...");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "new section");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Create a new section.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_S));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.FOLDER_NEW_ICON_FILENAME)); //load the correct icon
//G***del when works			new ImageIcon(ReaderFrame.class.getResource("book_open.gif")));	//load the correct icon
//G***del when works			putValue(SMALL_ICON, new ImageIcon(ReaderFrame.class.getResource("book_open.gif")));	//load the correct icon
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{
/*G***fix
			QTIItemListPanel itemListPanel=(QTIItemListPanel)sectionTabbedPane.getSelectedComponent(); //get the section panel selected in the tabbed pane
			if(itemListPanel==null)  //if there is no section panel selected, assume there are no section panels
			{
				itemListPanel=addItemListPanel(qti.getItemList()); //create a section UI for the items outside the section
			}
			final ItemListModel itemListModel=(ItemListModel)itemListPanel.itemList.getModel(); //get the list model being used in the section
			final Item item=askNewItem(); //ask for a new item
			if(item!=null)  //if an item was created
				itemListModel.addItem(item);  //add the item to the list model
*/
		}
	}


/*G***del; testing

//G***move all this somewhere so that we can easily edit XHTML text			final String textHTML="<p>This is a <b>bold</b> test.</p>";

		  final XMLTextPane xmlTextPane=new XMLTextPane();  //G***testing
//G***del xmlTextPane.setPaged(true); //G***testing
			xmlTextPane.setContentType("text/html");  //G**testing
//G***del		  xmlTextPane.setEditorKit(new com.garretwilson.swing.text.xml.oeb.OEBEditorKit()); //G***switch to using an XML editor kit

		  final com.garretwilson.text.xml.XMLDocument xmlDocument=(com.garretwilson.text.xml.XMLDocument)com.garretwilson.text.xml.oeb.OEBUtilities.createDefaultOEB1Document();  //create an XML document G***switch to only using XHTML
//G***del		  xmlDocument.getStyleSheetList().add(new com.garretwilson.text.xml.stylesheets.css.oeb.DefaultOEBCSSStyleSheet());	//add the default stylesheet for OEB G***fix for DOM, whatever needs to be done to associate this in a general way; a bit hacked at the moment
//G***fix			tidyOEBXMLDocument(xmlDocument);	//tidy up the document (an important step if the document has text directly in the body and such) G***test, comment


			final com.garretwilson.text.xml.XMLProcessor xmlProcessor=new com.garretwilson.text.xml.XMLProcessor(); //G***testing
		  final com.garretwilson.text.xml.XMLElement bodyElement=(com.garretwilson.text.xml.XMLElement)com.garretwilson.text.xml.xhtml.XHTMLUtilities.getBodyElement(xmlDocument); //get the XHTML body element

			try
			{
Debug.trace("Body element before: ", com.garretwilson.text.xml.XMLUtilities.getText(bodyElement, true)); //G***del
		  xmlProcessor.parseElementContent(bodyElement, textHTML);  //G***testing
Debug.trace("Body element after: ", com.garretwilson.text.xml.XMLUtilities.getText(bodyElement, true)); //G***del
com.garretwilson.text.xml.XMLUtilities.printTree(xmlDocument, Debug.getOutput()); //G***del

			}
			catch(java.io.IOException ioException)
			{
				Debug.error(ioException); //G***fix
			}

//G***del xmlTextPane.setPaged(true); //G***testing
		  ((XMLEditorKit)xmlTextPane.getEditorKit()).setXML(xmlDocument, null, new MediaType("text/html"), (com.garretwilson.swing.text.xml.XMLDocument)xmlTextPane.getDocument()); //G***testing
//G***del xmlTextPane.setPaged(false); //G***testing



//G***del xmlTextPane.revalidate(); //G***testing

//G***fix		xmlTextPane.revalidate(); //G***testing
//G***fix			xmlTextPane.getUI().getRootView(xmlTextPane).preferenceChanged();
		  //G***del xmlTextPane.getUI().getRootView(xmlTextPane).invalid
//G***del		  xmlTextPane.getUI().getRootView(xmlTextPane).removeAll(); //G***testing

				//G***testing; del

xmlTextPane.setPreferredSize(new Dimension(200, 300));  //G***fix
				JOptionPane.showConfirmDialog(QTIItemListInternalFrame.this, xmlTextPane, "XML Text", JOptionPane.OK_CANCEL_OPTION);
				  //G***add all these classes as imports, even though we reference them by their absolute class names
				final org.w3c.dom.Document outputXMLDocument=((XMLEditorKit)xmlTextPane.getEditorKit()).getXML((com.garretwilson.swing.text.xml.XMLDocument)xmlTextPane.getDocument()); //G***fix; comment

			  final org.w3c.dom.Element outputBodyElement=com.garretwilson.text.xml.xhtml.XHTMLUtilities.getBodyElement(outputXMLDocument); //get the XHTML body element

				final ByteArrayOutputStream byteArrayOutputStream=new ByteArrayOutputStream();  //create a new byte array output stream

			try
			{
				new com.garretwilson.text.xml.XMLSerializer().serializeContent(outputBodyElement, byteArrayOutputStream, CharacterEncoding.UTF_8); //serialize the content of the element to a byte array using UTF-8
			}
			catch(java.io.IOException ioException)
			{
				Debug.error(ioException); //G***fix
			}

			try
			{
				final String outputHTML=byteArrayOutputStream.toString(CharacterEncoding.UTF_8); //convert the byte array input stream to a string
				JOptionPane.showMessageDialog(QTIItemListInternalFrame.this, outputHTML); //G***fix; comment


//G***testing
Debug.trace("Ready to print tree"); //G***del
			if(Debug.isDebug())	//if debugging is turned on
				com.garretwilson.swing.text.ViewUtilities.printViews(xmlTextPane, Debug.getOutput());	//G***testing
			// Create a frame containing an instance of
			// ElementTreePanel.
			final JFrame elementTreeFrame=new JFrame("Elements");
			elementTreeFrame.addWindowListener(new WindowAdapter()
			{
					public void windowClosing(WindowEvent weeee) {
				elementTreeFrame.setVisible(false);
					}
			});
			Container fContentPane = elementTreeFrame.getContentPane();
			fContentPane.setLayout(new BorderLayout());
			final JPanel elementTreePanel = new com.garretwilson.temp.ElementTreePanel(xmlTextPane);
			fContentPane.add(elementTreePanel);
			elementTreeFrame.pack();
			elementTreeFrame.show();



			}
			catch(java.io.UnsupportedEncodingException unsupportedEncodingException)
			{
				Debug.error(unsupportedEncodingException); //G***fix
			}
//G***del }}.run(); //G***testing
*/

}