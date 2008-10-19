package com.garretwilson.swing.qti;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
//G***del import javax.swing.text.*;
import javax.swing.tree.*;
import com.garretwilson.resources.icon.IconResources;
import com.garretwilson.swing.*;
import com.garretwilson.swing.text.ViewUtilities;
import com.garretwilson.swing.text.xml.*;
import com.garretwilson.swing.text.xml.qti.QTIViewFactory;
import com.garretwilson.swing.tree.*;
import com.garretwilson.swing.qti.tree.*;
import com.globalmentor.java.Booleans;
import com.globalmentor.mentoract.qti.Assessment;
import com.globalmentor.mentoract.qti.Item;
import com.globalmentor.mentoract.qti.Material;
import com.globalmentor.mentoract.qti.Presentation;
import com.globalmentor.mentoract.qti.QTI;
import com.globalmentor.mentoract.qti.QTIXMLifier;
import com.globalmentor.mentoract.qti.Response;
import com.globalmentor.mentoract.qti.ResponseLID;
import com.globalmentor.mentoract.qti.Section;
import com.globalmentor.text.xml.XMLSerializer;
import com.globalmentor.text.xml.XML;
import com.globalmentor.util.*;

import org.w3c.dom.*;

/**Panel for exploring a QTI data model in a tree+panel view.
	Currently only supports a list of QTI items, such as in a section.
@author Garret Wilson
*/
public class QTIExplorePanel extends JPanel implements TreeSelectionListener, Modifiable
{

	/**The object containing the QTI information; the model for this frame.*/
	private QTI qti=new QTI();  //G***don't initialize this, and instead do this in the default constructor, with another constructor for initialization with QTI data

		/**@return The QTI data model.*/ //G***do we want to do this and allow it to be edited elsewhere?
		public QTI getQTI() {return qti;}

		/**Sets the QTI data model.
		@param newQTI The QTI data.
		*/
		public void setQTI(final QTI newQTI)
		{
	Debug.trace("Setting QTI"); //G***del
		  assessment=null;  //show that there is no assessment
			qti=newQTI; //save the QTI data
			qtiTree.setModel(new DefaultTreeModel(new QTITreeNode(qti)));  //G***testing
			qtiTree.expandRow(1);  //G***fix; testing
			splitPane.resetToPreferredSizes();  //G***testing
			setModified(false); //show that, since we just received the data model, the data model hasn't been modified
		}

	/**The assessment stored in the panel, if this panel is only holding an assessment.*/
	private Assessment assessment=null;

		/**@return The assessment stored in the panel, if this panel is only
		  holding an assessment, or <code>null</code> if QTI in general or another
			object is being explored.
		*/
		public Assessment getAssessment() {return assessment;}

		/**Sets the assessment displayed in the panel.
		@param newAssessment The assessment to explore.
		*/
		public void setAssessment(final Assessment newAssessment)
		{
			qti=null; //show that we have no general QTI
		  assessment=newAssessment; //set the assessment
			qtiTree.setModel(new DefaultTreeModel(new QTIAssessmentTreeNode(assessment)));  //create a tree model for the assessment
			qtiTree.expandRow(2);  //expand the assessment to show the section and its data
			splitPane.resetToPreferredSizes();  //set the split pane to its referred size
			setModified(false); //show that, since we just received the data model, the data model hasn't been modified
		}

	/**Whether the QTI data model has been modified; the default is not modified.*/
	private boolean modified=false;

		/**@return Whether the QTI data model has been modified.*/
		public boolean isModified() {return modified;}

		/**Sets whether the QTI data model has been modified.
		  This is a bound property.
		@param newModified The new modification status.
		*/
		public void setModified(final boolean newModified)
		{
			final boolean oldModified=modified; //get the old modified value
			if(oldModified!=newModified)  //if the value is really changing
			{
			  modified=newModified; //update the value
				  //show that the modified property has changed
        firePropertyChange(MODIFIED_PROPERTY, Boolean.valueOf(oldModified), Boolean.valueOf(newModified));
			}
		}

	/**The tree model used for the explore view.*/
//G***del;	private TreeModel treeModel=new DefaultTreeModel();

		/**@return The tree model used for the explore view.*/
//G***del;		private TreeModel treeModel=new DefaultTreeModel();

	/**The action for editing an item.*/
//G***del	protected final Action editItemAction=new EditItemAction();

		/**@return The action for editing an item.*/
//G***del		public Action getEditItemAction() {return editItemAction;}

	/**The action for editing.*/
	protected final ProxyAction editAction=new EditAction();

		/**@return The action for editing.*/
		public Action getEditAction() {return editAction;}

	/**The action for deleting.*/
	protected final ProxyAction deleteAction=new DeleteAction();

		/**@return The action for deleting.*/
		public Action getDeleteAction() {return deleteAction;}

	/**The action for adding a new question.*/
	private final Action addItemAction=new AddItemAction();

		/**@return The action for adding a new question.*/
		public Action getAddItemAction() {return addItemAction;}

	/**The action for adding a new section.*/
	private final Action addSectionAction=new AddSectionAction();

		/**@return The action for adding a new section.*/
		public Action getAddSectionAction() {return addSectionAction;}

	/**The XML serializer used for serializing the QTI.*/
	protected final XMLSerializer xmlSerializer;

	/**The text pane used to display information.*/
	XMLTextPane xmlTextPane=new XMLTextPane();

  BorderLayout borderLayout = new BorderLayout();
  JSplitPane splitPane = new JSplitPane();
  JScrollPane qtiScrollPane = new JScrollPane();
  ActionTree qtiTree = new ActionTree();
  JTabbedPane viewTabbedPane = new JTabbedPane();
  JPanel previewPanel = new JPanel();
  JPanel sourcePanel = new JPanel();
  BorderLayout previewBorderLayout = new BorderLayout();
  JScrollPane previewScrollPane = new JScrollPane();
  BorderLayout sourceBorderLayout = new BorderLayout();
  JScrollPane sourceScrollPane = new JScrollPane();
  JTextArea sourceTextArea = new JTextArea();
	UserObjectTreeCellRenderer qtiTreeCellRenderer=new UserObjectTreeCellRenderer();

	/**Default constructor.*/
	public QTIExplorePanel()
	{
		this(new QTI());  //create a default QTI data model and construct the class
	}

	/**Creates an explore panel to represent the QTI items.
	@param newQTI The QTI data.
	*/
	public QTIExplorePanel(final QTI newQTI)
	{
		xmlSerializer=new XMLSerializer();  //create an XML serializer
		xmlSerializer.setFormatted(true); //tell the serializer to format the QTI data
			//register a QTI view factory with the QTI namespace, with the normal XML view factory as the fallback
		xmlTextPane.registerViewFactory(QTI.QTI_1_1_NAMESPACE_URI, new QTIViewFactory());  //G***testing
			//setup tree cell icons
				//assessment
		qtiTreeCellRenderer.registerOpenIcon(Assessment.class, IconResources.getIcon(IconResources.DOCUMENT_QUESTION_ICON_FILENAME));
		qtiTreeCellRenderer.registerClosedIcon(Assessment.class, IconResources.getIcon(IconResources.DOCUMENT_QUESTION_ICON_FILENAME));
		qtiTreeCellRenderer.registerLeafIcon(Assessment.class, IconResources.getIcon(IconResources.DOCUMENT_QUESTION_ICON_FILENAME));
			//section
		qtiTreeCellRenderer.registerOpenIcon(Section.class, IconResources.getIcon(IconResources.FOLDER_OPEN_ICON_FILENAME));
		qtiTreeCellRenderer.registerClosedIcon(Section.class, IconResources.getIcon(IconResources.FOLDER_ICON_FILENAME));
		qtiTreeCellRenderer.registerLeafIcon(Section.class, IconResources.getIcon(IconResources.FOLDER_ICON_FILENAME));
			//item
		qtiTreeCellRenderer.registerOpenIcon(Item.class, IconResources.getIcon(IconResources.QUESTION_ICON_FILENAME));
		qtiTreeCellRenderer.registerClosedIcon(Item.class, IconResources.getIcon(IconResources.QUESTION_ICON_FILENAME));
		qtiTreeCellRenderer.registerLeafIcon(Item.class, IconResources.getIcon(IconResources.QUESTION_ICON_FILENAME));
			//material
		qtiTreeCellRenderer.registerOpenIcon(Material.class, IconResources.getIcon(IconResources.DOCUMENT_CONTENT_ICON_FILENAME));
		qtiTreeCellRenderer.registerClosedIcon(Material.class, IconResources.getIcon(IconResources.DOCUMENT_CONTENT_ICON_FILENAME));
		qtiTreeCellRenderer.registerLeafIcon(Material.class, IconResources.getIcon(IconResources.DOCUMENT_CONTENT_ICON_FILENAME));
			//presentation
		qtiTreeCellRenderer.registerOpenIcon(Presentation.class, IconResources.getIcon(IconResources.MONITOR_ICON_FILENAME));
		qtiTreeCellRenderer.registerClosedIcon(Presentation.class, IconResources.getIcon(IconResources.MONITOR_ICON_FILENAME));
		qtiTreeCellRenderer.registerLeafIcon(Presentation.class, IconResources.getIcon(IconResources.MONITOR_ICON_FILENAME));
			//responseLID
		qtiTreeCellRenderer.registerOpenIcon(ResponseLID.class, IconResources.getIcon(IconResources.PROPERTY_ICON_FILENAME));
		qtiTreeCellRenderer.registerClosedIcon(ResponseLID.class, IconResources.getIcon(IconResources.PROPERTY_ICON_FILENAME));
		qtiTreeCellRenderer.registerLeafIcon(ResponseLID.class, IconResources.getIcon(IconResources.PROPERTY_ICON_FILENAME));
    jbInit();
		setQTI(newQTI); //store the QTI data
	}

	/**Initializes the user interface.*/
  private void jbInit()
  {
    this.setLayout(borderLayout);
		qtiTree.addTreeSelectionListener(this);
    qtiTree.addActionListener(getEditAction()); //call the edit action when the item is selected
		qtiTree.setCellRenderer(qtiTreeCellRenderer); //render the icons using our cell renderer for QTI
    viewTabbedPane.setTabPlacement(JTabbedPane.BOTTOM);
    previewPanel.setLayout(previewBorderLayout);
    sourcePanel.setLayout(sourceBorderLayout);
//G***del    sourceTextArea.setText("jTextArea1");
//G***del    previewTextPane.setText("jTextPane1");
    sourceTextArea.setFont(new java.awt.Font("Dialog", 0, 10));
    sourceTextArea.setEditable(false);
    sourceTextArea.setTabSize(2);
		xmlTextPane.setPaged(false);
    xmlTextPane.setEditable(false);
    this.add(splitPane, BorderLayout.CENTER);
    splitPane.add(qtiScrollPane, JSplitPane.LEFT);
		qtiScrollPane.setMinimumSize(new Dimension(100, 100)); //G***testing
    qtiScrollPane.getViewport().add(qtiTree, null);
    splitPane.add(viewTabbedPane, JSplitPane.RIGHT);
    viewTabbedPane.add(previewPanel,  "Preview"); //G***i18n
    viewTabbedPane.add(sourcePanel,  "Source"); //G***i18n
    previewPanel.add(previewScrollPane, BorderLayout.CENTER);
    sourcePanel.add(sourceScrollPane, BorderLayout.CENTER);
    sourceScrollPane.getViewport().add(sourceTextArea, null);
    previewScrollPane.getViewport().add(xmlTextPane, null);
  }

	/**Called when a selection changes in the QTI tree.
	@param treeSelectionEvent The event containing the selection information.
	*/
	public void valueChanged(final TreeSelectionEvent treeSelectionEvent)
	{
		updateSelection(treeSelectionEvent.getPath()); //get the selected resource from the event's selection path and update the UI
	}

	/**Updates the user interface controls in response to a QTI object being
		selected.
	@param selectionPath The selected path.
	*/
	protected void updateSelection(final TreePath selectionPath)
	{
		final Document xmlDocument;  //we'll store the XML version of the QTI here, if applicable
		final Object selectedObject=selectionPath.getLastPathComponent();  //get the selected object
		if(selectedObject instanceof QTIItemTreeNode)  //item
		{
			final QTIItemTreeNode itemTreeNode=(QTIItemTreeNode)selectedObject; //cast the object to an item tree node
			final Item item=itemTreeNode.getItem(); //get the object stored in the node
			xmlDocument=QTIXMLifier.createDocument(item);  //create an XML document from the item
			editAction.setProxiedAction(new EditItemAction(item, itemTreeNode)); //create an action for editing the item
			deleteAction.setProxiedAction(new DeleteItemAction(item, itemTreeNode)); //create an action for removing the item
		}
		else if(selectedObject instanceof QTIMaterialTreeNode)  //material
		{
			final Material material=((QTIMaterialTreeNode)selectedObject).getMaterial(); //get the object stored in the node
			xmlDocument=QTIXMLifier.createDocument(); //create a blank document G***fix
			xmlDocument.replaceChild(QTIXMLifier.createElement(xmlDocument, material), xmlDocument.getDocumentElement());  //create an XML element from the material and set it as the document element
			editAction.setProxiedAction(null); //G***fix
			deleteAction.setProxiedAction(null); //G***fix
		}
		else if(selectedObject instanceof QTIPresentationTreeNode)  //presentation
		{
			final Presentation presentation=((QTIPresentationTreeNode)selectedObject).getPresentation(); //get the object stored in the node
			xmlDocument=QTIXMLifier.createDocument(); //create a blank document G***fix
			xmlDocument.replaceChild(QTIXMLifier.createElement(xmlDocument, presentation), xmlDocument.getDocumentElement());  //create an XML element from the presentation and set it as the document element
			editAction.setProxiedAction(null); //G***fix
			deleteAction.setProxiedAction(null); //G***fix
		}
		else if(selectedObject instanceof QTISectionTreeNode)  //section
		{
			final Section section=((QTISectionTreeNode)selectedObject).getSection(); //get the object stored in the node
			xmlDocument=QTIXMLifier.createDocument(); //create a blank document G***fix
			xmlDocument.replaceChild(QTIXMLifier.createElement(xmlDocument, section), xmlDocument.getDocumentElement());  //create an XML element from the section and set it as the document element
			editAction.setProxiedAction(null); //G***fix
			deleteAction.setProxiedAction(null); //G***fix
		}
		else if(selectedObject instanceof QTIAssessmentTreeNode)  //assessment
		{
			final Assessment assessment=((QTIAssessmentTreeNode)selectedObject).getAssessment(); //get the object stored in the node
			xmlDocument=QTIXMLifier.createDocument(); //create a blank document G***fix
			xmlDocument.replaceChild(QTIXMLifier.createElement(xmlDocument, assessment), xmlDocument.getDocumentElement());  //create an XML element from the assessment and set it as the document element
			editAction.setProxiedAction(null); //G***fix
			deleteAction.setProxiedAction(null); //G***fix
		}
		else  //if we don't recognize what is selected
		{
			xmlDocument=null; //show that we have no XML
			editAction.setProxiedAction(null); //G***fix
			deleteAction.setProxiedAction(null); //G***fix
		}
		if(xmlDocument!=null) //if we have XML from the selected object
		{
			//setup the source view
			final String qtiXMLString=xmlSerializer.serialize(xmlDocument.getDocumentElement());	//serialize the document element to a string
			sourceTextArea.setText(qtiXMLString); //put the QTI XML source in the preview window
			sourceTextArea.setCaretPosition(0);  //scroll to the top of the text
			//setup the preview view


	/*G***del
	Debug.trace("Created document from QTI item: ");
	com.garretwilson.text.xml.XMLUtilities.printTree(xmlDocument, Debug.getOutput()); //G***del
	*/
	//G***del Debug.trace("navigating");  //G***del

		  //G***testing removal of all QTI stuff; instead, add a qtiViewVactory.resetState() or something
//G***fix; this didn't fix the problem, we do have to clear the state somehow		xmlTextPane.registerViewFactory(QTIConstants.QTI_1_1_NAMESPACE_URI, new QTIViewFactory());  //G***testing

			xmlTextPane.setContentType("text/xml");  //G***use constant, put elsewhere
	//G***del Debug.trace("navigating");  //G***del
			final XMLDocument swingDocument=(XMLDocument)xmlTextPane.getDocument(); //get the currently loaded document
	//G***del Debug.trace("navigating");  //G***del

//G***del if not needed Debug.trace("****hiding views");  //G***del
//G***del if not needed			ViewUtilities.hideView(xmlTextPane.getUI().getRootView(xmlTextPane)); //G***testing remove components
		  //G***testing removal of all QTI stuff; instead, add a qtiViewVactory.resetState() or something
//G***fix; this didn't fix the problem, we do have to clear the state somehow		xmlTextPane.registerViewFactory(QTIConstants.QTI_1_1_NAMESPACE_URI, new QTIViewFactory());  //G***testing
//G***fix				xmlTextPane.registerViewFactory(QTIConstants.QTI_1_1_NAMESPACE_URI, new QTIViewFactory());  //G***testing
	//G***del Debug.trace("navigating");  //G***del

				javax.swing.SwingUtilities.invokeLater(new Runnable()	//invoke the hyperlink traversal until a later time in the event thread, so the mouse click won't be re-interpreted when we arrive at the hyperlink destination
				{
					public void run()
					{

	//G***del Debug.trace("navigating");  //G***del
Debug.trace("****setting XML");  //G***del

//TODO use the XMLTextPane.setXML() version if we can
						//G***testing for style application
			((XMLEditorKit)xmlTextPane.getEditorKit()).setXML(xmlDocument, null, XML.XML_CONTENT_TYPE, swingDocument); //set the XML document in the text pane
	//G***del Debug.trace("navigating");  //G***del
					}
				});
		}
	}

	/**Queries the user for the required information for a new item, allows the
		user to edit the item, then returns the item.
	@return The new item, or <code>null</code> if the action was cancelled.
	*/
	protected Item askNewItem()
	{
		final QTIResponseTypePanel responseTypePanel=new QTIResponseTypePanel();  //create a new response type panel
Debug.trace("asking new item"); //G***del
//G***testing optionpane
//G***del		OptionPane.test();  //G***del
			//ask what type of response should be added; if they make a selection G***i18n
		if(BasicOptionPane.showConfirmDialog(this, responseTypePanel, "Response Type", JOptionPane.OK_CANCEL_OPTION)==JOptionPane.OK_OPTION)
		{
			final Response response=responseTypePanel.createResponse();  //create a response from the given information in the panel
			if(response!=null)  //if we got a response
			{
				final Presentation presentation=new Presentation("", response);  //create presentation with the given reseponse
/*G***del org.doomdark.uuid.UUID version when new Java 5.0 version works
//G***del				final UUID uuid=UUIDGenerator.getInstance().generateTimeBasedUUID();  //create a new UUID
				final UUID uuid=UUIDGenerator.getInstance().generateRandomBasedUUID();  //create a new UUID
				final String ident="uuid:"+uuid.toString(); //create a UUID URI for the ID G***use a constant; fix better
*/
				final UUID uuid=UUID.randomUUID();  //create a new UUID
				final String ident="uuid:"+uuid.toString(); //create a UUID URI for the ID G***use a constant; fix better
				
				final Item item=new Item(ident, presentation); //create an item with the presentation
				return editItem(item);  //edit the item and return the edited item
/*G***del when works
				final QTIItemPanel itemPanel=new QTIItemPanel(item);  //create an panel to edit the item
					//edit the new item; if the user accepts the changes
				if(JOptionPane.showConfirmDialog(QTIInternalFrame.this, itemPanel, "Item", JOptionPane.OK_CANCEL_OPTION)==JOptionPane.OK_OPTION)  //G***i18n
				{
				  return itemPanel.getItem();  //return the new, edited item
				}
*/
			}
		}
		return null;  //show that we weren't able to create an item -- the user probably cancelled
	}

	/**Allows the user to edit the item, then returns the item.
	@param item The item to edit.
	@return The new item, or <code>null</code> if the action was cancelled.
	*/
	protected Item editItem(final Item item)
	{
		final QTIItemPanel itemPanel=new QTIItemPanel(item);  //create an panel to edit the item
		itemPanel.setPreferredSize(new Dimension(400, 400));  //G***fix preferred size QTI
			//edit the new item; if the user accepts the changes
		if(BasicOptionPane.showConfirmDialog(QTIExplorePanel.this, itemPanel, "Item", JOptionPane.OK_CANCEL_OPTION)==JOptionPane.OK_OPTION)  //G***i18n
		{
			return itemPanel.getItem();  //return the new, edited item
		}
		else  //if the user cancelled
		  return null;  //show that the user cancelled
	}

	/**General action for editing.*/
	class EditAction extends ProxyAction
	{

		/**Default constructor.*/
		public EditAction()
		{
			super("Edit...");	//create the base class G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.EDIT_ICON_FILENAME)); //load the correct icon
		}
	}

	/**Action for editing an item.*/
	class EditItemAction extends AbstractAction
	{

		/**The item to be edited.*/
		protected final Item item;

		/**The tree node of the item to be edited.*/
		protected final QTIItemTreeNode itemTreeNode;

		/**Item constructor.
		@param qtiItem The item to be edited.
		@param qtiItemTreeNode The tree node of the item to be edited.
		*/
		public EditItemAction(final Item qtiItem, final QTIItemTreeNode qtiItemTreeNode)
		{
			super("Edit item...");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Edit item");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Edit the selected item.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_E));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.EDIT_ICON_FILENAME)); //load the correct icon
			item=qtiItem; //save the item for editing
			itemTreeNode=qtiItemTreeNode; //save the item tree node
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{
			final Item newItem=editItem(item); //edit the item
			if(newItem!=null && itemTreeNode!=null)  //if we edited the item successfully and there is a tree node for the item
			{
//G***del				Debug.assert(itemTreeNode.getParent() instanceof DefaultMutableTreeNode, "Invalid QTI tree structure.");
				  //G***del next line
				if(itemTreeNode.getParent() instanceof DefaultMutableTreeNode)  //if the parent is a default mutable tree node
				{
					final DefaultMutableTreeNode parentTreeNode=(DefaultMutableTreeNode)itemTreeNode.getParent(); //cast the parent node to a default mutable tree node
					final Object parentObject=parentTreeNode.getUserObject(); //get the item's parent
					if(parentObject instanceof Section) //if the item is in a section
					{
						Lists.replace(((Section)parentObject).getItemList(), item, newItem);  //replace the item with the new item
						setModified(true);  //show that the data model has been modified
					}
					else if(parentObject instanceof QTI) //if the item is in QTI
					{
						Lists.replace(((QTI)parentObject).getItemList(), item, newItem);  //replace the item with the new item
						setModified(true);  //show that the data model has been modified
					}
					itemTreeNode.setUserObject(newItem);  //update the item being represented
					((DefaultTreeModel)qtiTree.getModel()).reload(itemTreeNode);  //show that we've changed this node
		  		updateSelection(qtiTree.getSelectionPath()); //update the selection in case our editing changed things
				}

/*G***fix
					final int treeNodeIndex=parentTreeNode.getIndex(itemTreeNode);  //get the index of the tree node in its parent
					parentTreeNode.a
				((DefaultTreeModel)qtiTree.getModel()).va
				.insertNodeInto(itemTreeNode, treeNode, treeNode.getChildCount());
				qtiTree.scrollPathToVisible(path.pathByAddingChild(itemTreeNode)); //scroll to the path that includes the added child
*/
//G***fix				final ItemListModel itemListModel=(ItemListModel)itemListPanel.itemList.getModel(); //get the list model being used in the section
//G***fix				itemListModel.replaceItem(oldItem, newItem);  //replace the old item with the new one
			}
		}
	}

	/**General action for deleting.*/
	class DeleteAction extends ProxyAction
	{

		/**Default constructor.*/
		public DeleteAction()
		{
			super("Delete");	//create the base class G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.DELETE_ICON_FILENAME)); //load the correct icon
		}
	}

	/**Action for deleting an item.*/
	class DeleteItemAction extends AbstractAction
	{

		/**The item to be deleted.*/
		protected final Item item;

		/**The tree node of the item to be deleted.*/
		protected final QTIItemTreeNode itemTreeNode;

		/**Item constructor.
		@param qtiItem The item to be deleted.
		@param qtiItemTreeNode The tree node of the item to be deleted.
		*/
		public DeleteItemAction(final Item qtiItem, final QTIItemTreeNode qtiItemTreeNode)
		{
			super("Delete item");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Delete item");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Delete the selected item.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_D));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.DELETE_ICON_FILENAME)); //load the correct icon
			item=qtiItem; //save the item for editing
			itemTreeNode=qtiItemTreeNode; //save the item tree node
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{
				//if they really want to remove the item
			if(JOptionPane.showConfirmDialog(QTIExplorePanel.this, "Are you sure you want to delete item with ID "+item.getIdent()+"?", "Delete Item", JOptionPane.OK_CANCEL_OPTION)==JOptionPane.OK_OPTION)
			{
				  //G***del next line
				if(itemTreeNode.getParent() instanceof DefaultMutableTreeNode)  //if the parent is a default mutable tree node
				{
					final DefaultMutableTreeNode parentTreeNode=(DefaultMutableTreeNode)itemTreeNode.getParent(); //cast the parent node to a default mutable tree node
					final Object parentObject=parentTreeNode.getUserObject(); //get the item's parent
					if(parentObject instanceof Section) //if the item is in a section
					{
						((Section)parentObject).getItemList().remove(item); //remove the item from the section
						setModified(true);  //show that the data model has been modified
					}
					else if(parentObject instanceof QTI) //if the item is in QTI
					{
						((QTI)parentObject).getItemList().remove(item); //remove the item from the QTI data model
						setModified(true);  //show that the data model has been modified
					}
						//remove the item tree node from the tree
					((DefaultTreeModel)qtiTree.getModel()).removeNodeFromParent(itemTreeNode);
						//G***fix; find out how to update the selection after deleting the node from the tree
				  updateSelection(qtiTree.getSelectionPath()); //update the selection in case our editing changed things
				}
			}
		}
	}

	/**Action for creating a new question.*/
	class AddItemAction extends AbstractAction
	{
		/**Default constructor.*/
		public AddItemAction()
		{
			super("Add Question...");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Add question");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Create a new question.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_Q));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.QUESTION_NEW_ICON_FILENAME)); //load the correct icon
//G***del when works			new ImageIcon(ReaderFrame.class.getResource("book_open.gif")));	//load the correct icon
//G***del when works			putValue(SMALL_ICON, new ImageIcon(ReaderFrame.class.getResource("book_open.gif")));	//load the correct icon
		}

		/**Called when the action should be performed.
		@param e The event causing the action.
		*/
		public void actionPerformed(final ActionEvent e)
		{
//G***del			final ItemListModel itemListModel=(ItemListModel)itemListPanel.itemList.getModel(); //get the list model being used in the section
			final Item item=askNewItem(); //ask for a new item
			if(item!=null)  //if an item was created
			{
				final MutableTreeNode itemTreeNode=new QTIItemTreeNode(item);  //create a new node for the item
				TreePath path=qtiTree.getSelectionPath(); //get the selected path
				if(path==null)  //if there is no path selected
				  path=TreeUtilities.getLastPath(qtiTree.getModel());  //create a path on the last part of the tree
				while(path!=null) //while we haven't reached the root
				{
					final Object pathComponent=path.getLastPathComponent(); //get the last path component
//G***del Debug.trace("Looking at path component: ", pathComponent.getClass().getName()); //G***del
					if(pathComponent instanceof DefaultMutableTreeNode) //if this is a tree node with a user object
					{
						final DefaultMutableTreeNode treeNode=(DefaultMutableTreeNode)pathComponent;  //cast the path component to the desired type
						final Object userObject=treeNode.getUserObject();  //get the user object
//G***del Debug.trace("Looking at user object: ", userObject.getClass().getName()); //G***del
						if(userObject instanceof Section) //if a section is selected
						{
//G***del Debug.trace("found section");  //G***del
							((Section)userObject).getItemList().add(item); //add the item to the section
								//add the item tree node to the tree
							((DefaultTreeModel)qtiTree.getModel()).insertNodeInto(itemTreeNode, treeNode, treeNode.getChildCount());
							setModified(true);  //show that the data model has been modified
//G***fix							qtiTree.setSelectionPath(path.pathByAddingChild(itemTreeNode)); //select the new item G***don't duplicate path code
							qtiTree.scrollPathToVisible(path.pathByAddingChild(itemTreeNode)); //scroll to the path that includes the added child
							splitPane.resetToPreferredSizes();  //G***testing
							break;  //stop looking for the correct container
						}
						else if(userObject instanceof Assessment) //if an assessment is selected
						{
//G***del Debug.trace("found assessment");  //G***del
						  final Assessment assessment=(Assessment)userObject; //get the selected assessment
							final Section section;  //we'll find out which section to use, or add a new one
							final MutableTreeNode sectionTreeNode; //we'll also find the section node being shown
							if(assessment.getSectionList().size()>0)  //if there are sections
							{
								section=(Section)assessment.getSectionList().get(assessment.getSectionList().size()-1);  //use the last section
								sectionTreeNode=(MutableTreeNode)treeNode.getLastChild();  //get the last section
							}
							else  //if there are no sections
							{
							  section=new Section();  //create new section
								assessment.getSectionList().add(section); //add the section to the list
								sectionTreeNode=new QTISectionTreeNode(section);  //create a new node for the section
									//add the section tree node to the tree
								((DefaultTreeModel)qtiTree.getModel()).insertNodeInto(sectionTreeNode, treeNode, treeNode.getChildCount());
							}
							path=path.pathByAddingChild(sectionTreeNode);  //update our path to include the section
							section.getItemList().add(item); //add the item to the section
								//add the section tree node to the tree
							((DefaultTreeModel)qtiTree.getModel()).insertNodeInto(itemTreeNode, sectionTreeNode, sectionTreeNode.getChildCount());
							setModified(true);  //show that the data model has been modified
							qtiTree.scrollPathToVisible(path.pathByAddingChild(itemTreeNode)); //scroll to the path that includes the added child
							splitPane.resetToPreferredSizes();  //G***testing
							break;  //stop looking for the correct container
						}
						else if(userObject instanceof QTI) //if the QTI data model itself is selected
						{
							((QTI)userObject).getItemList().add(item); //add the item to the QTI data model
								//add the item tree node to the tree
							((DefaultTreeModel)qtiTree.getModel()).insertNodeInto(itemTreeNode, treeNode, treeNode.getChildCount());
							setModified(true);  //show that the data model has been modified
							qtiTree.scrollPathToVisible(path.pathByAddingChild(itemTreeNode)); //scroll to the path that includes the added child
							splitPane.resetToPreferredSizes();  //G***testing
							break;  //stop looking for the correct container
						}
					}
					path=path.getParentPath();  //search up the path
				}
			}
		}
	}

	/**Action for creating a new section.*/
	class AddSectionAction extends AbstractAction
	{
		/**Default constructor.*/
		public AddSectionAction()
		{
			super("Add Section...");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Add section");	//set the short description G***i18n
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
		  setModified(true);  //show that the data model has been modified
		}
	}

}