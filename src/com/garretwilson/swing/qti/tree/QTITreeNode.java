package com.garretwilson.swing.qti.tree;

import java.util.*;
import javax.swing.tree.*;
import com.garretwilson.assess.qti.*;

/**A tree node that represents a QTI data model.
	The QTI object is stored as the user object of the tree node.
@author Garret Wilson
*/
public class QTITreeNode extends DefaultMutableTreeNode
{

	/**Convenience function for retrieving the represented QTI data model.
	@return The QTI data model this tree node represents, already cast to
		the correct type.
	@see DefaultMutableTreeNode#getUserObject
	*/
	public QTI getQTI()
	{
		return (QTI)getUserObject();  //return the user object cast to the QTI data model type
	}

	/**Constructs a tree node from QTI data model.
	@param qti The QTI data model to represent in the tree.
	*/
	public QTITreeNode(final QTI qti)
	{
		super(qti); //store the QTI data model as the user object
		constructChildNodes(); //construct the child nodes
	}

	/**Constructs child nodes for this node, based upon the current data model
		contents. All existing child nodes are first removed.
	*/
	protected void constructChildNodes()
	{
		removeAllChildren(); //remove all the existing child nodes, if any
		final QTI qti=getQTI(); //get a reference to the QTI data model
			//add nodes for the assessments
		final Iterator assessmentIterator=qti.getAssessmentList().iterator(); //get an iterator to the assessments
		while(assessmentIterator.hasNext()) //while there are more assessments
		{
		  final Assessment assessment=(Assessment)assessmentIterator.next();  //get the next assessment
			add(new QTIAssessmentTreeNode(assessment)); //create a node for the assessment and add it to this node
		}
			//add nodes for the sections
		final Iterator sectionIterator=qti.getSectionList().iterator(); //get an iterator to the sections
		while(sectionIterator.hasNext()) //while there are more sections
		{
		  final Section section=(Section)sectionIterator.next();  //get the next section
			add(new QTISectionTreeNode(section)); //create a node for the section and add it to this node
		}
			//add nodes for the items
		final Iterator itemIterator=qti.getItemList().iterator(); //get an iterator to all the items
		while(itemIterator.hasNext()) //while there are more items
		{
			final Item item=(Item)itemIterator.next();  //get the next item
			add(new QTIItemTreeNode(item)); //create a node for the item and add it to this node
		}
	}

	/**@return A string representation to display as the tree node's label: "QTI".*/
	public String toString()
	{
		return "QTI"; //return the string representing the QTI data model G***i18n; G***what about showing the filename, if any?
	}
}