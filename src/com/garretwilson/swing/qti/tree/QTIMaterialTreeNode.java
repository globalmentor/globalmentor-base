package com.garretwilson.swing.qti.tree;

import javax.swing.tree.*;
import com.garretwilson.assess.qti.*;

/**A tree node that represents QTI material.
	The QTI object is stored as the user object of the tree node.
@author Garret Wilson
*/
public class QTIMaterialTreeNode extends DefaultMutableTreeNode
{

	/**Convenience function for retrieving the represented QTI item.
	@return The QTI object this tree node represents, already cast to
		the correct type.
	@see DefaultMutableTreeNode#getUserObject
	*/
	public Material getMaterial()
	{
		return (Material)getUserObject();  //return the user object cast to the correct type of QTI object
	}

	/**Constructs a tree node from QTI material.
	@param material The QTI material to represent in the tree.
	*/
	public QTIMaterialTreeNode(final Material material)
	{
		super(material); //store the QTI object as the user object
		constructChildNodes(); //construct the child nodes
	}

	/**Constructs child nodes for this node, based upon the current data model
		contents. All existing child nodes are first removed.
	*/
	protected void constructChildNodes()
	{
		removeAllChildren(); //remove all the existing child nodes, if any
/*G***del when works
		final Material material=getMaterial(); //get a reference to the material
		material.
		final Presentation presentation=item.getPresentation(); //get the item presentation
		if(presentation!=null)  //if there is presentation
		{
			add(new QTIMaterialTreeNode(item)); //create a node for the presentation and add it to this node
		}
*/
	}

	/**@return A string representation to display as the tree node's label.*/
	public String toString()
	{
		return "Material: "+getMaterial().toString();  //return a string version of the material G***fix to work with other material types G***i18n
	}
}