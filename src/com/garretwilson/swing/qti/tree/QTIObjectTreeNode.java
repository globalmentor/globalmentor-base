package com.garretwilson.swing.qti.tree;

import javax.swing.tree.*;
import com.garretwilson.text.xml.qti.*;

/**A tree node that represents a QTI object.
	The QTI object is stored as the user object of the tree node.
	Children are automatically loaded, and a reload facility is given.
@author Garret Wilson
*/
public abstract class QTIObjectTreeNode extends DefaultMutableTreeNode
{

	/**Constructs a tree node from a QTI object.
	@param qtiObject The QTI object to represent in the tree.
	@see #reload
	*/
	public QTIObjectTreeNode(final Object qtiObject)
	{
		super(qtiObject); //store the QTI object as the user object
		reload(); //construct the child nodes
	}

	/**Sets the user object for this node to <code>userObject</code> and reloads
		the content of the object.
	@param userObject The object that constitutes this node's user-specified data.
	@see #reload
	*/
	public void setUserObject(Object userObject)
	{
		super.setUserObject(userObject);  //set the object normally
		reload(); //construct the child nodes
	}

	/**Constructs child nodes for this node, based upon the current data model
		contents. All existing child nodes are first removed.
	*/
	public abstract void reload();
}