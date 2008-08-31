package com.garretwilson.swing.qti.tree;

import java.util.*;
import javax.swing.tree.*;
import com.garretwilson.assess.qti.*;
import com.globalmentor.mentoract.qti.ResponseLID;

/**A tree node that represents a QTI logical ID response.
	The QTI object is stored as the user object of the tree node.
@author Garret Wilson
*/
public class QTIResponseLIDTreeNode extends DefaultMutableTreeNode
{

	/**Convenience function for retrieving the represented QTI item.
	@return The QTI object this tree node represents, already cast to
		the correct type.
	@see DefaultMutableTreeNode#getUserObject
	*/
	public ResponseLID getResponseLID()
	{
		return (ResponseLID)getUserObject();  //return the user object cast to the correct type of QTI object
	}

	/**Constructs a tree node from a QTI logical ID response.
	@param item The QTI logical ID response to represent in the tree.
	*/
	public QTIResponseLIDTreeNode(final ResponseLID responseLID)
	{
		super(responseLID); //store the QTI object as the user object
		constructChildNodes(); //construct the child nodes
	}

	/**Constructs child nodes for this node, based upon the current data model
		contents. All existing child nodes are first removed.
	*/
	protected void constructChildNodes()
	{
		removeAllChildren(); //remove all the existing child nodes, if any
		final ResponseLID responseLID=getResponseLID(); //get a reference to the logical ID response
/*G***fix
		final Presentation presentation=item.getPresentation(); //get the item presentation
		if(presentation!=null)  //if there is presentation
		{
			add(new QTIPresentationTreeNode(presentation)); //create a node for the presentation and add it to this node
		}
*/
	}

	/**@return A string representation to display as the tree node's label.*/
	public String toString()
	{
		return "Logical ID Response"; //G***fix
/*G***fix
		final Item item=getItem();  //get the item we represent
		final StringBuffer stringBuffer=new StringBuffer(); //create a new string buffer for constructing the string
		  //append "[ID]"
		stringBuffer.append('[');
		stringBuffer.append(item.getIdent());  //append the ID
		stringBuffer.append(']');
			//append the presentation
		final Presentation presentation=item.getPresentation();  //get the presentation information
		if(presentation!=null)  //if there is presentation information
		{
			stringBuffer.append(' '); //append a space
			stringBuffer.append(presentation.toString()); //append the string representation of the presentation
		}
		return stringBuffer.toString(); //return the string we constructed
*/
	}
}