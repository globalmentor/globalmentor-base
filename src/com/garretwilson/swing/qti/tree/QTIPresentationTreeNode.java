package com.garretwilson.swing.qti.tree;

import java.util.*;
import javax.swing.tree.*;
import com.garretwilson.assess.qti.*;
import com.globalmentor.mentoract.qti.Material;
import com.globalmentor.mentoract.qti.Presentation;
import com.globalmentor.mentoract.qti.Response;
import com.globalmentor.mentoract.qti.ResponseLID;

/**A tree node that represents QTI presentation.
	The QTI object is stored as the user object of the tree node.
@author Garret Wilson
*/
public class QTIPresentationTreeNode extends DefaultMutableTreeNode
{

	/**Convenience function for retrieving the represented QTI item.
	@return The QTI object this tree node represents, already cast to
		the correct type.
	@see DefaultMutableTreeNode#getUserObject
	*/
	public Presentation getPresentation()
	{
		return (Presentation)getUserObject();  //return the user object cast to the correct type of QTI object
	}

	/**Constructs a tree node from QTI presentation.
	@param presentation The QTI presentation to represent in the tree.
	*/
	public QTIPresentationTreeNode(final Presentation presentation)
	{
		super(presentation); //store the QTI object as the user object
		constructChildNodes(); //construct the child nodes
	}

	/**Constructs child nodes for this node, based upon the current data model
		contents. All existing child nodes are first removed.
	*/
	protected void constructChildNodes()
	{
		removeAllChildren(); //remove all the existing child nodes, if any
		final Presentation presentation=getPresentation(); //get a reference to the item
		final Material material=presentation.getMaterial(); //get the material
		if(material!=null)  //if there is presentation
		{
			add(new QTIMaterialTreeNode(material)); //create a node for the material and add it to this node
		}
			//add nodes for the responses
		final Iterator responseIterator=presentation.getResponseList().iterator(); //get an iterator to all the responses
		while(responseIterator.hasNext()) //while there are more responses
		{
			final Response response=(Response)responseIterator.next();  //get the next response
			if(response instanceof ResponseLID) //if this is a logical ID response
				add(new QTIResponseLIDTreeNode((ResponseLID)response)); //create a node for the logical ID response and add it to this node
				//G***fix for other response types
		}
	}

	/**@return A string representation to display as the tree node's label.*/
	public String toString()
	{
		return "Presentation";  //G***i18n
	}
}