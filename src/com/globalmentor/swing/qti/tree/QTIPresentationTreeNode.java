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

package com.globalmentor.swing.qti.tree;

import java.util.*;
import javax.swing.tree.*;
import com.globalmentor.mentoract.qti.*;

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
				//TODO fix for other response types
		}
	}

	/**@return A string representation to display as the tree node's label.*/
	public String toString()
	{
		return "Presentation";  //TODO i18n
	}
}