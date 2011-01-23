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

import javax.swing.tree.*;
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
/*TODO fix
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
		return "Logical ID Response"; //TODO fix
/*TODO fix
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