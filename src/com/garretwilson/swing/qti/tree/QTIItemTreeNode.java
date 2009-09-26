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

package com.garretwilson.swing.qti.tree;

import javax.swing.tree.*;
import com.globalmentor.mentoract.qti.Item;
import com.globalmentor.mentoract.qti.Presentation;

/**A tree node that represents a QTI item.
	The QTI object is stored as the user object of the tree node.
@author Garret Wilson
*/
public class QTIItemTreeNode extends QTIObjectTreeNode
{

	/**Convenience function for retrieving the represented QTI item.
	@return The QTI object this tree node represents, already cast to
		the correct type.
	@see DefaultMutableTreeNode#getUserObject
	*/
	public Item getItem()
	{
		return (Item)getUserObject();  //return the user object cast to the correct type of QTI object
	}

	/**Constructs a tree node from a QTI item.
	@param item The QTI item to represent in the tree.
	*/
	public QTIItemTreeNode(final Item item)
	{
		super(item); //store the QTI object as the user object
	}

	/**Constructs child nodes for this node, based upon the current data model
		contents. All existing child nodes are first removed.
	*/
	public void reload()
	{
		removeAllChildren(); //remove all the existing child nodes, if any
		final Item item=getItem(); //get a reference to the item
		final Presentation presentation=item.getPresentation(); //get the item presentation
		if(presentation!=null)  //if there is presentation
		{
			add(new QTIPresentationTreeNode(presentation)); //create a node for the presentation and add it to this node
		}
	}

	/**@return A string representation to display as the tree node's label.*/
	public String toString()
	{
		final Item item=getItem();  //get the item we represent
		final StringBuffer stringBuffer=new StringBuffer(); //create a new string buffer for constructing the string
		  //append "[ID]"
		stringBuffer.append('[');
		if(item.getIdent().length()<=16)  //TODO testing; fix
		  stringBuffer.append(item.getIdent());  //append the ID
		else  //TODO testing; fix
		  stringBuffer.append("..."+item.getIdent().substring(item.getIdent().length()-16));  //append the ID TODO fix
		stringBuffer.append(']');
			//append the presentation
		final Presentation presentation=item.getPresentation();  //get the presentation information
		if(presentation!=null)  //if there is presentation information
		{
			stringBuffer.append(' '); //append a space
			stringBuffer.append(presentation.toString()); //append the string representation of the presentation
		}
		return stringBuffer.toString(); //return the string we constructed
	}
}