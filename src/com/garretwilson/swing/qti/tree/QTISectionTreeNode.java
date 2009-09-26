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

import java.util.*;
import javax.swing.tree.*;
import com.globalmentor.mentoract.qti.*;

/**A tree node that represents a QTI section.
	The QTI object is stored as the user object of the tree node.
@author Garret Wilson
*/
public class QTISectionTreeNode extends DefaultMutableTreeNode
{

	/**Convenience function for retrieving the represented QTI section.
	@return The QTI object this tree node represents, already cast to
		the correct type.
	@see DefaultMutableTreeNode#getUserObject
	*/
	public Section getSection()
	{
		return (Section)getUserObject();  //return the user object cast to the correct type of QTI object
	}

	/**Constructs a tree node from a QTI section.
	@param section The QTI section to represent in the tree.
	*/
	public QTISectionTreeNode(final Section section)
	{
		super(section); //store the QTI object as the user object
		constructChildNodes(); //construct the child nodes
	}

	/**Constructs child nodes for this node, based upon the current data model
		contents. All existing child nodes are first removed.
	*/
	protected void constructChildNodes()
	{
		removeAllChildren(); //remove all the existing child nodes, if any
		final Section section=getSection(); //get a reference to the section
			//add nodes for the items
		final Iterator itemIterator=section.getItemList().iterator(); //get an iterator to all the items
		while(itemIterator.hasNext()) //while there are more items
		{
			final Item item=(Item)itemIterator.next();  //get the next item
			add(new QTIItemTreeNode(item)); //create a node for the item and add it to this node
		}
	}

	/**@return A string representation to display as the tree node's label.*/
	public String toString()
	{
		final Section section=getSection();  //get the section we represent
		final String title=section.getTitle(); //get the title
		if(title!=null && title.length()>0) //if there is a title
		{
			return title; //return the title
		}
		else  //if there is no title
		{
			final String id=section.getIdent();  //get the ID
			if(id!=null && id.length()>0) //if there is an ID
			{
				return new StringBuffer().append('[').append(id).append(']').toString();  //return "[id]"
			}
			else  //if there is no ID
			  return "(untitled)"; //show that this section has no title TODO i18n
		}
	}
}