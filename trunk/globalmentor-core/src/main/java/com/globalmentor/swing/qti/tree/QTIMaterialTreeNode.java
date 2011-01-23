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
import com.globalmentor.mentoract.qti.Material;

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
/*TODO del when works
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
		return "Material: "+getMaterial().toString();  //return a string version of the material TODO fix to work with other material types TODO i18n
	}
}