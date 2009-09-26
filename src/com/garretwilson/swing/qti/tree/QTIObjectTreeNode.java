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