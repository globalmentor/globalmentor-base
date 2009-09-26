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
import com.globalmentor.mentoract.qti.Assessment;
import com.globalmentor.mentoract.qti.Section;

/**A tree node that represents a QTI assessment.
	The QTI object is stored as the user object of the tree node.
@author Garret Wilson
*/
public class QTIAssessmentTreeNode extends DefaultMutableTreeNode
{

	/**Convenience function for retrieving the represented QTI assessment.
	@return The QTI object this tree node represents, already cast to
		the correct type.
	@see DefaultMutableTreeNode#getUserObject
	*/
	public Assessment getAssessment()
	{
		return (Assessment)getUserObject();  //return the user object cast to the correct type of QTI object
	}

	/**Constructs a tree node from a QTI assessment.
	@param assessment The QTI assessment to represent in the tree.
	*/
	public QTIAssessmentTreeNode(final Assessment assessment)
	{
		super(assessment); //store the QTI object as the user object
		constructChildNodes(); //construct the child nodes
	}

	/**Constructs child nodes for this node, based upon the current data model
		contents. All existing child nodes are first removed.
	*/
	protected void constructChildNodes()
	{
		removeAllChildren(); //remove all the existing child nodes, if any
		final Assessment assessment=getAssessment(); //get a reference to the assessment
			//add nodes for the sections
		final Iterator sectionIterator=assessment.getSectionList().iterator(); //get an iterator to the sections
		while(sectionIterator.hasNext()) //while there are more sections
		{
		  final Section section=(Section)sectionIterator.next();  //get the next section
			add(new QTISectionTreeNode(section)); //create a node for the section and add it to this node
		}
	}

	/**@return A string representation to display as the tree node's label.*/
	public String toString()
	{
		final Assessment assessment=getAssessment();  //get the assessment we represent
		final String title=assessment.getTitle(); //get the title
		if(title!=null && title.length()>0) //if there is a title
		{
			return title; //return the title
		}
		else  //if there is no title
		{
			final String id=assessment.getIdent();  //get the ID
			if(id!=null && id.length()>0) //if there is an ID
			{
				return new StringBuffer().append('[').append(id).append(']').toString();  //return "[id]"
			}
			else  //if there is no ID
			  return "(untitled)"; //show that this assessment has no title TODO i18n
		}
	}
}