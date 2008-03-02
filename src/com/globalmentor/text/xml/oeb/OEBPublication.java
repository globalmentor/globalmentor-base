/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text.xml.oeb;

import java.util.*;
import java.net.*;
import com.garretwilson.rdf.xeb.Book;

/**Class which represents an OEB publication.
@author Garret Wilson
*/
public class OEBPublication extends Book
{

	/**The list of guides, in the order they are specified in the publication.
	@see OEBGuide
	*/
	private List<OEBGuide> guideList=new ArrayList<OEBGuide>();

		/**@return The list of guides, in the order they are specified in the publication.
		@see OEBGuide
		*/
		protected List<OEBGuide> getGuideList() {return guideList;}

		/**Sets a new list to hold guides.
		@param newGuideList The new list to hold guides.
		@see OEBGuide
		*/
		private void setGuideList(final List<OEBGuide> newGuideList) {guideList=newGuideList;}

	/**Adds a guide to the list of guides.
	@param oebGuide The guide to add.
	*/
	public void addGuide(final OEBGuide oebGuide)
	{
		getGuideList().add(oebGuide);
	}

	/**Adds a guide to the list of guides at the specified index.
	@param index The index at which the guide should be added.
	@param oebGuide The guide to add.
	*/
	public void addGuide(final int index, final OEBGuide oebGuide)
	{
		getGuideList().add(index, oebGuide);
	}

	/**@return An interator of all available guides.*/
	public Iterator getGuideIterator()
	{
		return getGuideList().iterator(); //return an iterator of the guides
	}

	/**Default constructor.*/
	public OEBPublication()
	{
		super();	//construct the parent class
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	protected OEBPublication(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
