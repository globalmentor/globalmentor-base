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

package com.globalmentor.rdf.maqro;

import java.net.URI;
import java.util.*;

import com.globalmentor.rdf.*;
import static com.globalmentor.rdf.maqro.MAQRO.*;

/**Designates an object is an interaction that can be part of a MAQRO activity.
@author Garret Wilson
*/
public abstract class Interaction extends TypedRDFResource
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**Default constructor.*/
	public Interaction()
	{
		super();	//construct the parent class
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Interaction(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Adds a category to the interaction.
	@param category The category to add.
	@param language The language of the category, or <code>null</code> if
		no language should be specified.
	*/
	public void addCategory(final String category, final Locale language)
	{
		MAQRO.addCategory(this, category, language);	//add the category to the interaction
	}

	/**@return An iterable to categories, if any, of the interaction.*/
	public Iterable<RDFObject> getCategories()
	{
		return MAQRO.getCategories(this);	//return an iterable to the categories
	}

	/**Determines if the interaction has a category in the given category set.
	<p>If the category set contains <code>NO_CATEGORY</code>, this method returns
		<code>true</code> if the interactions has no categories specified.</p>
	@param categorySet The set of categories, any category of which will allow
		the interaction to be selected.
	@return <code>true</code> if the interaction has a category that is
		included in the category set.
	*/
	public boolean hasCategory(final Set categorySet)
	{
		return MAQRO.hasCategory(this, categorySet);	//see whether this category has one of the supplied categories
	}

}
