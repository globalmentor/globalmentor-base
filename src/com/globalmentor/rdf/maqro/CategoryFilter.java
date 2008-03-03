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

import com.globalmentor.rdf.RDFLiteral;
import com.globalmentor.rdf.RDFObject;

import static com.globalmentor.rdf.maqro.MAQRO.*;

/**Filter for choosing MAQRO interactions based upon category.
@author Garret Wilson
*/
public class CategoryFilter extends Filter
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return CATEGORY_FILTER_CLASS_NAME;}

	/**Default constructor.*/
	public CategoryFilter()
	{
		super();	//construct the parent class
	}

	/**Constructs selection criteria with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public CategoryFilter(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Adds a category to the interaction.
	@param categoryLiteral A literal category value.
	*/
	public void addCategory(final RDFLiteral categoryLiteral)
	{
		MAQRO.addCategory(this, categoryLiteral);	//add the category to the filter
	}

	/**Adds a category to the filter.
	@param category The category to add.
	@param language The language of the category, or <code>null</code> if
		no language should be specified.
	*/
	public void addCategory(final String category, final Locale language)
	{
		MAQRO.addCategory(this, category, language);	//add the category to the filter
	}

	/**@return An iterable to categories, if any, of the interaction.*/
	public Iterable<RDFObject> getCategories()
	{
		return MAQRO.getCategories(this);	//return an iterator to the categories
	}

}
