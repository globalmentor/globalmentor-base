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

package org.urframework.maqro;

import java.net.URI;
import java.util.*;

import static org.urframework.maqro.MAQRO.*;

/**Filter for choosing MAQRO interactions based upon category.
@author Garret Wilson
*/
public class CategoryFilter extends AbstractFilter
{

	/**Default constructor.*/
	public CategoryFilter()
	{
		this(null);	//construct the class with no URI
	}

	/**Constructs selection criteria with a URI.
	@param uri The URI for the new resource.
	*/
	public CategoryFilter(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Adds a category to the interaction.
	@param categoryLiteral A literal category value.
	*/
/*TODO fix
	public void addCategory(final RDFLiteral categoryLiteral)
	{
		MAQRO.addCategory(this, categoryLiteral);	//add the category to the filter
	}
*/

	/**Adds a category to the filter.
	@param category The category to add.
	@param language The language of the category, or <code>null</code> if
		no language should be specified.
	*/
/*TODO fix
	public void addCategory(final String category, final Locale language)
	{
		MAQRO.addCategory(this, category, language);	//add the category to the filter
	}
*/

	/**@return An iterable to categories, if any, of the interaction.*/
/*TODO fix
	public Iterable<RDFObject> getCategories()
	{
		return MAQRO.getCategories(this);	//return an iterator to the categories
	}
*/

}
