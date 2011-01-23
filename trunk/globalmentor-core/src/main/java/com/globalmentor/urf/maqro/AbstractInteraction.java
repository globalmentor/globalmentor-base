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

package com.globalmentor.urf.maqro;

import java.net.URI;
import java.util.*;

import com.globalmentor.urf.AbstractClassTypedURFResource;

import static com.globalmentor.urf.maqro.MAQRO.*;

/**Abstract implementation of an interaction which can be part of a MAQRO activity.
@author Garret Wilson
*/
public abstract class AbstractInteraction extends AbstractClassTypedURFResource implements Interaction
{

	/**URI constructor with a type namespace of {@value MAQRO#MAQRO_NAMESPACE_URI}.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	*/
	public AbstractInteraction(final URI uri)
	{
		this(uri, MAQRO_NAMESPACE_URI);	//construct the parent class
	}

	/**URI and type namespace URI constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeNamespaceURI The namespace URI of the URI of the type to be added.
	@exception NullPointerException if the given type type namespace URI is <code>null</code>.
	*/
	public AbstractInteraction(final URI uri, final URI typeNamespaceURI)
	{
		super(uri, typeNamespaceURI);	//construct the parent class
	}

	/**Adds a category to the interaction.
	@param category The category to add.
	@param language The language of the category, or <code>null</code> if
		no language should be specified.
	*/
/*TODO fix
	public void addCategory(final String category, final Locale language)
	{
		MAQRO.addCategory(this, category, language);	//add the category to the interaction
	}
*/

	/**@return An iterable to categories, if any, of the interaction.*/
/*TODO fix
	public Iterable<RDFObject> getCategories()
	{
		return MAQRO.getCategories(this);	//return an iterable to the categories
	}
*/

	/**Determines if the interaction has a category in the given category set.
	<p>If the category set contains <code>NO_CATEGORY</code>, this method returns
		<code>true</code> if the interactions has no categories specified.</p>
	@param categorySet The set of categories, any category of which will allow
		the interaction to be selected.
	@return <code>true</code> if the interaction has a category that is
		included in the category set.
	*/
/*TODO fix
	public boolean hasCategory(final Set categorySet)
	{
		return MAQRO.hasCategory(this, categorySet);	//see whether this category has one of the supplied categories
	}
*/

}
