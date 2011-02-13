/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf.select;

import java.net.URI;

import static com.globalmentor.java.Objects.asInstance;
import static com.globalmentor.urf.select.Select.*;

/**An abstract selector that works as an operator on the results of other selections.
@author Garret Wilson
*/
public abstract class AbstractOperatorSelector extends AbstractSelector implements OperatorSelector
{

	/**Default constructor.*/
	public AbstractOperatorSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public AbstractOperatorSelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Returns the selector identified by this selector.
	@return This selector's first select declaration, or <code>null</code> if this rule has no <code>selector</code> property or the value is not a {@link Selector}.
	@see Select#SELECTOR_PROPERTY_URI
	*/
	public Selector getSelector()
	{
		return asInstance(getPropertyValue(SELECTOR_PROPERTY_URI), Selector.class);	//return the select.select value
	}

	/**@return This operator selector's selector declarations.
	@see Select#SELECTOR_PROPERTY_URI
	*/
	public Iterable<Selector> getSelectors()
	{
		return getPropertyValues(SELECTOR_PROPERTY_URI, Selector.class);	//return the select.select values
	}

	/**Adds a selector to this operator selector.
	@param selector The selector to add.
	@see Select#SELECTOR_PROPERTY_URI
	*/
	public void addSelector(final Selector selector)
	{
		addPropertyValue(SELECTOR_PROPERTY_URI, selector);	//add the given selector
	}
}