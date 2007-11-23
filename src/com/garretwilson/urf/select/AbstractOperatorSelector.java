package com.garretwilson.urf.select;

import java.net.URI;

import static com.garretwilson.lang.ObjectUtilities.asInstance;
import static com.garretwilson.urf.select.Select.*;

/**An abstract selector that works as an operator on the results of other selections.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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