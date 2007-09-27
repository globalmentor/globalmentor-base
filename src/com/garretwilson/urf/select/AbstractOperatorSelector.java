package com.garretwilson.urf.select;

import java.net.URI;

import static com.garretwilson.urf.select.Select.*;

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

	/**@return This operator selector's select declarations.*/
	public Iterable<Selector> getSelects()
	{
		return getPropertyValues(SELECT_PROPERTY_URI, Selector.class);	//return the select:select values
	}

	/**Adds a selector to this operator selector.
	@param selector The selector to add.
	*/
	public void addSelect(final Selector selector)
	{
		addPropertyValue(SELECT_PROPERTY_URI, selector);	//add the given selector
	}
}