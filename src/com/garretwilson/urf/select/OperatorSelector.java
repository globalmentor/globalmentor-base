package com.garretwilson.urf.select;

/**A selector that works as an operator on the results of other selections.
@author Garret Wilson
*/
public interface OperatorSelector extends Selector
{

	/**@return This operator selector's select declarations.*/
	public Iterable<Selector> getSelects();

	/**Adds a selector to this operator selector.
	@param selector The selector to add.
	*/
	public void addSelect(final Selector selector);

}