package com.garretwilson.urf.select;

/**A selector that works as an operator on the results of other selections.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public interface OperatorSelector extends Selector
{

	/**@return This operator selector's select declarations.*/
	public Iterable<Selector> getSelectors();

	/**Adds a selector to this operator selector.
	@param selector The selector to add.
	*/
	public void addSelector(final Selector selector);

}