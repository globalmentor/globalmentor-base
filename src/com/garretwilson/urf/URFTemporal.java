package com.garretwilson.urf;

/**The base for all URF temporal types.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public interface URFTemporal
{

	/**Appends the canonical lexical representation of this temporal.
	@param stringBuild The string builder to which the lexical representation will be appended.
	@return The string builder.
	*/
	public StringBuilder append(final StringBuilder stringBuilder);

	/**Returns the canonical lexical representation of this temporal
	@return The canonical lexical representation of this temporal.
	*/
	public String toString();

}
