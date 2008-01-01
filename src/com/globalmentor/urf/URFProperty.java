package com.globalmentor.urf;

import java.net.URI;

/**An encapsulation of a parent scope, property URI, value, and the associated property-value scope.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public interface URFProperty extends URFValueContext
{

	/**@return The scope to which the property belongs, or <code>null</code> if this is a property definition not attached to any scope.*/
	public URFScope getSubjectScope();

	/**@return The URI of the property.*/
	public URI getPropertyURI();

}
