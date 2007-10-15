package com.garretwilson.urf.select;

import java.net.URI;

import com.garretwilson.urf.*;
import static com.garretwilson.urf.select.Select.*;

/**An abstract selector.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public abstract class AbstractSelector extends AbstractClassTypedURFResource implements Selector
{

	/**Default constructor.*/
	public AbstractSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public AbstractSelector(final URI uri)
	{
		super(uri, SELECT_NAMESPACE_URI);  //construct the parent class
	}
}