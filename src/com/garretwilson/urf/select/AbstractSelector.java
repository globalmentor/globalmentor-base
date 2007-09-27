package com.garretwilson.urf.select;

import java.net.URI;

import com.garretwilson.urf.*;
import static com.garretwilson.urf.select.Select.*;

/**An abstract selector.
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