package com.garretwilson.util;

import java.util.*;

/**A set that implements <code>Modifiable</code> so that it can keep
	track of whether it has been modified. 
<p>This class is meant as a wrapper to an existing set.</p>
@author Garret Wilson
@see Modifiable
*/
public class ModifiableSet extends ModifiableCollection implements Set
{

	/**Set constructor.
	@param set The set this set should wrap.
	*/
	public ModifiableSet(final Set set)
	{
		super(set);	//construct the parent class with the set
	}

}
