package com.garretwilson.util;

import java.util.Map;

/**An object that can have arbitrary byte arrays attached to it.
@author Garret Wilson
*/
public interface BinaryObjectHolder //G***do we want to rename this to AttachmentOwner or something?
{

	/**@return A map of binary objects, each an array of bytes keyed to some ID
		string.
	*/
	public Map getBinaryObjectMap();

}
