package com.garretwilson.util;

import static com.garretwilson.lang.ClassUtilities.*;

/**Indicates the object not only can be modified, it can keep track of whether it has been modified.
@author Garret Wilson
*/
public interface Modifiable
{

	/**The name of the modified property, if it is bound in any modifiable object.*/
	public final static String MODIFIED_PROPERTY=getPropertyName(Modifiable.class, "modified");

	/**@return Whether the object has been modified.*/
	public boolean isModified();

	/**Sets whether the object has been modified.
	@param newModified The new modification status.
	*/
	public void setModified(final boolean newModified);

}