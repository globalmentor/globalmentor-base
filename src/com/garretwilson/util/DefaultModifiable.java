package com.garretwilson.util;

/**A default implementation of a modifiable object. The object defaults to not
	having been modified.
@author Garret Wilson
*/
public class DefaultModifiable implements Modifiable
{

	/**Default constructor.*/
	public DefaultModifiable() {}

	/**Whether the object has been modified; the default is not modified.*/
	private boolean modified=false;

	/**@return Whether the object has been modified.*/
	public boolean isModified() {return modified;}

	/**Sets whether the object has been modified.
	@param newModified The new modification status.
	*/
	public void setModified(final boolean newModified) {modified=newModified;}

}