package com.garretwilson.util;

import com.garretwilson.lang.BooleanUtilities;

/**A default implementation of a modifiable object. The object defaults to not
	having been modified.
<p>Bound properties:</p>
<dl>
	<dt><code>Modifiable.MODIFIED_PROPERTY_NAME</code> (<code>Boolean</code>)</dt>
	<dd>Indicates a the boolean modified property has been changed.</dd>
</dl>

<ul>
	<li><code>Modifiable.MODIFIED_PROPERTY_NAME</code> (<code>Boolean</code>)</li>
</ul>
@author Garret Wilson
@see Modifiable#MODIFIED_PROPERTY_NAME
*/
public class DefaultModifiable extends BoundPropertyObject implements Modifiable
{

	/**Whether the object has been modified; the default is not modified.*/
	private boolean modified=false;

		/**@return Whether the object been modified.*/
		public boolean isModified() {return modified;}

		/**Sets whether the object has been modified.
			This is a bound property.
		@param newModified The new modification status.
		*/
		public void setModified(final boolean newModified)
		{
			final boolean oldModified=modified; //get the old modified value
			if(oldModified!=newModified)  //if the value is really changing
			{
				modified=newModified; //update the value
					//show that the modified property has changed
				firePropertyChange(MODIFIED_PROPERTY_NAME, BooleanUtilities.toBoolean(oldModified), BooleanUtilities.toBoolean(newModified));
			}
		}
}