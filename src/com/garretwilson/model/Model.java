package com.garretwilson.model;

import static com.garretwilson.lang.ClassUtilities.*;
import com.garretwilson.util.Modifiable;

/**Indicates that an implementing class is a data model.
@author Garret Wilson
*/
public interface Model //G***del if not needed extends Modifiable
{
	/**The property for the data model for which a component provides a view.*/
	public final String MODEL_PROPERTY=getFullName(Model.class, "model");
	
}
