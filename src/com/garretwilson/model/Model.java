package com.garretwilson.model;

import com.garretwilson.lang.JavaConstants;
import com.garretwilson.util.Modifiable;

/**Indicates that an implementing class is a data model.
@author Garret Wilson
*/
public interface Model extends Modifiable
{

	/**The property for the data model for which a component provides a view.*/
	public final String MODEL_PROPERTY=Model.class.getName()+JavaConstants.PACKAGE_SEPARATOR+"model";
	
}
