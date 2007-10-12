package com.garretwilson.urf.ploop;

import java.net.URI;

import static com.garretwilson.lang.ClassUtilities.*;
import static com.garretwilson.urf.URF.*;

/**General PLOOP definitions.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class PLOOP
{

	/**Determines the URF property URI for a property of a given object.
	@param object The object for which a property URI should be determined.
	@param propertyName The name of the property.
	@return The URF property URI for the object's property.
	@exception NullPointerException if the given object, and/or property name is <code>null</code>.
	*/
	public static URI getPropertyURI(final Object object, final String propertyName)
	{
		final URI namespaceURI=createInfoJavaURI(object.getClass().getPackage());	//the URI of the object's class package will be the namespace of the object's class
		return createResourceURI(namespaceURI, propertyName);	//create and return a property URI for the given property
	}

}