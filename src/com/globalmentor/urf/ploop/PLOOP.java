package com.globalmentor.urf.ploop;

import java.net.URI;

import static com.globalmentor.java.Classes.*;
import static com.globalmentor.urf.URF.*;

/**General PLOOP definitions.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class PLOOP
{

	/**Determines the URF property URI for a property of a given object.
	The namespace of the property URI will be the URI of the object's class.
	@param object The object for which a property URI should be determined.
	@param propertyName The name of the property.
	@return The URF property URI for the object's property.
	@exception NullPointerException if the given object and/or property name is <code>null</code>.
	*/
	public static URI createPropertyURI(final Object object, final String propertyName)
	{
		return createPropertyURI(object.getClass(), propertyName);	//create a property URI from the object's class
	}

	/**Determines the URF property URI for a property of a given class.
	The namespace of the property URI will be the URI of the class.
	@param objectClass The class for which a property URI should be determined.
	@param propertyName The name of the property.
	@return The URF property URI for the class' property.
	@exception NullPointerException if the given class and/or property name is <code>null</code>.
	*/
	public static URI createPropertyURI(final Class<?> objectClass, final String propertyName)
	{
		final URI namespaceURI=createJavaURI(objectClass);	//the URI of the class will be the namespace of the property URI
		return createResourceURI(namespaceURI, propertyName);	//create and return a property URI for the given property
	}

}