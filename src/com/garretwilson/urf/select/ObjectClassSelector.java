package com.garretwilson.urf.select;

import java.net.URI;

import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.select.Select.*;

/**A selector that selects an object based upon its runtime class (as opposed to its URF resource type).
This is a convenience class; identical functionality may be obtained by using an {@link ObjectPropertySelector} with a property name of "class"
and a {@link URISelector} subselector specifying the class's URI.
This implementation lazily caches any selected class, so that any later updates to the selected class will not be reflected in {@link #getSelectClass()}.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class ObjectClassSelector extends AbstractSelector
{

	/**The lazily-cached select class.*/
	private Class<?> selectClass=null;

	/**Default constructor.*/
	public ObjectClassSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public ObjectClassSelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Returns the select class identified by this selector.
	This implementation permanently caches locally any select class.
	The property value is expected to be a resource with a <code>info:java/</code> URI.
	@return This selector's class designation, or <code>null</code> if this rule has no <code>select:selectClass</code> property that is a Java class.
	@exception ClassNotFoundException if the class identified by the <code>select:selectClass</code> property cannot be found.
	*/
	public Class<?> getSelectClass() throws ClassNotFoundException
	{
		if(selectClass==null)	//if we haven't cached a select class (or there has never been a select class found); the race condition here is benign
		{
			selectClass=asClass(getPropertyValue(SELECT_CLASS_PROPERTY_URI));	//get the select:selectClass property as a Java class
		}
		return selectClass;	//return the class, if any
	}

	/**Determines if this selector selects a given object.
	This version checks to see if the object is an instance of the selected class.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@exception IllegalStateException if this selector doesn't specify a selected class.
	@see #getSelectClass()
	@see Class#isInstance(Object)
	*/
	public boolean selects(final Object object)
	{
		try
		{
			final Class<?> objectClass=getSelectClass();	//get the designated class
			if(objectClass!=null)	//if a class is specified
			{
				if(!objectClass.isInstance(object))	//if the object is not an instance of the specified class
				{
					return false;	//the object doesn't match
				}
			}
			else
			{
				throw new IllegalStateException("Class selector missing class selection property.");
			}
			return true;	//the object passed all the tests
		}
		catch(final ClassNotFoundException classNotFoundException)	//if an unknown class is found
		{
			throw new IllegalStateException(classNotFoundException);	//this error would have probably already been found by now
		}
	}
}