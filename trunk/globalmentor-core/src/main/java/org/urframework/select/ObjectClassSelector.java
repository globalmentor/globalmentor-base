/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.urframework.select;

import java.net.URI;

import static org.urframework.URF.*;
import static org.urframework.select.Select.*;

/**A selector that selects an object based upon its runtime class (as opposed to its URF resource type).
This is a convenience class; identical functionality may be obtained by using an {@link ObjectPropertySelector} with a property name of "class"
and a {@link URISelector} subselector specifying the class's URI.
This implementation lazily caches any selected class, so that any later updates to the selected class will not be reflected in {@link #getSelectClass()}.
@author Garret Wilson
*/
public class ObjectClassSelector extends AbstractSelector
{

	/**The lazily-cached select class.*/
	private Class<?> selectClass=null;

	/**Default constructor.*/
	public ObjectClassSelector()
	{
		this((URI)null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource, or <code>null</code> if this resource has no URI.
	*/
	public ObjectClassSelector(final URI uri)
	{
		this(uri, null);  //construct the class with no select class
	}

	/**Class constructor.
	@param selectClass The select class identified by this selector, or <code>null</code> if no select class is specified.
	*/
	public ObjectClassSelector(final Class<?> selectClass)
	{
		this(null, selectClass);	//construct the class with no URI
	}

	/**URI and class constructor.
	@param uri The URI for the new resource, or <code>null</code> if this resource has no URI.
	@param selectClass The select class identified by this selector, or <code>null</code> if no select class is specified.
	*/
	public ObjectClassSelector(final URI uri, final Class<?> selectClass)
	{
		super(uri);  //construct the parent class
		if(selectClass!=null)	//if there is a select class
		{
			setPropertyValue(SELECT_CLASS_PROPERTY_URI, DEFAULT_URF_RESOURCE_FACTORY.createClassResource(selectClass));	//create a new resource for the class and use it to set the select class value
			this.selectClass=selectClass;	//cache the class so we won't have to load it later
		}
	}

	/**Returns the select class identified by this selector.
	This implementation permanently caches locally any select class.
	The property value is expected to be a resource with a Java URI.
	@return This selector's class designation, or <code>null</code> if this rule has no <code>selectClass</code> property that is a Java class.
	@exception ClassNotFoundException if the class identified by the <code>selectClass</code> property cannot be found.
	@see Select#SELECT_CLASS_PROPERTY_URI
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
				throw new IllegalStateException("Object class selector missing class selection property.");
			}
			return true;	//the object passed all the tests
		}
		catch(final ClassNotFoundException classNotFoundException)	//if an unknown class is found
		{
			throw new IllegalStateException(classNotFoundException);	//this error would have probably already been found by now
		}
	}
}