package com.garretwilson.urf.select;

import java.lang.reflect.InvocationTargetException;
import java.net.URI;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.select.Select.*;

/**A selector that selects an object based upon a runtime property value (as opposed to an URF property value).
The property specified by this selector is used to retrieve a property value from the given object and compare it with the specified selector in this class.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class ObjectPropertySelector extends AbstractSelector
{

	/**Default constructor.*/
	public ObjectPropertySelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public ObjectPropertySelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Returns the name of the property identified by this selector.
	@return This selector's property name designation, or <code>null</code> if this selector has no <code>selectPropertyName</code> property with a string value.
	*/
	public String getSelectPropertyName()
	{
		return asString(getPropertyValue(SELECT_PROPERTY_NAME_PROPERTY_URI));	//get the selectPropertyName property as a string
	}

	/**Sets the property name identified by this selector.
	@param selectPropertyName The name of the property to be selected.
	*/
	public void setSelectPropertyName(final String selectPropertyName)
	{
		setPropertyValue(SELECT_PROPERTY_NAME_PROPERTY_URI, selectPropertyName);	//set the given select property name
	}

	/**@return This selector's select declaration, or <code>null</code> if this rule has no <code>select.select</code> property or the value is not a {@link Selector}.*/
	public Selector getSelect() throws ClassCastException
	{
		return asInstance(getPropertyValue(SELECTOR_PROPERTY_URI), Selector.class);	//return the select.select value
	}

	/**Determines if this selector selects a given object.
	This version returns whether the object's property value matches the given subselector, or <code>false</code> if the object has no such property and/or if there is no subselector.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@exception IllegalStateException if this selector doesn't specify a selected property or the specified property cannot be accessed or throws an exception.
	@see #getSelectPropertyName()
	@see #getSelect()
	*/
	public boolean selects(final Object object)
	{
		if(object!=null)	//if there is an object being selected
		{
			final String propertyName=getSelectPropertyName();	//get the property name
			if(propertyName!=null)	//if a property name was given
			{
				final Selector selector=getSelect();	//get the subselector
				if(selector!=null)	//if a selector was given
				{
					final Object propertyValue;
					try
					{
						propertyValue=getProperty(object, propertyName);	//get the property value from the object
					}
					catch(final NoSuchMethodException noSuchMethodException)	//if the object has no such property
					{
						return false;	//the object isn't selected
					}
					catch(final InvocationTargetException invocationTargetException)	//if the getter throws an exception
					{
						throw new IllegalStateException(invocationTargetException);
					}
					catch(final IllegalAccessException illegalAccessException)	//if the getter can't be accessed
					{
						throw new IllegalStateException(illegalAccessException);
					}
					return selector.selects(propertyValue);	//return whether the subselector selects the property value
				}
				return false;	//if there is no subselector, there is no selection
			}
			else
			{
				throw new IllegalStateException("Property selector missing property selection property.");
			}
		}
		else	//if no object is being selected
		{
			return false;	//there is no match
		}
	}
}