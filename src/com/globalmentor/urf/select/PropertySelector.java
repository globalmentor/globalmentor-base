package com.globalmentor.urf.select;

import java.net.URI;

import static com.garretwilson.lang.Objects.*;

import com.globalmentor.urf.URFResource;

import static com.globalmentor.urf.select.Select.*;

/**A selector that selects an object based upon a property value.
The property specified by this selector is used to retrieve a property value from the given object and compare it with the specified selector in this class.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class PropertySelector extends AbstractSelector
{

	/**Default constructor.*/
	public PropertySelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public PropertySelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Returns the property identified by this selector.
	@return This selector's property designation, or <code>null</code> if this selector has no <code>selectProperty</code> property.
	@see Select#SELECT_PROPERTY_PROPERTY_URI
	*/
	public URFResource getSelectProperty()
	{
		return getPropertyValue(SELECT_PROPERTY_PROPERTY_URI);	//get the selectProperty property
	}

	/**Returns the URI of the property identified by this selector.
	@return This selector's property URI designation, or <code>null</code> if this selector has no <code>selectProperty</code> property or the desginated property has no URI.
	@see Select#SELECT_PROPERTY_PROPERTY_URI
	*/
	public URI getSelectPropertyURI()
	{
		final URFResource selectProperty=getSelectProperty();	//get the select property
		return selectProperty!=null ? selectProperty.getURI() : null;	//if there is a select property, return its URI
	}

	/**Sets the property identified by this selector.
	@param selectProperty The property to be selected.
	@see Select#SELECT_PROPERTY_PROPERTY_URI
	*/
	public void setSelectProperty(final String selectProperty)
	{
		setPropertyValue(SELECT_PROPERTY_PROPERTY_URI, selectProperty);	//set the given select property
	}

	/**Returns the selector identified by this selector.
	@return This selector's first select declaration, or <code>null</code> if this rule has no <code>selector</code> property or the value is not a {@link Selector}.
	@see Select#SELECTOR_PROPERTY_URI
	*/
	public Selector getSelector()
	{
		return asInstance(getPropertyValue(SELECTOR_PROPERTY_URI), Selector.class);	//return the selector value
	}

	/**Determines if this selector selects a given object.
	This version returns whether the object is a resource and its property value matches the given subselector,
	or <code>false</code> if the object is not a resource, the resource has no such property, and/or if there is no subselector.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@exception IllegalStateException if this selector doesn't specify a selected property.
	@see #getSelectProperty()
	@see #getSelectPropertyURI()
	@see #getSelector()
	*/
	public boolean selects(final Object object)
	{
		if(object instanceof URFResource)	//if there is a resource being selected
		{
			final URI selectPropertyURI=getSelectPropertyURI();	//get the URI of the property
			if(selectPropertyURI!=null)	//if a property was given
			{
				final Selector selector=getSelector();	//get the subselector
				if(selector!=null)	//if a selector was given
				{
					final URFResource propertyValue=((URFResource)object).getPropertyValue(selectPropertyURI);	//get the resource property value
					return selector.selects(propertyValue);	//return whether the subselector selects the property value
				}
				return false;	//if there is no subselector, there is no selection
			}
			else
			{
				throw new IllegalStateException("Property selector missing property selection property.");
			}
		}
		else	//if no resource is being selected
		{
			return false;	//there is no match
		}
	}
}