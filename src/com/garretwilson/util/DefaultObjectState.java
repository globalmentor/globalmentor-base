package com.garretwilson.util;

import java.util.*;
import com.garretwilson.util.*;

/**A default implementation of object state information.
<p>Bound properties:</p>
<ul>
	<li><code>Modifiable.MODIFIED_PROPERTY_NAME</code> ("modified") <code>Boolean</code></li>
</ul>
<p>Each property changed using <code>setProperty</code> will also fire a
	property change event if its key is a string.</p>
@author Garret Wilson
@see Modifiable#MODIFIED_PROPERTY_NAME
@see java.beans.PropertyChangeEvent
*/
public class DefaultObjectState extends DefaultModifiable implements ObjectState	//G***maybe just store the modified value in the properties rather than having an explicit modified variable; right now, setting a property of "modified" would result in an identical property change firing
{
	
	/**The non-<code>null</code> object being described.*/
	private Object object;

		/**@return The non-<code>null</code> object being described.*/
		public Object getObject() {return object;}
		
		/**Sets the object being described.
		@param object The new object to describe.
		@exception IllegalArgumentException Thrown if the object is <code>null</code>.
		*/
		protected void setObject(final Object object)
		{
			if(object==null)	//if the object is null
			{
				throw new IllegalArgumentException("Object cannot be null.");	//throw an exception
			}			
			this.object=object;
		}

	/**The map of properties.*/
	private final Map propertyMap=new HashMap();
	
		/**Gets a property of the object state.
		@param key The key to the property.
		@return The value of the object state's property, or <code>null</code> if
			that property does not exist.
		*/
		public Object getProperty(final Object key)
		{
			return propertyMap.get(key);	//return the property from the property map
		}
		
		/**Sets the value of an object state property, and fires a property changed
			event if the key is a string.
		If the property represented by the key already exists, it will be replaced.
		@param key The non-<code>null</code> property key.
		@param value The property value.
		@return The old property value associated with the key, or <code>null</code>
			if no value was associated with the key previously.
		@see PropertyChangeEvent
		*/
		public Object setProperty(final Object key, final Object value)
		{
			final Object oldValue=propertyMap.put(key, value);	//put the value in the map keyed to the key and save the old value
			if(key instanceof String)	//if they key was a string
			{					
				firePropertyChange((String)key, oldValue, value);	//show that the property value has changed
			}
			return oldValue;	//return the old property value, if there was one
		}
	
		/**Removes a property of the object state.
		If the property represented by the key does not exist, no action is taken.
		@param key The non-<code>null</code> property key.
		@return The removed property value, or <code>null</code> if there was no
			property.
		*/
		public Object removeProperty(final Object key)
		{
			return propertyMap.remove(key);	//remove and return the property value keyed to the key
		}

	/**Constructs an object state with an object.
	@param object The non-<code>null</code> object being described
	@exception IllegalArgumentException Thrown if the object is <code>null</code>.
	*/
	public DefaultObjectState(final Object object)
	{
		setObject(object);	//save the object
	}


	/**Compares object states by comparing their respective objects.
	@param object The object with which to compare this RDF resource; should be
		another object state.
	@return <code>true<code> if this object state refers to the same object as
		specified in the object state <code>object</code>.
	@see ObjectState
	@see #getObject
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof ObjectState)	//if the other object implements the object state methods
		{
			return getObject().equals(((ObjectState)object).getObject());	//compare our object with that of the object state
		}
		return super.equals(object);	//try to compare the objects normally if the object isn't an object state
	}

	/**@return The hashcode value of the object represented.*/
	public int hashCode()
	{
		return getObject().hashCode();	//return the represented object's hash code
	}

}