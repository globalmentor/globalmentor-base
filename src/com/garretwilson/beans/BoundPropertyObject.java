package com.garretwilson.beans;

import java.beans.*;

/**An object that automatically supports bound properties.
<p>Property change support is only created when needed; if no property change
	listeners are added, the property change support will never be created or
	invoked, even when firing property change events.</p>
<p>This class was modeled from the property support of {@link java.awt.Component}.</p>
<p>This class creates generic-aware property value change events.</p>
@author Garret Wilson
@see PropertyChangeSupport
@see PropertyValueChangeEvent
@see PropertyValueChangeListener
*/
public class BoundPropertyObject implements PropertyBindable
{

	/**A convenience static empty array of property change listeners.*/
	protected final static PropertyChangeListener[] NO_PROPERTY_CHANGE_LISTENERS=new PropertyChangeListener[0];

	/**Lazily-created support that keeps track of <code>PropertyChangeListener</code>s that have been registered, and handles firing of events.*/
	private PropertyChangeSupport propertyChangeSupport=null;

		/**@return Support for property change event management, creating one if necessary.*/
		protected synchronized PropertyChangeSupport getPropertyChangeSupport()
		{
			if(propertyChangeSupport==null)	//if we don't have property change support, yet
			{
				propertyChangeSupport=new PropertyChangeSupport(this);  //create new property change support				
			}
			return propertyChangeSupport;	//return the property change support
		}

	/**Default constructor.*/
	public BoundPropertyObject()
	{
	}

	/**Adds a property change listener to the listener list. The listener is registered for all properties.
	<p>If the listener is <code>null</code>, no exception is thrown and no action is performed.</p>
	@param listener The <code>PropertyChangeListener</code> to be added.
	@see PropertyChangeEvent
	*/
	public void addPropertyChangeListener(final PropertyChangeListener listener)
	{
		if(listener==null)  //if no listener was provided
		  return; //don't do anything
		getPropertyChangeSupport().addPropertyChangeListener(listener); //add the property change listener to our change support
	}

	/**Remove a property change listener from the listener list.
	This removes a <code>PropertyChangeListener</code> that was registered for all properties.
	<p>If the listener is <code>null</code>, no exception is thrown and no action is performed.</p>
	@param listener The <code>PropertyChangeListener</code> to be removed.
	*/
	public void removePropertyChangeListener(final PropertyChangeListener listener)
	{
		if(listener==null)  //if no listener was provided
		  return; //don't do anything
		if(propertyChangeSupport!=null) //if we have property change support (if not, no listeners could have been added)
		  propertyChangeSupport.removePropertyChangeListener(listener);  //remove the property change listener from our change support
	}

	/**Add a property change listener for a specific property.
	The listener will be invoked only when a call to {@link #firePropertyChange(String, V, V)} names that specific property.
	<p>If the listener is <code>null</code>, no exception is thrown and no action is performed.</p>
	@param propertyName The name of the property to listen on.
	@param listener The <code>PropertyChangeListener</code> to be added.
	*/
	public void addPropertyChangeListener(final String propertyName, final PropertyChangeListener listener)
	{
		if(listener==null)  //if no listener was provided
		  return; //don't do anything
		getPropertyChangeSupport().addPropertyChangeListener(propertyName, listener); //add the property change listener to our change support, listening for the specified property
	}

	/**Remove a property change listener for a specific property.
	<p>If the listener is <code>null</code>, no exception is thrown and no action is performed.</p>
	@param propertyName The name of the property that was listened on.
	@param listener The <code>PropertyChangeListener</code> to be removed
	*/
	public void removePropertyChangeListener(final String propertyName, final PropertyChangeListener listener)
	{
		if(listener==null)  //if no listener was provided
		  return; //don't do anything
		if(propertyChangeSupport!=null) //if we have property change support (if not, no listeners could have been added)
			propertyChangeSupport.removePropertyChangeListener(propertyName, listener);  //remove the property change listener that was listening to a specified property from our change support
	}

  /**Returns an array of all the listeners that were added to the with {@link #addPropertyChangeListener(PropertyChangeListener)}.
	If some listeners have been added with a named property, then
	the returned array will be a mixture of <code>PropertyChangeListener</code>s
	and <code>PropertyChangeListenerProxy</code>s. If the calling
	method is interested in distinguishing the listeners then it must
	test each element to see if it's a <code>PropertyChangeListenerProxy</code>, perform the cast, and examine
	the parameter.
	@return all of the <code>PropertyChangeListener</code>s added or an empty array if no listeners have been added
	@see PropertyChangeListenerProxy
	*/
	public PropertyChangeListener[] getPropertyChangeListeners()
	{
		return propertyChangeSupport!=null ? propertyChangeSupport.getPropertyChangeListeners() : NO_PROPERTY_CHANGE_LISTENERS;	//if we have property change support, delegate to that, else return an empty list
	}

	/**Returns an array of all the listeners which have been associated with the named property.
	@return All of the <code>PropertyChangeListener</code>s associated with the named property;
	if no such listeners have been added or if <code>propertyName</code> is <code>null</code>, an empty array is returned
	*/
	public PropertyChangeListener[] getPropertyChangeListeners(final String propertyName)
	{
		return propertyChangeSupport!=null ? propertyChangeSupport.getPropertyChangeListeners(propertyName) : NO_PROPERTY_CHANGE_LISTENERS;	//if we have property change support, delegate to that, else return an empty list
  }

	/**Checks if there are any listeners for a specific property, including those registered on all properties.
	If <code>propertyName</code> <code>null</code>, this method only checks for listeners registered on all properties.
	@param propertyName  the property name.
	@return true if there are one or more listeners for the given property
	*/
	public boolean hasListeners(final String propertyName)
	{
		return propertyChangeSupport!=null ? propertyChangeSupport.hasListeners(propertyName) : false;	//if we have property change support, ask it about listeners; if there is no support, there can be no listeners
	}

	/**Reports that a bound property has changed. This method can be called	when a bound property has changed and it will send the appropriate property change event to any registered property change listeners.
	No event is fired if old and new are equal and non-<code>null</code>.
	No event is fired if no listeners are registered for the given property.
	This method delegates actual firing of the event to {@link #firePropertyChange(PropertyChangeEvent)}.
	@param propertyName The name of the property being changed.
	@param oldValue The old property value.
	@param newValue The new property value.
	@see #firePropertyChange(PropertyChangeEvent)
	@see #hasListeners(String)
	@see PropertyValueChangeEvent
	@see PropertyValueChangeListener
	*/
	protected <V> void firePropertyChange(final String propertyName, V oldValue, final V newValue)
	{
		if(hasListeners(propertyName)) //if we have listeners registered for this property
		{
			if(oldValue==null || newValue==null || !oldValue.equals(newValue))	//if one of the values are null, or the values are actually different
			{					
				firePropertyChange(new PropertyValueChangeEvent<V>(this, propertyName, oldValue, newValue));	//create and fire a genericized subclass of a property change event
			}
		}
	}

	/**Reports that a bound property has changed.
	This method does the actual delegation to the property change support.
	@param propertyChangeEvent The event to fire.
	*/
	protected void firePropertyChange(final PropertyChangeEvent propertyChangeEvent)
	{
		if(propertyChangeSupport!=null)	//if we have property change support (otherwise, no listeners would be listening)
		{
			propertyChangeSupport.firePropertyChange(propertyChangeEvent);	//delegate to the property change support
		}
	}

	/**Creates an object representing a postponement of firing the property change event.
	@param propertyChangeEvent The property change event the firing of which to postpone.
	@return A newly created postponed property change event.
	*/
	protected PostponedPropertyChangeEvent createPostponedPropertyChangeEvent(final PropertyChangeEvent propertyChangeEvent)
	{
		return new PostponedPropertyChangeEvent(propertyChangeSupport, propertyChangeEvent);	//create a new postponed property change event with our current property change support, if any		
	}
}