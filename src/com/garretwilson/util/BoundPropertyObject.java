package com.garretwilson.util;

import java.beans.*;

/**An object that natively supports bound properties.
	<p>Property change support is only created when needed; if no property change
	listeners are added, the property change support will never be created or
	invoked, even when firing property change events.</p>
	<p>This class was modeled from the property support of
	<code>java.awt.Component</code>.</p>
@author Garret Wilson
@see PropertyChangeSupport
*/
public class BoundPropertyObject
{

	/**Keeps track of <code>PropertyChangeListener</code>s that have been
		registered, and handles firing of events.
	@see #addPropertyChangeListener
	@see #removePropertyChangeListener
	@see #firePropertyChange
	*/
	private PropertyChangeSupport propertyChangeSupport=null;

	/**Retrieves the support for property changes that keeps track of
	  <code>PropertyChangeListener</code>s that have been registered and handles
		firing of events. The support object is lazily created; if this method is
		never called, it will never be created.
	@return The property change support object.
	*/
/*G***del
	protected PropertyChangeSupport getPropertyChangeSupport()
	{
		if(propertyChangeSupport==null) //if no property change support has been created
			propertyChangeSupport=new PropertyChangeSupport(this);  //create new property change support
		return propertyChangeSupport; //return the support for property changes
	}
*/

	/**Default constructor.*/
	public BoundPropertyObject()
	{
	}

	/**Adds a property change listener to the listener list.
		The listener is registered for all properties.
		<p>If the listener is <code>null</code>, no exception is thrown and no action
		is performed.</p>
	@param listener The <code>PropertyChangeListener</code> to be added.
	@see PropertyChangeEvent
	*/
	public synchronized void addPropertyChangeListener(final PropertyChangeListener listener)
	{
		if(listener==null)  //if no listener was provided
		  return; //don't do anything
		if(propertyChangeSupport==null) //if no property change support has been created
			propertyChangeSupport=new PropertyChangeSupport(this);  //create new property change support
		propertyChangeSupport.addPropertyChangeListener(listener); //add the property change listener to our change support
	}

	/**Remove a property change listener from the listener list.
		This removes a <code>PropertyChangeListener</code> that was registered for
		all properties.
		<p>If the listener is <code>null</code>, no exception is thrown and no action
		is performed.</p>
	@param listener The <code>PropertyChangeListener</code> to be removed.
	*/
	public synchronized void removePropertyChangeListener(final PropertyChangeListener listener)
	{
		if(listener==null)  //if no listener was provided
		  return; //don't do anything
		if(propertyChangeSupport!=null) //if we have property change support (if not, no listeners could have been added)
		  propertyChangeSupport.removePropertyChangeListener(listener);  //remove the property change listener from our change support
	}

	/**Add a property change listener for a specific property.
		The listener will be invoked only when a call to
		<code>firePropertyChange()</code> names that specific property.
		<p>If the listener is <code>null</code>, no exception is thrown and no action
		is performed.</p>
	@param propertyName The name of the property to listen on.
	@param listener The <code>PropertyChangeListener</code> to be added.
	*/
	public synchronized void addPropertyChangeListener(final String propertyName, final PropertyChangeListener listener)
	{
		if(listener==null)  //if no listener was provided
		  return; //don't do anything
		if(propertyChangeSupport==null) //if no property change support has been created
			propertyChangeSupport=new PropertyChangeSupport(this);  //create new property change support
		propertyChangeSupport.addPropertyChangeListener(propertyName, listener); //add the property change listener to our change support, listening for the specified property
	}

	/**Remove a property change listener for a specific property.
		<p>If the listener is <code>null</code>, no exception is thrown and no
		action is performed.</p>
	@param propertyName The name of the property that was listened on.
	@param listener The <code>PropertyChangeListener</code> to be removed
	*/
	public synchronized void removePropertyChangeListener(final String propertyName, final PropertyChangeListener listener)
	{
		if(listener==null)  //if no listener was provided
		  return; //don't do anything
		if(propertyChangeSupport!=null) //if we have property change support (if not, no listeners could have been added)
			propertyChangeSupport.removePropertyChangeListener(propertyName, listener);  //remove the property change listener that was listening to a specified property from our change support
	}

	/**Reports that a bound property has changed. This method can be called
		when a bound property has changed and it will send the appropriate
		property change event to any registered property change listeners.
	@param propertyName The name of the property being changed.
	@param oldValue The old property value.
	@param newValue The new property value.
	@see PropertyChangeEvent
	@see PropertyChangeListener
	*/
	protected void firePropertyChange(final String propertyName, Object oldValue, final Object newValue)
	{
		if(propertyChangeSupport!=null) //if we have property change support (if not, no listeners could have been added so there would be no reason to fire change events)
			propertyChangeSupport.firePropertyChange(propertyName, oldValue, newValue);  //let the change support fire the property change
	}

}