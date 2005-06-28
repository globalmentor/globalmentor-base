package com.garretwilson.beans;

import java.beans.*;

/**Indicates that an object supports bound properties and listener notification.
@author Garret Wilson
*/
public interface PropertyBindable
{
	/**Adds a property change listener to the listener list. The listener is registered for all properties.
	<p>If the listener is <code>null</code>, no exception is thrown and no action is performed.</p>
	@param listener The <code>PropertyChangeListener</code> to be added.
	@see PropertyChangeEvent
	*/
	public void addPropertyChangeListener(final PropertyChangeListener listener);

	/**Remove a property change listener from the listener list.
	This removes a <code>PropertyChangeListener</code> that was registered for all properties.
	<p>If the listener is <code>null</code>, no exception is thrown and no action is performed.</p>
	@param listener The <code>PropertyChangeListener</code> to be removed.
	*/
	public void removePropertyChangeListener(final PropertyChangeListener listener);

	/**Add a property change listener for a specific property.
	The listener will be invoked only when a call to {@link #firePropertyChange(String, V, V)} names that specific property.
	<p>If the listener is <code>null</code>, no exception is thrown and no action is performed.</p>
	@param propertyName The name of the property to listen on.
	@param listener The <code>PropertyChangeListener</code> to be added.
	*/
	public void addPropertyChangeListener(final String propertyName, final PropertyChangeListener listener);

	/**Remove a property change listener for a specific property.
	<p>If the listener is <code>null</code>, no exception is thrown and no action is performed.</p>
	@param propertyName The name of the property that was listened on.
	@param listener The <code>PropertyChangeListener</code> to be removed
	*/
	public void removePropertyChangeListener(final String propertyName, final PropertyChangeListener listener);

  /**Returns an array of all the listeners that were added to the with {@link addPropertyChangeListener()}.
	If some listeners have been added with a named property, then
	the returned array will be a mixture of <code>PropertyChangeListener</code>s
	and <code>PropertyChangeListenerProxy</code>s. If the calling
	method is interested in distinguishing the listeners then it must
	test each element to see if it's a <code>PropertyChangeListenerProxy</code>, perform the cast, and examine
	the parameter.
	@return all of the <code>PropertyChangeListener</code>s added or an empty array if no listeners have been added
	@see PropertyChangeListenerProxy
	*/
	public PropertyChangeListener[] getPropertyChangeListeners();

	/**Returns an array of all the listeners which have been associated with the named property.
	@return All of the <code>PropertyChangeListener</code>s associated with the named property;
	if no such listeners have been added or if <code>propertyName</code> is <code>null</code>, an empty array is returned
	*/
	public PropertyChangeListener[] getPropertyChangeListeners(final String propertyName);

}