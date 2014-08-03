/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.beans;

import java.beans.*;

import com.globalmentor.java.Objects;

/**
 * An object that automatically supports bound and constrained properties.
 * <p>
 * Property change support is only created when needed; if no property change listeners are added, the property change support will never be created or invoked,
 * even when firing property change events.
 * </p>
 * <p>
 * Vetoable change support is only created when needed; if no vetoable change listeners are added, the vetoable change support will never be created or invoked,
 * even when firing vetoable change events.
 * </p>
 * <p>
 * This class was modeled in part from the property support of {@link java.awt.Component}.
 * </p>
 * <p>
 * This implementation creates generic-aware property value change events.
 * </p>
 * @author Garret Wilson
 * @see PropertyChangeSupport
 * @see GenericPropertyChangeEvent
 * @see GenericPropertyChangeListener
 */
public class BoundPropertyObject implements PropertyBindable, PropertyConstrainable {

	/** A convenience static empty array of property change listeners. */
	protected final static PropertyChangeListener[] NO_PROPERTY_CHANGE_LISTENERS = new PropertyChangeListener[0];

	/** A convenience static empty array of vetoable change listeners. */
	protected final static VetoableChangeListener[] NO_VETOABLE_CHANGE_LISTENERS = new VetoableChangeListener[0];

	/** Lazily-created support that keeps track of {@link PropertyChangeListener}s that have been registered, and handles firing of events. */
	private PropertyChangeSupport propertyChangeSupport = null;

	/** @return Support for property change event management, creating support if necessary. */
	protected synchronized PropertyChangeSupport getPropertyChangeSupport() {
		if(propertyChangeSupport == null) { //if we don't have property change support, yet
			propertyChangeSupport = new PropertyChangeSupport(this); //create new property change support				
		}
		return propertyChangeSupport; //return the property change support
	}

	/** Lazily-created support that keeps track of {@link VetoableChangeListener}s that have been registered, and handles firing of events. */
	private VetoableChangeSupport vetoableChangeSupport = null;

	/** @return Support for vetoable change management, creating support if necessary. */
	protected synchronized VetoableChangeSupport getVetoableChangeSupport() {
		if(vetoableChangeSupport == null) { //if we don't have vetoable change support, yet
			vetoableChangeSupport = new VetoableChangeSupport(this); //create new vetoable change support				
		}
		return vetoableChangeSupport; //return the vetoable change support
	}

	/** A lazily-created property change listener to repeat copies of events received, using this object as the source. */
	private PropertyChangeListener repeatPropertyChangeListener = null;

	/** @return A property change listener to repeat copies of events received, using this object as the source. */
	protected synchronized PropertyChangeListener getRepeatPropertyChangeListener() { //TODO update to work with PropertyValueChangeEvent	//TODO synchronize on something else
		if(repeatPropertyChangeListener == null) { //if we have not yet created the repeater property change listener
			repeatPropertyChangeListener = new PropertyChangeListener() { //create a listener to listen for a changing property value

				public void propertyChange(final PropertyChangeEvent propertyChangeEvent) { //if a property value changes
					final PropertyChangeEvent repeatPropertyChangeEvent = new GenericPropertyChangeEvent<Object>(BoundPropertyObject.this, propertyChangeEvent); //copy the property change event with this class as its source, but keeping the same target if present
					firePropertyChange(repeatPropertyChangeEvent); //fire the repeated property change event
				}
			};
		}
		return repeatPropertyChangeListener; //return the repeater property change listener
	}

	/** A lazily-created property change listener to forward along events received unmodified. */
	private PropertyChangeListener forwardPropertyChangeListener = null;

	/** @return A lazily-created property change listener to forward along events received unmodified. */
	protected synchronized PropertyChangeListener getForwardPropertyChangeListener() { //TODO synchronize on something else
		if(forwardPropertyChangeListener == null) { //if we have not yet created the forward property change listener
			forwardPropertyChangeListener = new PropertyChangeListener() { //create a listener to listen for a changing property value

				public void propertyChange(final PropertyChangeEvent propertyChangeEvent) { //if a property value changes
					firePropertyChange(propertyChangeEvent); //forward the property change event unmodified
				}
			};
		}
		return forwardPropertyChangeListener; //return the forward property change listener
	}

	/** A lazily-created vetoable change listener to repeat copies of events received, using this object as the source. */
	private VetoableChangeListener repeatVetoableChangeListener = null;

	/** @return A vetoable change listener to repeat copies of events received, using this object as the source. */
	protected synchronized VetoableChangeListener getRepeatVetoableChangeListener() { //TODO synchronize on something else
		if(repeatVetoableChangeListener == null) { //if we have not yet created the repeater vetoable change listener
			repeatVetoableChangeListener = new VetoableChangeListener() { //create a listener to listen for a changing property value

				public void vetoableChange(final PropertyChangeEvent propertyChangeEvent) throws PropertyVetoException { //if a property value changes
					final PropertyChangeEvent repeatPropertyChangeEvent = new GenericPropertyChangeEvent<Object>(BoundPropertyObject.this, propertyChangeEvent); //copy the property change event with this class as its source, but keeping the same target if present
					fireVetoableChange(repeatPropertyChangeEvent); //fire the repeated vetoable change event
				}
			};
		}
		return repeatVetoableChangeListener; //return the repeater vetoable change listener
	}

	/** Default constructor. */
	public BoundPropertyObject() {
	}

	/**
	 * Adds a property change listener to the listener list. The listener is registered for all properties.
	 * <p>
	 * If the listener is <code>null</code>, no exception is thrown and no action is performed.
	 * </p>
	 * @param listener The <code>PropertyChangeListener</code> to be added.
	 * @see PropertyChangeEvent
	 */
	public void addPropertyChangeListener(final PropertyChangeListener listener) {
		if(listener == null) //if no listener was provided
			return; //don't do anything
		getPropertyChangeSupport().addPropertyChangeListener(listener); //add the property change listener to our change support
	}

	/**
	 * Remove a property change listener from the listener list. This removes a <code>PropertyChangeListener</code> that was registered for all properties.
	 * <p>
	 * If the listener is <code>null</code>, no exception is thrown and no action is performed.
	 * </p>
	 * @param listener The <code>PropertyChangeListener</code> to be removed.
	 */
	public void removePropertyChangeListener(final PropertyChangeListener listener) {
		if(listener == null) //if no listener was provided
			return; //don't do anything
		if(propertyChangeSupport != null) //if we have property change support (if not, no listeners could have been added)
			propertyChangeSupport.removePropertyChangeListener(listener); //remove the property change listener from our change support
	}

	/**
	 * Add a property change listener for a specific property. The listener will be invoked only when a call to {@link #firePropertyChange(String, V, V)} names
	 * that specific property.
	 * <p>
	 * If the listener is <code>null</code>, no exception is thrown and no action is performed.
	 * </p>
	 * @param propertyName The name of the property to listen on.
	 * @param listener The <code>PropertyChangeListener</code> to be added.
	 */
	public void addPropertyChangeListener(final String propertyName, final PropertyChangeListener listener) {
		if(listener == null) //if no listener was provided
			return; //don't do anything
		getPropertyChangeSupport().addPropertyChangeListener(propertyName, listener); //add the property change listener to our change support, listening for the specified property
	}

	/**
	 * Remove a property change listener for a specific property.
	 * <p>
	 * If the listener is <code>null</code>, no exception is thrown and no action is performed.
	 * </p>
	 * @param propertyName The name of the property that was listened on.
	 * @param listener The <code>PropertyChangeListener</code> to be removed
	 */
	public void removePropertyChangeListener(final String propertyName, final PropertyChangeListener listener) {
		if(listener == null) //if no listener was provided
			return; //don't do anything
		if(propertyChangeSupport != null) //if we have property change support (if not, no listeners could have been added)
			propertyChangeSupport.removePropertyChangeListener(propertyName, listener); //remove the property change listener that was listening to a specified property from our change support
	}

	/**
	 * Returns an array of all the listeners that were added to the with {@link #addPropertyChangeListener(PropertyChangeListener)}. If some listeners have been
	 * added with a named property, then the returned array will be a mixture of <code>PropertyChangeListener</code>s and <code>PropertyChangeListenerProxy</code>
	 * s. If the calling method is interested in distinguishing the listeners then it must test each element to see if it's a
	 * <code>PropertyChangeListenerProxy</code>, perform the cast, and examine the parameter.
	 * @return all of the <code>PropertyChangeListener</code>s added or an empty array if no listeners have been added
	 * @see PropertyChangeListenerProxy
	 */
	public PropertyChangeListener[] getPropertyChangeListeners() {
		return propertyChangeSupport != null ? propertyChangeSupport.getPropertyChangeListeners() : NO_PROPERTY_CHANGE_LISTENERS; //if we have property change support, delegate to that, else return an empty list
	}

	/**
	 * Returns an array of all the listeners which have been associated with the named property.
	 * @return All of the <code>PropertyChangeListener</code>s associated with the named property; if no such listeners have been added or if
	 *         <code>propertyName</code> is <code>null</code>, an empty array is returned
	 */
	public PropertyChangeListener[] getPropertyChangeListeners(final String propertyName) {
		return propertyChangeSupport != null ? propertyChangeSupport.getPropertyChangeListeners(propertyName) : NO_PROPERTY_CHANGE_LISTENERS; //if we have property change support, delegate to that, else return an empty list
	}

	/**
	 * Checks if there are any listeners for a specific property, including those registered on all properties. If <code>propertyName</code> is <code>null</code>,
	 * this method only checks for listeners registered on all properties.
	 * @param propertyName The property name.
	 * @return <code>true</code> if there are one or more listeners for the given property.
	 */
	public boolean hasPropertyChangeListeners(final String propertyName) {
		return propertyChangeSupport != null ? propertyChangeSupport.hasListeners(propertyName) : false; //if we have property change support, ask it about listeners; if there is no support, there can be no listeners
	}

	/**
	 * Reports that a bound property has changed. This method can be called when a bound property has changed and it will send the appropriate property change
	 * event to any registered property change listeners. No event is fired if old and new are both <code>null</code> or are both non-<code>null</code> and equal
	 * according to the {@link Object#equals(java.lang.Object)} method. No event is fired if no listeners are registered for the given property. This method
	 * delegates actual firing of the event to {@link #firePropertyChange(PropertyChangeEvent)}.
	 * @param propertyName The name of the property being changed.
	 * @param oldValue The old property value.
	 * @param newValue The new property value.
	 * @see #firePropertyChange(PropertyChangeEvent)
	 * @see #hasPropertyChangeListeners(String)
	 * @see GenericPropertyChangeEvent
	 * @see GenericPropertyChangeListener
	 */
	protected <V> void firePropertyChange(final String propertyName, final V oldValue, final V newValue) {
		if(hasPropertyChangeListeners(propertyName)) { //if we have listeners registered for this property
			if(!Objects.equals(oldValue, newValue)) { //if the values are different					
				firePropertyChange(new GenericPropertyChangeEvent<V>(this, propertyName, oldValue, newValue)); //create and fire a genericized subclass of a property change event
			}
		}
	}

	/**
	 * Reports that a bound integer property has changed, reporting old and new values of type {@link Integer}. No event is fired if the values are equal, or if
	 * no event is fired if no listeners are registered for the given property. This method delegates actual firing of the event to
	 * {@link #firePropertyChange(String, V, V)}.
	 * @param propertyName The name of the property being changed.
	 * @param oldValue The old property value.
	 * @param newValue The new property value.
	 */
	protected void firePropertyChange(final String propertyName, final int oldValue, final int newValue) {
		if(oldValue != newValue) { //if the values are different
			firePropertyChange(propertyName, Integer.valueOf(oldValue), Integer.valueOf(newValue)); //convert the primitive values to objects and fire the event
		}
	}

	/**
	 * Reports that a bound long property has changed, reporting old and new values of type {@link Long}. No event is fired if the values are equal, or if no
	 * event is fired if no listeners are registered for the given property. This method delegates actual firing of the event to
	 * {@link #firePropertyChange(String, V, V)}.
	 * @param propertyName The name of the property being changed.
	 * @param oldValue The old property value.
	 * @param newValue The new property value.
	 */
	protected void firePropertyChange(final String propertyName, final long oldValue, final long newValue) {
		if(oldValue != newValue) { //if the values are different
			firePropertyChange(propertyName, Long.valueOf(oldValue), Long.valueOf(newValue)); //convert the primitive values to objects and fire the event
		}
	}

	/**
	 * Reports that a bound boolean property has changed, reporting old and new values of type <code>Boolean</code>. No event is fired if the values are equal, or
	 * if no event is fired if no listeners are registered for the given property. This method delegates actual firing of the event to
	 * {@link #firePropertyChange(String, V, V)}.
	 * @param propertyName The name of the property being changed.
	 * @param oldValue The old property value.
	 * @param newValue The new property value.
	 */
	protected void firePropertyChange(final String propertyName, final boolean oldValue, final boolean newValue) {
		if(oldValue != newValue) { //if the values are different
			firePropertyChange(propertyName, Boolean.valueOf(oldValue), Boolean.valueOf(newValue)); //convert the primitive values to objects and fire the event
		}
	}

	/**
	 * Reports that a bound property has changed. This method does the actual delegation to the property change support.
	 * @param propertyChangeEvent The event to fire.
	 */
	protected void firePropertyChange(final PropertyChangeEvent propertyChangeEvent) {
		if(propertyChangeSupport != null) { //if we have property change support (otherwise, no listeners would be listening)
			propertyChangeSupport.firePropertyChange(propertyChangeEvent); //delegate to the property change support
		}
	}

	/**
	 * Creates an object representing a postponement of firing the property change event.
	 * @param propertyChangeEvent The property change event the firing of which to postpone.
	 * @return A newly created postponed property change event.
	 */
	protected PostponedPropertyChangeEvent createPostponedPropertyChangeEvent(final PropertyChangeEvent propertyChangeEvent) {
		return new PostponedPropertyChangeEvent(propertyChangeSupport, propertyChangeEvent); //create a new postponed property change event with our current property change support, if any		
	}

	/**
	 * Adds a vetoable listener to the listener list. The listener is registered for all properties.
	 * <p>
	 * The same listener object may be added more than once, and will be called as many times as it is added.
	 * </p>
	 * <p>
	 * If <code>listener</code> is <code>null</code>, no exception is thrown and no action is taken.<;p>
	 * @param listener The <code>VetoableChangeListener</code> to be added.
	 */
	public void addVetoableChangeListener(final VetoableChangeListener listener) {
		if(listener == null) //if no listener was provided
			return; //don't do anything
		getVetoableChangeSupport().addVetoableChangeListener(listener); //add the vetoable change listener to our change support		
	}

	/**
	 * Removes a vetoable change listener from the listener list. This removes a VetoableChangeListener that was registered for all properties.
	 * <p>
	 * If <code>listener</code> was added more than once to the same event source, it will be notified one less time after being removed.
	 * </p>
	 * <p>
	 * If <code>listener</code> is <code>null</code>, or was never added, no exception is thrown and no action is taken.
	 * </p>
	 * @param listener The <code>VetoableChangeListener</code> to be removed
	 */
	public void removeVetoableChangeListener(final VetoableChangeListener listener) {
		if(listener == null) //if no listener was provided
			return; //don't do anything
		if(vetoableChangeSupport != null) //if we have vetoable change support (if not, no listeners could have been added)
			vetoableChangeSupport.removeVetoableChangeListener(listener); //remove the vetoable change listener from our change support
	}

	/**
	 * Adds a vetoable change listener for a specific property. The listener will be invoked only when a call to {@link #fireVetoableChange(String, int, int)}
	 * names that specific property.
	 * <p>
	 * The same listener object may be added more than once. For each property, the listener will be invoked the number of times it was added for that property.
	 * </p>
	 * <p>
	 * If <code>propertyName</code> or <code>listener</code> is <code>null</code>, no exception is thrown and no action is taken.
	 * </p>
	 * @param propertyName The name of the property to listen on.
	 * @param listener The <code>VetoableChangeListener</code> to be added.
	 */
	public void addVetoableChangeListener(final String propertyName, final VetoableChangeListener listener) {
		if(listener == null) //if no listener was provided
			return; //don't do anything
		getVetoableChangeSupport().addVetoableChangeListener(propertyName, listener); //add the vetoable change listener to our change support, listening for the specified property
	}

	/**
	 * Removes a vetoable change listener for a specific property.
	 * <p>
	 * If <code>listener</code> was added more than once to the same event source for the specified property, it will be notified one less time after being
	 * removed.
	 * </p>
	 * <p>
	 * If <code>propertyName</code> is <code>null</code>, no exception is thrown and no action is taken.
	 * </p>
	 * <p>
	 * If <code>listener</code> is <code>null</code>, or was never added for the specified property, no exception is thrown and no action is taken.
	 * </p>
	 * @param propertyName The name of the property that was listened on.
	 * @param listener The <code>VetoableChangeListener</code> to be removed.
	 */
	public void removeVetoableChangeListener(final String propertyName, final VetoableChangeListener listener) {
		if(listener == null) //if no listener was provided
			return; //don't do anything
		if(vetoableChangeSupport != null) //if we have vetoable change support (if not, no listeners could have been added)
			vetoableChangeSupport.removeVetoableChangeListener(propertyName, listener); //remove the vetoable change listener that was listening to a specified property from our change support
	}

	/**
	 * Returns the list of vetoable change listeners. If named vetoable change listeners were added, then {@link VetoableChangeListenerProxy} wrappers will
	 * returned.
	 * @return All added vetoable change listeners, including vetoable change listener proxys if named vetoable change listeners were added.
	 */
	public VetoableChangeListener[] getVetoableChangeListeners() {
		return vetoableChangeSupport != null ? vetoableChangeSupport.getVetoableChangeListeners() : NO_VETOABLE_CHANGE_LISTENERS; //if we have vetoable change support, delegate to that, else return an empty list
	}

	/**
	 * Returns an array of all the listeners which have been associated with the named property.
	 * @param propertyName The name of the property being listened to.
	 * @return All the <code>VetoableChangeListeners</code> associated with the named property; if no such listeners have been added, or if
	 *         <code>propertyName</code> is <code>null</code>, an empty array is returned.
	 */
	public VetoableChangeListener[] getVetoableChangeListeners(final String propertyName) {
		return vetoableChangeSupport != null ? vetoableChangeSupport.getVetoableChangeListeners(propertyName) : NO_VETOABLE_CHANGE_LISTENERS; //if we have vetoable change support, delegate to that, else return an empty list
	}

	/**
	 * Checks if there are any vetoable change listeners for a specific property, including those registered on all properties. If <code>propertyName</code> is
	 * <code>null</code>, this method only checks for listeners registered on all properties.
	 * @param propertyName The property name.
	 * @return <code>true</code> if there are one or more listeners for the given property.
	 */
	public boolean hasVetoableChangeListeners(final String propertyName) {
		return vetoableChangeSupport != null ? vetoableChangeSupport.hasListeners(propertyName) : false; //if we have vetoable change support, ask it about listeners; if there is no support, there can be no listeners
	}

	/**
	 * Reports a vetoable property update to any registered listeners. If any veotable change listeners vetos the change, then a new event will be fired reverting
	 * all the listeners to the old value, after which the {@link PropertyVetoException} will be rethrown. No event is fired if old and new are equal and non-
	 * <code>null</code>. This method delegates actual firing of the event to {@link #fireVetoableChange(PropertyChangeEvent)}.
	 * @param propertyName The name of the property that is about to change.
	 * @param oldValue The old value of the property.
	 * @param newValue The new value of the property.
	 * @throws PropertyVetoException if the recipient wishes the property change to be rolled back.
	 * @see #fireVetoableChange(PropertyChangeEvent)
	 * @see #hasVetoableChangeListeners(String)
	 * @see GenericPropertyChangeEvent
	 * @see GenericVetoableChangeListener
	 */
	public <V> void fireVetoableChange(final String propertyName, V oldValue, V newValue) throws PropertyVetoException {
		if(hasVetoableChangeListeners(propertyName)) { //if we have listeners registered for this property
			if(!Objects.equals(oldValue, newValue)) { //if the values are different					
				fireVetoableChange(new GenericPropertyChangeEvent<V>(this, propertyName, oldValue, newValue)); //create and fire a genericized subclass of a property change event
			}
		}
	}

	/**
	 * Reports that an integer vetoable property updtae to any registered listeners, reporting old and new values of type <code>Integer</code>. No event is fired
	 * if the values are equal, or if no event is fired if no listeners are registered for the given property. This method delegates actual firing of the event to
	 * {@link #fireVetoableChange(String, V, V)}.
	 * @param propertyName The name of the property that is about to change.
	 * @param oldValue The old property value.
	 * @param newValue The new property value.
	 * @throws PropertyVetoException if the recipient wishes the property change to be rolled back.
	 */
	protected void fireVetoableChange(final String propertyName, final int oldValue, final int newValue) throws PropertyVetoException {
		if(oldValue != newValue) { //if the values are different
			fireVetoableChange(propertyName, new Integer(oldValue), new Integer(newValue)); //convert the primitive values to objects and fire the event
		}
	}

	/**
	 * Reports that an integer vetoable property updtae to any registered listeners, reporting old and new values of type <code>Boolean</code>. No event is fired
	 * if the values are equal, or if no event is fired if no listeners are registered for the given property. This method delegates actual firing of the event to
	 * {@link #fireVetoableChange(String, V, V)}.
	 * @param propertyName The name of the property that is about to change.
	 * @param oldValue The old property value.
	 * @param newValue The new property value.
	 * @throws PropertyVetoException if the recipient wishes the property change to be rolled back.
	 */
	protected void fireVetoableChange(final String propertyName, final boolean oldValue, final boolean newValue) throws PropertyVetoException {
		if(oldValue != newValue) { //if the values are different
			fireVetoableChange(propertyName, new Boolean(oldValue), new Boolean(newValue)); //convert the primitive values to objects and fire the event
		}
	}

	/**
	 * Fires a vetoable property update to any registered listeners. If any veotable change listeners vetos the change, then a new event will be fired reverting
	 * all the listeners to the old value, after which the {@link PropertyVetoException} will be rethrown. No event is fired if old and new are equal and non-
	 * <code>null</code>. This method does the actual delegation to the vetoable change support.
	 * @param propertyChangeEvent The event to fire.
	 * @throws PropertyVetoException if the recipient wishes the property change to be rolled back.
	 */
	public void fireVetoableChange(final PropertyChangeEvent propertyChangeEvent) throws PropertyVetoException {
		if(vetoableChangeSupport != null) { //if we have vetoable change support (otherwise, no listeners would be listening)
			vetoableChangeSupport.fireVetoableChange(propertyChangeEvent); //delegate to the vetoable change support
		}
	}

}