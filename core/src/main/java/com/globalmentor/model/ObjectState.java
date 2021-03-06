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

package com.globalmentor.model;

import java.beans.PropertyChangeListener;

/**
 * Provides state information of an existing object, such as:
 * <ul>
 * <li>Descriptive properties of the object.</li>
 * <li>Whether the object has been modified.</li>
 * </ul>
 * @param <T> The type of object the state of which is being stored.
 * @author Garret Wilson
 * @deprecated
 */
public interface ObjectState<T> extends Modifiable {

	/** @return The non-<code>null</code> object being described. */
	public T getObject();

	/**
	 * Gets a property of the object state.
	 * @param key The key to the property.
	 * @return The value of the object state's property, or <code>null</code> if that property does not exist.
	 */
	public Object getProperty(final Object key);

	/**
	 * Sets the value of an object state property. If the property represented by the key already exists, it will be replaced.
	 * @param key The non-<code>null</code> property key.
	 * @param value The property value.
	 * @return The old property value associated with the key, or <code>null</code> if no value was associated with the key previously.
	 */
	public Object setProperty(final Object key, final Object value);

	/**
	 * Removes a property of the object state. If the property represented by the key does not exist, no action is taken.
	 * @param key The non-<code>null</code> property key.
	 * @return The removed property value, or <code>null</code> if there was no property.
	 */
	public Object removeProperty(final Object key);

	/**
	 * Adds a property change listener to the listener list. The listener is registered for all properties.
	 * <p>
	 * If the listener is <code>null</code>, no exception is thrown and no action is performed.
	 * </p>
	 * @param listener The <code>PropertyChangeListener</code> to be added.
	 * @see java.beans.PropertyChangeEvent
	 */
	public void addPropertyChangeListener(final PropertyChangeListener listener);

	/**
	 * Remove a property change listener from the listener list. This removes a <code>PropertyChangeListener</code> that was registered for all properties.
	 * <p>
	 * If the listener is <code>null</code>, no exception is thrown and no action is performed.
	 * </p>
	 * @param listener The <code>PropertyChangeListener</code> to be removed.
	 */
	public void removePropertyChangeListener(final PropertyChangeListener listener);

	/**
	 * Add a property change listener for a specific property. The listener will be invoked only when a call to <code>firePropertyChange()</code> names that
	 * specific property.
	 * <p>
	 * If the listener is <code>null</code>, no exception is thrown and no action is performed.
	 * </p>
	 * @param propertyName The name of the property to listen on.
	 * @param listener The <code>PropertyChangeListener</code> to be added.
	 */
	public void addPropertyChangeListener(final String propertyName, final PropertyChangeListener listener);

	/**
	 * Remove a property change listener for a specific property.
	 * <p>
	 * If the listener is <code>null</code>, no exception is thrown and no action is performed.
	 * </p>
	 * @param propertyName The name of the property that was listened on.
	 * @param listener The <code>PropertyChangeListener</code> to be removed
	 */
	public void removePropertyChangeListener(final String propertyName, final PropertyChangeListener listener);

}