/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.beans;

import java.beans.*;

/**
 * Indicates that an object supports bound properties and property change listener notification.
 * @author Garret Wilson
 */
public interface PropertyBindable {

	/**
	 * Adds a property change listener to the listener list. The listener is registered for all properties.
	 * <p>
	 * If the listener is <code>null</code>, no exception is thrown and no action is performed.
	 * </p>
	 * @param listener The <code>PropertyChangeListener</code> to be added.
	 * @see PropertyChangeEvent
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
	 * Add a property change listener for a specific property.
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

	/**
	 * Returns an array of all the listeners that were added to the with {@link #addPropertyChangeListener(PropertyChangeListener)}. If some listeners have been
	 * added with a named property, then the returned array will be a mixture of <code>PropertyChangeListener</code>s and <code>PropertyChangeListenerProxy</code>
	 * s. If the calling method is interested in distinguishing the listeners then it must test each element to see if it's a
	 * <code>PropertyChangeListenerProxy</code>, perform the cast, and examine the parameter.
	 * @return all of the <code>PropertyChangeListener</code>s added or an empty array if no listeners have been added
	 * @see PropertyChangeListenerProxy
	 */
	public PropertyChangeListener[] getPropertyChangeListeners();

	/**
	 * Returns an array of all the listeners which have been associated with the named property.
	 * @param propertyName The name of the property.
	 * @return All of the <code>PropertyChangeListener</code>s associated with the named property; if no such listeners have been added or if
	 *         <code>propertyName</code> is <code>null</code>, an empty array is returned
	 */
	public PropertyChangeListener[] getPropertyChangeListeners(final String propertyName);

	/**
	 * Checks if there are any property change listeners for a specific property, including those registered on all properties. If <code>propertyName</code> is
	 * <code>null</code>, this method only checks for listeners registered on all properties.
	 * @param propertyName The property name.
	 * @return <code>true</code> if there are one or more listeners for the given property.
	 */
	public boolean hasPropertyChangeListeners(final String propertyName);

}