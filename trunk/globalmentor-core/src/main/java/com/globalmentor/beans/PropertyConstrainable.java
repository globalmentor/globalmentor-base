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

/**Indicates that an object supports constrained properties and vetoable change listener notification.
@author Garret Wilson
*/
public interface PropertyConstrainable
{
	/**Adds a vetoable listener to the listener list. The listener is registered for all properties.
	<p>The same listener object may be added more than once, and will be called as many times as it is added.</p>
	<p>If <code>listener</code> is <code>null</code>, no exception is thrown and no action is taken.<;p>
	@param listener The <code>VetoableChangeListener</code> to be added.
	*/
	public void addVetoableChangeListener(final VetoableChangeListener listener);
	
	/**Removes a vetoable change listener from the listener list.
	This removes a VetoableChangeListener that was registered for all properties.
	<p>If <code>listener</code> was added more than once to the same event source, it will be notified one less time after being removed.</p>
	<p>If <code>listener</code> is <code>null</code>, or was never added, no exception is thrown and no action is taken.</p>
	@param listener The <code>VetoableChangeListener</code> to be removed
	*/
	public void removeVetoableChangeListener(final VetoableChangeListener listener);

	/**Adds a vetoable change listener for a specific property.
	The listener will be invoked only when a call to {@link #fireVetoableChange(String, int, int)} names that specific property.
	<p>The same listener object may be added more than once.  For each property,  the listener will be invoked the number of times it was added for that property.</p>
	<p>If <code>propertyName</code> or <code>listener</code> is <code>null</code>, no exception is thrown and no action is taken.</p>
	@param propertyName The name of the property to listen on.
	@param listener The <code>VetoableChangeListener</code> to be added.
	*/
	public void addVetoableChangeListener(final String propertyName, final VetoableChangeListener listener);

	/**Removes a vetoable change listener for a specific property.
	<p>If <code>listener</code> was added more than once to the same event source for the specified property, it will be notified one less time after being removed.</p>
	<p>If <code>propertyName</code> is <code>null</code>, no exception is thrown and no action is taken.</p>
	<p>If <code>listener</code> is <code>null</code>, or was never added for the specified property, no exception is thrown and no action is taken.</p>
	@param propertyName The name of the property that was listened on.
	@param listener The <code>VetoableChangeListener</code> to be removed.
	*/
	public void removeVetoableChangeListener(final String propertyName, final VetoableChangeListener listener);

	/**Returns the list of vetoable change listeners. If named vetoable change listeners were added, then {@link VetoableChangeListenerProxy} wrappers will returned.
	@return All added vetoable change listeners, including vetoable change listener proxys if named vetoable change listeners were added.
	*/
	public VetoableChangeListener[] getVetoableChangeListeners();

  /**Returns an array of all the listeners which have been associated with the named property.
	@param propertyName The name of the property being listened to.
	@return All the <code>VetoableChangeListeners</code> associated with the named property;
	if no such listeners have been added, or if <code>propertyName</code> is <code>null</code>, an empty array is returned.
  */
	public VetoableChangeListener[] getVetoableChangeListeners(final String propertyName);

	/**Checks if there are any vetoable change listeners for a specific property, including those registered on all properties.
	If <code>propertyName</code> is <code>null</code>, this method only checks for listeners registered on all properties.
	@param propertyName The property name.
	@return <code>true</code> if there are one or more listeners for the given property.
	*/
	public boolean hasVetoableChangeListeners(final String propertyName);

}