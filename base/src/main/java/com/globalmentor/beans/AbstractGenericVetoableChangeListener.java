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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;

import static com.globalmentor.beans.AbstractGenericPropertyChangeListener.getGenericPropertyChangeEvent;

/**
 * A Java Beans vetoable change listener retrofitted to use generics to cast to proper value type.
 * @param <V> The type of property value.
 * @author Garret Wilson
 */
public abstract class AbstractGenericVetoableChangeListener<V> implements GenericVetoableChangeListener<V> {

	/**
	 * Called when a constrained property is changed. This not-generics version calls the generic version, creating a new event if necessary.
	 * @param propertyChangeEvent An event object describing the event source, the property that is changing, and its old and new values.
	 * @throws PropertyVetoException if the recipient wishes the property change to be rolled back.
	 * @see GenericPropertyChangeListener#propertyChange(GenericPropertyChangeEvent)
	 */
	@SuppressWarnings("unchecked")
	public void vetoableChange(final PropertyChangeEvent propertyChangeEvent) throws PropertyVetoException {
		vetoableChange((GenericPropertyChangeEvent<V>)getGenericPropertyChangeEvent(propertyChangeEvent)); //call the generic version of the method with the genericized event object
	}
}
