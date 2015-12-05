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

import static com.globalmentor.java.Objects.*;

import java.beans.PropertyChangeEvent;

import com.globalmentor.event.TargetedEvent;

/**
 * A property value change event is a Java Beans property change event retrofitted to use generics to cast to proper value type. This event is also
 * <dfn>targeted</dfn>, specifying an event target which may or may not be the same as the source object firing this event.
 * @param <V> The type of property value.
 * @author Garret Wilson
 */
public class GenericPropertyChangeEvent<V> extends PropertyChangeEvent implements TargetedEvent {

	/** The target of the event, or <code>null</code> if the event target is not known. */
	private final Object target;

	/**
	 * Returns the object to which the event applies. This may be a different than <dfn>source</dfn>, which is the object that generated this event instance.
	 * @return The target of the event.
	 */
	public Object getTarget() {
		return target;
	}

	/**
	 * Source and property name constructor with old and new values. The target will be set to be the same as the given source.
	 * @param source The bean that fired the event.
	 * @param propertyName The programmatic name of the property that was changed.
	 * @param oldValue The old value of the property, or <code>null</code> if no old value is not available.
	 * @param newValue The new value of the property, or <code>null</code> if the new value is not available.
	 */
	public GenericPropertyChangeEvent(final Object source, final String propertyName, final V oldValue, V newValue) {
		this(source, source, propertyName, oldValue, newValue); //construct the class with the same target as the source
	}

	/**
	 * Source, target, and property name constructor with old and new values.
	 * @param source The bean that fired the event.
	 * @param target The target of the event.
	 * @param propertyName The programmatic name of the property that was changed.
	 * @param oldValue The old value of the property, or <code>null</code> if no old value is not available.
	 * @param newValue The new value of the property, or <code>null</code> if the new value is not available.
	 * @throws NullPointerException if the given source and/or target is <code>null</code>.
	 */
	public GenericPropertyChangeEvent(final Object source, final Object target, final String propertyName, final V oldValue, V newValue) {
		super(checkInstance(source, "Event source object cannot be null."), propertyName, oldValue, newValue); //construct the parent class
		this.target = checkInstance(target, "Event target object cannot be null."); //save the target
	}

	/**
	 * Property change event copy constructor. If the property change event is {@link TargetedEvent}, the target is copied from that event; otherwise, the event
	 * source will be used as the target.
	 * @param propertyChangeEvent A property change event the values of which will later cast to this class' generic type.
	 */
	public GenericPropertyChangeEvent(final PropertyChangeEvent propertyChangeEvent) {
		this(propertyChangeEvent.getSource(), propertyChangeEvent); //construct the class using the same source and optionally the same target as the given property change event
	}

	/**
	 * Property change event copy constructor that specifies a different source. If the property change event is {@link TargetedEvent}, the target is copied from
	 * that event; otherwise, the given source will be used as the target.
	 * @param source The object on which the event initially occurred.
	 * @param propertyChangeEvent A property change event the values of which will later cast to this class' generic type.
	 * @throws NullPointerException if the given source is <code>null</code>.
	 */
	@SuppressWarnings("unchecked")
	//we can only assume that the given event's old and new values are of the correct type
	public GenericPropertyChangeEvent(final Object source, final PropertyChangeEvent propertyChangeEvent) {
		this(source, propertyChangeEvent instanceof TargetedEvent ? ((TargetedEvent)propertyChangeEvent).getTarget() : source, propertyChangeEvent
				.getPropertyName(), (V)propertyChangeEvent.getOldValue(), (V)propertyChangeEvent.getNewValue()); //construct the parent class with identical values except for source
		setPropagationId(propertyChangeEvent.getPropagationId()); //update our propagation ID to match
	}

	/** @return The old value of the property, or <code>null</code> if the old value is not available. */
	@SuppressWarnings("unchecked")
	public V getOldValue() {
		return (V)super.getOldValue(); //cast the value to the appropriate generic type
	}

	/** @return The new value of the property, or <code>null</code> if the new value is not available. */
	@SuppressWarnings("unchecked")
	public V getNewValue() {
		return (V)super.getNewValue(); //cast the value to the appropriate generic type
	}

}
