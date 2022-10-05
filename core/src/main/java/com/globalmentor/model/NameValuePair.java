/*
 * Copyright © 1996-2017 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.model;

import java.util.Map;
import java.util.Objects;

import javax.annotation.*;

/**
 * A convenience tuple for a name and a value. The class allows identification (though not necessarily unique) through its name object.
 * @author Garret Wilson
 */
public class NameValuePair<N, V> extends DefaultNamed<N> implements IDed<N>, Valued<V> {

	/** The value of the object. */
	final private V value;

	/** @return The value of the object. */
	public V getValue() {
		return value;
	}

	/** @return The name of the name-value pair, allowing identification. */
	public N getID() {
		return getName();
	}

	/**
	 * Constructor specifying the name and value.
	 * @param newName The object's new name.
	 * @param newValue The object's new value
	 */
	public NameValuePair(final N newName, final V newValue) {
		super(newName); //construct the parent class
		value = newValue; //set the value
	}

	/**
	 * Static factory method to create a name-value pair from a map entry copy.
	 * @param <MEK> The type of map entry key.
	 * @param <MEV> The type of map entry value.
	 * @param mapEntry The map entry the values of which to use in creating this name/value pair.
	 * @return A name-value pair from the map entry.
	 * @throws NullPointerException if the given map entry is <code>null</code>.
	 */
	public static <MEK, MEV> NameValuePair<MEK, MEV> fromMapEntry(@Nonnull final Map.Entry<MEK, MEV> mapEntry) {
		return new NameValuePair<>(mapEntry.getKey(), mapEntry.getValue());
	}

	@Override
	public int hashCode() {
		return Objects.hash(getName(), getValue()); //calculate a hash code from the name and value
	}

	/**
	 * Determines if the given object is another name value pair with the same name and value.
	 * @param object The object with which to compare this name/value pair.
	 * @return <code>true</code> if this name/value pair equals that specified in <code>object</code>.
	 * @see #getName()
	 * @see #getValue()
	 */
	@Override
	public boolean equals(final Object object) {
		if(this == object) {
			return true;
		}
		if(!(object instanceof NameValuePair)) {
			return false;
		}
		//compare the names and values, taking into account that one or the other may be null
		return Objects.equals(getName(), ((NameValuePair<?, ?>)object).getName()) && Objects.equals(getValue(), ((NameValuePair<?, ?>)object).getValue());
	}

	/**
	 * Compares this object to another object. This method determines order based upon first the name, and then the value. Both objects must implement
	 * <code>Comparable</code> or an exception will be thrown.
	 * @param object The object with which to compare this object. This must be another <code>NameValuePair</code> object.
	 * @return A negative integer, zero, or a positive integer as this name is less than, equal to, or greater than the name and value of the specified object,
	 *         respectively.
	 * @throws ClassCastException Thrown if the specified object's type is not a <code>NameValuePair</code>, or if the name and/or the value does not implement
	 *           <code>Comparable</code>.
	 * @see #getName
	 * @see #getValue
	 */
	/*TODO move to ComparableNameValuePair
		public int compareTo(NameValuePair<N, V> nameValuePair) throws ClassCastException
		{
			final int result=super.compareTo(nameValuePair); //compare names
				//if the names are equal, compare the values
			return result!=0 ? result : ((Comparable<V>)getValue()).compareTo(nameValuePair.getValue()); //compare values
		}
	*/

	/** @return A string representation of this object in the format "name=\"value\"". */
	public String toString() {
		final V value = getValue(); //get the value
		return getName() + "=" + (value != null ? "\"" + value + "\"" : value); //return a string constructed from the name and the value
	}

}
