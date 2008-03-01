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

import java.beans.PropertyChangeListener;

/**A JavaBeans property change listener retrofitted to use generics to cast to proper value type.
An implementing class must override the non-generic property change method and call the generic version,
so that the property change can be noted by either method. 
The abstract version of this class, {@link AbstractGenericPropertyChangeListener}, will be used most often.
@param <V> The type of property value.
@author Garret Wilson
*/
public interface GenericPropertyChangeListener<V> extends PropertyChangeListener
{

	/**Called when a bound property is changed.
	@param genericPropertyChangeEvent An event object describing the event source, the property that has changed, and its old and new values.
	*/
	public void propertyChange(final GenericPropertyChangeEvent<V> genericPropertyChangeEvent);

}
