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

package com.globalmentor.event;

import java.util.EventObject;

/**
 * Encapsulation of an event and the means to fire it at a later time.
 * @param <E> The type of postponed event.
 * @author Garret Wilson
 */
public interface PostponedEvent<E extends EventObject> {

	/** @return Whether the postponed event has been fired. */
	public boolean isFired();

	/** @return The event that has been postponed. */
	public E getEvent();

	/**
	 * Fires the postponed event.
	 * @throws IllegalStateException if the postponed event has already been fired.
	 */
	public void fireEvent();
}
