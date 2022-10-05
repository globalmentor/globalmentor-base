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

/**
 * An interface indicating that an event knows its target, or the object to which the event applies (which may be different than the object that fired the
 * event).
 * @author Garret Wilson
 */
public interface TargetedEvent extends Event {

	/**
	 * Returns the object to which the event applies. This may be a different than <dfn>source</dfn>, which is the object that generated this event instance.
	 * @return The target of the event, or <code>null</code> if the event target is not known.
	 */
	public Object getTarget();
}
