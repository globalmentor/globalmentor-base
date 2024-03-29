/*
 * Copyright © 2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static java.util.Objects.*;

/**
 * The base class for events, extending the Java event object as well as implementing the event interface.
 * @author Garret Wilson
 */
public abstract class AbstractEvent extends EventObject implements Event {

	private static final long serialVersionUID = -6018177999395658568L;

	/**
	 * Source constructor.
	 * @param source The object on which the event initially occurred.
	 * @throws NullPointerException if the given source is <code>null</code>.
	 */
	public AbstractEvent(final Object source) {
		super(requireNonNull(source, "Event source object cannot be null.")); //construct the parent class
	}

}
