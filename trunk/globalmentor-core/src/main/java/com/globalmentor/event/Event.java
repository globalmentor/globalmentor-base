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

package com.globalmentor.event;

import java.util.EventObject;

/**
 * An event interface. This interface is useful because Java's included {@link EventObject} is an abstract class, not an interface.
 * @author Garret Wilson
 */
public interface Event {

	/**
	 * The object on which the event initially occurred.
	 * @return The object on which the event initially occurred.
	 */
	public Object getSource();

}
