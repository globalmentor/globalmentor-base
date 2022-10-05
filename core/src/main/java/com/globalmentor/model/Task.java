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

package com.globalmentor.model;

import static com.globalmentor.java.Classes.*;

import com.globalmentor.beans.*;

/**
 * Represents a task being performed.
 * @author Garret Wilson
 */
public interface Task extends PropertyBindable, PropertyConstrainable {

	/** The bound property of the task state. */
	public static final String STATE_PROPERTY = getPropertyName(Task.class, "state");

	/** @return The current state of the task. */
	public TaskState getState();

}
