/*
 * Copyright © 2005-2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import com.globalmentor.lex.Identifier;

/**
 * Represents the progress of a task.
 * @author Garret Wilson
 */
public enum TaskState implements Identifier {

	/** The task has not yet begun. */
	UNSTARTED,

	/** The task is preparing to begin. */
	INITIALIZE,

	/** The task has been started but is incomplete. */
	INCOMPLETE,

	/** The task has been started but there is an error. */
	ERROR,

	/** The task is temporarily paused. */
	PAUSED,

	/** The task is stopped. */
	STOPPED,

	/** The task has been abandoned. */
	CANCELED,

	/** The task has been completed. */
	COMPLETE;

}
