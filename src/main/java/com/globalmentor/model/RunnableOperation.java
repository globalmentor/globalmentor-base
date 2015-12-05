/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.model;

import static com.globalmentor.java.Objects.*;

/**
 * An operation that delegates to a provided {@link Runnable}.
 * <p>
 * This implementation is itself {@link Runnable}.
 * </p>
 * @author Garret Wilson
 */
public class RunnableOperation extends AbstractOperation {

	/** The decorated runnable. */
	private final Runnable runnable;

	/**
	 * Runnable constructor.
	 * @param runnable The runnable to be decorated by this operation.
	 * @throws NullPointerException if the given runnable is <code>null</code>.
	 */
	public RunnableOperation(final Runnable runnable) {
		this.runnable = checkInstance(runnable);
	}

	/** {@inheritDoc} This method delegates to the decorated runnabled {@link Runnable#run()} method. */
	@Override
	public void execute() {
		runnable.run(); //run the runnable
	}

}
