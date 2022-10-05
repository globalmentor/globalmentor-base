/*
 * Copyright © 2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.management.profile;

import static com.globalmentor.java.Conditions.*;

import com.globalmentor.model.*;

import static java.util.Objects.*;

import java.time.Duration;

/**
 * Profiler class that allows various profiling operations such as brute-force probing executing threads at a given interval.
 * 
 * @author Garret Wilson
 */
public class Profiler {

	/** The operation manager for managing probe operations. */
	private static final OperationManager probeOperationManager = new OperationManager();

	/** The operation to use for probing the stack, or <code>null</code> if no stack probe operation has been specified. */
	private static StackProbeOperation stackProbeOperation = null;

	/** @return The current configured stack probe operation, or <code>null</code> if no stack probe operation is configured. */
	public static synchronized StackProbeOperation getStackProbeOperation() {
		return stackProbeOperation;
	}

	/**
	 * Determines the current stack probe operation. If no stack probe operation is configured, one is created.
	 * @return The current configured stack probe operation.
	 */
	public static synchronized StackProbeOperation determineStackProbeOperation() {
		if(stackProbeOperation == null) {
			stackProbeOperation = new StackProbeOperation();
		}
		return stackProbeOperation;
	}

	/**
	 * Sets the current stack probe operation. No stack probes must currently be started, although one may still be scheduled but canceled.
	 * @param stackProbeOperation The stack probe operation to be set.
	 * @throws NullPointerException if the given stack probe operation is <code>null</code>.
	 * @throws IllegalStateException if a stack probe operation is underway.
	 */
	public static synchronized void setStackProbeOperation(final StackProbeOperation stackProbeOperation) {
		checkState(stackProbeCount == 0, "Stack probe operation cannot be changed once started.");
		Profiler.stackProbeOperation = requireNonNull(stackProbeOperation);
	}

	/** The number of stack probe starts that have been requested, to ensure that stops are correctly paired. */
	private static long stackProbeCount = 0;

	/**
	 * Starts a stack probe.
	 * <p>
	 * Every call to this method must have a later call to {@link #stopStackProbe()}.
	 * </p>
	 * @see #stopStackProbe()
	 */
	public synchronized static void startStackProbe() {
		if(stackProbeCount == 0) { //if we haven't started a stack probe
			probeOperationManager.schedule(determineStackProbeOperation(), Duration.ofMillis(100), true); //schedule a stack probe operation every 100ms, creating one if necessary
		}
		++stackProbeCount;
	}

	/**
	 * Stops a stack probe.
	 * <p>
	 * This method must be called for each time that {@link #startStackProbe()} is called.
	 * </p>
	 * @throws IllegalStateException if this method is called before {@link #startStackProbe()} is called.
	 * @see #startStackProbe()
	 */
	public synchronized static void stopStackProbe() {
		checkState(stackProbeCount > 0, "Stack probe stop not paired with start.");
		if(--stackProbeCount == 0) { //if we get the correct number of stop requests
			stackProbeOperation.cancel(); //cancel the stack probe operation
			stackProbeOperation = null;
		}
	}

}
