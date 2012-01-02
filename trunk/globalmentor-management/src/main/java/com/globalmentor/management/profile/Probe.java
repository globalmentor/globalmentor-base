/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.management.profile;

import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Java.*;
import static com.globalmentor.model.Counter.*;

import java.lang.management.*;
import java.util.HashMap;
import java.util.Map;

import com.globalmentor.model.*;
import com.globalmentor.time.Duration;

/**
 * Profiling class for brute-force probing executing threads at a given interval.
 * @author Garret Wilson
 */
public class Probe
{

	/** The operation manager for managing probe operations. */
	private final static OperationManager probeOperationManager = new OperationManager();

	/** The current stack probe operation, or <code>null</code> if there is no stack probe in progress. */
	private static Operation stackProbeOperation = null;

	private static final Map<String, Counter> methodCounters = new HashMap<String, Counter>();

	/**
	 * Starts a stack probe.
	 * @throws IllegalStateException if a stack probe is already started.
	 */
	public synchronized static void startStackProbe()
	{
		checkState(stackProbeOperation == null, "Stack probe already started.");
		stackProbeOperation = new StackProbeOperation();
		probeOperationManager.schedule(stackProbeOperation, Duration.of(100), true); //schedule a stack probe operation every 100ms
	}

	/**
	 * Stops a stack probe.
	 * @throws IllegalStateException if a stack probe is not started.
	 */
	public synchronized static void stopStackProbe()
	{
		checkState(stackProbeOperation != null, "Stack probe not started.");
		stackProbeOperation.cancel(); //cancel the stack probe operation
		stackProbeOperation = null;
	}

	/**
	 * The operation that probes the stack.
	 * @author Garret Wilson
	 */
	protected static class StackProbeOperation extends AbstractOperation
	{

		final ThreadMXBean threadMXBean = ManagementFactory.getThreadMXBean();

		@Override
		protected void execute()
		{
			final ThreadInfo[] threadInfos = threadMXBean.dumpAllThreads(false, false); //get info on all current threads
			for(final ThreadInfo threadInfo : threadInfos) //look at info on each thread
			{
				final StackTraceElement[] stackTrace = threadInfo.getStackTrace(); //get a stack trace for this thread
				for(final StackTraceElement stackTraceElement : stackTrace)
				{
					final String fullMethodName = stackTraceElement.getClassName() + PACKAGE_SEPARATOR + stackTraceElement.getMethodName();
					incrementCounterMapCount(methodCounters, fullMethodName); //increment the number of times we've seen this method
				}
			}
		}
	}

}
