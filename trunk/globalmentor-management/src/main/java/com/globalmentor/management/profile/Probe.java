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
import static com.globalmentor.model.Count.*;
import static java.util.Collections.*;

//import java.io.IOException;
import java.io.IOException;
import java.lang.management.*;
import java.util.*;

import com.globalmentor.collections.Maps;
import com.globalmentor.collections.comparators.SortOrder;
import com.globalmentor.java.StackTrace;
//import com.globalmentor.log.Log;
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

	private static long stackProbeCount = 0;

	private static final Map<String, Count> methodCounters = new HashMap<String, Count>();

	/**
	 * Starts a stack probe.
	 * <p>
	 * Every call to this method must have a later call to {@link #stopStackProbe()}.
	 * </p>
	 * @see #stopStackProbe()
	 */
	public synchronized static void startStackProbe()
	{
		if(stackProbeOperation == null)
		{
			stackProbeOperation = new StackProbeOperation();
			probeOperationManager.schedule(stackProbeOperation, Duration.of(100), true); //schedule a stack probe operation every 100ms
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
	public synchronized static void stopStackProbe()
	{
		checkState(stackProbeCount > 0, "Stack probe stop not paired with start.");
		if(--stackProbeCount == 0)
		{
			stackProbeOperation.cancel(); //cancel the stack probe operation
			stackProbeOperation = null;
		}
	}

	/** Retrieves a list of collected class+method names and their associated counts, in the order of highest to lowest occurrence. */
	public synchronized static List<NameValuePair<String, Count>> getSortedStackProbeCounts()
	{
		//get the stack probe counts
		final List<NameValuePair<String, Count>> stackProbeCounts = Maps.getKeyValuesCloned(methodCounters, new ArrayList<NameValuePair<String, Count>>());
		sort(stackProbeCounts, new ValuedComparator<Count>(SortOrder.DESCENDING)); //sort the stack probe counts in order of highest to lowest count
		return stackProbeCounts;
	}

	/**
	 * Prints the currently collected stack probe results, in the order of highest to lowest occurrence.
	 * @param appendable The appendable to which the results should be printed.
	 * @return The given appendable.
	 * @throws NullPointerException if the given appendable is <code>null</code>.
	 * @throws IOException if there is an error printing to the given appendable.
	 */
	public static <A extends Appendable> A printStackProbeCounts(final A appendable) throws IOException
	{
		for(final NameValuePair<String, Count> stackProbeCount : getSortedStackProbeCounts())
		{
			appendable.append(stackProbeCount.toString()).append('\n');
		}
		return appendable;
	}

	/**
	 * The operation that probes the stack.
	 * @author Garret Wilson
	 */
	protected static class StackProbeOperation extends AbstractOperation
	{

		/** The stack trace of the execute method. */
		//		private StackTrace executeStackTrace;

		private final ThreadMXBean threadMXBean = ManagementFactory.getThreadMXBean();

		@Override
		protected void execute() throws CancelException
		{
			/*
						if(executeStackTrace == null) //create a new stack trace to represent this method
						{
							executeStackTrace = new StackTrace();
							try
							{
								executeStackTrace.print(System.out);
							}
							catch(final IOException ioException)
							{
								throw unexpected(ioException);
							}
						}
			*/
			final long threadID = Thread.currentThread().getId();
			final ThreadInfo[] threadInfos = threadMXBean.dumpAllThreads(false, false); //get info on all current threads
			for(final ThreadInfo threadInfo : threadInfos) //look at info on each thread
			{
				if(threadInfo.getThreadId() != threadID) //ignore ourselves
				{
					final StackTrace stackTrace = new StackTrace(threadInfo.getStackTrace()); //get a stack trace for this thread
					//				if(!executeStackTrace.isTopMethodIntersected(stackTrace)) //ignore the stack trace from this method---that is, ignore our own thread and all stack probes
					for(final StackTraceElement stackTraceElement : stackTrace.getStackTraceElements())
					{
						final String fullMethodName = stackTraceElement.getClassName() + PACKAGE_SEPARATOR + stackTraceElement.getMethodName();
						incrementCounterMapCount(methodCounters, fullMethodName); //increment the number of times we've seen this method
					}
				}
			}
		}
	}

}
