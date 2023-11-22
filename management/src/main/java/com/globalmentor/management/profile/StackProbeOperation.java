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

import static com.globalmentor.java.Characters.*;
import static com.globalmentor.java.Classes.*;
import static com.globalmentor.java.Packages.*;
import static com.globalmentor.model.Count.*;
import static com.globalmentor.text.TextFormatter.*;

import static java.util.Objects.*;
import static java.util.concurrent.ConcurrentHashMap.*;
import static java.util.stream.Collectors.*;

import java.io.IOException;
import java.lang.management.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import com.globalmentor.collections.*;
import com.globalmentor.java.Objects;
import com.globalmentor.java.StackTrace;
import com.globalmentor.model.*;

/**
 * Operation that probes the stack.
 * @author Garret Wilson
 */
public class StackProbeOperation extends AbstractReadWriteLockOperation {

	/** The running count of class+method names. */
	@SuppressWarnings("this-escape") //`this` provides access to the locks, which have been initialized; decorator class is trusted not to circumvent interface
	private final ReadWriteLockMap<String, Count> classMethodCounts = new DecoratorReadWriteLockMap<String, Count>(new HashMap<String, Count>(), this);

	/** The MX bean for analyzing current threads. */
	private final ThreadMXBean threadMXBean = ManagementFactory.getThreadMXBean();

	/** The package names to be ignored; these packages include child packages. */
	@SuppressWarnings("this-escape") //`this` provides access to the locks, which have been initialized; decorator class is trusted not to circumvent interface
	private final ReadWriteLockSet<String> ignoreParentPackageNames = new DecoratorReadWriteLockSet<String>(new HashSet<String>(), this);

	/** The explicit package names to be included. */
	@SuppressWarnings("this-escape") //`this` provides access to the locks, which have been initialized; decorator class is trusted not to circumvent interface
	private final ReadWriteLockSet<String> includePackageNames = new DecoratorReadWriteLockSet<String>(new HashSet<String>(), this);

	/** The thread-safe map of line numbers for each class method found. */
	private final Map<String, Set<Integer>> classMethodLineNumbers = new ConcurrentHashMap<>();

	/**
	 * Adds a package to be ignored. Child packages will also be ignored.
	 * @param parentPackage The package to be ignored.
	 * @throws NullPointerException if the given package is <code>null</code>.
	 */
	public void addIgnoreParentPackage(final Package parentPackage) {
		addIgnoreParentPackage(parentPackage.getName());
	}

	/**
	 * Adds a package to be ignored. Child packages will also be ignored.
	 * @param packageName The name of the package to be ignored.
	 * @throws NullPointerException if the given package name is <code>null</code>.
	 */
	public void addIgnoreParentPackage(final String packageName) {
		requireNonNull(packageName);
		writeLock().lock();
		try {
			ignoreParentPackageNames.add(packageName); //add the package name
			includePackageNames.remove(packageName); //if we're ignoring it, it shouldn't be included anymore
			final Iterator<String> includePackageNameIterator = includePackageNames.iterator();
			while(includePackageNameIterator.hasNext()) { //remove child package names we've added because they weren't ignored
				final String includePackageName = includePackageNameIterator.next();
				if(isInsidePackage(packageName, includePackageName)) { //if this package is inside an ignored package, it can't be included
					includePackageNameIterator.remove();
				}
			}
		} finally {
			writeLock().unlock();
		}
	}

	@Override
	protected void execute() throws CancelException {
		final long threadID = Thread.currentThread().getId();
		final ThreadInfo[] threadInfos = threadMXBean.dumpAllThreads(false, false); //get info on all current threads
		for(final ThreadInfo threadInfo : threadInfos) { //look at info on each thread
			if(threadInfo.getThreadId() == threadID) { //ignore ourselves
				continue;
			}
			if(threadInfo.isSuspended()) { //ignore suspended threads (not likely; Thread.suspend() is deprecated)
				continue;
			}
			final Thread.State threadState = threadInfo.getThreadState();
			if(threadState != Thread.State.RUNNABLE) { //ignore threads that are new, blocked, waiting, etc.
				continue;
			}
			final StackTrace stackTrace = new StackTrace(threadInfo.getStackTrace()); //get a stack trace for this thread
			//				if(!executeStackTrace.isTopMethodIntersected(stackTrace)) //ignore the stack trace from this method---that is, ignore our own thread and all stack probes
			for(final StackTraceElement stackTraceElement : stackTrace.getStackTraceElements()) {
				final String className = stackTraceElement.getClassName();
				final String packageName = getPackageName(className);
				Boolean isPackageIncluded = null;
				readLock().lock(); //first see if we know whether this package is included
				try {
					if(ignoreParentPackageNames.contains(packageName)) {
						isPackageIncluded = Boolean.FALSE;
					} else if(includePackageNames.contains(packageName)) {
						isPackageIncluded = Boolean.TRUE;
					}
				} finally {
					readLock().unlock();
				}
				if(isPackageIncluded == null) { //if we don't know whether this package is included, look at all its parent classes to see if any are ignored; if not, the package is included 
					writeLock().lock();
					try {
						for(final String parentPackageName : getParentPackageNames(packageName)) { //look at all the parent packages
							if(ignoreParentPackageNames.contains(parentPackageName)) { //if this parent package is ignored
								isPackageIncluded = Boolean.FALSE;
								ignoreParentPackageNames.add(packageName); //we now know we can exclude the package itself for next time
								break; //stop looking
							}
						}
						if(isPackageIncluded == null) { //at this point, if the package wasn't excluded, we can explicitly include it
							isPackageIncluded = Boolean.TRUE;
							includePackageNames.add(packageName); //we know we can include the package itself for next time
						}
					} finally {
						writeLock().unlock();
					}
				}
				assert isPackageIncluded != null;
				if(isPackageIncluded.booleanValue()) { //if we determined we can include this package
					final String classMethodName = getMethodName(stackTraceElement.getClassName(), stackTraceElement.getMethodName());
					incrementCounterMapCount(classMethodCounts, classMethodName); //increment the number of times we've seen this method
					final int lineNumber = stackTraceElement.getLineNumber();
					if(lineNumber >= 0) { //update our record of line numbers
						classMethodLineNumbers.computeIfAbsent(classMethodName, __ -> newKeySet()).add(lineNumber);
					}
				}
			}
		}
	}

	/**
	 * Returns a list of collected class+method names and their associated counts, in the order of highest to lowest occurrence.
	 * @return A list of collected class+method names and their associated counts, in the order of highest to lowest occurrence.
	 */
	public synchronized List<Map.Entry<String, Count>> getSortedStackProbeCounts() {
		final List<Map.Entry<String, Count>> stackProbeCounts;
		readLock().lock();
		try {
			stackProbeCounts = classMethodCounts.entrySet().stream().map(entry -> Map.entry(entry.getKey(), Objects.clone(entry.getValue())))
					//TODO testing
					.sorted(Map.Entry.<String, Count>comparingByValue().reversed()).collect(toUnmodifiableList()); //clone and get the stack probe counts
		} finally {
			readLock().unlock();
		}
		return stackProbeCounts;
	}

	/**
	 * Prints the currently collected stack probe results, in the order of highest to lowest occurrence.
	 * @param <A> The type of the appendable.
	 * @param appendable The appendable to which the results should be printed.
	 * @return The given appendable.
	 * @throws NullPointerException if the given appendable is <code>null</code>.
	 * @throws IOException if there is an error printing to the given appendable.
	 */
	public <A extends Appendable> A printStackProbeCounts(final A appendable) throws IOException {
		readLock().lock();
		try {
			for(final Map.Entry<String, Count> classMethodCountEntry : getSortedStackProbeCounts()) {
				formatAttribute(appendable, classMethodCountEntry, UNDEFINED_CHAR); //name=value
				final String classMethodName = classMethodCountEntry.getKey();
				final Set<Integer> lineNumbers = classMethodLineNumbers.get(classMethodName);
				if(lineNumbers != null) { //if we have any line numbers
					appendable.append(' ').append('(');
					formatList(appendable, lineNumbers); //format the list of line numbers
					appendable.append(')');
				}
				appendable.append('\n');
			}
		} finally {
			readLock().unlock();
		}
		return appendable;
	}

	/**
	 * Returns default stack probe operation useful for a client application, ignoring packages normally found on a client.
	 * <p>
	 * This implementation ignores packages from the Java API and from the Sun JVM implementation.
	 * </p>
	 * @return A default stack probe operation useful for a client application, ignoring packages normally found on a client.
	 */
	public static StackProbeOperation forClient() {
		final StackProbeOperation stackProbeOperation = new StackProbeOperation();
		stackProbeOperation.addIgnoreParentPackage("java");
		stackProbeOperation.addIgnoreParentPackage("javax");
		stackProbeOperation.addIgnoreParentPackage("sun");
		stackProbeOperation.addIgnoreParentPackage("com.sun");
		return stackProbeOperation;
	}

	/**
	 * Returns default stack probe operation useful for a server application, ignoring packages normally found on a server.
	 * <p>
	 * This implementation ignores packages for Apache Tomcat.
	 * </p>
	 * @return A default stack probe operation useful for a server application, ignoring packages normally found on a server.
	 */
	public static StackProbeOperation forServer() {
		final StackProbeOperation stackProbeOperation = forClient(); //start out with a default client stack probe operation
		stackProbeOperation.addIgnoreParentPackage("org.apache.catalina");
		stackProbeOperation.addIgnoreParentPackage("org.apache.coyote");
		stackProbeOperation.addIgnoreParentPackage("org.apache.tomcat");
		return stackProbeOperation;
	}

}
