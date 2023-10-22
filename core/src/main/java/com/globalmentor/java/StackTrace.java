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

package com.globalmentor.java;

import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Packages.*;
import static java.util.Arrays.*;
import static java.util.stream.Collectors.*;

import java.io.*;
import java.util.*;

/**
 * Convenience class and methods for working with stack traces.
 * 
 * @author Garret Wilson
 */
public class StackTrace {

	/** The list of stack trace elements. */
	private final List<StackTraceElement> stackTraceElements;

	/** @return A read only list of elements in this stack trace, with the deepest elements at the front of the list. */
	public List<StackTraceElement> getStackTraceElements() {
		return stackTraceElements;
	}

	/**
	 * Creates a stack trace from the current caller's location. The stack trace includes all elements up to the caller; it does not include this constructor
	 * call.
	 */
	public StackTrace() {
		final StackTraceElement[] stackTraceElements = new Throwable().getStackTrace(); //get the current stack trace elements
		assert stackTraceElements.length > 1 : "A caller must have called this constructor, so there must be a stack trace with more than one element.";
		this.stackTraceElements = stream(stackTraceElements, 1, stackTraceElements.length).collect(toUnmodifiableList());
	}

	/**
	 * Creates a stack trace from an array of stack elements.
	 * @param stackTraceElements Stack elements from which to create a stack trace.
	 * @throws NullPointerException if the given array of stack trace elements is <code>null</code>.
	 */
	public StackTrace(final StackTraceElement[] stackTraceElements) {
		this(stackTraceElements, 0, stackTraceElements.length);
	}

	/**
	 * Creates a stack trace from an array of stack elements.
	 * @param stackTraceElements Stack elements from which to create a stack trace.
	 * @param start the initial index of the range to be included, inclusive
	 * @param end the final index of the range to be included, exclusive.
	 * @throws NullPointerException if the given array of stack trace elements is <code>null</code>.
	 * @throws IllegalArgumentException if the start index is greater than the end index.
	 * @throws ArrayIndexOutOfBoundsException if the start index is less than zero or the end index is greater than the length.
	 */
	public StackTrace(final StackTraceElement[] stackTraceElements, final int start, final int end) {
		this.stackTraceElements = stream(stackTraceElements, start, end).collect(toUnmodifiableList());
	}

	/**
	 * Prints this stack trace to the specified appendable in a format similar to {@link Throwable#printStackTrace(PrintStream)}.
	 * @param <A> The type of appendable.
	 * @param appendable The appendable to use for output.
	 * @return The given appendable.
	 * @throws NullPointerException if the given appendable is <code>null</code>.
	 * @throws IOException if there is an error printing to the appendable.
	 */
	public <A extends Appendable> A print(final A appendable) throws IOException {
		for(final StackTraceElement stackTraceElement : getStackTraceElements()) {
			appendable.append("\tat ").append(stackTraceElement.toString()).append('\n');
			/*TODO fix cause
			          Throwable ourCause = getCause();
			          if (ourCause != null)
			              ourCause.printStackTraceAsCause(s, trace);
			*/
		}
		return appendable;
	}

	/**
	 * Determines if the given stack trace passes through the top method of this stack.
	 * @param stackTrace The stack trace to compare with this one.
	 * @return <code>true</code> if the class and method of the initial stack trace element of this stack trace is found in the given stack trace.
	 * @throws NullPointerException if the given stack trace is <code>null</code>.
	 */
	public boolean isCurrentMethodIntersected(final StackTrace stackTrace) {
		final List<StackTraceElement> stackTraceElements1 = getStackTraceElements();
		final List<StackTraceElement> stackTraceElements2 = stackTrace.getStackTraceElements();
		if(stackTraceElements1.isEmpty() || stackTraceElements2.isEmpty()) {
			return false;
		}
		final StackTraceElement stackTraceElement1 = stackTraceElements.get(0);
		final Class<?> stackTraceElementClass = stackTraceElement1.getClass();
		final String stackTraceElementMethodName = stackTraceElement1.getMethodName();
		for(final StackTraceElement stackTraceElement : stackTraceElements2) { //look at the stack trace elements of the other stack trace
			if(stackTraceElement.getClass().equals(stackTraceElementClass) && stackTraceElement.getMethodName().equals(stackTraceElementMethodName)) { //if the classes and methods are the same
				return true;
			}
		}
		return false;
	}

	/**
	 * Retrieves the class representing the caller of the class calling this method.
	 * <p>
	 * The current stack trace element is assumed to be in this method itself, so it is ignored. The stack trace element before that is assumed to be the called
	 * to <em>this</em> method, and it is ignored as well. Therefore the class of the stack trace element at least two levels deep that isn't of the ignored
	 * packaged is returned.
	 * </p>
	 * @param ignorePackage The package to ignore, or <code>null</code> if no package should be ignored.
	 * @return The calling class, or <code>null</code> if no stack trace element meeting the given criteria could be found.
	 */
	public static Class<?> getCallingClass(final Package ignorePackage) {
		final StackTraceElement callingStackTraceElement = getCallingStackTraceElement(ignorePackage);
		try {
			return callingStackTraceElement != null ? Class.forName(callingStackTraceElement.getClassName()) : null; //determine the class of the calling stack trace element
		} catch(final ClassNotFoundException classNotFoundException) { //since the class is calling us, the class should exist and already be loaded
			throw unexpected(classNotFoundException);
		}
	}

	/**
	 * Retrieves the stack trace element representing the caller of the class calling this method.
	 * <p>
	 * The current stack trace element is assumed to be in this method itself, so it is ignored. The stack trace element before that is assumed to be the called
	 * to <em>this</em> method, and it is ignored as well. Therefore the stack trace element at least two levels deep that isn't of the ignored packaged is
	 * returned.
	 * </p>
	 * @param ignorePackage The package to ignore, or <code>null</code> if no package should be ignored.
	 * @return The calling stack trace element, or <code>null</code> if no stack trace element meeting the given criteria could be found.
	 */
	public static StackTraceElement getCallingStackTraceElement(final Package ignorePackage) {
		final StackTraceElement[] stackTraceElements = new Throwable().getStackTrace(); //get the current stack TODO integrate new StackTrace class
		final int stackTraceElementsLength = stackTraceElements.length;
		if(stackTraceElementsLength < 3) {
			return null;
		}
		if(ignorePackage == null) { //if we shouldn't ignore any package
			return stackTraceElements[2]; //return this caller's caller
		}
		final String ignorePackageName = ignorePackage.getName();
		for(int i = 2; i < stackTraceElementsLength; ++i) {
			final StackTraceElement stackTraceElement = stackTraceElements[i];
			if(!ignorePackageName.equals(getPackageName(stackTraceElement.getClassName()))) { //if this stack trace element isn't from the ignored package
				return stackTraceElement;
			}
		}
		return null;
	}

	/**
	 * Retrieves the first stack trace element that is not from one of the ignored classes.
	 * @param ignoreClasses The classes to ignore.
	 * @return The stack trace element ignoring the classes, or <code>null</code> if there is no stack trace element without one of the ignored classes.
	 */
	public StackTraceElement getStackTraceElementIgnoreClasses(final Class<?>... ignoreClasses) {
		final Set<String> ignoreClassNames = new HashSet<String>(ignoreClasses.length);
		for(final Class<?> ignoreClass : ignoreClasses) {
			ignoreClassNames.add(ignoreClass.getName());
		}
		return getStackTraceElementIgnoreClasses(ignoreClassNames);
	}

	/**
	 * Retrieves the first stack trace element that is not from one of the ignored classes.
	 * @param ignoreClassNames The names of the classes to ignore.
	 * @return The stack trace element ignoring the named classes, or <code>null</code> if there is no stack trace element without one of the ignored class names.
	 */
	public StackTraceElement getStackTraceElementIgnoreClasses(final String... ignoreClassNames) {
		return getStackTraceElementIgnoreClasses(Set.of(ignoreClassNames));
	}

	/**
	 * Retrieves the first stack trace element that is not from one of the ignored classes.
	 * @param ignoreClassNames The names of the classes to ignore.
	 * @return The stack trace element ignoring the named classes, or <code>null</code> if there is no stack trace element without one of the ignored class names.
	 */
	public StackTraceElement getStackTraceElementIgnoreClasses(final Set<String> ignoreClassNames) {
		for(final StackTraceElement stackTraceElement : getStackTraceElements()) { //look at the stack trace elements
			if(!ignoreClassNames.contains(stackTraceElement.getClassName())) { //if this stack trace element doesn't have an ignored class
				return stackTraceElement;
			}
		}
		return null; //we couldn't find a stack trace element without one of the ignored classes
	}

	/**
	 * Retrieves the first stack trace element that is not from one of the ignored packages.
	 * @param ignorePackages The packages to ignore.
	 * @return The stack trace element ignoring the named packages, or <code>null</code> if there is no stack trace element without one of the ignored packages.
	 */
	public StackTraceElement getStackTraceElementIgnorePackages(final Package... ignorePackages) {
		final Set<String> ignorePackageNames = new HashSet<String>(ignorePackages.length);
		for(final Package ignorePackage : ignorePackages) {
			ignorePackageNames.add(ignorePackage.getName());
		}
		return getStackTraceElementIgnoreClasses(ignorePackageNames);
	}

	/**
	 * Retrieves the first stack trace element that is not from one of the ignored packages.
	 * @param ignorePackageNames The names of the packages to ignore.
	 * @return The stack trace element ignoring the named packages, or <code>null</code> if there is no stack trace element without one of the ignored package
	 *         names.
	 */
	public StackTraceElement getStackTraceElementIgnorePackages(final String... ignorePackageNames) {
		return getStackTraceElementIgnorePackages(Set.of(ignorePackageNames));
	}

	/**
	 * Retrieves the first stack trace element that is not from one of the ignored packages.
	 * @param ignorePackageNames The names of the packages to ignore.
	 * @return The stack trace element ignoring the named packages, or <code>null</code> if there is no stack trace element without one of the ignored package
	 *         names.
	 */
	public StackTraceElement getStackTraceElementIgnorePackages(final Set<String> ignorePackageNames) {
		for(final StackTraceElement stackTraceElement : getStackTraceElements()) { //look at the stack trace elements
			if(!ignorePackageNames.contains(getPackageName(stackTraceElement.getClassName()))) { //if this stack trace element doesn't have an ignored package
				return stackTraceElement;
			}
		}
		return null; //we couldn't find a stack trace element without one of the ignored packages
	}

	@Override
	public String toString() {
		try {
			return print(new StringBuilder()).toString();
		} catch(final IOException ioException) {
			throw unexpected(ioException);
		}
	}
}
