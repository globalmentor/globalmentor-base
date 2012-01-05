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

package com.globalmentor.java;

import static com.globalmentor.java.Conditions.*;

import java.io.*;
import java.util.List;

import com.globalmentor.collections.Lists;

/**
 * Convenience class and methods for working with stack traces.
 * 
 * @author Garret Wilson
 */
public class StackTrace
{

	/** The list of stack trace elements. */
	private final List<StackTraceElement> stackTraceElements;

	/** @return A read only list of elements in this stack trace, with the deepest elements at the front of the list. */
	public List<StackTraceElement> getStackTraceElements()
	{
		return stackTraceElements;
	}

	/**
	 * Creates a stack trace from the current caller's location. The stack trace includes all elements up to the caller; it does not include this constructor
	 * call.
	 */
	public StackTrace()
	{
		final StackTraceElement[] stackTraceElements = new Throwable().getStackTrace(); //get the current stack trace elements
		assert stackTraceElements.length > 1 : "A caller must have called this constructor, so there must be a stack trace with more than one element.";
		this.stackTraceElements = Lists.immutableListOf(stackTraceElements, 1, stackTraceElements.length);
	}

	/**
	 * Creates a stack trace from an array of stack elements.
	 * @param stackTraceElements Stack elements from which to create a stack trace.
	 * @throws NullPointerException if the given array of stack trace elements is <code>null</code>.
	 */
	public StackTrace(final StackTraceElement[] stackTraceElements)
	{
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
	public StackTrace(final StackTraceElement[] stackTraceElements, final int start, final int end)
	{
		this.stackTraceElements = Lists.immutableListOf(stackTraceElements, start, end);
	}

	/**
	 * Prints this stack trace to the specified appendable in a format similar to {@link Throwable#printStackTrace(PrintStream)}.
	 * @param appendable The appendable to use for output.
	 * @throws NullPointerException if the given appendable is <code>null</code>.
	 * @throws IOException if there is an error printing to the appendable.
	 */
	public <A extends Appendable> A print(final A appendable) throws IOException
	{
		for(final StackTraceElement stackTraceElement : getStackTraceElements())
		{
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
	public boolean isTopMethodIntersected(final StackTrace stackTrace)
	{
		final List<StackTraceElement> stackTraceElements1 = getStackTraceElements();
		final List<StackTraceElement> stackTraceElements2 = stackTrace.getStackTraceElements();
		if(stackTraceElements1.isEmpty() || stackTraceElements2.isEmpty())
		{
			return false;
		}
		final StackTraceElement stackTraceElement1 = stackTraceElements.get(0);
		final Class<?> stackTraceElementClass = stackTraceElement1.getClass();
		final String stackTraceElementMethodName = stackTraceElement1.getMethodName();
		for(final StackTraceElement stackTraceElement : stackTraceElements2) //look at the stack trace elements of the other stack trace
		{
			if(stackTraceElement.getClass().equals(stackTraceElementClass) && stackTraceElement.getMethodName().equals(stackTraceElementMethodName)) //if the classes and methods are the same
			{
				return true;
			}
		}
		return false;
	}

	@Override
	public String toString()
	{
		try
		{
			return print(new StringBuilder()).toString();
		}
		catch(final IOException ioException)
		{
			throw unexpected(ioException);
		}
	}
}
