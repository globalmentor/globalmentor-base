/*
 * Copyright © 2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.io.*;

/**
 * Utilities for working with throwbles and exceptions.
 * @author Garret Wilson
 */
public class Throwables {

	/**
	 * Returns a string representing the stack trace of the given throwable. This implementation returns a string in the same format as produced by
	 * {@link Throwable#printStackTrace(PrintWriter)}.
	 * @param throwable The throwable for which a strack trace string should be produced.
	 * @return A string representation of the throwable's stack trace.
	 * @throws NullPointerException if the given throwable is <code>null</code>.
	 * @see Throwable#printStackTrace(PrintWriter)
	 */
	public static String getStackTraceString(final Throwable throwable) {
		final StringWriter stringWriter = new StringWriter();
		final PrintWriter printWriter = new PrintWriter(stringWriter);
		throwable.printStackTrace(printWriter); //print to the print writer which prints to the string writer which prints to the string
		printWriter.flush(); //flush the output
		return stringWriter.toString(); //return the printed string
	}

}
