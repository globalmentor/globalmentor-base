/*
 * Copyright © 2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

/**
 * Various checks on objects given as arguments in a method.
 * 
 * <p>
 * The name of this class was inspired by the <a href="http://code.google.com/p/guava-libraries/">Google Guava Library</a>.
 * </p>
 * 
 * @author Garret Wilson
 */
public class Preconditions
{

	/**
	 * Checks the results of an expression to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is <code>false</code>.
	 * @param test The result of the test.
	 * @exception IllegalArgumentException if the given value is <code>false</code>
	 */
	public static void checkArgument(final boolean test)
	{
		checkArgument(test, null); //check the test with no description
	}

	/**
	 * Checks the results of an expression to see if an argument is correct, and throws an {@link IllegalArgumentException} if the value is <code>false</code>.
	 * @param test The result of the test.
	 * @param description A description of the test to be used when generating an exception, or <code>null</code> for no description.
	 * @exception IllegalArgumentException if the given value is <code>false</code>
	 */
	public static void checkArgument(final boolean test, final String description)
	{
		if(!test)
		{
			throw new IllegalArgumentException(description);
		}
	}

}
