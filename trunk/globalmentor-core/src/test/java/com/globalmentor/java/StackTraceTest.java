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

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import org.junit.Test;

/**
 * Tests of a stack trace.
 * @author Garret Wilson
 */
public class StackTraceTest
{

	private final StackTrace classVariableStackTrace = new StackTrace();

	@Test
	public void testStackElements()
	{
		final StackTraceElement[] stackTraceElements = new Throwable().getStackTrace();
		assertThat("Stack trace elements not the length expected.", new StackTrace().getStackTraceElements().size(), equalTo(stackTraceElements.length));
	}

	@Test
	public void testEqualsMethod()
	{
		final StackTrace methodStackTrace1 = new StackTrace();
		final StackTrace methodStackTrace2 = new StackTrace();
		assertTrue(methodStackTrace1.isCurrentMethodIntersected(methodStackTrace2));
		assertFalse(methodStackTrace1.isCurrentMethodIntersected(classVariableStackTrace));
	}

}
