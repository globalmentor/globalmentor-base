/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.time;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

/**
 * Tests for millisecond processing.
 * 
 * @author Garret Wilson
 */
public class MillisecondsTest {

	@Test
	public void testSeconds() {
		assertEquals(5 * 1000, Milliseconds.fromSeconds(5));
	}

	@Test
	public void testMinutes() {
		assertEquals(5 * 60 * 1000, Milliseconds.fromMinutes(5));
	}

	@Test
	public void testHours() {
		assertEquals(5 * 60 * 60 * 1000, Milliseconds.fromHours(5));
	}

	@Test
	public void testDays() {
		assertEquals(5 * 24 * 60 * 60 * 1000, Milliseconds.fromDays(5));
	}

}
