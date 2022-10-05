/*
 * Copyright © 2022 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.net;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link HTTP}.
 * @author Garret Wilson
 */
public class HTTPTest {

	/** @see HTTP.ResponseClass#forStatusCode(int) */
	@Test
	void testResponseClassForStatusCode() {
		assertThrows(IllegalArgumentException.class, () -> HTTP.ResponseClass.forStatusCode(-123));
		assertThrows(IllegalArgumentException.class, () -> HTTP.ResponseClass.forStatusCode(-1));
		assertThrows(IllegalArgumentException.class, () -> HTTP.ResponseClass.forStatusCode(0));
		assertThrows(IllegalArgumentException.class, () -> HTTP.ResponseClass.forStatusCode(1));
		assertThrows(IllegalArgumentException.class, () -> HTTP.ResponseClass.forStatusCode(50));
		assertThrows(IllegalArgumentException.class, () -> HTTP.ResponseClass.forStatusCode(99));
		assertThat(HTTP.ResponseClass.forStatusCode(100), is(HTTP.ResponseClass.INFORMATIONAL));
		assertThat(HTTP.ResponseClass.forStatusCode(101), is(HTTP.ResponseClass.INFORMATIONAL));
		assertThat(HTTP.ResponseClass.forStatusCode(123), is(HTTP.ResponseClass.INFORMATIONAL));
		assertThat(HTTP.ResponseClass.forStatusCode(199), is(HTTP.ResponseClass.INFORMATIONAL));
		assertThat(HTTP.ResponseClass.forStatusCode(200), is(HTTP.ResponseClass.SUCCESSFUL));
		assertThat(HTTP.ResponseClass.forStatusCode(299), is(HTTP.ResponseClass.SUCCESSFUL));
		assertThat(HTTP.ResponseClass.forStatusCode(300), is(HTTP.ResponseClass.REDIRECTION));
		assertThat(HTTP.ResponseClass.forStatusCode(399), is(HTTP.ResponseClass.REDIRECTION));
		assertThat(HTTP.ResponseClass.forStatusCode(400), is(HTTP.ResponseClass.CLIENT_ERROR));
		assertThat(HTTP.ResponseClass.forStatusCode(499), is(HTTP.ResponseClass.CLIENT_ERROR));
		assertThat(HTTP.ResponseClass.forStatusCode(500), is(HTTP.ResponseClass.SERVER_ERROR));
		assertThat(HTTP.ResponseClass.forStatusCode(599), is(HTTP.ResponseClass.SERVER_ERROR));
		assertThrows(IllegalArgumentException.class, () -> HTTP.ResponseClass.forStatusCode(600));
		assertThrows(IllegalArgumentException.class, () -> HTTP.ResponseClass.forStatusCode(699));
		assertThrows(IllegalArgumentException.class, () -> HTTP.ResponseClass.forStatusCode(999));
		assertThrows(IllegalArgumentException.class, () -> HTTP.ResponseClass.forStatusCode(1000));
	}

}
