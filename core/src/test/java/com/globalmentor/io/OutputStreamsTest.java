/*
 * Copyright © 2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.io;

import static java.nio.charset.StandardCharsets.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import org.junit.jupiter.api.Test;

/**
 * Tests of {@link OutputStreams}.
 * @author Garret Wilson
 */
public class OutputStreamsTest {

	/** @see OutputStreams#collectBytes(com.globalmentor.io.function.IOConsumer) */
	@Test
	void testCollectBytes() {
		final byte[] testBytes = "abcdefghijklmnopqrstuvwxyz0123456798".getBytes(US_ASCII);
		assertThat(OutputStreams.collectBytes(os -> os.write(testBytes)), is(testBytes));
	}

}
