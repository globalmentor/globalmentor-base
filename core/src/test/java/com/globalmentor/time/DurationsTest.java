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

package com.globalmentor.time;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.time.Duration;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link Durations}.
 * @author Garret Wilson
 */
public class DurationsTest {

	@Test
	void testParseUserInput() {
		//P7DT6H5M4.321S
		assertThat(Durations.parseUserInput("P7DT6H5M4.321S"), is(Duration.parse("P7DT6H5M4.321S")));
		assertThat(Durations.parseUserInput("p7dt6h5m4.321s"), is(Duration.parse("P7DT6H5M4.321S")));
		assertThat(Durations.parseUserInput("7D6H5M4.321S"), is(Duration.parse("P7DT6H5M4.321S")));
		assertThat(Durations.parseUserInput("7d6h5m4.321s"), is(Duration.parse("P7DT6H5M4.321S")));
		//+P7DT6H5M4.321S
		assertThat(Durations.parseUserInput("+P7DT6H5M4.321S"), is(Duration.parse("+P7DT6H5M4.321S")));
		assertThat(Durations.parseUserInput("+p7dt6h5m4.321s"), is(Duration.parse("+P7DT6H5M4.321S")));
		assertThat(Durations.parseUserInput("+7D6H5M4.321S"), is(Duration.parse("+P7DT6H5M4.321S")));
		assertThat(Durations.parseUserInput("+7d6h5m4.321s"), is(Duration.parse("+P7DT6H5M4.321S")));
		//PT20.345S
		assertThat(Durations.parseUserInput("PT20.345S"), is(Duration.parse("PT20.345S")));
		assertThat(Durations.parseUserInput("pt20.345s"), is(Duration.parse("PT20.345S")));
		assertThat(Durations.parseUserInput("PT20,345S"), is(Duration.parse("PT20.345S")));
		assertThat(Durations.parseUserInput("pt20,345s"), is(Duration.parse("PT20.345S")));
		assertThat(Durations.parseUserInput("20.345S"), is(Duration.parse("PT20.345S")));
		assertThat(Durations.parseUserInput("20.345s"), is(Duration.parse("PT20.345S")));
		assertThat(Durations.parseUserInput("20,345S"), is(Duration.parse("PT20.345S")));
		assertThat(Durations.parseUserInput("20,345s"), is(Duration.parse("PT20.345S")));
		//PT2.0S
		assertThat(Durations.parseUserInput("PT2.0S"), is(Duration.parse("PT2.0S")));
		assertThat(Durations.parseUserInput("pt2.0s"), is(Duration.parse("PT2.0S")));
		assertThat(Durations.parseUserInput("PT2,0S"), is(Duration.parse("PT2.0S")));
		assertThat(Durations.parseUserInput("pt2,0s"), is(Duration.parse("PT2.0S")));
		assertThat(Durations.parseUserInput("2.0S"), is(Duration.parse("PT2.0S")));
		assertThat(Durations.parseUserInput("2.0s"), is(Duration.parse("PT2.0S")));
		assertThat(Durations.parseUserInput("2,0S"), is(Duration.parse("PT2.0S")));
		assertThat(Durations.parseUserInput("2,0s"), is(Duration.parse("PT2.0S")));
		//PT2.S
		assertThat(Durations.parseUserInput("PT2.S"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("pt2.s"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("PT2,S"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("pt2,s"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("2.S"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("2.s"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("2,S"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("2,s"), is(Duration.parse("PT2.S")));
		//PT2S
		assertThat(Durations.parseUserInput("PT2S"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("pt2s"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("PT2S"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("pt2s"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("2S"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("2s"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("2S"), is(Duration.parse("PT2.S")));
		assertThat(Durations.parseUserInput("2s"), is(Duration.parse("PT2.S")));
		//PT0.2S
		assertThat(Durations.parseUserInput("PT0.2S"), is(Duration.parse("PT0.2S")));
		assertThat(Durations.parseUserInput("pt0.2s"), is(Duration.parse("PT0.2S")));
		assertThat(Durations.parseUserInput("PT0,2S"), is(Duration.parse("PT0.2S")));
		assertThat(Durations.parseUserInput("pt0,2s"), is(Duration.parse("PT0.2S")));
		assertThat(Durations.parseUserInput("0.2S"), is(Duration.parse("PT0.2S")));
		assertThat(Durations.parseUserInput("0.2s"), is(Duration.parse("PT0.2S")));
		assertThat(Durations.parseUserInput("0,2S"), is(Duration.parse("PT0.2S")));
		assertThat(Durations.parseUserInput("0,2s"), is(Duration.parse("PT0.2S")));
		//seconds missing initial zero (allowed for user input)
		assertThat(Durations.parseUserInput(".2S"), is(Duration.parse("PT0.2S")));
		assertThat(Durations.parseUserInput(".2s"), is(Duration.parse("PT0.2S")));
		assertThat(Durations.parseUserInput(".12S"), is(Duration.parse("PT0.12S")));
		assertThat(Durations.parseUserInput(".12s"), is(Duration.parse("PT0.12S")));
		assertThat(Durations.parseUserInput("4h.123s"), is(Duration.parse("PT4H0.123S")));
		//PT0.001S (one millisecond)
		assertThat(Durations.parseUserInput("PT0.001S"), is(Duration.parse("PT0.001S")));
		assertThat(Durations.parseUserInput("pt0.001s"), is(Duration.parse("PT0.001S")));
		assertThat(Durations.parseUserInput("PT0,001S"), is(Duration.parse("PT0.001S")));
		assertThat(Durations.parseUserInput("pt0,001s"), is(Duration.parse("PT0.001S")));
		assertThat(Durations.parseUserInput("0.001S"), is(Duration.parse("PT0.001S")));
		assertThat(Durations.parseUserInput("0.001s"), is(Duration.parse("PT0.001S")));
		assertThat(Durations.parseUserInput("0,001S"), is(Duration.parse("PT0.001S")));
		assertThat(Durations.parseUserInput("0,001s"), is(Duration.parse("PT0.001S")));
		//PT15M
		assertThat(Durations.parseUserInput("PT15M"), is(Duration.parse("PT15M")));
		assertThat(Durations.parseUserInput("pt15m"), is(Duration.parse("PT15M")));
		assertThat(Durations.parseUserInput("15M"), is(Duration.parse("PT15M")));
		assertThat(Durations.parseUserInput("15m"), is(Duration.parse("PT15M")));
		//PT10H
		assertThat(Durations.parseUserInput("PT10H"), is(Duration.parse("PT10H")));
		assertThat(Durations.parseUserInput("pt10h"), is(Duration.parse("PT10H")));
		assertThat(Durations.parseUserInput("10H"), is(Duration.parse("PT10H")));
		assertThat(Durations.parseUserInput("10h"), is(Duration.parse("PT10H")));
		//P2D
		assertThat(Durations.parseUserInput("P2D"), is(Duration.parse("P2D")));
		assertThat(Durations.parseUserInput("p2d"), is(Duration.parse("P2D")));
		assertThat(Durations.parseUserInput("2D"), is(Duration.parse("P2D")));
		assertThat(Durations.parseUserInput("2d"), is(Duration.parse("P2D")));
		//P2DT3H4M
		assertThat(Durations.parseUserInput("P2DT3H4M"), is(Duration.parse("P2DT3H4M")));
		assertThat(Durations.parseUserInput("p2dt3h4m"), is(Duration.parse("P2DT3H4M")));
		assertThat(Durations.parseUserInput("2D3H4M"), is(Duration.parse("P2DT3H4M")));
		assertThat(Durations.parseUserInput("2d3h4m"), is(Duration.parse("P2DT3H4M")));
		//P-2DT3H4M
		assertThat(Durations.parseUserInput("P-2DT3H4M"), is(Duration.parse("P-2DT3H4M")));
		assertThat(Durations.parseUserInput("p-2dt3h4m"), is(Duration.parse("P-2DT3H4M")));
		assertThat(Durations.parseUserInput("-2D3H4M"), is(Duration.parse("P-2DT3H4M")));
		assertThat(Durations.parseUserInput("-2d3h4m"), is(Duration.parse("P-2DT3H4M")));
		//PT6H3M
		assertThat(Durations.parseUserInput("PT6H3M"), is(Duration.parse("PT6H3M")));
		assertThat(Durations.parseUserInput("pt6h3m"), is(Duration.parse("PT6H3M")));
		assertThat(Durations.parseUserInput("6H3M"), is(Duration.parse("PT6H3M")));
		assertThat(Durations.parseUserInput("6h3m"), is(Duration.parse("PT6H3M")));
		//PT-6H3M
		assertThat(Durations.parseUserInput("PT-6H3M"), is(Duration.parse("PT-6H3M")));
		assertThat(Durations.parseUserInput("pt-6h3m"), is(Duration.parse("PT-6H3M")));
		assertThat(Durations.parseUserInput("-6H3M"), is(Duration.parse("PT-6H3M")));
		assertThat(Durations.parseUserInput("-6h3m"), is(Duration.parse("PT-6H3M")));
		//PT6H-3M
		assertThat(Durations.parseUserInput("PT6H-3M"), is(Duration.parse("PT6H-3M")));
		assertThat(Durations.parseUserInput("pt6h-3m"), is(Duration.parse("PT6H-3M")));
		assertThat(Durations.parseUserInput("6H-3M"), is(Duration.parse("PT6H-3M")));
		assertThat(Durations.parseUserInput("6h-3m"), is(Duration.parse("PT6H-3M")));
		//PT-6H-3M
		assertThat(Durations.parseUserInput("PT-6H-3M"), is(Duration.parse("PT-6H-3M")));
		assertThat(Durations.parseUserInput("pt-6h-3m"), is(Duration.parse("PT-6H-3M")));
		assertThat(Durations.parseUserInput("-6H-3M"), is(Duration.parse("PT-6H-3M")));
		assertThat(Durations.parseUserInput("-6h-3m"), is(Duration.parse("PT-6H-3M")));
		//-PT6H3M (no non-ISO-8601 equivalent)
		assertThat(Durations.parseUserInput("-PT6H3M"), is(Duration.parse("-PT6H3M")));
		assertThat(Durations.parseUserInput("-pt6h3m"), is(Duration.parse("-PT6H3M")));
		assertThrows(IllegalArgumentException.class, () -> Durations.parseUserInput(""), "Empty string not accepted.");
		assertThrows(IllegalArgumentException.class, () -> Durations.parseUserInput("-"), "Sign only not accepted.");
		assertThrows(IllegalArgumentException.class, () -> Durations.parseUserInput("P7D6H5M4.321S"), "Partial ISO 8601 form not accepted.");
		assertThrows(IllegalArgumentException.class, () -> Durations.parseUserInput("7DT6H5M4.321S"), "Mixed ISO 8601 form not accepted.");
	}

}
