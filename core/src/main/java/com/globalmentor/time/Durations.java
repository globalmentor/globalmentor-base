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

import static com.globalmentor.iso.ISO8601.*;
import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Conditions.*;
import static java.lang.String.format;

import java.time.Duration;
import java.time.format.DateTimeParseException;

import org.jspecify.annotations.*;

import com.globalmentor.text.ASCII;

/**
 * Utilities for working with {@link Duration}.
 * @author Garret Wilson
 */
public final class Durations {

	private Durations() {
	}

	/**
	 * Parses a duration as a user might enter it. This might be the full ISO 8601 form as accepted by {@link Duration#parse(CharSequence)} (e.g.
	 * <code>P7DT6H5M4.321S</code>, or a form without the <code>P</code> and <code>D</code> ISO 8601 delimiters, without regard to case, e.g.
	 * <code>7d6H5M4.321s</code>. In addition fractional seconds are allowed without a whole digits zero placeholder (e.g. both <code>0.123s</code> and
	 * <code>.123s</code>).
	 * @implSpec A leading minus sign negating the entire time, e.g. <code>-PT6H3M</code> or <code>-PT-6H+3M</code>, is only accepted in the strict
	 *           {@link Duration#parse(CharSequence)} format, as there is no way to provide such a "global" negation without the <code>P</code> designation.
	 * @implNote This implementation, following the current implementation of {@link Duration#parse(CharSequence)}, does not accept any designations (e.g. years)
	 *           with larger granularity than days, although the same value can be indicated using larger values of smaller granularity designations.
	 * @param userInput The user input to parse.
	 * @return The parsed duration.
	 * @throws IllegalArgumentException if the given text cannot be parsed as duration user input or as a strict ISO 8601 duration.
	 */
	public static Duration parseUserInput(@NonNull final CharSequence userInput) {
		final CharSequence text;
		final int userInputLength = userInput.length();
		checkArgument(userInputLength > 0, "Duration input must not be empty.");
		final boolean isSigned = SIGNS.contains(userInput.charAt(0));
		final int unsignedStartIndex = isSigned ? 1 : 0; //determine where the unsigned part starts
		checkArgument(userInputLength > unsignedStartIndex, "Incomplete duration input `%s`.", userInput); //"-" with nothing following
		if(ASCII.equalsIgnoreCase(userInput.charAt(unsignedStartIndex), PERIOD_BEGIN)) { //`P` or `-P` 
			text = userInput; //parse the user input as ISO 8601
		} else { //fix up input to match that expected by Duration.parse()
			final StringBuilder textBuilder = new StringBuilder(userInput);
			final int periodDesignatorInsertionIndex = 0; //always insert `P` at the beginning, because the `-P` form is not supported in non-ISO-8601 format
			textBuilder.insert(periodDesignatorInsertionIndex, PERIOD_BEGIN); //prepend `P`
			final int dayDesignatorIndex = ASCII.indexOfIgnoreCase(textBuilder, DAY_DESIGNATOR, periodDesignatorInsertionIndex + 1);
			//add time designator `T` if needed
			if(dayDesignatorIndex < textBuilder.length() - 1) { //if `D` appears at the last, there is no need for a time designator
				final int timeDesignatorInsertionIndex = dayDesignatorIndex == -1 ? periodDesignatorInsertionIndex + 1 //`PT`
						: dayDesignatorIndex + 1; //`P…DT` 
				textBuilder.insert(timeDesignatorInsertionIndex, TIME_BEGIN); //insert `T`
				//fix up missing seconds decimal sign as appropriate
				final int secondDesignatorIndex = ASCII.indexOfIgnoreCase(textBuilder, SECOND_DESIGNATOR, timeDesignatorInsertionIndex + 1);
				if(secondDesignatorIndex != -1) {
					final int lastSecondNondigitIndex = lastIndexNotOf(textBuilder, ASCII.DIGIT_CHARACTERS, secondDesignatorIndex - 1); //work backwards to find the last non-digit, e.g. `H`/`M`/`.`
					assert lastSecondNondigitIndex >= timeDesignatorInsertionIndex; //if nothing else we should always encounter the `T` we inserted
					final char lastSecondNondigit = textBuilder.charAt(lastSecondNondigitIndex);
					if(DECIMAL_SIGNS.contains(lastSecondNondigit) //if seconds portion contains a decimal at the start, e.g. `T.123S`
							&& !ASCII.DIGIT_CHARACTERS.contains(textBuilder.charAt(lastSecondNondigitIndex - 1))) {
						textBuilder.insert(lastSecondNondigitIndex, '0'); //insert a zero, e.g. `T0.123S`
					}
				}
			}
			text = textBuilder;
		}
		try {
			return Duration.parse(text);
		} catch(final DateTimeParseException dateTimeParseException) {
			throw new IllegalArgumentException(format("Input `%s` is not a valid duration.", userInput), dateTimeParseException);
		}
	}

}
