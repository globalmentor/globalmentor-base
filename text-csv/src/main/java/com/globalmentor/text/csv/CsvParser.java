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

package com.globalmentor.text.csv;

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Characters.*;
import static com.globalmentor.text.csv.CSV.*;
import static java.lang.Math.*;
import static java.util.Objects.*;

import java.text.ParseException;
import java.util.*;

/**
 * Parses Comma Separated Value (CSV) information.
 * @author Garret Wilson
 * @see <a href="https://www.rfc-editor.org/rfc/rfc4180.html">RFC 4180: Common Format and MIME Type for Comma-Separated Values (CSV) Files</a>
 * @see <a href="https://www.rfc-editor.org/rfc/inline-errata/rfc4180.html">RFC 4180 with errata</a>
 */
public class CsvParser {

	/** Constructor. */
	public CsvParser() {
	}

	/**
	 * Parses the CSV fields from a line of text. The line will always have at least one field, i.e. an empty string will yield an array with a single empty
	 * character sequence.
	 * @apiNote This method assumes that the caller has some way to guarantee that the complete line is given. CSV allows for quoted fields to contain newline
	 *          characters, so a naive parsing approach that merely parses a file and collects lines before examining the content will produce partial lines if
	 *          any quoted fields contain newlines. Nevertheless this method will produce an error if such a condition is detected.
	 * @apiNote This method uses arrays rather than more modern collections; combined with {@link Field} instead of {@link String}, in preparation for future
	 *          implementations that will provide more efficient parsing and extended options for efficiency.
	 * @param line The line of text to parse.
	 * @return An array of parsed fields.
	 * @throws ParseException if an error was encountered parsing the line.
	 */
	public Field[] parseLine(final CharSequence line) throws ParseException {
		final List<CharSequence> fields = new ArrayList<>();
		final int length = line.length();
		int fieldStartIndex = 0;
		int fieldIndex = -1;
		boolean hasMoreFields = true; //indicates the start of parsing or a trailing delimiter
		do {
			fieldIndex++;
			Field field;
			if(fieldStartIndex >= length) { //if we've reached the end of the line
				field = Field.EMPTY;
				hasMoreFields = false;
			} else {
				if(line.charAt(fieldStartIndex) == QUOTATION_MARK_CHAR) {
					StringBuilder stringBuilder = null; //we'll only build a string if we have escaped quotes
					boolean hasMoreRuns = true;
					int runStartIndex = fieldStartIndex + 1;
					int nextQuoteIndex;
					do {
						nextQuoteIndex = indexOf(line, QUOTATION_MARK_CHAR, runStartIndex);
						if(nextQuoteIndex == -1) {
							throw new ParseException("Quoted field has no ending quote.", fieldStartIndex);
						}
						if(nextQuoteIndex < length - 1 && line.charAt(nextQuoteIndex + 1) == QUOTATION_MARK_CHAR) { //two subsequent double quotes signifies an escaped double quote 
							if(stringBuilder == null) { //we'll need to construct the unescaped string
								stringBuilder = new StringBuilder(max((nextQuoteIndex - runStartIndex) * 2, 16)); //start with enough capacity for at least double the size of the current field (assuming that on average the quote would be in the middle of the string)
							}
							stringBuilder.append(line, runStartIndex, nextQuoteIndex); //append everything we've would have collected so far, skipping the initial quote
							stringBuilder.append(QUOTATION_MARK_CHAR); //unescape the quote
							runStartIndex = nextQuoteIndex + 2;
						} else { //if we've reached the ending quote
							hasMoreRuns = false; //a quoted string always has more runs until it reaches its ending quote
						}
					} while(hasMoreRuns);
					if(stringBuilder != null) { //if we have been collecting characters (because we encountered an escape quote)
						if(nextQuoteIndex > runStartIndex) { //if there are remaining characters to collect
							stringBuilder.append(line, runStartIndex, nextQuoteIndex);
						}
						field = new CharSequenceField(stringBuilder.toString());
					} else { //if nothing was escaped
						field = new CharSequenceField(line.subSequence(fieldStartIndex + 1, nextQuoteIndex)); //skip the initial quote, and don't include the final quote
					}
					final int afterQuotedFieldIndex = nextQuoteIndex + 1;
					if(afterQuotedFieldIndex == length) { //if the ending quote was the last character on the line
						hasMoreFields = false;
					} else {
						final char nextChar = line.charAt(afterQuotedFieldIndex);
						if(nextChar == FIELD_DELIMITER_CHAR) { //if we're not at the end of the line, the only valid possibility for the next character is a field delimiter
							fieldStartIndex = afterQuotedFieldIndex + 1; //skip the field delimiter
						} else {
							throw new ParseException("Unexpected character %s; expected field delimiter %s.".formatted(toLabel(nextChar), toLabel(FIELD_DELIMITER_CHAR)),
									afterQuotedFieldIndex);
						}
					}
				} else {
					final int nextFieldDelimiterIndex = indexOf(line, FIELD_DELIMITER_CHAR, fieldStartIndex);
					final boolean hasNextFieldDelimiter = nextFieldDelimiterIndex != -1;
					final int fieldEndIndex = hasNextFieldDelimiter ? nextFieldDelimiterIndex : length;
					field = new CharSequenceField(line.subSequence(fieldStartIndex, fieldEndIndex));
					fieldStartIndex = fieldEndIndex + 1; //skip the delimiter; if the end of the line was reached, this will increase the index beyond the length of the line, which is benign
					hasMoreFields = hasNextFieldDelimiter;
				}
			}
			assert fields.size() == fieldIndex;
			fields.add(field);
		} while(hasMoreFields);
		return fields.toArray(Field[]::new);
	}

	/**
	 * Represents a parsed field in a CSV record, providing access to convenience data type specific retrieval.
	 * @apiNote <strong>Important: Long-term references should not be kept to field instances.</strong> Their contents may change after further parsing, and they
	 *          maintain references to much more data than is accessible in the field itself. If a long-term reference to the field information is needed,
	 *          retrieve a string using {@link #toString()}. It is preferable and potentially much more efficient to access the field temporarily using its
	 *          {@link CharSequence} accessor methods, or to use one of the value type converter methods.
	 * @author Garret Wilson
	 */
	public interface Field extends CharSequence {

		/** A shared reference to an immutable, empty field. */
		public static final Field EMPTY = new CharSequenceField("");

	}

	/**
	 * A field backed by a another {@link CharSequence}.
	 * @apiNote This is a provisional class and may be replace with a more efficient version that allows changing of the backing {@link CharSequence} and its
	 *          range without creation of a new field.
	 */
	private static class CharSequenceField implements Field {

		private final CharSequence charSequence;

		/**
		 * Constructor.
		 * @param charSequence The backing character sequence.
		 */
		public CharSequenceField(final CharSequence charSequence) {
			this.charSequence = requireNonNull(charSequence);
		}

		@Override
		public int length() {
			return charSequence.length();
		}

		@Override
		public char charAt(final int index) {
			return charSequence.charAt(index);
		}

		@Override
		public CharSequence subSequence(final int start, final int end) {
			return charSequence.subSequence(start, end);
		}

		@Override
		public String toString() {
			return charSequence.toString();
		}

	}

}
