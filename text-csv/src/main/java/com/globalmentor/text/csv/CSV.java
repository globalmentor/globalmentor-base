/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.io.*;
import java.nio.file.Path;

import com.globalmentor.java.Characters;
import com.globalmentor.net.MediaType;

import static com.globalmentor.java.Characters.*;
import static java.nio.charset.StandardCharsets.*;
import static java.nio.file.Files.*;
import static java.nio.file.StandardOpenOption.*;

/**
 * Constant values for Comma Separated Value (CSV) files.
 * @author Garret Wilson
 * @see <a href="https://www.rfc-editor.org/rfc/rfc4180.html">RFC 4180: Common Format and MIME Type for Comma-Separated Values (CSV) Files</a>
 * @see <a href="https://www.rfc-editor.org/rfc/inline-errata/rfc4180.html">RFC 4180 with errata</a>
 */
public final class CSV {

	private CSV() {
	}

	/** CSV subtype. */
	public static final String CSV_SUBTYPE = "csv";

	/** The media type for CSV: <code>text/csv</code>. */
	public static final MediaType CSV_MEDIA_TYPE = MediaType.of(MediaType.TEXT_PRIMARY_TYPE, CSV_SUBTYPE);

	/** The name extension for Comma Separated Value (CSV) files. */
	public static final String CSV_FILENAME_EXTENSION = "csv";

	/** The character that delimits CSV fields. */
	public static final char FIELD_DELIMITER_CHAR = COMMA_CHAR;

	/** The string that delimits records. */
	public static final String RECORD_DELIMITER_STRING = "" + CARRIAGE_RETURN_CHAR + LINE_FEED_CHAR;

	/** Characters that require a field to be quoted. */
	public static final Characters RESTRICTED_CHARACTERS = Characters.of(RECORD_DELIMITER_STRING.toCharArray()).add(FIELD_DELIMITER_CHAR, QUOTATION_MARK_CHAR);

	/** A quote character that has been escaped. */
	public static final String ESCAPED_QUOTATION_MARK_STRING = "" + QUOTATION_MARK_CHAR + QUOTATION_MARK_CHAR;

	/**
	 * Appends a record to the file at the given path with no headers. The information is encoded using UTF-8.
	 * @param path The path to the CSV file.
	 * @param objects The objects to serialize.
	 * @throws IOException if there is an error writing to the file.
	 */
	public static void appendRecord(final Path path, final Object... objects) throws IOException {
		appendRecord(path, null, objects); //write the objects to the file without headers
	}

	/**
	 * Appends a record to the file at the given path, first adding the provided headers (if any) if the file does not yet exist. The information is encoded using
	 * UTF-8.
	 * @implNote This implementation has a small race condition in which the header could be written twice (and not necessarily at the beginning of the file) if
	 *           multiple threads are appending to the file, and the file does not exist when this method is invoked while another thread creates the file and
	 *           appends at the same time.
	 * @param path The path to the CSV file.
	 * @param headers The headers with which to initialize the file, or <code>null</code> if no headers are desired.
	 * @param objects The objects to serialize, each of which may be <code>null</code>, or <code>null</code> if there are no objects to serialize.
	 * @throws IOException if there is an error writing to the file.
	 */
	public static void appendRecord(final Path path, final String[] headers, final Object... objects) throws IOException {
		final boolean exists = exists(path);
		//create a writer to the file, encoding in UTF-8; append to the an existing file, creating one if needed
		try (final OutputStream outputStream = newOutputStream(path, CREATE, APPEND);
				final BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(outputStream);
				final Writer writer = new OutputStreamWriter(bufferedOutputStream, UTF_8)) {
			if(!exists && headers != null) { //if the file didn't exist and there headers
				CsvSerializer.serialize(writer, (Object[])headers); //write the headers
			}
			if(objects != null) { //if there are objects to serialize
				CsvSerializer.serialize(writer, objects); //write the objects
			}
			writer.flush(); //flush everything we've written
		}
	}
}
