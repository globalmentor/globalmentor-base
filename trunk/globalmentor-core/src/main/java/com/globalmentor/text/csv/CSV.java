/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text.csv;

import java.io.*;

import com.globalmentor.java.Characters;
import com.globalmentor.net.ContentType;

import static com.globalmentor.io.Charsets.*;
import static com.globalmentor.java.Characters.*;

/**
 * Constant values for Comma Separated Value (CSV) files.
 * @author Garret Wilson
 * @see <a href="http://www.ietf.org/rfc/rfc4180.txt">RFC 4180: Common Format and MIME Type for Comma-Separated Values (CSV) Files</a>
 */
public class CSV {

	/** CSV subtype. */
	public final static String CSV_SUBTYPE = "csv";

	/** The content type for CSV: <code>text/csv</code>. */
	public static final ContentType CSV_CONTENT_TYPE = ContentType.create(ContentType.TEXT_PRIMARY_TYPE, CSV_SUBTYPE);

	/** The name extension for Comma Separated Value (CSV) files. */
	public final static String CSV_NAME_EXTENSION = "csv";

	/** The character that delimits CSV fields. */
	public final static char FIELD_DELIMITER_CHAR = COMMA_CHAR;

	/** The string that delimits records. */
	public final static String RECORD_DELIMITER_STRING = "" + CARRIAGE_RETURN_CHAR + LINE_FEED_CHAR;

	/** Characters that require a field to be quoted. */
	public final static Characters RESTRICTED_CHARACTERS = new Characters(RECORD_DELIMITER_STRING.toCharArray()).add(FIELD_DELIMITER_CHAR, QUOTATION_MARK_CHAR);

	/** A quote character that has been escaped. */
	public final static String ESCAPED_QUOTATION_MARK_STRING = "" + QUOTATION_MARK_CHAR + QUOTATION_MARK_CHAR;

	/**
	 * Appends a record to the given file with no headers. The information is encoded using UTF-8.
	 * @param file The CSV file.
	 * @param objects The objects to serialize.
	 * @throws IOException if there is an error writing to the file.
	 */
	public static void appendRecord(final File file, final Object[] objects) throws IOException {
		appendRecord(file, null, objects); //write the objects to the file without headers
	}

	/**
	 * Appends a record to the given file, first adding the provided headers (if any) if the file does not yet exist. The information is encoded using UTF-8.
	 * @param file The CSV file.
	 * @param headers The headers with which to initialize the file, or <code>null</code> if no headers are desired.
	 * @param objects The objects to serialize, each of which may be <code>null</code>, or <code>null</code> if there are no objects to serialize.
	 * @throws IOException if there is an error writing to the file.
	 */
	public static void appendRecord(final File file, final String[] headers, final Object[] objects) throws IOException {
		final boolean exists = file.exists(); //see if the file exists
		//create a writer to the file, encoding in UTF-8
		final BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file, true), UTF_8_CHARSET));
		try {
			if(!exists && headers != null) { //if the file didn't exist and there headers
				CSVSerializer.serialize(bufferedWriter, (Object[])headers); //write the headers
			}
			if(objects != null) { //if there are objects to serialize
				CSVSerializer.serialize(bufferedWriter, objects); //write the objects
			}
			bufferedWriter.flush(); //flush everything we've written
		} finally {
			bufferedWriter.close(); //always close the stream
		}
	}
}
