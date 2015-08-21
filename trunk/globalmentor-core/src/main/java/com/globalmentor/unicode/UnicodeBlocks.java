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

package com.globalmentor.unicode;

import java.io.*;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.*;

import static com.globalmentor.java.CharSequences.*;
import static java.nio.charset.StandardCharsets.*;

/**
 * Accesses Unicode named character blocks. The set iterator will return the blocks in sequential order. This class contains functionality to read and parse
 * information from a Unicode <code>Blocks.txt</code> file in the following format: <blockquote><code>Start Code (hex)..End Code (hex); Block Name</code>
 * </blockquote>
 * @see UnicodeBlock
 */
public class UnicodeBlocks {

	/** The default name used by Unicode to store block data. */
	protected static final String BLOCKS_FILENAME = "Blocks.txt";

	/** The data file's field delimiter character. */
	public static final char FIELD_DELIMITER = ';';

	/** A reference to a cached unmodifiable set of Unicode blocks. */
	private static Reference<SortedSet<UnicodeBlock>> blockSetReference = null;

	/** @return An unmodifiable set of Unicode blocks, reloading them if needed. */
	public static SortedSet<UnicodeBlock> getUnicodeBlocks() {
		SortedSet<UnicodeBlock> blockSet = blockSetReference != null ? blockSetReference.get() : null; //get the cached Unicode block set, if there is one
		if(blockSet == null) { //if we haven't loaded the blocks, or they have been reclaimed by the garbage collector
			try {
				blockSet = Collections.unmodifiableSortedSet(load()); //load a new block set
			} catch(final IOException ioException) { //if there is an error loading the blocks (there never should be, as they should be known resources)
				throw new AssertionError(ioException);
			}
			blockSetReference = new SoftReference<SortedSet<UnicodeBlock>>(blockSet); //create a new soft reference to the set of Unicode blocks we just loaded
		}
		return blockSet; //return the block set
	}

	/**
	 * Returns the block in which the specified character falls.
	 * @param codePoint The Unicode character.
	 * @return The Unicode block in which the specified character falls, or <code>null</code> if this character does not fall in any known block.
	 */
	public static UnicodeBlock getUnicodeBlockByCodePoint(final int codePoint) {
		for(final UnicodeBlock block : getUnicodeBlocks()) { //look at each block TODO later, implement a better searching algorithm; this is, after all, a tree
			if(codePoint >= block.getStartCode() && codePoint <= block.getEndCode()) //if this character falls within this block
				return block; //return this block
		}
		return null; //show that we can't find a matching block
	}

	/**
	 * @return A reader to the Unicode blocks resource file.
	 * @throws UnsupportedEncodingException Thrown if the blocks file encoding (ISO 8859-1) is unsupported. This situation should never occur.
	 */
	protected static Reader getBlocksReader() throws UnsupportedEncodingException {
		//get an input stream to our Unicode data resource file
		final InputStream inputStream = UnicodeData.class.getResourceAsStream(BLOCKS_FILENAME);
		//buffer the input stream and turn it into an ASCII reader
		return new InputStreamReader(new BufferedInputStream(inputStream), ISO_8859_1);
	}

	/**
	 * Loads a set of Unicode blocks from the Unicode blocks resource text file.
	 * @return A set of Unicode blocks.
	 * @throws IOException Thrown if there was an error parsing the blocks.
	 */
	protected static SortedSet<UnicodeBlock> load() throws IOException {
		final Reader reader = getBlocksReader(); //get a reader to our data
		try {
			return parse(reader); //parse and return the Unicode blocks
		} finally {
			reader.close(); //always close our reader
		}
	}

	/**
	 * Parses an input reader which contains the unicode block ranges and names, and creates and returns a set of Unicode blocks.
	 * @param reader The reader which contains the data in the Unicode blocks format.
	 * @return A set of Unicode blocks objects.
	 * @throws IOException Thrown if there was an error parsing the Unicode data.
	 * @see UnicodeBlock
	 */
	protected static SortedSet<UnicodeBlock> parse(final Reader reader) throws IOException {
		final SortedSet<UnicodeBlock> blockSet = new TreeSet<UnicodeBlock>(); //create a set in which we can store the unicode blocks
		final LineNumberReader lineNumberReader = new LineNumberReader(reader); //create a reader to allow us to read the file line by line
		try {
			String blockLine = lineNumberReader.readLine(); //read the first line of text
			while(blockLine != null) { //while there are more lines left
				//TODO del Log.trace("Block line: "+blockLine);
				final UnicodeBlock block = parseLine(blockLine); //parse this line
				if(block != null) //if this is not a comment
					blockSet.add(block); //add the resulting Unicode block to our set
				blockLine = lineNumberReader.readLine(); //read the next line of text
			}
			return blockSet; //return our set of Unicode block objects
		} catch(final Exception exception) { //if any errors occur
			throw (IOException)new IOException("Error parsing line " + lineNumberReader.getLineNumber() + ": " + exception.toString()).initCause(exception); //indicate the line we were trying to parse
		}
	}

	/**
	 * Parses a line in a Unicode block file and constructs and returns a Unicode block object.
	 * @param blockLine The line of text to parse.
	 * @return The Unicode blocok object that represents the character range the information for which was contained in the line, or <code>null</code> if the line
	 *         was a comment or an empty line.
	 * @throws IOException Thrown if there was an error parsing the Unicode block data.
	 */
	protected static UnicodeBlock parseLine(final String blockLine) throws IOException {
		final String trimmedBlockLine = blockLine.trim(); //trim the line TODO maybe forego the trimming
		if(trimmedBlockLine.length() == 0 || trimmedBlockLine.startsWith("#")) //if this is a blank line or a comment line
			return null; //show that this is a comment or blank line
		final CharSequence[] fields = split(blockLine, FIELD_DELIMITER); //split the line based upon the delimiter
		if(fields.length == 2) { //if there are two fields
			final StringTokenizer rangeTokenizer = new StringTokenizer(fields[0].toString(), String.valueOf('.'), false); //create an object to tokenize the block range TODO use a constant string instead of creating one each time
			if(rangeTokenizer.hasMoreTokens()) { //if there are more fields on this line
				final String startCodeValue = rangeTokenizer.nextToken().trim(); //get the start code value and trim it
				//TODO del Log.trace("Start: "+startCodeValue);
				final int startCode = Integer.parseInt(startCodeValue, 16); //convert the hex value to a character
				if(rangeTokenizer.hasMoreTokens()) { //if there are more fields on this line
					final String endCodeValue = rangeTokenizer.nextToken().trim(); //get the end code value and trim it
					//TODO del Log.trace("End: "+endCodeValue);
					final int endCode = Integer.parseInt(endCodeValue, 16); //convert the hex value to a character
					final String name = fields[1].toString().trim(); //get the block name and trim it
					//TODO del Log.trace("Name: "+name);
					return new UnicodeBlock(name, startCode, endCode); //create a new Unicode block and return it
				}
			}
		}
		//if we reach this point, we've ran out of field
		throw new IOException("Incorrect number of fields."); //show that there weren't enough fields on this line
	}

}