package com.garretwilson.text.unicode;

import java.io.IOException;
import java.io.Reader;
import java.io.LineNumberReader;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.util.TreeSet;
import com.garretwilson.util.Debug;

/**A set of Unicode named character blocks. The iterator returned by the class
will return the blocks in sequential order. This class contains functionality
to read and parse information from a Unicode <code>Blocks.txt</code> file in
the following format:
<blockquote><code>Start Code (hex); End Code (hex); Block Name</code></blockquote>
@see UnicodeBlock
*/
public class UnicodeBlockSet extends TreeSet
{
	/**The default name used by Unicode to store block data.*/
	public final static String DEFAULT_BLOCKS_FILENAME="Blocks.txt";

	/**The data file's field delimiter character.*/
	public final static char FIELD_DELIMITER=';';

	/**Default constructor.*/
	public UnicodeBlockSet() {}

	/**Returns the block in which the specified character falls.
	@param character The Unicode character.
	@return The Unicode block in which the specified character falls, or
		<code>null</code> if this character does not fall in any known block.
	*/
	public UnicodeBlock getUnicodeBlockByCharacter(final char character)
	{
			//G***later, implement a better searching algorithm; this is, after all, a tree
		final Iterator blockIterator=iterator(); //get an iterator to look through each block
		while(blockIterator.hasNext())  //keep looking until we run out of blocks
		{
		  final UnicodeBlock block=(UnicodeBlock)blockIterator.next();  //get the next block
			if(character>=block.getStartCode() && character<=block.getEndCode())  //if this character falls within this block
				return block; //return this block
		}
		return null;  //show that we can't find a matching block
	}

	/**Parses an input reader which contains the unicode block ranges and names,
		and creates and returns a set of Unicode blocks.
	@param reader The reader which contains the data in the Unicode blocks format.
	@return A set of Unicode blocks objects.
	@exception IOException Thrown if there was an error parsing the Unicode data.
	@see UnicodeBlock
	*/
	public static UnicodeBlockSet parse(final Reader reader) throws IOException
	{
		final UnicodeBlockSet blockSet=new UnicodeBlockSet(); //create a set in which we can store the unicode blocks
		final LineNumberReader lineNumberReader=new LineNumberReader(reader);	//create a reader to allow us to read the file line by line
		try
		{
			String blockLine=lineNumberReader.readLine();	//read the first line of text
			while(blockLine!=null)	//while there are more lines left
			{
//G***del Debug.trace("Block line: "+blockLine);
				final UnicodeBlock block=parseLine(blockLine);  //parse this line
				if(block!=null) //if this is not a comment
					blockSet.add(block);	//add the resulting Unicode block to our set
				blockLine=lineNumberReader.readLine();	//read the next line of text
			}
			return blockSet;	//return our set of Unicode block objects
		}
		catch(Exception e)	//if any errors occur
		{
			throw new IOException("Error parsing line "+lineNumberReader.getLineNumber()+": "+e.toString());	//indicate the line we were trying to parse
		}
	}

	/**Parses a line in a Unicode block file and constructs and returns a Unicode block object.
	@param blockLine The line of text to parse.
	@return The Unicode blocok object that represents the character range the
		information for which was contained in the line, or <code>null</code> if
		the line was a comment or an empty line.
	@exception IOException Thrown if there was an error parsing the Unicode block data.
	*/
	protected static UnicodeBlock parseLine(String blockLine) throws IOException
	{
		final String trimmedBlockLine=blockLine.trim();  //trim the line
		if(trimmedBlockLine.length()==0 || trimmedBlockLine.startsWith("#"))  //if this is a blank line or a comment line
			return null;  //show that this is a comment or blank line
		blockLine+=FIELD_DELIMITER;	//since the Unicode block file doesn't have an ending delimiter, add one so that StringTokenizer will recognize the last field
//G***fix		final UnicodeCharacter unicodeCharacter=new UnicodeCharacter();	//create a new Unicode character that will hold the data we parse from this line
		final StringTokenizer fieldTokenizer=new StringTokenizer(trimmedBlockLine, String.valueOf(FIELD_DELIMITER), false);	//create an object to tokenize the line of Unicode block data G***use a constant string instead of creating one each time
		if(fieldTokenizer.hasMoreTokens())	//if there are more fields on this line
		{
			final String startCodeValue=fieldTokenizer.nextToken().trim();	//get the start code value and trim it
//G***del Debug.trace("Start: "+startCodeValue);
		  final char startCode=(char)Integer.parseInt(startCodeValue, 16); //convert the hex value to a character
			if(fieldTokenizer.hasMoreTokens())	//if there are more fields on this line
			{
				final String endCodeValue=fieldTokenizer.nextToken().trim();	//get the end code value and trim it
//G***del Debug.trace("End: "+endCodeValue);
				final char endCode=(char)Integer.parseInt(endCodeValue, 16); //convert the hex value to a character
				if(fieldTokenizer.hasMoreTokens())	//if there are more fields on this line
				{
					final String name=fieldTokenizer.nextToken().trim();	//get the block name and trim it
//G***del Debug.trace("Name: "+name);
				  return new UnicodeBlock(name, startCode, endCode);  //create a new Unicode block and return it
				}
			}
		}
			//if we reach this point, we've ran out of field
		throw new IOException("Missing field.");	//show that there weren't enough fields on this line
	}

}