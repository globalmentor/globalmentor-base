/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text.directory;

import java.io.*;

import com.globalmentor.io.ProcessingBufferedReader;

import static com.globalmentor.text.ABNF.*;

/**
 * Reader that unfolds lines of type <code>text/directory</code> as defined "RFC 2425: A MIME Content-Type for Directory Information".
 * @author Garret Wilson
 */
public class LineUnfoldReader extends ProcessingBufferedReader
{

	/**
	 * Constructor that specifies another reader.
	 * @param reader The reader that contains the directory data.
	 */
	public LineUnfoldReader(final Reader reader)
	{
		super(reader); //construct the base class
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This implementation processes newly buffered data by unfolding lines as specified in RFC 2425. Specifically, any CRLF combination followed by a space is
	 * removed.
	 * </p>
	 * @param newDataBeginIndex The starting index of the newly fetched data.
	 * @throws if an I/O error occurs.
	 * @see #getFetchBufferIndex()
	 */
	@Override
	protected void processBufferedData(final int newDataBeginIndex) throws IOException
	{
		super.processBufferedData(newDataBeginIndex); //do the default processing of the data (this currently does nothing)
		final char[] buffer = getBuffer(); //get a reference to our buffer
		final int bufferEndIndex = getFetchBufferIndex(); //find out the effective end of our new data
		int sourceIndex, destIndex; //we'll start looking at the beginning of the new data
		//start at the beginning of the data and go to the end, copying data backwards to erase characters if needed
		//ignore the last two characters for the moment, giving us enough guaranteed room to look for CRLF+SP at the end of the line
		for(sourceIndex = newDataBeginIndex, destIndex = newDataBeginIndex; sourceIndex < bufferEndIndex - 2; ++sourceIndex, ++destIndex)
		{
			if(buffer[sourceIndex] == CR && buffer[sourceIndex + 1] == LF && buffer[sourceIndex + 2] == SP) //if this is CRLF+SP
			{
				sourceIndex += 3; //skip the entire CRLF+SP
			}
			if(sourceIndex != destIndex) //if we've collapsed at least one CRLF+SP, we'll be copying information
				buffer[destIndex] = buffer[sourceIndex]; //copy this byte
		}
		int uncertainCharacterCount = 0; //we'll determine if there are characters at the end of which we are doubtful of whether they form a CRLF+SP
		if(sourceIndex < bufferEndIndex) //if we haven't reached the end of the buffer, copy the remaining characters (if the last characters were CRLF+SP, we will have already finished all the characters and we won't need to do anything here)
		{
			if(buffer[sourceIndex] == CR && buffer[sourceIndex + 1] == LF) //if the buffer ends in CRLF
				uncertainCharacterCount = 2; //there are two character's we're uncertain about
			else if(buffer[sourceIndex + 1] == CR) //if the buffer ends in CR
				uncertainCharacterCount = 1; //there is one character's we're uncertain about
			if(sourceIndex != destIndex) //if we've collapsed at least one line, we'll be copying information
			{
				buffer[destIndex] = buffer[sourceIndex]; //copy this byte
				buffer[destIndex + 1] = buffer[sourceIndex + 1]; //copy the next byte (the last byte)
			}
			sourceIndex += 2; //advance the source index to include the two characters we copied
			destIndex += 2; //advance the destination index to include the two characters we copied
		}
		final int moveDistance = bufferEndIndex - destIndex; //find out how far to move the buffer pointers back
		setBufferEndIndex(getBufferEndIndex() - moveDistance); //show where the new end of the buffer is
		if(!isLastBuffer() && uncertainCharacterCount > 0) //if there were characters we were uncertain about and it's the last buffer
		{
			setFetchBufferIndex(getFetchBufferIndex() - moveDistance); //move the fetch buffer index back, compensating for the characters we're not sure about
		}
		else
		//if this is the last buffer, or we weren't concerned about any of the ending characters
		{
			setFetchBufferIndex(getFetchBufferIndex() - moveDistance); //move the fetch buffer index back the same amount as we moved the buffer end index
		}
	}

}