package com.garretwilson.io;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import com.garretwilson.text.CharacterEncoding;

/**Class to manipulate input streams.*/
public class InputStreamUtilities
{

	/**Loads the contents of an input stream into an array of bytes. This is
		accomplished by creating a series of smaller buffers and, once the end of
		the stream has been reached, creating a new buffer and copying the contents
		of each buffer into the new buffer. This is assumed to be faster than using
		<code>java.io.ByteArrayOutputStream</code> because that class copies all
		bytes that have been read each time the buffer needs to be expanded.
		The input stream will be left open after this operation.
	@param inputStream The input stream from which to read.
	@return An array of bytes from the input stream.
	@exception IOException Thrown if there is an error loading the bytes.
	*/
	public static byte[] getBytes(final InputStream inputStream) throws IOException
	{
//G***del		final BufferedInputStream bufferedInputStream=new BufferedInputStream(inputStream);	//create a buffered input stream from the input stream
//G***del		final BufferedInputStream bufferedInputStream=new BufferedInputStream(inputStream);	//create a buffered input stream from the input stream
		  //G***for some reason, if we don't have a buffered input stream the stream will be truncated at times -- it can't read the whole file at once; make sure it's OK to use a buffered input stream like this
//G***fix Debug.trace("Ready to read from input stream.");  //G***fix
		final List bufferList=new ArrayList();	//create a list for the buffers
		final int bufferSize=64*1024;	//use a series of 64K buffers
		int totalBytesRead=0;	//show that we haven't read anything at all yet
		int bufferBytesRead;		//we'll use this to store the number of bytes read into this buffer
		do
		{
			final byte[] buffer=new byte[bufferSize];	//create a new buffer
			bufferBytesRead=0;		//show that we haven't read any bytes in this buffer
		  int segmentBytesRead; //we'll use this to hold the number of bytes we read in each segment
			do  //read each segment of the buffer; the buffer may take several segments, since we're not guaranteed to have a buffered reader
			{
					//read as much as we can into this buffer, at the next location, although this may not fill up the buffer
				segmentBytesRead=inputStream.read(buffer, bufferBytesRead, buffer.length-bufferBytesRead);
				if(segmentBytesRead>=0) //if we haven't hit the end of the stream
					bufferBytesRead+=segmentBytesRead;  //update the number of bytes we've read for the buffer
//G***del Debug.trace("Read segment of length: "+segmentBytesRead+" in buffer of length: "+bufferSize);
			}
			while(segmentBytesRead>=0 && bufferBytesRead<bufferSize);  //keep reading segments while we haven't reached the end of the file and we haven't filled up the buffer
		  totalBytesRead+=bufferBytesRead;	//update our total bytes read
			bufferList.add(buffer);	//add this buffer to our list of buffers
		}
		while(bufferBytesRead==bufferSize);	//keep adding new buffers until we run out of bytes (leaving one buffer not fully filled)
		final byte[] finalBuffer=new byte[totalBytesRead];	//create a buffer of the correct length of all the bytes we've read
		int bytesCopied=0;	//show that we haven't copied any bytes yet
		for(int i=0; i<bufferList.size(); ++i)	//look at each of our buffers
		{
			//all the buffers should be full except for the last one, which will only hold the number of bytes last read
			final int bytesToCopy=(i<bufferList.size()-1) ? bufferSize : bufferBytesRead;
			System.arraycopy((byte[])bufferList.get(i), 0, finalBuffer, bytesCopied, bytesToCopy);	//copy bytes from this buffer to our final buffer
			bytesCopied+=bytesToCopy;	//show that we copied this many bytes
		}
		return finalBuffer;	//return the final buffer
	}

	/**Attempts to automatically detect the character encoding of a particular
		input stream based upon its byte order marker (BOM).
	<p>The input stream must be at its beginning and must support marking
		and resetting.</p>
	@param inputStream The stream the encoding of which will be detected.
	@return The character encoding detected, or <code>null</code> no encoding
		could be detected.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public static CharacterEncoding getBOMEncoding(final InputStream inputStream) throws IOException
	{
		final int BYTE_ORDER_MARK_LENGTH=4; //the number of bytes in the largest byte order mark
		inputStream.mark(BYTE_ORDER_MARK_LENGTH); //we won't read more than the byte order mark
		final byte[] byteOrderMarkArray=new byte[BYTE_ORDER_MARK_LENGTH];	//create an array to hold the byte order mark G***make sure this is initialized to zero bytes---or just read them all, which will but -1 in all the remaining bytes
		Arrays.fill(byteOrderMarkArray, (byte)0);	//fill the array with zeros, in case we can't completely fill it with bytes from the input stream
		final int byteOrderMarkCount=inputStream.read(byteOrderMarkArray);	//read as many characters of the byte order mark as we can
/*G***del when works
		boolean eof=false;	//we'll set this to true if we reach the end of the file
		for(int i=0; i<byteOrderMarkArray.length && !eof; ++i)	//read each character unless we reach the end of the file (we're using a loop instead of read(int[]) because the latter could read less than the number of bytes requested, even if there are more available)
		{
			final int byteOrderMark=inputStream.read();	//read the next byte
			if(byteOrderMark!=-1)	//if we didn't reach the end of the file
			{
				byteOrderMarkArray[i]=(byte)byteOrderMark;	//store the next byte order mark
			}
			else	//if we did reach the end of the file
			{
				eof=true;	//show that we reached the end of the file
			}
		}
		if(!eof)	//if we didn't reach the end of the data
		{
*/
		if(byteOrderMarkCount>0)	//if we read any characters as all
		{
			CharacterEncoding characterEncoding=CharacterEncoding.create(byteOrderMarkArray);	//see if we can recognize the encoding by the beginning characters G***probably create a static CharacterEncoding method to return a string without creating a new objet
		  if(characterEncoding!=null) //if we got a character encoding
		  {
					//throw away the BOM
				inputStream.reset();  //reset the stream back to where we found it
				for(int i=characterEncoding.getByteOrderMark().length-1; i>=0; --i)	//throw away the correct number of bytes
				{
					inputStream.read();	//throw away a byte
				}
				return characterEncoding; //return the encoding
		  }
		}
		inputStream.reset();  //reset the stream back to where we found it
		return null;	//we couldn't find an encoding
	}

}