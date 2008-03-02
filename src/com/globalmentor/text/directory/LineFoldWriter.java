package com.globalmentor.text.directory;

import java.io.*;

import static com.globalmentor.text.directory.DirectoryConstants.*;


/**Reader that folds lines of type <code>text/directory</code> as defined
	"RFC 2425: A MIME Content-Type for Directory Information".
<p>Line folding is performed by adding a CRLF+SP after each run of 75
	characters, the length recommended by RFC 2425.</p>
@author Garret Wilson
*/
public class LineFoldWriter extends Writer
{

	/**The underlying writer that will actually write the data.*/
	protected final Writer writer;
	
	/**The length of the current line; the number of characters already written.*/
	protected int lineLength=0;

	/**Constructor that specifies another writer.
	@param writer The writer to which directory data will be written.
	*/
	public LineFoldWriter(final Writer writer)
	{
		this.writer=writer;	//save the writer
	}

	/**Write a portion of an array of characters.
	<p>Line folding is performed by adding a CRLF+SP after each run of 75
		characters, the length recommended by RFC 2425.</p>
	@param cbuf The array of characters.
	@param off The offset from which to start writing characters.
	@param len The number of characters to write.
	@exception IOException Thrown if an I/O error occurs.
	*/
	public void write(final char cbuf[], final int off, final int len) throws IOException
	{
		int writeBeginIndex=off;	//when we actually write something, we'll start writing where they asked us to start
		for(int i=0; i<len; ++i)	//look at each character to write
		{
			final int bufferIndex=off+i;	//make a note of where we are in the buffer
			switch(cbuf[bufferIndex])	//see which character this is
			{
				case CR:	//we'll assume a CR is the end of the line
					lineLength=0;	//reset the length of the line
					break;
				case LF:	//we'll assume LF is part of a CRLF and ignore it
					break;
				default:	//every other character makes the line longer
					++lineLength;	//make the line longer
					break;
			}
			if(lineLength>LONG_LINE_LENGTH)	//if we're ready to write a character, and writing this character would be past a full line
			{
				writer.write(cbuf, writeBeginIndex, bufferIndex-writeBeginIndex);	//write everything up to but not including this character
				writer.write(CRLF);	//write CRLF
				writer.write(SP);	//write a space
				lineLength=1;	//show that if we were to write this character it would be all by itself on a new line
				writeBeginIndex=bufferIndex;	//next time we'll start writing at this character
			}
		}
		if(writeBeginIndex<off+len)	//if we have data we haven't written
		{
			writer.write(cbuf, writeBeginIndex, (off+len)-writeBeginIndex);	//write the remaining data
		}
	}

	/**
	 * Flush the stream.  If the stream has saved any characters from the
	 * various write() methods in a buffer, write them immediately to their
	 * intended destination.  Then, if that destination is another character or
	 * byte stream, flush it.  Thus one flush() invocation will flush all the
	 * buffers in a chain of Writers and OutputStreams.
	 *
	 * @exception  IOException  If an I/O error occurs
	 */
	public void flush() throws IOException
	{
		writer.flush();	//pass the request on to the writer
	}

	/**
	 * Close the stream, flushing it first.  Once a stream has been closed,
	 * further write() or flush() invocations will cause an IOException to be
	 * thrown.  Closing a previously-closed stream, however, has no effect.
	 *
	 * @exception  IOException  If an I/O error occurs
	 */
	public void close() throws IOException
	{
		writer.close();	//pass the request on to the writer
	}

}