/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.io;

import static com.globalmentor.io.Files.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Objects.*;

import java.io.*;

import com.globalmentor.log.Log;

/**
 * An input stream to a file that, when closed, deletes the file.
 * 
 * <p>
 * It is very important to properly close this output stream when finished using it; otherwise, orphaned temporary files may remain.
 * </p>
 * 
 * @author Garret Wilson
 */
public class TempFileInputStream extends InputStreamDecorator<FileInputStream> {

	/** The temporary file in use, or <code>null</code> if the class has been disposed. */
	private File tempFile;

	/**
	 * File constructor.
	 * @param tempFile The temporary file to which an input stream should be created.
	 * @throws NullPointerException if the given file is <code>null</code>.
	 * @throws FileNotFoundException if the file does not exist, is a directory rather than a regular file, or for some other reason cannot be opened for reading.
	 */
	public TempFileInputStream(final File tempFile) throws FileNotFoundException {
		super(new FileInputStream(checkInstance(tempFile)));
		this.tempFile = tempFile;
	}

	/**
	 * {@inheritDoc} This version does not allow closing without closing the decorated stream.
	 * @throws IllegalArgumentException if the close decorated stream flag is <code>false</code>.
	 */
	@Override
	public synchronized void close(final boolean closeDecoratedStream) throws IOException {
		checkArgument(closeDecoratedStream == true, "This decorated input stream does not allow closing with closing the underlying stream.");
		super.close(closeDecoratedStream);
	}

	/**
	 * {@inheritDoc} This deletes the temporary file, if any. Because this method is called indirectly from {@link #close()}, this results in the temporary file
	 * being deleted after the input stream is closed.
	 */
	@Override
	public synchronized void dispose() {
		try {
			super.dispose();
		} finally {
			if(tempFile != null) { //if we still have a temporary file
				try {
					delete(tempFile); //try to delete the temporary file
				} catch(final IOException ioException) {
					Log.error(ioException);
				}
				tempFile = null;
			}
		}
	}

}
