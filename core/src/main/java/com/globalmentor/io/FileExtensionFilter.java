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

package com.globalmentor.io;

import java.io.*;
import java.util.*;
import static java.util.Collections.*;
import static com.globalmentor.io.Filenames.*;
import static com.globalmentor.io.Files.*;

/**
 * A class that can filter files based upon extension.
 * @author Garret Wilson
 */
public class FileExtensionFilter implements FileFilter, FilenameFilter {

	/** The extensions to include. */
	private final Set<String> extensionSet = new HashSet<String>();

	/** @return The extensions to include. */
	protected Set<String> getExtensionSet() {
		return extensionSet;
	}

	/**
	 * Constructor specifying extensions to include.
	 * @param extensions The extensions to include.
	 */
	public FileExtensionFilter(final String... extensions) {
		addAll(extensionSet, extensions); //add all the extensions to the extension set
	}

	/**
	 * Tests if a specified file should be included in a file list. This version accepts a file if the extension is included in the extension set.
	 * @param directory The directory in which the file was found.
	 * @param name the name of the file.
	 * @return <code>true</code> if and only if the name should be included in the file list; <code>false</code> otherwise.
	 */
	public boolean accept(final File directory, final String name) {
		return findExtension(name).map(getExtensionSet()::contains).orElse(false); //see if the extension set contains the extension of the filename
	}

	/**
	 * Tests whether or not the specified abstract pathname should be included in a pathname list. This version accepts a file if the extension is included in the
	 * extension set.
	 * @param file The abstract pathname to be tested.
	 * @return <code>true</code> if and only if <code>pathname</code> should be included
	 */
	public boolean accept(final File file) {
		return findExtension(file).map(getExtensionSet()::contains).orElse(false); //see if the extension set contains the extension of the file  	
	}
}
