package com.garretwilson.io;

import java.io.*;
import java.util.*;
import static java.util.Collections.*;

import static com.garretwilson.io.FileUtilities.*;

/**A class that can filter files based upon extension.
@author Garret Wilson
*/
public class FileExtensionFilter implements FileFilter, FilenameFilter
{
	/**The extensions to include.*/
	private final Set<String> extensionSet=new HashSet<String>();

		/**@return The extensions to include.*/
		protected Set<String> getExtensionSet() {return extensionSet;}

	/**Constructor specifying extensions to include.
	@param extensions The extensions to include.
	*/
	public FileExtensionFilter(final String... extensions)
	{
		addAll(extensionSet, extensions);	//add all the extensions to the extension set
	}

	/**Tests if a specified file should be included in a file list.
	This version accepts a file if the extension is included in the extension set.
	@param directory The directory in which the file was found.
	@param name the name of the file.
	@return  <code>true</code> if and only if the name should be
		included in the file list; <code>false</code> otherwise.
	*/
	public boolean accept(final File directory, final String name)
	{
		return getExtensionSet().contains(getExtension(name));	//see if the extension set contains the extension of the filename
	}

  /**Tests whether or not the specified abstract pathname should be included in a pathname list.
	This version accepts a file if the extension is included in the extension set.
	@param  pathname  The abstract pathname to be tested.
	@return <code>true</code> if and only if <code>pathname</code> should be included
	*/
  public boolean accept(final File pathname)
  {
		return getExtensionSet().contains(getExtension(pathname));	//see if the extension set contains the extension of the file  	
  }
}
