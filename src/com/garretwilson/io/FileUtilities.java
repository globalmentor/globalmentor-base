package com.garretwilson.io;

import java.io.*;
import java.net.MalformedURLException;
import com.garretwilson.lang.*;
import com.garretwilson.net.URIUtilities;
import com.garretwilson.net.URLUtilities;
import com.garretwilson.util.Debug;

/**Various utilities for examining files.
@author Garret Wilson
*/
public class FileUtilities implements FileConstants
{

	/**This class cannot be publicly instantiated.*/
  private FileUtilities()
	{
	}

	/**Adds the given extension to a file and returns the new file with
		the new extension.
		The filename is not checked to see if it currently has an extension.
	@param file The file to which to add an extension.
	@param extension The extension to set
	@return The file with the new extension.
	*/
	public static File addExtension(final File file, final String extension)
	{
		return new File(addExtension(file.getPath(), extension));	//add an extension to the path and create and return a new file with that 
	}
	
	/**Adds the given extension to a filename and returns the new filename with
		the new extension.
		The filename is not checked to see if it currently has an extension.
	@param filename The filename to which to add an extension.
	@param extension The extension to set
	@return The filename with the new extension.
	*/
	public static String addExtension(final String filename, final String extension)
	{
		return new StringBuffer(filename).append(EXTENSION_SEPARATOR).append(extension).toString();  //add the requested extension and return the new filename
	}
	
	/**Creates a new file, throwing an exception if unsuccessful.
	@param file The file to create.
	@exception IOException Thrown if there is an error creating the file.
	*/
	public static void createNewFile(final File file) throws IOException
	{
		if(!file.createNewFile())	//create the file; if unsuccessful
		{
			if(file.exists())	//if the file already exists
				throw new IOException("File "+file+" already exists and cannot be created.");	//throw an exception G***i18n
			else	//if the file doesn't exist, there must have been some other creation error
				throw new IOException("Cannot create "+file);	//throw an exception G***i18n
		}
	}

	/**Deletes a directory or file, throwing an exception if unsuccessful.
	@param file The directory or file to delete.
	@exception IOException Thrown if there is an problem deleting any directory
		or file.
	*/
	public static void delete(final File file) throws IOException
	{
		delete(file, false);	//delete the file without recursion
	}

	/**Deletes a directory or file, throwing an exception if unsuccessful.
		The operation will stop on the first error.
	@param file The directory or file to delete.
		If a directory is passed, all its child files and directories will
			recursively be deleted if <code>recursive</code> is <code>true</code>.
		If a file is passed, it will be deleted normally.
	@param recursive <code>true</code> if all child directories and files of a
		directory should recursively be deleted.
	@exception IOException Thrown if there is an problem deleting any directory
		or file.
	*/
	public static void delete(final File file, final boolean recursive) throws IOException
	{
		if(recursive && file.isDirectory())	//if this is a directory and we should recursively delete files
		{
			final File[] files=file.listFiles();	//get all the files in the directory
			assert files!=null : "File "+file+" is not a directory.";
			for(int i=files.length-1; i>=0; --i)	//look at each file in the directory
			{
				delete(files[i], recursive);	//delete this file
			}
		}
		if(!file.delete())	//delete the file; if unsuccessful
		{
			throw new IOException("Unable to delete "+file);	//throw an exception G***i18n
		}
	}
	
	/**Extracts the extension from a file.
	@param file The file to examine.
	@return The extension of the file (not including '.'), or <code>null</code> if
		no extension is present.
	*/
	public static String getExtension(final File file)
	{
//G***del when works		final String filename=file.getName();  //get the name of the file
		return getExtension(file.getName());  //return the extension of the filename
	}

	/**Extracts the extension from a filename.
	@param filename The filename to examine.
	@return The extension of the file (not including '.'), or <code>null</code> if
		no extension is present.
	*/
	public static String getExtension(final String filename)
	{
			//G***we may first want to chop off anything before the last '/' or '\'
		final int separatorIndex=filename.lastIndexOf(EXTENSION_SEPARATOR); //see if we can find the extension separator, which will be the last such character in the string
		if(separatorIndex>=0)  //if we found a separator
			return filename.substring(separatorIndex+1);  //return everything after the separator
		else  //if there is no separator
			return null;  //show that there is no extension
	}

	/**Changes the extension of a file and returns a new file with the new
		extension. If the file does not currently have an extension, one will be
		added. If the file has no path or name, the same file will be returned.
	@param file The file to examine.
	@param extension The extension to set
	@return The file with the new extension.
	*/
	public static File changeExtension(final File file, final String extension)
	{
		String filename=file.getName();  //get the name of the file
		if(filename.length()!=0)  //if we found a filename
		{
			final int separatorIndex=filename.lastIndexOf(EXTENSION_SEPARATOR); //see if we can find the extension separator, which will be the last such character in the string
			if(separatorIndex!=-1)  //if we found a separator
				filename=filename.substring(0, separatorIndex); //remove the extension
			filename=addExtension(filename, extension);	//add the requested extension
			return new File(file.getParent(), filename);  //return a file based on the name with the new extension
		}
		else  //if there is no filename
			return file;  //return the unmodified file
	}

	/**Returns the media type for the specified file based on its extension.
	@param file The file for which to return a media type.
	@return The default media type for the file's extension, or <code>null</code>
		if no known media type is associated with this file's extension, or if this
		file has no extension.
	@see MediaType#getMediaType
	*/
	public static MediaType getMediaType(final File file)
	{
		final String extension=getExtension(file);  //get the file's extension
		return extension!=null ? MediaType.getMediaType(extension) : null; //return the media type based on the file's extension, if there is one
	}

	/**Returns the media type for the specified filename based on its extension.
	@param filename The filename to examine.
	@return The default media type for the filename's extension, or <code>null</code>
		if no known media type is associated with this file's extension or if the
		filename has no extension.
	@see MediaType#getMediaType
	*/
	public static MediaType getMediaType(final String filename)
	{
		final String extension=getExtension(filename);  //get the file's extension
		return extension!=null ? MediaType.getMediaType(extension) : null; //return the media type based on the filename's extension, if there is one
	}

	/**Determines the path of the file relative to a root directory. Backslashes
		are correctly converted to slashes.
	@param rootDirectory The root directory relative to which a path should be
		returned, or <code>null</code> if the root directory is the absolute root
		directory.
	@param file The file for which a relative path should be returned.
	@return The path of the file relative to the root directory, or the normal
		file path if no root directory was given.
	*/
	public static String getRelativePath(final File rootDirectory, final File file)
	{
		if(rootDirectory!=null) //if we were given a root directory
		{
			try
			{
					//get the relative path of the file and put a path separator on the
					//  front of it to represent the local root directory
//TODO fix this to work with the new URI relativize
				return String.valueOf(FileConstants.PATH_SEPARATOR)+
						URLUtilities.getRelativePath(rootDirectory.toURL(), file.toURL());
			}
			catch(MalformedURLException malformedURLException)  //if a URL was malformed
			{
				Debug.error(malformedURLException); //we should never get this error
				return file.getPath().replace('\\', FileConstants.PATH_SEPARATOR);  //this should never be reached G***use a constant here
			}
		}
		else  //if we were not given a root directory
		{
			return file.getPath().replace('\\', FileConstants.PATH_SEPARATOR);  //return the normal file path with the correct path separator G***use a constant here
		}
	}

	/**Returns a file suitable for a temporary file, based on the specified
		filename, by adding a ".temp" extension.
	@param file The file for which a temporary file should be returned.
	@return The file suitable for temporary access.
	@see FileConstants#TEMP_EXTENSION
	*/
	public static File getTempFile(final File file)
	{
		return new File(file.getParent(), addExtension(file.getName(), TEMP_EXTENSION)); //return the file with a "temp" extension
	}

	/**Returns a file suitable for backup, based on the specified filename, by
		adding a ".backup" extension.
	@param file The file for which a backup file should be returned.
	@return The file suitable for backup.
	@see FileConstants#BACKUP_EXTENSION
	*/
	public static File getBackupFile(final File file)
	{
		return new File(file.getParent(), addExtension(file.getName(), BACKUP_EXTENSION)); //return the file with a "backup" extension
	}

	/**@return The user's current directory.
	@exception SecurityException Thrown if we don't have permission to access the
		user's directory.
	@see System
	@see SystemConstants#USER_DIR_PROPERTY
	*/
	public static File getUserDirectory() throws SecurityException
	{
		return new File(System.getProperty(SystemConstants.USER_DIR_PROPERTY)); //try to get the current directory
	}

	/**@return The characters that are not allowed in filenames of this operating system.*/
/*G***del if not needed
	public static String getSystemFilenameReservedCharacters()
	{
		if(SystemUtilities.isWindowsOS())	//if we're running on Windows
			return WINDOWS_FILENAME_RESERVED_CHARACTERS;	//return the Windows filename reserved characters
		else	//for all other operating systems (TODO fix for Macintosh)
			return POSIX_FILENAME_RESERVED_CHARACTERS;	//return the POSIX filename reserved characters
	}
*/

	/**Checks to ensure that a particular string is a valid filename across
		operating systems. This method does not ensure that such a file actually exists.
	@param string The string of characters which may represent a filename.
	@return <code>true</code> if the string contains no illegal filname characters.
	*/
	public static boolean isCrossPlatformFilename(final String string)
	{
		return isFilename(string, CROSS_PLATFORM_FILENAME_RESERVED_CHARACTERS, CROSS_PLATFORM_FILENAME_RESERVED_FINAL_CHARACTERS);	//check the filename using cross-platform reserved characters
	}

	/**Checks to ensure that a particular string is a valid filename for the
		operating system. This method does not ensure that such a file actually exists.
	<p>The reserved characters of the operating system will be used.</p>
	@param string The string of characters which may represent a filename.
	@return <code>true</code> if the string contains no illegal filname characters.
	*/
	public static boolean isFilename(final String string)
	{
		if(SystemUtilities.isWindowsOS())	//if we're running on Windows
			return isFilename(string, WINDOWS_FILENAME_RESERVED_CHARACTERS, WINDOWS_FILENAME_RESERVED_FINAL_CHARACTERS);	//check the filename using Windows reserved characters
		else	//for all other operating systems TODO fix for Macintosh
			return isFilename(string, POSIX_FILENAME_RESERVED_CHARACTERS, null);	//check the filename for POSIX
	}

	/**Checks to ensure that a particular string is a valid filename.
	This method does not ensure that such a file actually exists.
	@param string The string of characters which may represent a filename.
	@param reservedCharacters The reserved characters which should be encoded.
	@param reservedFinalCharacters The characters that should be encoded if they
		appear in the final position of the filename, or <code>null</code> if the
		final character doesn't have to meet special rules.
	@return <code>true</code> if the string contains no reserved filename characters.
	*/
	public static boolean isFilename(final String string, final String reservedCharacters, final String reservedFinalCharacters)
	{
			//the string is a filename if the string ing isn't null and there are no illegal characters in the string
		final boolean isFilename=string!=null && CharSequenceUtilities.charIndexOf(string, reservedCharacters)<0;
		if(isFilename && reservedFinalCharacters!=null && reservedFinalCharacters.length()>0)	//if we should check the final character
		{
			if(string.length()>0)	//if we have any characters at all
			{
				final char lastChar=string.charAt(string.length()-1);	//see what the last character is
				if(reservedFinalCharacters.indexOf(lastChar)>=0)	//if the last character is reserved
				{
					return false;	//this is not a valid filename
				}
			}
		}
		return isFilename;	//return what we thought to begin with
	}


	/**Escape all reserved filename characters to a two-digit hex
		representation using '^' as an escape character.
	<p>Note that this encodes path separators, and therefore this
		method should only be called on filenames, not paths.</p>
	@param string The filename string to be encoded.

	/**Checks to see if a particular file exists. If the file does not exist, yet
		a backup file exists, the backup file will be moved to the original file
		location. If this method returns true, there will be a file located at
		<code>file</code>.
	@param file The file to check for existence.
	@param backupFile The file to use as a backup if the original does not exist.
	@return <code>true</code> if the file existed or exists now after moving
		the backup file, else <code>false</code> if neither file exists.
	@exception IOException Thrown if the backup file cannot be moved.
	*/
	public static boolean checkExists(final File file, final File backupFile) throws IOException
	{
		if(file.exists()) //if the file exists
		{
			return true;  //there's nothing more to do; return true
		}
		else  //if the file doesn't exist
		{
			if(backupFile.exists()) //if the backup file exists
			{
				renameTo(backupFile, file);  //try to rename the backup file to the original file
				return true;	//show that a file now exists where it is expected to be
			}
			else  //if the backup file does not exist
				return false; //show that we can't find either file
		}
	}

	/**Checks to see if a particular file exists. If the file does not exist, yet
		a backup file exists, the backup file will be moved to the original file
		location. If this method returns true, there will be a file located at
		<code>file</code>.
		This method automatically determines the name of the backup file.
	@param file The file to check for existence.
	@return <code>true</code> if the file existed or exists now after moving
		the backup file, else <code>false</code> if neither file exists.
	@exception IOException Thrown if the backup file cannot be moved.
	@see #getBackupFile
	*/
	public static boolean checkExists(final File file) throws IOException
	{
		return checkExists(file, getBackupFile(file)); //check to see if the file exists, using the default filename for the backup file
	}

	protected final static char REPLACEMENT_CHAR='_';  //the character to use to replace any other character  G***maybe move these up and/or rename

	/**Escape all reserved filename characters to a two-digit hex
		representation using '^' as an escape character so that the filename can
		be used across operating systems.
	<p>Note that this encodes path separators, and therefore this
		method should only be called on filenames, not paths.</p>
	@param string The filename string to be encoded.
	@return The string modified to be a filename.
	@see FileConstants#RESERVED_CHARACTERS
	@see FileConstants#ESCAPE_CHARACTER
	@see CharSequenceUtilities#escapeHex
	@see #isFilename
	*/
	public static String encodeCrossPlatformFilename(final String filename)
	{
		return encodeFilename(filename, CROSS_PLATFORM_FILENAME_RESERVED_CHARACTERS, CROSS_PLATFORM_FILENAME_RESERVED_FINAL_CHARACTERS);	//encode the filename using cross-platform reserved characters
	}

	/**Escape all reserved filename characters to a two-digit hex
		representation using '^' as an escape character.
	<p>Note that this encodes path separators, and therefore this
		method should only be called on filenames, not paths.</p>
	<p>The filename is encoded using the reserved characters of the current
		operating system.</p>
	@param string The filename string to be encoded.
	@return The string modified to be a filename.
	@see FileConstants#RESERVED_CHARACTERS
	@see FileConstants#ESCAPE_CHARACTER
	@see CharSequenceUtilities#escapeHex
	@see #isFilename
	*/
	public static String encodeFilename(final String filename)
	{
		if(SystemUtilities.isWindowsOS())	//if we're running on Windows
			return encodeFilename(filename, WINDOWS_FILENAME_RESERVED_CHARACTERS, WINDOWS_FILENAME_RESERVED_FINAL_CHARACTERS);	//encode the filename using Windows reserved characters
		else	//for all other operating systems TODO fix for Macintosh
			return encodeFilename(filename, POSIX_FILENAME_RESERVED_CHARACTERS, null);	//encode the filename for POSIX
	}

	/**Escape all reserved filename characters to a two-digit hex
		representation using '^' as an escape character.
	<p>Note that this encodes path separators, and therefore this
		method should only be called on filenames, not paths.</p>
	@param string The filename string to be encoded.
	@param reservedCharacters The reserved characters which should be encoded.
	@param reservedFinalCharacters The characters that should be encoded if they
		appear in the final position of the filename, or <code>null</code> if the
		final character doesn't have to meet special rules.
	@return The string modified to be a filename.
	@see FileConstants#RESERVED_CHARACTERS
	@see FileConstants#ESCAPE_CHARACTER
	@see CharSequenceUtilities#escapeHex
	@see #isFilename
	*/
	public static String encodeFilename(final String filename, final String reservedCharacters, final String reservedFinalCharacters)
	{
			//check to see if this is already a valid filename; if so (it usually is), this will give us a performance increase
			//even if this is a valid filename, make sure it doesn't have the escape character in it---we would have to escape that, too, even though it isn't reserved
		if(isFilename(filename, reservedCharacters, reservedFinalCharacters)	//if this is a valid filename already	
				&& filename.indexOf(ESCAPE_CHARACTER)<0)	//if the filename doesn't contain the escape character	
		{
				return filename;	//return the string as is---it already is a valid filename
		}
		else	//if something about the filename isn't correct
		{
			final String encodedFilename=CharSequenceUtilities.escapeHex(filename, reservedCharacters, ESCAPE_CHARACTER, 2);
			if(reservedFinalCharacters!=null && reservedFinalCharacters.length()>0)	//if we should check the final character (e.g. on Windows)
			{
				if(encodedFilename.length()>0)	//if we have a filename
				{
					final char lastChar=encodedFilename.charAt(encodedFilename.length()-1);	//see what the last character is
					if(reservedFinalCharacters.indexOf(lastChar)>=0)	//if the last character is a reserved character
					{
						return encodedFilename.substring(0, encodedFilename.length()-1)	//remove the last character
								+ESCAPE_CHARACTER+IntegerUtilities.toHexString(lastChar, 2);	//add the escaped version of the character in its place
					}
				}
			}
			return encodedFilename;	//return the encoded filename since we didn't need to modify it further
		}
	}

	/**Unescapes all characters in a string that are encoded 
		using '^' as an escape character followed by two hex digits.
	@param string The filename string to be decoded.
	@return The filename string decoded back to a normal string.
	@see FileConstants#RESERVED_CHARACTERS
	@see FileConstants#ESCAPE_CHARACTER
	@see CharSequenceUtilities#unescapeHex
	*/
	public static String decodeFilename(final String filename)
	{
		return CharSequenceUtilities.unescapeHex(filename, ESCAPE_CHARACTER, 2);	//decode the filename
	}

	/**Deletes a file, throwing an exception if unsuccessful.
	@param file The file to delete.
	@exception IOException Thrown if there is an error deleting the file.
	*/
/*G***del; this is already implemented here	
	public static void delete(final File file) throws IOException
	{
		if(!file.delete)	//delete file; if unsuccessful
		{
			throw new IOException("Cannot delete "+file);	//throw an exception G***i18n
		}
	}
*/
	
	/**Creates the directory named by this abstract pathname, throwing an
		exception if unsuccessful.
	@param directory The directory to create.
	@exception IOException Thrown if there is an error creating the directory.
	*/
	public static void mkdir(final File directory) throws IOException
	{
		if(!directory.mkdir())	//create the directory; if unsuccessful
		{
			throw new IOException("Cannot create directory "+directory);	//throw an exception G***i18n
		}
	}
	
	/**Creates the directory named by this abstract pathname, including any
		necessary but nonexistent parent directories, throwing an exception if
		unsuccessful.
	@param directory The directory to create.
	@exception IOException Thrown if there is an error creating the directory.
	*/
	public static void mkdirs(final File directory) throws IOException
	{
		if(!directory.mkdirs())	//create the directory; if unsuccessful
		{
			throw new IOException("Cannot create directories "+directory);	//throw an exception G***i18n
		}
	}

	/**Loads the contents of a file into an array of bytes. The file is closed
		after the operation.
	@param file The file from which to read.
	@return An array of bytes from the input stream.
	@exception IOException Thrown if there is an error loading the bytes.
	@see InputStreamUtilities#getBytes
	@see #write
	*/
	public static byte[] readBytes(final File file) throws IOException
	{
		final InputStream fileInputStream=new FileInputStream(file);  //create an input stream to the file
		try
		{
			return InputStreamUtilities.getBytes(fileInputStream);  //convert the file to an array of bytes
		}
		finally
		{
			fileInputStream.close();  //always close the file input stream
		}
	}

	/**Returns a string with the extension removed.
	@param filename The filename to examine.
	@return The filename without any extension.
	*/
	public static String removeExtension(final String filename)
	{
//G***del Debug.trace("removing extension from: ", filename);	//G***del
			//G***we may first want to chop off anything before the last '/' or '\'
			//G***better yet, make sure the extension is after the last slash
		final int separatorIndex=filename.lastIndexOf(EXTENSION_SEPARATOR); //see if we can find the extension separator, which will be the last such character in the string
		if(separatorIndex>=0)  //if we found a separator
		{
//G***del Debug.trace("returning: ", filename.substring(0, separatorIndex));	//G***del
			return filename.substring(0, separatorIndex);  //return everything before the separator
		}
		else  //if there is no separator
			return filename;  //there was no extension to begin with, so just return the original filename
	}

	/**Returns a file with the extension removed.
	@param file The file to examine.
	@return The file without any extension.
	*/
	public static File removeExtension(final File file)
	{
			//G***we may first want to chop off anything before the last '/' or '\'
			//G***better yet, make sure the extension is after the last slash
		return new File(removeExtension(file.getPath()));	//remove the extension from the file path and create a file from that
	}
	
	/**Renames the file, throwing an exception if unsuccessful.
	@param source The file to rename.
	@param destination The new name of the file
	@exception IOException Thrown if there is an error renaming the file.
	*/
	public static void renameTo(final File source, final File destination) throws IOException
	{
		if(!source.renameTo(destination))	//rename the file to its new filename; if unsuccessful
		{
			throw new IOException("Cannot rename "+source+" to "+destination);	//throw an exception G***i18n
		}
	}

	/**Moves a file to a different location, overwriting the destination file if
		it exists.
	@param sourceFile The file to be moved.
	@param destinationFile The location to where the source files should be be moved.
	@exception IOException Thrown if there is an error moving the file.
	*/
	public static void moveFile(final File sourceFile, final File destinationFile) throws IOException
	{
		moveFile(sourceFile, destinationFile, null); //move the source file to the destination file, specifying that no backup should be made
	}

	/**Moves a file to a different location, overwriting the destination file if
		it exists.
		If a backup file is specified, it is first deleted and the destination file
		is moved to the backup file location before the source file is moved.
	@param sourceFile The file to be moved.
	@param destinationFile The location to where the source files should be be moved.
	@param backupFile The backup file to where the destination file, if any, will
		first be moved, or <code>null</code> if no backup file is necessary.
	@exception IOException Thrown if there is an error renaming the file.
	*/
	public static void moveFile(final File sourceFile, final File destinationFile, final File backupFile) throws IOException
	{
		if(backupFile!=null && destinationFile.exists())  //if we should backup the original destination file, and the original destination file exists
		{
				//try to move the destination file to the backup file
				//if this fails, at worst it will leave the destination file at the same state it was in to begin with 
			moveFile(destinationFile, backupFile); 
		}
		if(destinationFile.exists())	//if the destination file exists
			delete(destinationFile);		//delete the destination file, throwing an exception if there is an error
		renameTo(sourceFile, destinationFile);  //move the source file to the destination file
	}

	/**Returns a relative path to the file from the given directory. This version
		requires the file to be on the same branch of the reference directory.
	@param referenceDirectory The reference directory to use in making the relative
		path.
	@param file The file for which a relative path should be returned, in relation
		to the reference directory.
	@return A relative path to the file in relation to the reference directory, or
		<code>null</code> if a relative path could not be determined.
	@exception IOException Thrown if an I/O error occurs, which is possible because
		the construction of canonical pathnames may require filesystem queries.
	*/
/*G***del
	public static String getRelativePath(final File referenceDirectory, final File file) throws IOException
	{
		final String canonicalReferenceDirectoryPath=referenceDirectory.getCanonicalPath(); //convert the reference directory to its canonical form
		final String canonicalFilePath=file.getCanonicalPath(); //convert the file to its canonical form
		final int canonicalReferenceDirectoryPathLength=canonicalReferenceDirectoryPath.length(); //see how long the canonical directory is
Debug.trace("Canonical directory: "+canonicalReferenceDirectoryPath);
Debug.trace("Canonical file: "+canonicalFilePath);
		if(canonicalReferenceDirectoryPathLength<canonicalFilePath.length())  //make sure the directory is shorter than the entire file path
		{
			if(canonicalFilePath.substring(0, canonicalReferenceDirectoryPathLength)) //if the file path contains the directory
			{

			}
		}
		return null;  //show that we couldn't determine a relative path
	}
*/

	/**Returns a relative path to the file from the given directory. The result is
		appropriate for a URL reference. This version requires the file to be on the
		same branch of the reference directory.
	@param referenceDirectory The reference directory to use in making the relative
		path.
	@param file The file for which a relative path should be returned, in relation
		to the reference directory.
	@return A relative path to the file in relation to the reference directory, or
		<code>null</code> if a relative path could not be determined.
	@exception IOException Thrown if an I/O error occurs, which is possible because
		the construction of canonical pathnames may require filesystem queries.
	*/
/*G***fix
	public static String getRelativePath(final File referenceDirectory, final File file) throws IOException
	{
		final String canonicalReferenceDirectoryPath=referenceDirectory.getCanonicalPath(); //convert the reference directory to its canonical form
		final String canonicalFilePath=file.getCanonicalPath(); //convert the file to its canonical form
		final int canonicalReferenceDirectoryPathLength=canonicalReferenceDirectoryPath.length(); //see how long the canonical directory is
Debug.trace("Canonical directory: "+canonicalReferenceDirectoryPath);
Debug.trace("Canonical file: "+canonicalFilePath);
		if(canonicalReferenceDirectoryPathLength<canonicalFilePath.length())  //make sure the directory is shorter than the entire file path
		{
			if(canonicalFilePath.substring(0, canonicalReferenceDirectoryPathLength)) //if the file path contains the directory
			{

			}
		}
		return null;  //show that we couldn't determine a relative path
	}
*/

	/**Stores an array of bytes in a file. The file is closed after the operation.
	@param file The file in which the bytes should be stored.
	@param bytes The bytes to store in the file.
	@exception IOException Thrown if there is an error loading the bytes.
	@see #readBytes
	*/
	public static void write(final File file, final byte[] bytes) throws IOException
	{
		final OutputStream fileOutputStream=new BufferedOutputStream(new FileOutputStream(file));  //create a buffered output stream to the file
		try
		{
		  fileOutputStream.write(bytes);  //write the bytes to the file
			fileOutputStream.flush(); //flush all our data to the file
		}
		finally
		{
			fileOutputStream.close();  //always close the file output stream
		}
	}

	/**Stores the contents of a file in an output stream.
	@param file The file to copy.
	@param outputStream The destination of the file contents.
	@exception IOException Thrown if there is an error copying the file.
	*/
	public static void write(final File file, final OutputStream outputStream) throws IOException
	{
		final InputStream fileInputStream=new BufferedInputStream(new FileInputStream(file)); //created a buffered input stream to the file
		try
		{
			OutputStreamUtilities.write(fileInputStream, outputStream);  //copy the contents of the input stream to the output stream
		}
		finally
		{
			fileInputStream.close();  //always close the file input stream
		}
	}

}