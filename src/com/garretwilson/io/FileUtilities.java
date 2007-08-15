package com.garretwilson.io;

import java.io.*;
import java.net.*;
import java.util.*;

import static java.util.Collections.*;

import javax.mail.internet.ContentType;
import com.garretwilson.lang.*;
import static com.garretwilson.lang.ObjectUtilities.*;

import com.garretwilson.net.*;
import com.garretwilson.rdf.RDF;
import com.garretwilson.rdf.RDFIO;

import static com.garretwilson.io.ContentTypeConstants.*;
import static com.garretwilson.io.FileConstants.*;
import static com.garretwilson.io.InputStreamUtilities.*;
import static com.garretwilson.lang.CharSequenceUtilities.*;
import static com.garretwilson.lang.IntegerUtilities.*;
import static com.garretwilson.lang.SystemUtilities.*;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.net.URIUtilities.*;

/**Various utilities for examining files.
@author Garret Wilson
*/
public class FileUtilities
{

	/**A singleton read-only map of lowercase file extensions and the corresponding content types they represent.*/
	public final static Map<String, ContentType> FILE_EXTENSION_CONTENT_TYPE_MAP;

	static
	{
		final Map<String, ContentType> tempFileExtensionContentTypeMap=new HashMap<String, ContentType>();	//create a new hash map in which to store extensions, and add the default extensions
		tempFileExtensionContentTypeMap.put("asi", new ContentType(APPLICATION, "x-qti-assessment", null));
		tempFileExtensionContentTypeMap.put(AU_EXTENSION, new ContentType(AUDIO, BASIC_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(BMP_EXTENSION, new ContentType(IMAGE, X_BITMAP_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(CLASS_EXTENSION, new ContentType(APPLICATION, JAVA_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(CSS_EXTENSION, new ContentType(TEXT, CSS_SUBTYPE, null));	//text/css
		tempFileExtensionContentTypeMap.put(DICTO_EXTENSION, new ContentType(APPLICATION, X_DICTO_RDF_XML_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(DOC_EXTENSION, new ContentType(APPLICATION, MSWORD_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(GIF_EXTENSION, new ContentType(IMAGE, GIF_SUBTYPE, null));	//image/gif
		tempFileExtensionContentTypeMap.put(HTM_EXTENSION, new ContentType(TEXT, HTML_SUBTYPE, null));	//TODO make sure changing this to text/html doesn't cause other methods to fail; nevertheless, we can't assume all .html files are XHTML (i.e. valid XML)
		tempFileExtensionContentTypeMap.put(HTML_EXTENSION, new ContentType(TEXT, HTML_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(ICAL_EXTENSION, new ContentType(TEXT, CALENDAR_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(JAVA_EXTENSION, new ContentType(TEXT, JAVA_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(JPEG_EXTENSION, new ContentType(IMAGE, JPEG_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(JPG_EXTENSION, new ContentType(IMAGE, JPEG_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(JS_EXTENSION, new ContentType(TEXT, JAVASCRIPT_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put("marmox", new ContentType(APPLICATION, "x-marmox-page+rdf+xml", null));
		tempFileExtensionContentTypeMap.put(MAQRO_EXTENSION, new ContentType(APPLICATION, X_MAQRO_RDF_XML_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(MP3_EXTENSION, new ContentType(AUDIO, MPEG_SUBTYPE, null));	//RFC 3003
		tempFileExtensionContentTypeMap.put(MPEG_EXTENSION, new ContentType(VIDEO, MPEG_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(MPG_EXTENSION, new ContentType(VIDEO, MPEG_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(OGG_EXTENSION, new ContentType(APPLICATION, OGG_SUBTYPE, null));	//application/ogg (RFC 3534)
		tempFileExtensionContentTypeMap.put(OEB_ZIP_EXTENSION, new ContentType(APPLICATION, X_OEB_PUBLICATION_ZIP_SUBTYPE, null));	//oebzip
		tempFileExtensionContentTypeMap.put(OEB1_PACKAGE_EXTENSION, new ContentType(APPLICATION, X_OEB1_PACKAGE_XML_SUBTYPE, null));	//opf
		tempFileExtensionContentTypeMap.put(PDF_EXTENSION, new ContentType(APPLICATION, PDF_SUBTYPE, null));	//pdf
		tempFileExtensionContentTypeMap.put(PNG_EXTENSION, new ContentType(IMAGE, PNG_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(QRO_EXTENSION, new ContentType(APPLICATION, X_QRO_RDF_XML_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put("qti", new ContentType(APPLICATION, "x-qti", null)); //G***use a constant here
		tempFileExtensionContentTypeMap.put(RAR_EXTENSION, new ContentType(APPLICATION, X_RAR_COMPRESSED_SUBTYPTE, null));
		tempFileExtensionContentTypeMap.put(TIF_EXTENSION, new ContentType(IMAGE, TIFF_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(TIFF_EXTENSION, new ContentType(IMAGE, TIFF_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(TXT_EXTENSION, new ContentType(TEXT, PLAIN_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(VCF_EXTENSION, new ContentType(TEXT, DIRECTORY_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(WAV_EXTENSION, new ContentType(AUDIO, X_WAV_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(XEB_EXTENSION, new ContentType(APPLICATION, X_XEBOOK_RDF_XML_SUBTYPE, null));
		tempFileExtensionContentTypeMap.put(XEB_ZIP_EXTENSION, new ContentType(APPLICATION, X_XEBOOK_RDF_XML_ZIP_SUBTYPE, null));	//oebzip
		tempFileExtensionContentTypeMap.put(ZIP_EXTENSION, new ContentType(APPLICATION, ZIP_SUBTYPE, null));
		FILE_EXTENSION_CONTENT_TYPE_MAP=unmodifiableMap(tempFileExtensionContentTypeMap);	//store read-only access to the map		
	}

	/**This class cannot be publicly instantiated.*/
  private FileUtilities()
	{
	}

	/**Adds the given extension to a file and returns the new file with
		the new extension.
		The filename is not checked to see if it currently has an extension.
	@param file The file to which to add an extension.
	@param extension The extension to add.
	@return The file with the new extension.
	*/
	public static File addExtension(final File file, final String extension)
	{
		return new File(addExtension(file.getPath(), extension));	//add an extension to the path and create and return a new file with that 
	}
	
	/**Adds the given extension to a filename and returns the new filename with the new extension.
	The filename is not checked to see if it currently has an extension.
	@param filename The filename to which to add an extension.
	@param extension The extension to add.
	@return The filename with the new extension.
	@exception NullPointerException if the given extension is <code>null</code>.
	*/
	public static String addExtension(final String filename, final String extension)
	{
		return new StringBuilder(filename).append(EXTENSION_SEPARATOR).append(checkInstance(extension, "Extension cannot be null")).toString();  //add the requested extension and return the new filename
	}

	/**Creates a temporary file in the standard temporary directory with automatic deletion on JVM exit.
	This convenience method provides more intuitive parameters than {@link File#createTempFile(String, String)}.
	@param baseName The base filename to be used in generating the filename.
	@param extension The extension to use for the temporary file, or <code>null</code> if a default extension should be used.
	@return A new temporary file.
	@exception NullPointerException if the given base name and/or extension is <code>null</code>.
	@exception IllegalArgumentException if the base name is the empty string.
	@exception IOException if there is a problem creating the temporary file.
	@see File#createTempFile(String, String)
	@see File#deleteOnExit()
	*/
	public static File createTempFile(final String baseName, final String extension) throws IOException
	{
		return createTempFile(baseName, extension, true);	//create a temporary file that is automatically scheduled for deletion
	}

	/**Creates a temporary file in the standard temporary directory with optional automatic deletion.
	This convenience method provides more intuitive parameters than {@link File#createTempFile(String, String)}.
	@param baseName The base filename to be used in generating the filename.
	@param extension The extension to use for the temporary file, or <code>null</code> if a default extension should be used.
	@param deleteOnExit Whether the file should be deleted when the JVM exits.
	@return A new temporary file.
	@exception NullPointerException if the given base name and/or extension is <code>null</code>.
	@exception IllegalArgumentException if the base name is the empty string.
	@exception IOException if there is a problem creating the temporary file.
	@see File#createTempFile(String, String)
	@see File#deleteOnExit()
	*/
	public static File createTempFile(final String baseName, final String extension, final boolean deleteOnExit) throws IOException
	{
		return createTempFile(baseName, extension, null, deleteOnExit);	//create a temp file using the standard temporary directory
	}
	
	/**Creates a temporary file in a given directory with optional automatic deletion.
	This convenience method provides more intuitive parameters than {@link File#createTempFile(String, String, File)}.
	@param baseName The base filename to be used in generating the filename.
	@param extension The extension to use for the temporary file, or <code>null</code> if a default extension should be used.
	@param directory The directory in which the file is to be created, or <code>null</code> if the default temporary-file directory is to be used.
	@param deleteOnExit Whether the file should be deleted when the JVM exits.
	@return A new temporary file.
	@exception NullPointerException if the given base name and/or extension is <code>null</code>.
	@exception IllegalArgumentException if the base name is the empty string.
	@exception IOException if there is a problem creating the temporary file.
	@see File#createTempFile(String, String, File)
	@see File#deleteOnExit()
	*/
	public static File createTempFile(String baseName, final String extension, final File directory, final boolean deleteOnExit) throws IOException
	{
		if(checkInstance(baseName, "Base name cannot be null.").length()==0)	//if the base name is empty
		{
			throw new IllegalArgumentException("Base name cannot be the empty string.");
		}
		if(baseName.length()<3)	//if the base name is under three characters long (the temp file creation API requires at least three characters)
		{
			baseName=baseName+"-temp";	//pad the base name to meet the requirements of File.createTempFile()
		}
		final File tempFile=File.createTempFile(baseName, new StringBuilder().append(EXTENSION_SEPARATOR).append(extension).toString(), directory);	//create a temporary file in the given directory, if any
		if(deleteOnExit)	//if the file should be deleted on JVM exit
		{
			tempFile.deleteOnExit();	//tell the file it should be deleted when the JVM exits
		}
		return tempFile;	//return the temporary file
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

	/**Returns the filename from a file path string in a cross-platform manner.
	This is useful to determine the filename if the file platform is not known.
	The characters after the last file path separator are returned.
	If there is no file path separator, the entire string is returned.
	@param filePath The file path.
	@return The filename after the last file path separator.
	@exception NullPointerException if the given file path is <code>null</code>.
	@see FileConstants#FILE_PATH_SEPARATOR_CHARACTERS
	*/
	public static String getFilename(final String filePath)	//TODO fix for Unix filenames; perhaps pass a system identification enum value
	{
		final int pathSeparatorIndex=charLastIndexOf(filePath, FILE_PATH_SEPARATOR_CHARACTERS);	//see if there are any file path separator characters (unfortunately, this will strip away any backslash found in a Unix filename if the entire path was sent, but it's better to have a too-short filename in a rare case than one that includes the full Windows path)
		return pathSeparatorIndex>=0 ? filePath.substring(pathSeparatorIndex+1) : filePath;	//if there is a path separator, remove everything but what comes after the last path separator
	}

	/**Extracts the extension from a file.
	Anything after the last path character ('/' or '\\') is ignored.
	@param file The file to examine.
	@return The extension of the file (not including '.'), or <code>null</code> if no extension is present.
	*/
	public static String getExtension(final File file)
	{
		return getExtension(file.getName());  //return the extension of the filename
	}

	/**Extracts the extension from a filename or path.
	Anything after the last path character ('/' or '\\') is ignored.
	@param filename The filename to examine.
	@return The extension of the file (not including '.'), or <code>null</code> if no extension is present.
	*/
	public static String getExtension(final String filename)
	{
		final int separatorIndex=getExtensionSeparatorIndex(filename); //see if we can find the extension separator
		return separatorIndex>=0 ? filename.substring(separatorIndex+1) : null;	//if we found a separator, return everything after it 
	}

	/**Returns the media type for the specified file extension.
	The file extension is first converted to lowercase before an attempt is made to look up a media type.
	@param fileExtension The file extension, without the '.', or <code>null</code> if there is no extension.
	@return The default media type for the file extension, or <code>null</code> if no known media type is associated with this file extension.
	*/
	public static ContentType getExtensionContentType(final String fileExtension)
	{
		return FILE_EXTENSION_CONTENT_TYPE_MAP.get(fileExtension!=null ? fileExtension.toLowerCase() : null);	//see if the file extension exists as a key in the file extension map
	}

	/**Changes the name of a file and returns a new file with the new name.
	@param file The file to examine.
	@param name The new name of the file.
	@return The file with the new name.
	@exception NullPointerException if the given file and/or name is <code>null</code>.
	*/
	public static File changeName(final File file, final String name)
	{
		final String path=file.getPath();	//get the file path
		final int pathLength=path.length();	//get the length of the path
		final String filename=file.getName();  //get the name of the file
		final int filenameLength=filename.length();	//get the length of the filename
		assert path.substring(pathLength).equals(filename) : "Expected last part of path to be filename.";	//the filename should always be the last part of the path, even if the file was originally crecated with an ending slash for a directory
		return new File(path.substring(0, pathLength-filenameLength)+checkInstance(name, "Name cannot be null."));
	}
	
	/**Changes the extension of a file and returns a new file with the new extension.
	If the file does not currently have an extension, one will be added.
	@param file The file to examine.
	@param extension The extension to set, or <code>null</code> if the extension should be removed.
	@return The file with the new extension.
	*/
	public static File changeExtension(final File file, final String extension)
	{
		return changeName(file, changeExtension(file.getName(), extension));  //return a file based on the name with the new extension
	}

	/**Changes the extension of a filename and returns a new filename with the new extension.
	If the filename does not currently have an extension, one will be added.
	@param filename The filename to examine.
	@param extension The extension to set, or <code>null</code> if the extension should be removed.
	@return The filename with the new extension.
	*/
	public static String changeExtension(String filename, final String extension)
	{
		final int separatorIndex=getExtensionSeparatorIndex(filename); //see if we can find the extension separator
		if(separatorIndex>=0)  //if we found a separator
		{
			filename=filename.substring(0, separatorIndex); //remove the extension
		}
		if(extension!=null)	//if an extension was given
		{
			filename=addExtension(filename, extension);	//add the requested extension
		}
		return filename;	//return the new filename
	}

	/**Removes the extension of a filename, if any, and returns a new file with no extension.
	This is a convenience method that delegates to {@link #changeExtension(File, String)}.
	@param file The file to examine.
	@param extension The extension to set, or <code>null</code> if the extension should be removed.
	@return The file with no extension.
	*/
	public static File removeExtension(final File file)
	{
		return changeExtension(file, null);	//replace the extension with nothing
	}

	/**Removes the extension of a filename, if any, and returns a new filename with no extension.
	This is a convenience method that delegates to {@link #changeExtension(String, String)}.
	@param filename The file to examine.
	@return The filename with no extension.
	*/
	public static String removeExtension(final String filename)
	{
		return changeExtension(filename, null);	//replace the extension with nothing
	}

	/**Appends a given string to the end of a filename before the extension, if any.
	This is useful for forming a locale-aware filename, such as <code>test_fr.txt</code> from <code>test.txt</code>. 
	@param path The path that may contain an extension.
	@param charSequence The characters to append to the filename.
	@return A path with the given string appended before the filename extension, if any.
	*/
	public static String appendFilename(final String path, final CharSequence charSequence)
	{
		final int separatorIndex=getExtensionSeparatorIndex(path); //see if we can find the extension separator
		final int insertionIndex=separatorIndex>=0 ? separatorIndex : path.length();	//insert the characters before the extension or, if there is no extension, at the end of the string
		return StringBuilderUtilities.insert(new StringBuilder(path), insertionIndex, charSequence).toString();	//create a new string builder, insert the characters, and return the new string
	}

	/**Determines the index of a file extension separator character ('.').
	Anything after the last path character ('/' or '\\') is ignored.
	@param path The filename or path to examine.
	@return The index of the extension separator character ('.'), or -1 if no extension is present.
	*/
	protected static int getExtensionSeparatorIndex(final String path)	//TODO fix to work with Windows backslashes as well
	{
		final int separatorIndex=path.lastIndexOf(EXTENSION_SEPARATOR); //see if we can find the extension separator, which will be the last such character in the string
		if(separatorIndex>=0)  //if we found a separator
		{
			if(charIndexOf(path, FILE_PATH_SEPARATOR_CHARACTERS, separatorIndex+1)<0)	//if there is no slash after after the extension separator
			{
				return separatorIndex;	//return the index of the extension separator
			}				
		}
		return -1;  //show that there is no extension		
	}

	/**Returns the media type for the specified file based on its extension.
	@param file The file for which to return a media type.
	@return The default media type for the file's extension, or <code>null</code>
		if no known media type is associated with this file's extension.
	@see FileUtilities#getExtensionContentType(String)
	*/
	public static ContentType getContentType(final File file)
	{
		final String extension=getExtension(file);  //get the file's extension
		return getExtensionContentType(extension); //return the media type based on the file's extension
	}

	/**Returns the appropriate URI for a directory, whether or not the directory
		exists. Contrast this behavior with <code>File.toURI()</code>, which will
		return a file URI without a trailing slash if the directory does not exist.
	@param directory The name of a directory, which may or may not exist.
	@return A URI, with trailing slash, to represent the given directory.
	*/
	public static URI getDirectoryURI(final File directory)
	{
		final URI fileURI=directory.toURI();	//create a URI from the file
		final String fileRawPath=fileURI.getRawPath();	//get the raw path of the directory URI
		if(endsWith(fileRawPath, PATH_SEPARATOR))	//if the file URI is a directory URI
		{
			return fileURI;	//return the URI as-is
		}
		else	//if the file URI isn't yet a directory URI
		{
				//create a new URI with the path separator appended
			return changeRawPath(fileURI, fileRawPath+PATH_SEPARATOR);
		}		
	}

	/**Returns the media type for the specified filename based on its extension.
	@param filename The filename to examine.
	@return The default media type for the filename's extension, or <code>null</code>
		if no known media type is associated with this file's extension or if the
		filename has no extension.
	@see FileUtilities#getExtensionContentType(String)
	*/
	public static ContentType getMediaType(final String filename)
	{
		final String extension=getExtension(filename);  //get the file's extension
		return extension!=null ? FileUtilities.getExtensionContentType(extension) : null; //return the media type based on the filename's extension, if there is one
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
				return "/"+
						URLUtilities.getRelativePath(rootDirectory.toURL(), file.toURL());
			}
			catch(MalformedURLException malformedURLException)  //if a URL was malformed
			{
				throw new AssertionError(malformedURLException); //we should never get this error
//TODO del				return file.getPath().replace('\\', FileConstants.PATH_SEPARATOR);  //this should never be reached G***use a constant here
			}
		}
		else  //if we were not given a root directory
		{
			return file.getPath().replace('\\', '/');  //return the normal file path with the correct path separator G***use a constant here
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
		if(isWindowsOS())	//if we're running on Windows
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
		final boolean isFilename=string!=null && charIndexOf(string, reservedCharacters)<0;
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
		if(isWindowsOS())	//if we're running on Windows
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
			final String encodedFilename=escapeHex(filename, null, reservedCharacters, ESCAPE_CHARACTER, 2);
			if(reservedFinalCharacters!=null && reservedFinalCharacters.length()>0)	//if we should check the final character (e.g. on Windows)
			{
				if(encodedFilename.length()>0)	//if we have a filename
				{
					final char lastChar=encodedFilename.charAt(encodedFilename.length()-1);	//see what the last character is
					if(reservedFinalCharacters.indexOf(lastChar)>=0)	//if the last character is a reserved character
					{
						return encodedFilename.substring(0, encodedFilename.length()-1)	//remove the last character
								+ESCAPE_CHARACTER+toHexString(lastChar, 2).toUpperCase();	//add the escaped version of the character in its place
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
		return unescapeHex(filename, ESCAPE_CHARACTER, 2);	//decode the filename
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
	
	/**Creates the directory named by this abstract pathname, throwing an exception if unsuccessful.
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
	
	/**Creates the directory named by this abstract pathname, including any necessary but nonexistent parent directories, throwing an exception if unsuccessful.
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

	/**If the directory does not exist, creates the directory named by this abstract pathname, including any necessary but nonexistent parent directories, throwing an exception if unsuccessful.
	@param directory The directory to create if necessary.
	@exception IOException Thrown if there is an error creating the directory.
	@see #mkdirs(File)
	*/
	public static void ensureDirectoryExists(final File directory) throws IOException
	{
		if(!directory.exists() || !directory.isDirectory())	//if the directory doesn't exist as a directory
		{
			mkdirs(directory);	//make the directories
		}		
	}

	/**Loads the contents of a file into an array of bytes. The file is closed after the operation.
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
			return getBytes(fileInputStream);  //convert the file to an array of bytes
		}
		finally
		{
			fileInputStream.close();  //always close the file input stream
		}
	}

	/**Reads an object from a file using the given I/O support.
	@param file The file from which to read.
	@param io The I/O support for reading the object.
	@return The object read from the file.
	@throws IOException if there is an error reading the data.
	*/ 
	public static <T> T read(final File file, final IO<T> io) throws IOException
	{
		final InputStream bufferedInputStream=new BufferedInputStream(new FileInputStream(file));	//create a buffered input stream to the file
		try
		{
			return io.read(bufferedInputStream, file.toURI());	//read the object, determining the base URI from the file
		}
		finally
		{
			bufferedInputStream.close();	//always close the input stream
		}
	}

	/**Reads an object from a file using the given RDF I/O support.
	@param file The file from which to read.
	@param rdf The RDF instance to use in creating new resources.
	@param io The I/O support for reading the object.
	@return The object read from the file.
	@throws IOException if there is an error reading the data.
	*/ 
	public static <T> T read(final File file, final RDF rdf, final RDFIO<T> io) throws IOException
	{
		final InputStream bufferedInputStream=new BufferedInputStream(new FileInputStream(file));	//create a buffered input stream to the file
		try
		{
			return io.read(rdf, bufferedInputStream, file.toURI());	//read the object, using the given RDF instance and determining the base URI from the file
		}
		finally
		{
			bufferedInputStream.close();	//always close the input stream
		}
	}

	/**Writes an object to a file using the given I/O support.
	@param file The file to which to write.
	@param object The object to write to the given file.
	@param io The I/O support for writing the object.
	@throws IOException if there is an error writing the data.
	*/
	public static <T> void write(final File file, final T object, final IO<T> io) throws IOException
	{
		final OutputStream bufferedOutputStream=new BufferedOutputStream(new FileOutputStream(file));//create a buffered output stream to the file
		try
		{
			io.write(bufferedOutputStream, file.toURI(), object);	//write the object, determining the base URI from the file
		}
		finally
		{
			bufferedOutputStream.close();	//always close the output stream
		}
	}

	/**Stores an array of bytes in a file. The file is closed after the operation.
	@param file The file in which the bytes should be stored.
	@param bytes The bytes to store in the file.
	@exception IOException Thrown if there is an error loading the bytes.
	@see #readBytes(File)
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

	
	
	/**Sorts a list of files in ascending order by modified time and secondly by file name.
	This method caches file information so that each file is accessed only once.
	@param fileList The list to be sorted.
	@throws IOException if there is an error accessing the file.
	@throws SecurityException if file access is not allowed.
	@throws UnsupportedOperationException if the specified list's
		list-iterator does not support the <code>set</code> operation.
	*/
	public static <T> void sortLastModified(final List<? extends File> fileList) throws IOException
	{
			//create a map of files mapped to modified times; use an identity map to speed things up (and will even allow files to appear in the list multiple times) 
		final Map<File, Long> lastModifiedMap=new IdentityHashMap<File, Long>(fileList.size());
		//create a map of files mapped to canonical paths; use an identity map to speed things up (and will even allow files to appear in the list multiple times) 
		final Map<File, String> canonicalPathMap=new IdentityHashMap<File, String>(fileList.size());
		for(final File file:fileList)	//get the last modified times and the canonical pathnames ahead of time to speed things up---and to throw an I/O exception here rather than during comparison
		{
			lastModifiedMap.put(file, Long.valueOf(file.lastModified()));	//cache the last modified time
			canonicalPathMap.put(file, file.getCanonicalPath());	//cache the canonical path
		}
		sort(fileList, new Comparator<File>()	//compare the files based upon last modified time and secondly canonical path 
				{
					public int compare(final File file1, final File file2)
					{
						final long lastModified1=lastModifiedMap.get(file1);	//get the last modified times from the map
						final long lastModified2=lastModifiedMap.get(file2);
						if(lastModified1!=lastModified2)	//if the modified times of the files are different
						{
							return lastModified1>lastModified2 ? 1 : -1;	//return the difference between modifications (we can't simply subtract, as the times theoretically could be farther apart than the integer range)
						}
						else	//if the files have the same modification times
						{
							return canonicalPathMap.get(file1).compareTo(canonicalPathMap.get(file2));	//compare canonical paths
						}
					}
				});
	}

	/**Stores the contents of an input stream in a file.
	@param inputStream The source of the file contents.
	@param file The destination of the file contents.
	@exception IOException Thrown if there is an error copying the information.
	*/
	public static void copy(final InputStream inputStream, final File file) throws IOException
	{
		final OutputStream fileOutputStream=new BufferedOutputStream(new FileOutputStream(file)); //created a buffered output stream to the file
		try
		{
			OutputStreamUtilities.copy(inputStream, fileOutputStream);  //copy the contents of the input stream to the output stream
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
	public static void copy(final File file, final OutputStream outputStream) throws IOException
	{
		final InputStream fileInputStream=new BufferedInputStream(new FileInputStream(file)); //created a buffered input stream to the file
		try
		{
			OutputStreamUtilities.copy(fileInputStream, outputStream);  //copy the contents of the input stream to the output stream
		}
		finally
		{
			fileInputStream.close();  //always close the file input stream
		}
	}

}