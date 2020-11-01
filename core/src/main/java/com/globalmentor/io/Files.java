/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
import java.net.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.function.Predicate;

import javax.annotation.*;

import static java.nio.file.Files.*;
import static java.util.Collections.*;
import static java.util.Objects.*;

import com.globalmentor.event.ProgressListener;
import com.globalmentor.java.*;
import com.globalmentor.net.*;
import com.globalmentor.text.*;

import static com.globalmentor.io.InputStreams.*;
import static com.globalmentor.io.Paths.*;
import static com.globalmentor.java.Classes.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.StringBuilders.*;
import static com.globalmentor.net.ContentTypeConstants.*;
import static com.globalmentor.net.URIs.*;

/**
 * Various constants and utilities for examining files.
 * <p>
 * For file URIs Java incorrectly uses the form <code>file:/mnt/sdcard/...</code> instead of <code>file:///mnt/sdcard/...</code>, but these utilities use the
 * former for consistency.
 * </p>
 * @author Garret Wilson
 * @see <a href="http://blogs.msdn.com/b/ie/archive/2006/12/06/file-uris-in-windows.aspx">File URIs in Windows</a>
 */
public class Files {

	/** The extension for backup files. */
	private static final String BACKUP_EXTENSION = "bak";

	/** The template for backup files in a rolling policy. */
	private static final StringTemplate NUMBERED_BACKUP_EXTENSION_TEMPLATE = StringTemplate.of(StringTemplate.STRING_PARAMETER, Filenames.EXTENSION_SEPARATOR,
			BACKUP_EXTENSION);
	/** The extension for the latest backup file in a rolling policy. */
	private static final String LATEST_NUMBERED_BACKUP_EXTENSION = NUMBERED_BACKUP_EXTENSION_TEMPLATE.apply(1);

	/** The default prefix for temporary files. */
	private static final String TEMP_PREFIX = "temp-";
	/** The extension for temporary files. */
	private static final String TEMP_EXTENSION = "tmp";

	/**
	 * The filename of the NTFS recycle bin folder.
	 * @see <a href="http://support.microsoft.com/kb/171694">Differences Between the Recycle Bin and the Recycler Folder</a>
	 */
	public static final String NTFS_RECYCLER_DIRECTORY_FILENAME = "RECYCLER";

	/**
	 * The NTFS delimiter for separating Alternate Data Stream identifiers from the rest of the filename.
	 * @see <a href="http://support.microsoft.com/kb/105763">How To Use NTFS Alternate Data Streams</a>
	 */
	public static final char NTFS_ADS_DELIMITER = ':';

	/**
	 * The name of the hidden system folder on Windows used by System Restore to store its information and restore points.
	 * @see <a href="http://support.microsoft.com/kb/309531">How to gain access to the System Volume Information folder</a>
	 */
	public static final String WINDOWS_SYSTEM_VOLUME_INFORMATION_DIRECTORY_FILENAME = "System Volume Information";

	//file extensions for certain media types TODO move to respective classes 
	/** The extension for Common Gateway Interface (CGI) files. */
	public static final String CGI_EXTENSION = "cgi";
	/** The extension for Microsoft Word files. */
	public static final String DOC_EXTENSION = "doc";
	/** The extension for iCalendar files. */
	public static final String ICAL_EXTENSION = "ical";
	/** An extension for Java Server Page (JSP) files. */
	public static final String JSP_EXTENSION = "jsp";
	/** The extension for Adobe PDF files. */
	public static final String PDF_EXTENSION = "pdf";
	/** The extension for vCard files. */
	public static final String VCF_EXTENSION = "vcf";

	/** The shared file filter that accepts all files. */
	public static final FileFilter WILDCARD_FILE_FILTER = new WildcardFileFilter();

	/**
	 * A singleton read-only map of lowercase file extensions and the corresponding content types they represent.
	 * @deprecated There are many reasons why this isn't a good idea. These things change, and more may be added. This is something better managed by some larger
	 *             entity or container.
	 */
	@Deprecated
	public static final Map<String, ContentType> FILE_EXTENSION_CONTENT_TYPE_MAP; //TODO convert to lazy weak referenced map

	static { //TODO move definitions to a resource file
		final Map<String, ContentType> tempFileExtensionContentTypeMap = new HashMap<String, ContentType>(); //create a new hash map in which to store extensions, and add the default extensions
		tempFileExtensionContentTypeMap.put("asi", ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, "x-qti-assessment"));
		tempFileExtensionContentTypeMap.put(Audio.AU_NAME_EXTENSION, ContentType.of(ContentType.AUDIO_PRIMARY_TYPE, BASIC_SUBTYPE));
		tempFileExtensionContentTypeMap.put(Images.BMP_NAME_EXTENSION, ContentType.of(ContentType.IMAGE_PRIMARY_TYPE, X_BITMAP_SUBTYPE));
		tempFileExtensionContentTypeMap.put(Classes.CLASS_NAME_EXTENSION, ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, JAVA_SUBTYPE));
		tempFileExtensionContentTypeMap.put("csv", ContentType.of(ContentType.TEXT_PRIMARY_TYPE, "csv"));
		tempFileExtensionContentTypeMap.put("css", ContentType.of(ContentType.TEXT_PRIMARY_TYPE, "css")); //text/css
		//		tempFileExtensionContentTypeMap.put(Dicto.DICTO_NAME_EXTENSION, ContentType.getInstance(ContentType.APPLICATION_PRIMARY_TYPE, X_DICTO_RDF_XML_SUBTYPE));
		tempFileExtensionContentTypeMap.put(DOC_EXTENSION, ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, MSWORD_SUBTYPE));
		tempFileExtensionContentTypeMap.put(Images.GIF_NAME_EXTENSION, ContentType.of(ContentType.IMAGE_PRIMARY_TYPE, GIF_SUBTYPE)); //image/gif
		tempFileExtensionContentTypeMap.put("htm", ContentType.of(ContentType.TEXT_PRIMARY_TYPE, "html")); //TODO make sure changing this to text/html doesn't cause other methods to fail; nevertheless, we can't assume all .html files are XHTML (i.e. valid XML)
		tempFileExtensionContentTypeMap.put("html", ContentType.of(ContentType.TEXT_PRIMARY_TYPE, "html"));
		tempFileExtensionContentTypeMap.put("xhtml",
				ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, "xhtml" + ContentType.SUBTYPE_SUFFIX_DELIMITER_CHAR + "xml"));
		tempFileExtensionContentTypeMap.put(ICAL_EXTENSION, ContentType.of(ContentType.TEXT_PRIMARY_TYPE, CALENDAR_SUBTYPE));
		tempFileExtensionContentTypeMap.put(Java.JAVA_NAME_EXTENSION, ContentType.of(ContentType.TEXT_PRIMARY_TYPE, JAVA_SUBTYPE));
		tempFileExtensionContentTypeMap.put(Images.JPEG_NAME_EXTENSION, ContentType.of(ContentType.IMAGE_PRIMARY_TYPE, JPEG_SUBTYPE));
		tempFileExtensionContentTypeMap.put(Images.JPG_NAME_EXTENSION, ContentType.of(ContentType.IMAGE_PRIMARY_TYPE, JPEG_SUBTYPE));
		tempFileExtensionContentTypeMap.put("js", ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, "javascript"));
		tempFileExtensionContentTypeMap.put("marmox", ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, "x-marmox-page+rdf+xml"));
		tempFileExtensionContentTypeMap.put("mact", ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, "x-mentor-activity+turf"));
		tempFileExtensionContentTypeMap.put(Audio.MP3_NAME_EXTENSION, ContentType.of(ContentType.AUDIO_PRIMARY_TYPE, MPEG_SUBTYPE)); //RFC 3003
		tempFileExtensionContentTypeMap.put(Video.MPEG_NAME_EXTENSION, ContentType.of(ContentType.VIDEO_PRIMARY_TYPE, MPEG_SUBTYPE));
		tempFileExtensionContentTypeMap.put(Video.MPG_NAME_EXTENSION, ContentType.of(ContentType.VIDEO_PRIMARY_TYPE, MPEG_SUBTYPE));
		tempFileExtensionContentTypeMap.put(Audio.OGG_NAME_EXTENSION, Audio.OGG_VORBIS_CONTENT_TYPE); //audio/ogg (RFC 5334)
		tempFileExtensionContentTypeMap.put("opf", ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE,
				ContentType.SUBTYPE_EXTENSION_PREFIX + "oeb1-package" + ContentType.SUBTYPE_SUFFIX_DELIMITER_CHAR + "xml")); //opf
		tempFileExtensionContentTypeMap.put(PDF_EXTENSION, ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, PDF_SUBTYPE)); //pdf
		tempFileExtensionContentTypeMap.put(Images.PNG_NAME_EXTENSION, ContentType.of(ContentType.IMAGE_PRIMARY_TYPE, PNG_SUBTYPE));
		tempFileExtensionContentTypeMap.put("qti", ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, "x-qti")); //TODO use a constant here
		tempFileExtensionContentTypeMap.put(Archive.RAR_NAME_EXTENSION, ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, X_RAR_COMPRESSED_SUBTYPTE));
		tempFileExtensionContentTypeMap.put(Images.TIF_NAME_EXTENSION, ContentType.of(ContentType.IMAGE_PRIMARY_TYPE, TIFF_SUBTYPE));
		tempFileExtensionContentTypeMap.put(Images.TIFF_NAME_EXTENSION, ContentType.of(ContentType.IMAGE_PRIMARY_TYPE, TIFF_SUBTYPE));
		tempFileExtensionContentTypeMap.put("rdf", ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, "rdf+xml"));
		tempFileExtensionContentTypeMap.put("turf", ContentType.of(ContentType.TEXT_PRIMARY_TYPE, "urf"));
		tempFileExtensionContentTypeMap.put(Text.TXT_NAME_EXTENSION, Text.PLAIN_CONTENT_TYPE);
		tempFileExtensionContentTypeMap.put(VCF_EXTENSION, ContentType.of(ContentType.TEXT_PRIMARY_TYPE, DIRECTORY_SUBTYPE));
		tempFileExtensionContentTypeMap.put(Audio.WAV_NAME_EXTENSION, ContentType.of(ContentType.AUDIO_PRIMARY_TYPE, X_WAV_SUBTYPE));
		//		tempFileExtensionContentTypeMap.put(RDFXEB.XEB_NAME_EXTENSION, ContentType.getInstance(ContentType.APPLICATION_PRIMARY_TYPE, X_XEBOOK_RDF_XML_SUBTYPE));
		tempFileExtensionContentTypeMap.put("xml", ContentType.of(ContentType.TEXT_PRIMARY_TYPE, "xml"));
		tempFileExtensionContentTypeMap.put(Archive.ZIP_NAME_EXTENSION, ContentType.of(ContentType.APPLICATION_PRIMARY_TYPE, ZIP_SUBTYPE));
		FILE_EXTENSION_CONTENT_TYPE_MAP = unmodifiableMap(tempFileExtensionContentTypeMap); //store read-only access to the map		
	}

	/** This class cannot be publicly instantiated. */
	private Files() {
	}

	//## File

	/**
	 * Adds the given extension to a file and returns the new file with the new extension. The filename is not checked to see if it currently has an extension.
	 * @param file The file to which to add an extension.
	 * @param extension The extension to add.
	 * @return The file with the new extension.
	 * @deprecated to be removed in favor of {@link #addNameExtension(File, String)}.
	 */
	@Deprecated
	public static File addExtension(final File file, final String extension) {
		return addNameExtension(file, extension);
	}

	/**
	 * Adds the given extension to a file name and returns the new file with the new extension. The filename is not checked to see if it currently has an
	 * extension.
	 * @implNote This implementation requires the given file to have a parent.
	 * @param file The file to which to add an extension.
	 * @param extension The extension to add.
	 * @return The file with the new name extension.
	 * @see File#getName()
	 */
	public static File addNameExtension(@Nonnull final File file, @Nonnull final String extension) {
		return new File(file.getParent(), Filenames.addExtension(file.getName(), extension)); //return the file with a "temp" extension
	}

	/** The characters recognized as wildcards in filenames. */
	@Deprecated
	public static final Characters FILENAME_WILDCARD_CHARACTERS = Characters.of(RegularExpressions.ZERO_OR_ONE_CHAR, RegularExpressions.ZERO_OR_MORE_CHAR);

	/** The characters to encode for patterns in a wildcard filename. */
	@Deprecated
	private static final Characters FILENAME_NON_WILDCARD_PATTERN_RESTRICTED_CHARACTERS = RegularExpressions.RESTRICTED.remove(FILENAME_WILDCARD_CHARACTERS);

	/**
	 * Lists files in the given file, the filename of which which may contain wildcards.
	 * <p>
	 * This implementation only recognizes the wildcard characters '*' and '?'. If these characters appear outside the filename, they will not be considered
	 * wildcards.
	 * </p>
	 * @param wildcardFile The file, the filename of which may contain wildcards.
	 * @return A list of files in the given path matching the given wildcard filename.
	 * @see #FILENAME_WILDCARD_CHARACTERS
	 */
	@Deprecated
	public static File[] listWildcards(final File wildcardFile) { //TODO improve to only return directories
		return listWildcards(wildcardFile.getParentFile(), wildcardFile.getName());
	}

	/**
	 * Lists files in the given directory with the given filename, which may contain wildcards.
	 * <p>
	 * This implementation only recognizes the wildcard characters '*' and '?'.
	 * </p>
	 * @param directory The directory in which to list the files.
	 * @param wildcardFilename The filename, which can contain wildcard characters.
	 * @return A list of files in the given directory with names matching the given wildcard filename.
	 * @see #FILENAME_WILDCARD_CHARACTERS
	 */
	@Deprecated
	public static File[] listWildcards(final File directory, final String wildcardFilename) {
		final StringBuilder filenamePatternStringBuilder = new StringBuilder(wildcardFilename);
		//1. Escape all pattern characters (including '.') except for '*' and '?' with '\'.
		escape(filenamePatternStringBuilder, FILENAME_NON_WILDCARD_PATTERN_RESTRICTED_CHARACTERS, RegularExpressions.ESCAPE);
		//2. Prefix every occurrence of '*' with '.'; ignore existing '.' characters, as we've already escaped them.
		escape(filenamePatternStringBuilder, Characters.of(RegularExpressions.ZERO_OR_MORE_CHAR), RegularExpressions.WILDCARD_CHAR, false);
		//3. Replace every occurrence of '?' with '.'.
		replace(filenamePatternStringBuilder, RegularExpressions.ZERO_OR_ONE_CHAR, RegularExpressions.WILDCARD_CHAR);
		final FileFilter fileFilter = new FilenamePatternFilter(filenamePatternStringBuilder.toString()); //create a file filter for this pattern
		return directory.listFiles(fileFilter);
	}

	/**
	 * Creates a temporary file in the standard temporary directory with no automatic deletion on JVM exit, using a {@value #TEMP_PREFIX} prefix and a
	 * {@value #TEMP_EXTENSION} extension.
	 * @return A new temporary file.
	 * @throws IOException if there is a problem creating the temporary file.
	 * @see File#createTempFile(String, String)
	 * @see #TEMP_EXTENSION
	 */
	public static File createTempFile() throws IOException {
		return createTempFile(TEMP_PREFIX);
	}

	/**
	 * Creates a temporary file in the standard temporary directory with no automatic deletion on JVM exit, using a {@value #TEMP_EXTENSION} extension.
	 * @param baseName The base filename to be used in generating the filename.
	 * @return A new temporary file.
	 * @throws NullPointerException if the given base name is <code>null</code>.
	 * @throws IllegalArgumentException if the base name is the empty string.
	 * @throws IOException if there is a problem creating the temporary file.
	 * @see File#createTempFile(String, String)
	 * @see #TEMP_EXTENSION
	 */
	public static File createTempFile(final String baseName) throws IOException {
		return createTempFile(baseName, TEMP_EXTENSION); //create a temporary file with a temp extension
	}

	/**
	 * Creates a temporary file in the standard temporary directory with no automatic deletion on JVM exit.
	 * @apiNote This convenience method provides more intuitive parameters than {@link File#createTempFile(String, String)}.
	 * @param baseName The base filename to be used in generating the filename.
	 * @param extension The extension to use for the temporary file, or <code>null</code> if a default extension should be used.
	 * @return A new temporary file.
	 * @throws NullPointerException if the given base name and/or extension is <code>null</code>.
	 * @throws IllegalArgumentException if the base name is the empty string.
	 * @throws IOException if there is a problem creating the temporary file.
	 * @see File#createTempFile(String, String)
	 */
	public static File createTempFile(final String baseName, final String extension) throws IOException {
		return createTempFile(baseName, extension, false); //create a temporary file that is automatically scheduled for deletion
	}

	/**
	 * Creates a temporary file in the standard temporary directory with optional automatic deletion, using a {@value #TEMP_EXTENSION} extension.
	 * @apiNote This convenience method provides more intuitive parameters than {@link File#createTempFile(String, String)}.
	 * @param baseName The base filename to be used in generating the filename.
	 * @param deleteOnExit Whether the file should be deleted when the JVM exits.
	 * @return A new temporary file.
	 * @throws NullPointerException if the given base name is <code>null</code>.
	 * @throws IllegalArgumentException if the base name is the empty string.
	 * @throws IOException if there is a problem creating the temporary file.
	 * @see File#createTempFile(String, String)
	 * @see File#deleteOnExit()
	 * @see #TEMP_EXTENSION
	 */
	public static File createTempFile(final String baseName, final boolean deleteOnExit) throws IOException {
		return createTempFile(baseName, TEMP_EXTENSION, deleteOnExit);
	}

	/**
	 * Creates a temporary file in the standard temporary directory with optional automatic deletion.
	 * @apiNote This convenience method provides more intuitive parameters than {@link File#createTempFile(String, String)}.
	 * @param baseName The base filename to be used in generating the filename.
	 * @param extension The extension to use for the temporary file, or <code>null</code> if a default extension should be used.
	 * @param deleteOnExit Whether the file should be deleted when the JVM exits.
	 * @return A new temporary file.
	 * @throws NullPointerException if the given base name and/or extension is <code>null</code>.
	 * @throws IllegalArgumentException if the base name is the empty string.
	 * @throws IOException if there is a problem creating the temporary file.
	 * @see File#createTempFile(String, String)
	 * @see File#deleteOnExit()
	 */
	public static File createTempFile(final String baseName, final String extension, final boolean deleteOnExit) throws IOException {
		return createTempFile(baseName, extension, null, deleteOnExit); //create a temp file using the standard temporary directory
	}

	/**
	 * Creates a temporary file for another file, with no automatic deletion on JVM exit.
	 * <p>
	 * This method can be used in two different ways, based upon the given file. If the given file is a directory, a temporary file will be created within the
	 * directory with the prefix {@value #TEMP_PREFIX}. Otherwise, if the given file is a directory and filename, a temporary file will be created in the same
	 * directory, using the given filename as a prefix.
	 * </p>
	 * @param file The file specifying the directory and optionally a filename to serve as a base name.
	 * @return A new temporary file.
	 * @throws NullPointerException if the given file is <code>null</code>.
	 * @throws IllegalArgumentException if the given file neither is a directory nor has a parent directory; or if the file is not a directory yet has no
	 *           filename.
	 * @throws IOException if there is a problem creating the temporary file.
	 * @see File#createTempFile(String, String, File)
	 * @see #TEMP_PREFIX
	 * @see #TEMP_EXTENSION
	 */
	public static File createTempFile(final File file) throws IOException {
		final File directory;
		final String baseName;
		if(file.isDirectory()) { //if a directory is given
			directory = file; //use the file as the directory
			baseName = TEMP_PREFIX; //use a generic temp prefix
		} else { //if the file is a directory+file 
			directory = file.getParentFile(); //put the temp file in the same directory
			checkArgument(directory != null, "Non-directory file %s has no parent directory.", file);
			baseName = file.getName();
			checkArgument(baseName != null, "Non-directory file %s has no filename.", file);
		}
		return createTempFile(baseName, directory);
	}

	/**
	 * Creates a temporary file in a given directory, using a {@value #TEMP_EXTENSION} extension.
	 * @apiNote This convenience method provides more intuitive parameters than {@link File#createTempFile(String, String, File)}.
	 * @param baseName The base filename to be used in generating the filename.
	 * @param directory The directory in which the file is to be created, or <code>null</code> if the default temporary-file directory is to be used.
	 * @return A new temporary file.
	 * @throws NullPointerException if the given base name is <code>null</code>.
	 * @throws IllegalArgumentException if the base name is the empty string.
	 * @throws IOException if there is a problem creating the temporary file.
	 * @see File#createTempFile(String, String, File)
	 * @see #TEMP_EXTENSION
	 */
	public static File createTempFile(final String baseName, final File directory) throws IOException {
		return createTempFile(baseName, directory, false);
	}

	/**
	 * Creates a temporary file in a given directory with optional automatic deletion, using a {@value #TEMP_EXTENSION} extension.
	 * @apiNote This convenience method provides more intuitive parameters than {@link File#createTempFile(String, String, File)}.
	 * @param baseName The base filename to be used in generating the filename.
	 * @param directory The directory in which the file is to be created, or <code>null</code> if the default temporary-file directory is to be used.
	 * @param deleteOnExit Whether the file should be deleted when the JVM exits.
	 * @return A new temporary file.
	 * @throws NullPointerException if the given base name is <code>null</code>.
	 * @throws IllegalArgumentException if the base name is the empty string.
	 * @throws IOException if there is a problem creating the temporary file.
	 * @see File#createTempFile(String, String, File)
	 * @see File#deleteOnExit()
	 * @see #TEMP_EXTENSION
	 */
	public static File createTempFile(final String baseName, final File directory, final boolean deleteOnExit) throws IOException {
		return createTempFile(baseName, TEMP_EXTENSION, directory, deleteOnExit);
	}

	/**
	 * Creates a temporary file in a given directory with optional automatic deletion.
	 * @apiNote This convenience method provides more intuitive parameters than {@link File#createTempFile(String, String, File)}.
	 * @param baseName The base filename to be used in generating the filename.
	 * @param extension The extension to use for the temporary file, or <code>null</code> if a default extension should be used.
	 * @param directory The directory in which the file is to be created, or <code>null</code> if the default temporary-file directory is to be used.
	 * @param deleteOnExit Whether the file should be deleted when the JVM exits.
	 * @return A new temporary file.
	 * @throws NullPointerException if the given base name and/or extension is <code>null</code>.
	 * @throws IllegalArgumentException if the base name is the empty string.
	 * @throws IOException if there is a problem creating the temporary file.
	 * @see File#createTempFile(String, String, File)
	 * @see File#deleteOnExit()
	 */
	public static File createTempFile(String baseName, final String extension, final File directory, final boolean deleteOnExit) throws IOException {
		if(requireNonNull(baseName, "Base name cannot be null.").length() == 0) { //if the base name is empty
			throw new IllegalArgumentException("Base name cannot be the empty string.");
		}
		if(baseName.length() < 3) { //if the base name is under three characters long (the temp file creation API requires at least three characters)
			baseName = baseName + "-temp"; //pad the base name to meet the requirements of File.createTempFile()
		}
		final File tempFile = File.createTempFile(baseName, new StringBuilder().append(Filenames.EXTENSION_SEPARATOR).append(extension).toString(), directory); //create a temporary file in the given directory, if any
		if(deleteOnExit) { //if the file should be deleted on JVM exit
			tempFile.deleteOnExit(); //tell the file it should be deleted when the JVM exits
		}
		return tempFile; //return the temporary file
	}

	/**
	 * Creates a new file, throwing an exception if unsuccessful.
	 * @param file The file to create.
	 * @throws IOException Thrown if there is an error creating the file.
	 */
	public static void createNewFile(final File file) throws IOException {
		if(!file.createNewFile()) { //create the file; if unsuccessful
			if(file.exists()) { //if the file already exists
				throw new IOException("File " + file + " already exists and cannot be created."); //throw an exception TODO i18n
			} else {
				//if the file doesn't exist, there must have been some other creation error
				throw new IOException("Cannot create " + file); //throw an exception TODO i18n
			}
		}
	}

	/**
	 * Deletes a directory or file, throwing an exception if unsuccessful.
	 * <p>
	 * If the file does not exist, no action occurs.
	 * </p>
	 * @param file The directory or file to delete.
	 * @throws IOException Thrown if there is an problem deleting any directory or file.
	 */
	public static void delete(final File file) throws IOException {
		delete(file, false); //delete the file without recursion
	}

	/**
	 * Deletes a directory or file, throwing an exception if unsuccessful. The operation will stop on the first error.
	 * <p>
	 * If the file does not exist, no action occurs.
	 * </p>
	 * @implNote This implementation appears to follow symbolic links.
	 * @param file The directory or file to delete. If a directory is passed, all its child files and directories will recursively be deleted if
	 *          <code>recursive</code> is <code>true</code>. If a file is passed, it will be deleted normally.
	 * @param recursive <code>true</code> if all child directories and files of a directory should recursively be deleted.
	 * @throws IOException Thrown if there is an problem deleting any directory or file.
	 * @deprecated in favor of of {@link #deleteFileTree(Path)}.
	 */
	@Deprecated
	public static void delete(final File file, final boolean recursive) throws IOException {
		if(recursive && file.isDirectory()) { //if this is a directory and we should recursively delete files
			final File[] files = file.listFiles(); //get all the files in the directory
			assert files != null : "File " + file + " is not a directory.";
			for(int i = files.length - 1; i >= 0; --i) { //look at each file in the directory
				delete(files[i], recursive); //delete this file
			}
		}
		if(file.exists() && !file.delete()) { //delete the file; if unsuccessful
			throw new IOException("Unable to delete " + file); //throw an exception TODO i18n
		}
	}

	/**
	 * Extracts the extension from a file. Anything after the last path character ('/' or '\\') is ignored.
	 * @param file The file to examine.
	 * @return The extension of the file (not including '.'), or <code>null</code> if no extension is present.
	 */
	public static Optional<String> findNameExtension(@Nonnull final File file) {
		return Filenames.findExtension(file.getName());
	}

	/**
	 * Extracts the extension from a file. Anything after the last path character ('/' or '\\') is ignored.
	 * @param file The file to examine.
	 * @return The extension of the file (not including '.'), or <code>null</code> if no extension is present.
	 * @deprecated to be removed in favor of {@link #findNameExtension(File)}.
	 */
	@Deprecated
	public static String getExtension(@Nonnull final File file) {
		return findNameExtension(file).orElse(null);
	}

	/**
	 * Returns the media type for the specified file extension. The file extension is first converted to lowercase before an attempt is made to look up a media
	 * type.
	 * @param fileExtension The file extension, without the '.', or <code>null</code> if there is no extension.
	 * @return The default media type for the file extension, or <code>null</code> if no known media type is associated with this file extension.
	 * @deprecated to be removed in favor of some other content type discovery mechanism.
	 */
	@Deprecated
	public static ContentType getExtensionContentType(final String fileExtension) {
		return FILE_EXTENSION_CONTENT_TYPE_MAP.get(fileExtension != null ? fileExtension.toLowerCase() : null); //see if the file extension exists as a key in the file extension map
	}

	/**
	 * Changes the name of a file and returns a new file with the new name.
	 * @param file The file to examine.
	 * @param name The new name of the file.
	 * @return The file with the new name.
	 * @throws NullPointerException if the given file and/or name is <code>null</code>.
	 */
	public static File changeName(final File file, final String name) {
		final String path = file.getPath(); //get the file path
		final int pathLength = path.length(); //get the length of the path
		final String filename = file.getName(); //get the name of the file
		final int filenameLength = filename.length(); //get the length of the filename
		assert path.substring(pathLength - filenameLength).equals(filename) : "Expected last part of path to be filename."; //the filename should always be the last part of the path, even if the file was originally created with an ending slash for a directory
		return new File(path.substring(0, pathLength - filenameLength) + requireNonNull(name, "Name cannot be null."));
	}

	/**
	 * Changes the extension of a file and returns a new file with the new extension. If the file does not currently have an extension, one will be added.
	 * @param file The file to examine.
	 * @param extension The extension to set, or <code>null</code> if the extension should be removed.
	 * @return The file with the new extension.
	 * @deprecated to be removed in favor of {@link #changeNameExtension(File, String)}.
	 */
	@Deprecated
	public static File changeExtension(final File file, final String extension) {
		return changeNameExtension(file, extension);
	}

	/**
	 * Changes the extension of a file name and returns a new file with the new extension. If the filename does not currently have an extension, one will be
	 * added.
	 * @param file The file to examine.
	 * @param extension The extension to set, or <code>null</code> if the extension should be removed.
	 * @return The file with the new extension.
	 */
	public static File changeNameExtension(@Nonnull final File file, @Nullable final String extension) {
		return changeName(file, Filenames.changeExtension(file.getName(), extension)); //return a file based on the name with the new extension
	}

	/**
	 * Checks to see if a given file exists as a directory, throwing an exception if not.
	 * @param directory The file to check for existence as a directory.
	 * @return The given file.
	 * @throws FileNotFoundException if the given file does not exist or is not a directory.
	 * @see File#isDirectory()
	 */
	public static File checkDirectoryExists(final File directory) throws FileNotFoundException {
		if(!directory.isDirectory()) { //if the given file is not a directory
			checkFileExists(directory); //see if the file isn't a directory or it doesn't exist altogether
			throw new FileNotFoundException("File does not exist as a directory: " + directory);
		}
		return directory;
	}

	/**
	 * Checks to see if a given file exists, throwing an exception if not.
	 * @param file The file to check for existence.
	 * @return The given file.
	 * @throws FileNotFoundException if the given file does not exist.
	 * @see File#exists()
	 */
	public static File checkFileExists(final File file) throws FileNotFoundException {
		if(!file.exists()) {
			throw new FileNotFoundException("File does not exist: " + file);
		}
		return file;
	}

	/**
	 * Removes the extension of a filename, if any, and returns a new file with no extension.
	 * @param file The file to examine.
	 * @return The file with no extension.
	 * @deprecated to be removed in favor of {@link #removeNameExtension(File)}.
	 */
	@Deprecated
	public static File removeExtension(final File file) {
		return removeNameExtension(file);
	}

	/**
	 * Removes the extension of a filename, if any, and returns a new file with no extension.
	 * @implSpec This is a convenience method that delegates to {@link #changeNameExtension(File, String)}.
	 * @param file The file to examine.
	 * @return The file with no extension.
	 */
	public static File removeNameExtension(@Nonnull final File file) {
		return changeNameExtension(file, null); //replace the extension with nothing
	}

	/**
	 * Returns the media type for the specified file based on its extension.
	 * @param file The file for which to return a media type.
	 * @return The default media type for the file's extension, or <code>null</code> if no known media type is associated with this file's extension.
	 * @see #getExtensionContentType(String)
	 * @deprecated to be removed in favor of some other content type discovery mechanism.
	 */
	@Deprecated
	public static ContentType getContentType(final File file) {
		return findNameExtension(file).map(Files::getExtensionContentType).orElse(null);
	}

	/**
	 * Returns the media type for the specified filename based on its extension.
	 * @param filename The filename to examine.
	 * @return The default media type for the filename's extension, or <code>null</code> if no known media type is associated with this file's extension or if the
	 *         filename has no extension.
	 * @see #getExtensionContentType(String)
	 * @deprecated to be removed in favor of some other content type discovery mechanism.
	 */
	@Deprecated
	public static ContentType getMediaType(final String filename) {
		return Filenames.findExtension(filename).map(Files::getExtensionContentType).orElse(null);
	}

	/**
	 * Returns a file suitable for a temporary file, based on the specified filename, by adding an extension for a temporary file.
	 * <p>
	 * This implementation requires the given file to have a parent.
	 * </p>
	 * @param file The file for which a temporary file should be returned.
	 * @return The file suitable for temporary access.
	 * @see TEMP_EXTENSION
	 */
	public static File getTempFile(final File file) {
		return new File(file.getParent(), Filenames.addExtension(file.getName(), TEMP_EXTENSION)); //return the file with a "temp" extension
	}

	/**
	 * Returns a file suitable for backup, based on the specified filename, by adding an extension for a backup file.
	 * <p>
	 * This implementation requires the given file to have a parent.
	 * </p>
	 * @param file The file for which a backup file should be returned.
	 * @return The file suitable for backup.
	 * @see BACKUP_EXTENSION
	 */
	public static File getBackupFile(final File file) {
		return new File(file.getParent(), Filenames.addExtension(file.getName(), BACKUP_EXTENSION)); //return the file with a "backup" extension
	}

	/**
	 * @return The user's current directory.
	 * @throws SecurityException Thrown if we don't have permission to access the user's directory.
	 * @see System
	 * @see OperatingSystem#USER_DIR_PROPERTY
	 */
	public static File getUserDirectory() throws SecurityException {
		return new File(System.getProperty(OperatingSystem.USER_DIR_PROPERTY)); //try to get the current directory
	}

	/**
	 * Checks to see if a particular file exists. If the file does not exist, yet a backup file exists, the backup file will be moved to the original file
	 * location. If this method returns true, there will be a file located at <code>file</code>.
	 * @param file The file to check for existence.
	 * @param backupFile The file to use as a backup if the original does not exist.
	 * @return <code>true</code> if the file existed or exists now after moving the backup file, else <code>false</code> if neither file exists.
	 * @throws IOException Thrown if the backup file cannot be moved.
	 */
	public static boolean ensureExistsFromBackup(final File file, final File backupFile) throws IOException {
		if(file.exists()) { //if the file exists
			return true; //there's nothing more to do; return true
		} else { //if the file doesn't exist
			if(backupFile.exists()) { //if the backup file exists
				move(backupFile, file); //try to move the backup file to the original file
				return true; //show that a file now exists where it is expected to be
			} else
				//if the backup file does not exist
				return false; //show that we can't find either file
		}
	}

	/**
	 * Checks to see if a particular file exists. If the file does not exist, yet a backup file exists, the backup file will be moved to the original file
	 * location. If this method returns true, there will be a file located at <code>file</code>. This method automatically determines the name of the backup file.
	 * @param file The file to check for existence.
	 * @return <code>true</code> if the file existed or exists now after moving the backup file, else <code>false</code> if neither file exists.
	 * @throws IOException Thrown if the backup file cannot be moved.
	 * @see #getBackupFile
	 */
	public static boolean ensureExistsFromBackup(final File file) throws IOException {
		return ensureExistsFromBackup(file, getBackupFile(file)); //check to see if the file exists, using the default filename for the backup file
	}

	/**
	 * Constructs a {@link URIs#FILE_SCHEME} scheme URI that represents this abstract pathname.
	 * <p>
	 * This functions similarly to {@link File#toURI()}, except that this method always returns a true URI in which the characters all are within ranges allowed
	 * by RFC 3986, notably that non-ASCII characters are all encoded.
	 * </p>
	 * <p>
	 * In addition, the character <code>';'</code> is encoded, as expected by HTTP servers such as Apache when part of the path.
	 * </p>
	 * <p>
	 * Following the examples in RFC 3986, this is guaranteed to produce only <em>lowercase</em> hexadecimal escape codes.
	 * </p>
	 * <p>
	 * This method may not return a URI with a trailing slash for directories that don't exist. If it is known whether the file represents a directory,
	 * {@link #toURI(File, boolean)} should be used instead.
	 * </p>
	 * @param file The file which should be converted to a URI.
	 * @return An absolute, hierarchical URI with non-ASCII characters encoded, with a {@link URIs#FILE_SCHEME} scheme, a path representing this abstract
	 *         pathname, and undefined authority, query, and fragment components.
	 * @throws NullPointerException if the given file is <code>null</code>.
	 * @throws SecurityException If a required system property value cannot be accessed.
	 * @see File#toURI()
	 * @see URIs#canonicalize(URI)
	 * @see URIs#PATH_CHARACTERS
	 */
	public static URI toURI(final File file) {
		return toURI(file, false);
	}

	/**
	 * Constructs a {@link URIs#FILE_SCHEME} scheme URI that represents this abstract pathname.
	 * <p>
	 * This functions similarly to {@link File#toURI()}, except that this method always returns a true URI in which the characters all are within ranges allowed
	 * by RFC 3986, notably that non-ASCII characters are all encoded.
	 * </p>
	 * <p>
	 * In addition, the character <code>';'</code> is encoded, as expected by HTTP servers such as Apache when part of the path.
	 * </p>
	 * <p>
	 * Following the examples in RFC 3986, this is guaranteed to produce only <em>lowercase</em> hexadecimal escape codes.
	 * </p>
	 * <p>
	 * If a directory URI is requested, the appropriate URI for a directory is returned, whether or not the directory exists. Contrast this behavior with
	 * {@link File#toURI()}, which will return a file URI without a trailing slash if the directory does not exist.
	 * </p>
	 * @param file The file which should be converted to a URI.
	 * @param forceDirectoryURI Whether the URI should be returned with a trailing slash, even if the file does not exist as a directory.
	 * @return An absolute, hierarchical URI with non-ASCII characters encoded, with a {@link URIs#FILE_SCHEME} scheme, a path representing this abstract
	 *         pathname, and undefined authority, query, and fragment components.
	 * @throws NullPointerException if the given file is <code>null</code>.
	 * @throws SecurityException If a required system property value cannot be accessed.
	 * @see File#toURI()
	 * @see URIs#canonicalize(URI)
	 * @see URIs#PATH_CHARACTERS
	 */
	public static URI toURI(final File file, final boolean forceDirectoryURI) {
		URI uri = file.toURI(); //create a URI from the file normally; Java may allow non-ASCII characters in this version
		//test the entire URI for non-ASCII characters, as well the ';' character, which has a special meaning in URIs
		//get the scheme-specific of the URI, as Windows UNC path URIs hide an extra couple of slash characters elsewhere than in the path
		//assuming that most URIs do not have non-ASCII characters, it will be more efficient to check the characters first, as we may not have to do any conversion
		String rawSSP = uri.getRawSchemeSpecificPart();
		for(int i = rawSSP.length() - 1; i >= 0; --i) { //for each character (iteration order doesn't matter)
			final char c = rawSSP.charAt(i);
			if(c > 127 || c == ';') { //if we found a non-ASCII character or the special character ';'
				//escape the scheme-specific part from scratch, but only consider the ';' character and characters above 127 invalid so as to preserve the originally encoded characters, if any 
				rawSSP = CharSequences.escapeHex(rawSSP, null, Characters.of(';'), 127, URIs.ESCAPE_CHAR, 2, Case.LOWERCASE); //TODO use a constant
				uri = URIs.changeRawSchemeSpecificPart(uri, rawSSP); //change the scheme-specific part of the URI
				break; //skip looking at the rest of the string
			}
		}
		uri = canonicalize(uri); //convert the URI to canonical form; even if we converted ASCII characters, the File.toURI() method might have produced uppercase hex escape codes when escaping illegal characters
		if(forceDirectoryURI) { //force a collection URI if needed
			uri = toCollectionURI(uri);
		}
		return uri;
	}

	/**
	 * Determines whether the given child file is a child of the parent file in the file system hierarchy. This method returns <code>false</code> if the files
	 * refer to the same file.
	 * <p>
	 * This implementation correctly recognizes parent/child relationships on case-insensitive file systems such as that used in Windows.
	 * </p>
	 * @param parentFile The supposed parent file.
	 * @param file The supposed child file.
	 * @return <code>true</code> if the files share a parent/child relationship in the file system hierarchy.
	 * @throws NullPointerException if the given parent file and/or child file is <code>null</code>.
	 */
	public static boolean isChild(final File parentFile, File file) {
		while((file = file.getParentFile()) != null) { //while there are parents
			if(parentFile.equals(file)) { //if this parent equals the given parent
				return true;
			}
		}
		return false;
	}

	/**
	 * Creates the directory named by this abstract pathname, throwing an exception if unsuccessful.
	 * @param directory The directory to create.
	 * @throws IOException Thrown if there is an error creating the directory.
	 */
	public static void mkdir(final File directory) throws IOException {
		if(!directory.mkdir()) { //create the directory; if unsuccessful
			throw new IOException("Cannot create directory " + directory); //throw an exception TODO i18n
		}
	}

	/**
	 * Creates the directory named by this abstract pathname, including any necessary but nonexistent parent directories, throwing an exception if unsuccessful.
	 * @param directory The directory to create.
	 * @throws IOException Thrown if there is an error creating the directory.
	 */
	public static void mkdirs(final File directory) throws IOException {
		if(!directory.mkdirs()) { //create the directory; if unsuccessful
			throw new IOException("Cannot create directories " + directory); //throw an exception TODO i18n
		}
	}

	/**
	 * If the directory does not exist, creates the directory named by this abstract pathname, including any necessary but nonexistent parent directories,
	 * throwing an exception if unsuccessful.
	 * @param directory The directory to create if necessary.
	 * @return The directory, which has been verified to exist.
	 * @throws IOException Thrown if there is an error creating the directory.
	 * @see #mkdirs(File)
	 */
	public static File ensureDirectoryExists(final File directory) throws IOException {
		if(!directory.exists() || !directory.isDirectory()) { //if the directory doesn't exist as a directory
			mkdirs(directory); //make the directories
		}
		return directory;
	}

	/**
	 * Loads the contents of a file into an array of bytes. The file is closed after the operation.
	 * @param file The file from which to read.
	 * @return An array of bytes from the input stream.
	 * @throws IOException Thrown if there is an error loading the bytes.
	 * @see InputStreams#getBytes
	 * @see #write
	 */
	public static byte[] readBytes(final File file) throws IOException {
		final InputStream fileInputStream = new FileInputStream(file); //create an input stream to the file
		try {
			return getBytes(fileInputStream); //convert the file to an array of bytes
		} finally {
			fileInputStream.close(); //always close the file input stream
		}
	}

	/**
	 * Reads an object from a file using the given I/O support.
	 * @param <T> //TODO write this
	 * @param file The file from which to read.
	 * @param io The I/O support for reading the object.
	 * @return The object read from the file.
	 * @throws IOException if there is an error reading the data.
	 */
	public static <T> T read(final File file, final IO<T> io) throws IOException {
		final InputStream bufferedInputStream = new BufferedInputStream(new FileInputStream(file)); //create a buffered input stream to the file
		try {
			return io.read(bufferedInputStream, toURI(file)); //read the object, determining the base URI from the file
		} finally {
			bufferedInputStream.close(); //always close the input stream
		}
	}

	/**
	 * Reads the contents of the given file.
	 * @apiNote This is a convenience that obviates the need for boilerplate for getting an {@link InputStream} from a {@link File}.
	 * @param file The file from which the information will be read.
	 * @param inputStreamReadable The object to read the information from the file.
	 * @throws IOException if there is an error loading the contents of the file.
	 */
	public static void read(final File file, final InputStreamReadable inputStreamReadable) throws IOException {
		final InputStream bufferedInputStream = new BufferedInputStream(new FileInputStream(file)); //create a buffered input stream to the file
		try {
			inputStreamReadable.read(bufferedInputStream, toURI(file)); //read from the stream, determining the base URI from the file
		} finally {
			bufferedInputStream.close(); //always close the input stream
		}
	}

	/**
	 * Writes to the given file.
	 * @apiNote This is a convenience that obviates the need for boilerplate for getting an {@link OutputStream} to a {@link File}.
	 * @param file The file to which the information will be written.
	 * @param outputStreamWritable The object to write the information to the file.
	 * @throws IOException if there is an error writing the contents to the file.
	 */
	public static void write(final File file, final OutputStreamWritable outputStreamWritable) throws IOException {
		final OutputStream bufferedOutputStream = new BufferedOutputStream(new FileOutputStream(file)); //create a buffered output stream to the file
		try {
			outputStreamWritable.write(bufferedOutputStream, toURI(file)); //write to the stream, determining the base URI from the file
		} finally {
			bufferedOutputStream.close(); //always close the input stream
		}
	}

	/**
	 * Renames the file, throwing an exception if unsuccessful.
	 * <p>
	 * This method is a direct delegation to {@link File#renameTo(File)} with added exception handling. If overwriting of a destination file is desired, use
	 * {@link #move(File, File)} or a related method.
	 * </p>
	 * @param source The file to rename.
	 * @param destination The new name of the file
	 * @throws IOException Thrown if there is an error renaming the file.
	 * @see #move(File, File)
	 */
	public static void renameTo(final File source, final File destination) throws IOException {
		if(!source.renameTo(destination)) { //rename the file to its new filename; if unsuccessful
			throw new IOException("Cannot rename " + source + " to " + destination); //throw an exception TODO i18n
		}
	}

	/**
	 * Moves a file to a different location, overwriting the destination file if it exists.
	 * @param sourceFile The file to be moved.
	 * @param destinationFile The location to where the source files should be be moved.
	 * @throws IOException Thrown if there is an error moving the file.
	 */
	public static void move(final File sourceFile, final File destinationFile) throws IOException {
		move(sourceFile, destinationFile, true);
	}

	/**
	 * Moves a file to a different location, overwriting the destination file if it exists.
	 * @param sourceFile The file to be moved.
	 * @param destinationFile The location to where the source files should be be moved.
	 * @param overwrite <code>true</code> if any existing file or directory at the destination should be overwritten, or <code>false</code> if an existing file or
	 *          directory at the destination should cause an exception to be thrown.
	 * @throws IllegalStateException if overwrite is turned off and a destination file exists.
	 * @throws IOException Thrown if there is an error moving the file.
	 */
	public static void move(final File sourceFile, final File destinationFile, final boolean overwrite) throws IOException {
		if(!overwrite) {
			if(destinationFile.exists()) {
				throw new IllegalStateException("Move destination file " + destinationFile + " already exists.");
			}
		}
		move(sourceFile, destinationFile, null); //move the source file to the destination file, specifying that no backup should be made
	}

	/**
	 * Moves a file to a different location, overwriting the destination file if it exists. If a backup file is specified, it is first deleted and the destination
	 * file is moved to the backup file location before the source file is moved.
	 * @param sourceFile The file to be moved.
	 * @param destinationFile The location to where the source files should be be moved.
	 * @param backupFile The backup file to where the destination file, if any, will first be moved, or <code>null</code> if no backup file is necessary.
	 * @throws IOException Thrown if there is an error renaming the file.
	 */
	public static void move(final File sourceFile, final File destinationFile, final File backupFile) throws IOException {
		if(destinationFile.exists()) { //if the destination file exists
			if(backupFile != null) { //if we should backup the original destination file, and the original destination file exists
				//try to move the destination file to the backup file; if this fails, at worst it will leave the destination file at the same state it was in to begin with 
				move(destinationFile, backupFile);
			}
			delete(destinationFile); //delete the destination file, throwing an exception if there is an error
		}
		renameTo(sourceFile, destinationFile); //move the source file to the destination file
	}

	/**
	 * Sorts a list of files in ascending order by modified time and secondly by file name. This method caches file information so that each file is accessed only
	 * once.
	 * @param <T> //TODO write this
	 * @param fileList The list to be sorted.
	 * @throws IOException if there is an error accessing the file.
	 * @throws SecurityException if file access is not allowed.
	 * @throws UnsupportedOperationException if the specified list's list-iterator does not support the <code>set</code> operation.
	 */
	public static <T> void sortLastModified(final List<? extends File> fileList) throws IOException {
		//create a map of files mapped to modified times; use an identity map to speed things up (and will even allow files to appear in the list multiple times) 
		final Map<File, Long> lastModifiedMap = new IdentityHashMap<File, Long>(fileList.size());
		//create a map of files mapped to canonical paths; use an identity map to speed things up (and will even allow files to appear in the list multiple times) 
		final Map<File, String> canonicalPathMap = new IdentityHashMap<File, String>(fileList.size());
		for(final File file : fileList) { //get the last modified times and the canonical pathnames ahead of time to speed things up---and to throw an I/O exception here rather than during comparison
			lastModifiedMap.put(file, Long.valueOf(file.lastModified())); //cache the last modified time
			canonicalPathMap.put(file, file.getCanonicalPath()); //cache the canonical path
		}
		sort(fileList, new Comparator<File>() { //compare the files based upon last modified time and secondly canonical path 

			public int compare(final File file1, final File file2) {
				final long lastModified1 = lastModifiedMap.get(file1); //get the last modified times from the map
				final long lastModified2 = lastModifiedMap.get(file2);
				if(lastModified1 != lastModified2) { //if the modified times of the files are different
					return lastModified1 > lastModified2 ? 1 : -1; //return the difference between modifications (we can't simply subtract, as the times theoretically could be farther apart than the integer range)
				} else { //if the files have the same modification times
					return canonicalPathMap.get(file1).compareTo(canonicalPathMap.get(file2)); //compare canonical paths
				}
			}
		});
	}

	/**
	 * Stores the contents of an input stream in a file.
	 * @param inputStream The source of the file contents.
	 * @param file The destination of the file contents.
	 * @throws IOException Thrown if there is an error copying the information.
	 */
	public static void copy(final InputStream inputStream, final File file) throws IOException {
		copy(inputStream, file, null);
	}

	/**
	 * Stores the contents of an input stream in a file.
	 * @param inputStream The source of the file contents.
	 * @param file The destination of the file contents.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @throws IOException Thrown if there is an error copying the information.
	 */
	public static void copy(final InputStream inputStream, final File file, final ProgressListener progressListener) throws IOException {
		final OutputStream fileOutputStream = new BufferedOutputStream(new FileOutputStream(file)); //created a buffered output stream to the file
		try {
			IOStreams.copy(inputStream, fileOutputStream, progressListener); //copy the contents of the input stream to the output stream
		} finally {
			fileOutputStream.close(); //always close the file output stream
		}
	}

	/**
	 * Stores the contents of a file in an output stream.
	 * @param file The file to copy.
	 * @param outputStream The destination of the file contents.
	 * @throws IOException Thrown if there is an error copying the file.
	 */
	public static void copy(final File file, final OutputStream outputStream) throws IOException {
		copy(file, outputStream, null);
	}

	/**
	 * Copies the given source file or directory to the given destination file or directory. Destination files are overwritten. The last modified date of the
	 * destination file or directory is updated to match that of the source.
	 * @param sourceFile The source file or directory to copy.
	 * @param destinationFile The destination of the copied file or directory.
	 * @throws NullPointerException if the given source file and/or destination file is <code>null</code>.
	 * @throws IllegalArgumentException if the given destination file is a child of the given source file, representing a circular copy.
	 * @throws FileNotFoundException if the given source file is <code>null</code>.
	 * @throws IllegalStateException if a directory copy is performed and the destination file's parent directory doesn't exist.
	 */
	public static void copy(final File sourceFile, final File destinationFile) throws IOException {
		copy(sourceFile, destinationFile, true);
	}

	/**
	 * Copies the given source file or directory to the given destination file or directory. Destination files are overwritten. The last modified date of the
	 * destination file or directory is updated to match that of the source.
	 * @param sourceFile The source file or directory to copy.
	 * @param destinationFile The destination of the copied file or directory.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @throws NullPointerException if the given source file and/or destination file is <code>null</code>.
	 * @throws IllegalArgumentException if the given destination file is a child of the given source file, representing a circular copy.
	 * @throws FileNotFoundException if the given source file is <code>null</code>.
	 * @throws IllegalStateException if a directory copy is performed and the destination file's parent directory doesn't exist.
	 */
	public static void copy(final File sourceFile, final File destinationFile, final ProgressListener progressListener) throws IOException {
		copy(sourceFile, destinationFile, true, progressListener);
	}

	/**
	 * Copies the given source file or directory to the given destination file or directory. Destination files are overwritten. The last modified date of the
	 * destination file or directory is updated to match that of the source.
	 * @param sourceFile The source file or directory to copy.
	 * @param destinationFile The destination of the copied file or directory.
	 * @param deep <code>true</code> if child files and directories of source directories should be recursively copied.
	 * @throws NullPointerException if the given source file and/or destination file is <code>null</code>.
	 * @throws IllegalArgumentException if the given destination file is a child of the given source file, representing a circular copy.
	 * @throws FileNotFoundException if the given source file is <code>null</code>.
	 * @throws IllegalStateException if a directory copy is performed and the destination file's parent directory doesn't exist.
	 */
	public static void copy(final File sourceFile, final File destinationFile, final boolean deep) throws IOException {
		copy(sourceFile, destinationFile, deep, WILDCARD_FILE_FILTER);
	}

	/**
	 * Copies the given source file or directory to the given destination file or directory. Destination files are overwritten. The last modified date of the
	 * destination file or directory is updated to match that of the source.
	 * @param sourceFile The source file or directory to copy.
	 * @param destinationFile The destination of the copied file or directory.
	 * @param deep <code>true</code> if child files and directories of source directories should be recursively copied.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @throws NullPointerException if the given source file and/or destination file is <code>null</code>.
	 * @throws IllegalArgumentException if the given destination file is a child of the given source file, representing a circular copy.
	 * @throws FileNotFoundException if the given source file is <code>null</code>.
	 * @throws IllegalStateException if a directory copy is performed and the destination file's parent directory doesn't exist.
	 */
	public static void copy(final File sourceFile, final File destinationFile, final boolean deep, final ProgressListener progressListener) throws IOException {
		copy(sourceFile, destinationFile, deep, WILDCARD_FILE_FILTER, progressListener);
	}

	/**
	 * Copies the given source file or directory to the given destination file or directory. The last modified date of the destination file or directory is
	 * updated to match that of the source.
	 * @param sourceFile The source file or directory to copy.
	 * @param destinationFile The destination of the copied file or directory.
	 * @param deep <code>true</code> if child files and directories of source directories should be recursively copied.
	 * @param overwrite <code>true</code> if any existing file or directory at the destination should be overwritten.
	 * @throws NullPointerException if the given source file and/or destination file is <code>null</code>.
	 * @throws IllegalArgumentException if the given destination file is a child of the given source file, representing a circular copy.
	 * @throws FileNotFoundException if the given source file is <code>null</code>.
	 * @throws IllegalStateException if a directory copy is performed and the destination file's parent directory doesn't exist.
	 * @throws IllegalStateException if overwrite is turned off and a destination file exists.
	 */
	public static void copy(final File sourceFile, final File destinationFile, final boolean deep, final boolean overwrite) throws IOException {
		copy(sourceFile, destinationFile, deep, WILDCARD_FILE_FILTER, overwrite);
	}

	/**
	 * Copies the given source file or directory to the given destination file or directory. The last modified date of the destination file or directory is
	 * updated to match that of the source.
	 * @param sourceFile The source file or directory to copy.
	 * @param destinationFile The destination of the copied file or directory.
	 * @param deep <code>true</code> if child files and directories of source directories should be recursively copied.
	 * @param overwrite <code>true</code> if any existing file or directory at the destination should be overwritten, or <code>false</code> if an existing file or
	 *          directory at the destination should cause an exception to be thrown.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @throws NullPointerException if the given source file and/or destination file is <code>null</code>.
	 * @throws IllegalArgumentException if the given destination file is a child of the given source file, representing a circular copy.
	 * @throws FileNotFoundException if the given source file is <code>null</code>.
	 * @throws IllegalStateException if a directory copy is performed and the destination file's parent directory doesn't exist.
	 * @throws IllegalStateException if overwrite is turned off and a destination file exists.
	 */
	public static void copy(final File sourceFile, final File destinationFile, final boolean deep, final boolean overwrite,
			final ProgressListener progressListener) throws IOException {
		copy(sourceFile, destinationFile, deep, WILDCARD_FILE_FILTER, overwrite, progressListener);
	}

	/**
	 * Stores the contents of a file in an output stream.
	 * @param file The file to copy.
	 * @param outputStream The destination of the file contents.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @throws IOException Thrown if there is an error copying the file.
	 */
	public static void copy(final File file, final OutputStream outputStream, final ProgressListener progressListener) throws IOException {
		final InputStream fileInputStream = new BufferedInputStream(new FileInputStream(file)); //created a buffered input stream to the file
		try {
			IOStreams.copy(fileInputStream, outputStream, file.length(), progressListener); //copy the contents of the input stream to the output stream
		} finally {
			fileInputStream.close(); //always close the file input stream
		}
	}

	/**
	 * Copies the given source file or directory to the given destination file or directory. Destination files are overwritten. The last modified date of the
	 * destination file or directory is updated to match that of the source.
	 * @param sourceFile The source file or directory to copy.
	 * @param destinationFile The destination of the copied file or directory.
	 * @param deep <code>true</code> if child files and directories of source directories should be recursively copied.
	 * @param fileFilter The file filter for copying children files if applicable.
	 * @throws NullPointerException if the given source file and/or destination file is <code>null</code>, or if deep copying of a directory is requested and the
	 *           given file filter is <code>null</code>.
	 * @throws IllegalArgumentException if the given destination file is a child of the given source file, representing a circular copy.
	 * @throws FileNotFoundException if the given source file is <code>null</code>.
	 * @throws IllegalStateException if a directory copy is performed and the destination file's parent directory doesn't exist.
	 */
	public static void copy(final File sourceFile, final File destinationFile, final boolean deep, final FileFilter fileFilter) throws IOException {
		copy(sourceFile, destinationFile, deep, fileFilter, null);
	}

	/**
	 * Copies the given source file or directory to the given destination file or directory. Destination files are overwritten. The last modified date of the
	 * destination file or directory is updated to match that of the source.
	 * @param sourceFile The source file or directory to copy.
	 * @param destinationFile The destination of the copied file or directory.
	 * @param deep <code>true</code> if child files and directories of source directories should be recursively copied.
	 * @param fileFilter The file filter for copying children files if applicable.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @throws NullPointerException if the given source file and/or destination file is <code>null</code>, or if deep copying of a directory is requested and the
	 *           given file filter is <code>null</code>.
	 * @throws IllegalArgumentException if the given destination file is a child of the given source file, representing a circular copy.
	 * @throws FileNotFoundException if the given source file is <code>null</code>.
	 * @throws IllegalStateException if a directory copy is performed and the destination file's parent directory doesn't exist.
	 */
	public static void copy(final File sourceFile, final File destinationFile, final boolean deep, final FileFilter fileFilter,
			final ProgressListener progressListener) throws IOException {
		copy(sourceFile, destinationFile, deep, fileFilter, true, progressListener);
	}

	/**
	 * Copies the given source file or directory to the given destination file or directory. The last modified date of the destination file or directory is
	 * updated to match that of the source.
	 * @param sourceFile The source file or directory to copy.
	 * @param destinationFile The destination of the copied file or directory.
	 * @param deep <code>true</code> if child files and directories of source directories should be recursively copied.
	 * @param fileFilter The file filter for copying children files if applicable.
	 * @param overwrite <code>true</code> if any existing file or directory at the destination should be overwritten, or <code>false</code> if an existing file or
	 *          directory at the destination should cause an exception to be thrown.
	 * @throws NullPointerException if the given source file and/or destination file is <code>null</code>, or if deep copying of a directory is requested and the
	 *           given file filter is <code>null</code>.
	 * @throws IllegalArgumentException if the given destination file is a child of the given source file, representing a circular copy.
	 * @throws FileNotFoundException if the given source file is <code>null</code>.
	 * @throws IllegalStateException if a directory copy is performed and the destination file's parent directory doesn't exist.
	 * @throws IllegalStateException if overwrite is turned off and a destination file exists.
	 */
	public static void copy(final File sourceFile, final File destinationFile, final boolean deep, final FileFilter fileFilter, final boolean overwrite)
			throws IOException {
		copy(sourceFile, destinationFile, deep, fileFilter, overwrite, null);
	}

	/**
	 * Copies the given source file or directory to the given destination file or directory. The last modified date of the destination file or directory is
	 * updated to match that of the source.
	 * @param sourceFile The source file or directory to copy.
	 * @param destinationFile The destination of the copied file or directory.
	 * @param deep <code>true</code> if child files and directories of source directories should be recursively copied.
	 * @param fileFilter The file filter for copying children files if applicable.
	 * @param overwrite <code>true</code> if any existing file or directory at the destination should be overwritten, or <code>false</code> if an existing file or
	 *          directory at the destination should cause an exception to be thrown.
	 * @param progressListener A listener to be notified of progress, or <code>null</code> if no progress notifications is requested.
	 * @throws NullPointerException if the given source file and/or destination file is <code>null</code>, or if deep copying of a directory is requested and the
	 *           given file filter is <code>null</code>.
	 * @throws IllegalArgumentException if the given destination file is a child of the given source file, representing a circular copy.
	 * @throws FileNotFoundException if the given source file is <code>null</code>.
	 * @throws IllegalStateException if a directory copy is performed and the destination file's parent directory doesn't exist.
	 * @throws IllegalStateException if overwrite is turned off and a destination file exists.
	 */
	public static void copy(final File sourceFile, final File destinationFile, final boolean deep, final FileFilter fileFilter, final boolean overwrite,
			final ProgressListener progressListener) throws IOException {
		//TODO add beginning and ending progress events, along with a system of levels
		if(isChild(sourceFile, destinationFile)) {
			throw new IllegalArgumentException("Cannot perform circular copy from " + sourceFile + " to " + destinationFile);
		}
		if(sourceFile.isDirectory()) { //directory
			if(!overwrite) {
				if(destinationFile.isDirectory()) {
					throw new IllegalStateException("Copy destination directory " + destinationFile + " already exists.");
				}
			}
			final File destinationParentFile = destinationFile.getParentFile(); //make sure the destination parent file exists
			if(destinationParentFile != null) {
				if(!destinationParentFile.isDirectory()) {
					throw new IllegalStateException("Copy destination parent directory " + destinationParentFile + " does not exist.");
				}
			}
			mkdir(destinationFile); //create the destination file
			if(deep) { //if we are copying deeply
				for(final File childSourceFile : sourceFile.listFiles(fileFilter)) { //list all the files in the directory
					final String filename = childSourceFile.getName();
					copy(childSourceFile, new File(destinationFile, filename), deep, fileFilter, overwrite, progressListener); //recursively copy the child file or directory
				}
			}
		} else { //file
			checkFileExists(sourceFile);
			if(!overwrite) {
				if(destinationFile.exists()) {
					throw new IllegalStateException("Copy destination file " + destinationFile + " already exists.");
				}
			}
			final OutputStream outputStream = new FileOutputStream(destinationFile); //create an output stream to the destination file
			try {
				copy(sourceFile, outputStream, progressListener); //copy the file contents
			} finally {
				outputStream.close();
			}
		}
		destinationFile.setLastModified(sourceFile.lastModified()); //update the destination file's last modified time to match that of the source
	}

	//## Path

	/**
	 * Copies all the bytes of a bytes from a class resource to a file in a file system.
	 * @implSpec This method delegates to {@link #copyFromResource(ClassLoader, String, Path, CopyOption...)}.
	 * @param contextClass The class the class loader of which to use for retrieving the resource.
	 * @param resourcePath The path of the resource <em>relative to the context class</em>.
	 * @param targetFile The path to the destination to where the resource should be copied. Any target parent directories will be created as needed.
	 * @param options Options specifying how the copy should be performed.
	 * @return The number of bytes copied.
	 * @throws IOException if an I/O error occurs when reading or writing. The exception may be a subclass of {@link FileSystemException} as per
	 *           {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @throws UnsupportedOperationException if {@code options} contains a copy option that is not supported.
	 * @throws SecurityException If the security manager does not permit the operation.
	 */
	public static long copyFromResource(@Nonnull final Class<?> contextClass, @Nonnull final String resourcePath, @Nonnull final Path targetFile,
			final CopyOption... options) throws IOException {
		return copyFromResource(contextClass.getClassLoader(), resolveResourcePath(contextClass, resourcePath), targetFile, options);
	}

	/**
	 * Copies all the bytes of a bytes from a class resource to a file in a file system.
	 * @apiNote This method requires the full classpath-relative path to the resource, unlike {@link #copyFromResource(Class, String, Path, CopyOption...)}, which
	 *          requires a path relative to a class.
	 * @implSpec This method delegates to {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @param classLoader The class loader to use for retrieving the resource.
	 * @param resourcePath The full relative path of the resource in relation to the class loader.
	 * @param targetFile The path to the destination to where the resource should be copied. Any target parent directories will be created as needed.
	 * @param options Options specifying how the copy should be performed.
	 * @return The number of bytes copied.
	 * @throws IOException if an I/O error occurs when reading or writing. The exception may be a subclass of {@link FileSystemException} as per
	 *           {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @throws UnsupportedOperationException if {@code options} contains a copy option that is not supported.
	 * @throws SecurityException If the security manager does not permit the operation.
	 */
	public static long copyFromResource(@Nonnull final ClassLoader classLoader, @Nonnull final String resourcePath, @Nonnull final Path targetFile,
			final CopyOption... options) throws IOException {
		final URL resourceUrl = classLoader.getResource(resourcePath); //getResourceAsStream() does this same thing, and this allows us not to throw away an IOException
		if(resourceUrl == null) {
			throw new FileNotFoundException("Resource not found: " + resourcePath);
		}
		final Path targetParent = targetFile.getParent();
		if(targetParent != null) {
			createDirectories(targetParent);
		}
		try (final InputStream resourceInputStream = resourceUrl.openStream()) {
			return java.nio.file.Files.copy(resourceInputStream, targetFile, options);
		}
	}

	/**
	 * Recursively deletes an entire file tree. Symbolic links are deleted, not followed.
	 * @implSpec This implementation uses {@link java.nio.file.Files#walkFileTree(Path, FileVisitor)}.
	 * @implNote This implementation was inspired by <a href="https://fahdshariff.blogspot.com/2011/08/java-7-deleting-directory-by-walking.html">Java 7: Deleting
	 *           a Directory by Walking the File Tree</a>.
	 * @param fileTreeRoot The root of the file tree to delete.
	 * @return The given file tree root, which will have been deleted.
	 * @throws SecurityException if the security manager denies access to the starting file.
	 * @throws IOException if an I/O error is thrown while deleting the tree.
	 */
	public static Path deleteFileTree(@Nonnull final Path fileTreeRoot) throws IOException {
		return walkFileTree(fileTreeRoot, new SimpleFileVisitor<Path>() {

			/** @implSpec Deletes each file. */
			@Override
			public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
				final FileVisitResult result = super.visitFile(file, attrs);
				deleteIfExists(file); //if another thread already deleted the file, that's fine, too
				return result;
			}

			/** @implSpec Deletes each directory after all files in it have been deleted. */
			@Override
			public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
				final FileVisitResult result = super.postVisitDirectory(dir, exc);
				deleteIfExists(dir); //if another thread already deleted the directory, that's fine, too
				return result;
			}

		});
	}

	/**
	 * Searches up an entire path hierarchy to find a given file, with no additional filtering.
	 * @apiNote For the purposes of this method, the given directory is considered an ancestor.
	 * @implSpec This implementation delegates to {@link #findAncestorFileByName(Path, String, Predicate)}, accepting any discovered path.
	 * @param directory The directory in which to start the search; may or may not exist.
	 * @param filename The name of the file to locate.
	 * @return The path to the matching file, if present in the given directory or up the hierarchy.
	 * @throws IOException if an I/O error occurred during the search.
	 */
	public static Optional<Path> findAncestorFileByName(@Nonnull Path directory, @Nonnull final String filename) throws IOException {
		return findAncestorFileByName(directory, filename, path -> true);
	}

	/**
	 * Searches up an entire path hierarchy to find a given file.
	 * @apiNote For the purposes of this method, the given directory is considered an ancestor.
	 * @implSpec This implementation delegates to {@link #findAncestorFileByName(Path, String, Predicate, Path)}.
	 * @param directory The directory in which to start the search; may or may not exist.
	 * @param filename The name of the file to locate.
	 * @param filter A predicate to indicate whether the file should be accepted.
	 * @return The path to the matching file, if present in the given directory or up the hierarchy.
	 * @throws IOException if an I/O error occurred during the search.
	 */
	public static Optional<Path> findAncestorFileByName(@Nonnull Path directory, @Nonnull final String filename, @Nonnull final Predicate<Path> filter)
			throws IOException {
		return findAncestorFileByName(directory, filename, filter, null);
	}

	/**
	 * Searches up a path hierarchy to find a given file, with no additional filtering, stopping the search after some root directory.
	 * @apiNote For the purposes of this method, the given directory is considered an ancestor.
	 * @implSpec This implementation delegates to {@link #findAncestorFileByName(Path, String, Predicate, Path)}, accepting any discovered path.
	 * @param directory The directory in which to start the search; may or may not exist.
	 * @param filename The name of the file to locate.
	 * @param rootDirectory The highest directory to search.
	 * @return The path to the matching file, if present in the given directory or up the hierarchy.
	 * @throws IllegalArgumentException if a root directory was given and the given root directory was not encountered up the hierarchy; that is, the given
	 *           directory was not in the given root directory.
	 * @throws IOException if an I/O error occurred during the search.
	 */
	public static Optional<Path> findAncestorFileByName(@Nonnull Path directory, @Nonnull final String filename, @Nullable final Path rootDirectory)
			throws IOException {
		return findAncestorFileByName(directory, filename, path -> true, rootDirectory);
	}

	/**
	 * Searches up a path hierarchy to find a given file, stopping the search after some root directory.
	 * @apiNote For the purposes of this method, the given directory is considered an ancestor.
	 * @implSpec This implementation delegates to {@link #findAncestorFileByName(Path, Iterable, Predicate, Path)}.
	 * @param directory The directory in which to start the search; may or may not exist.
	 * @param filename The name of the file to locate.
	 * @param filter A predicate to indicate whether the file should be accepted.
	 * @param rootDirectory The highest directory to search.
	 * @return The path to the matching file, if present in the given directory or up the hierarchy.
	 * @throws IllegalArgumentException if a root directory was given and the given root directory was not encountered up the hierarchy; that is, the given
	 *           directory was not in the given root directory.
	 * @throws IOException if an I/O error occurred during the search.
	 */
	public static Optional<Path> findAncestorFileByName(@Nonnull Path directory, @Nonnull final String filename, @Nonnull final Predicate<Path> filter,
			@Nullable final Path rootDirectory) throws IOException {
		return findAncestorFileByName(directory, singleton(filename), filter, rootDirectory);
	}

	/**
	 * Searches up a path hierarchy to find one of several possible files, stopping the search after some root directory.
	 * @apiNote For the purposes of this method, the given directory is considered an ancestor.
	 * @param directory The directory in which to start the search; may or may not exist.
	 * @param filenames The names of the file to locate. The iteration order is maintained when searching.
	 * @param filter A predicate to indicate whether the file should be accepted.
	 * @param rootDirectory The highest directory to search.
	 * @return The path to the matching file, if present in the given directory or up the hierarchy.
	 * @throws IllegalArgumentException if a root directory was given and the given root directory was not encountered up the hierarchy; that is, the given
	 *           directory was not in the given root directory.
	 * @throws IOException if an I/O error occurred during the search.
	 */
	public static Optional<Path> findAncestorFileByName(@Nonnull Path directory, @Nonnull final Iterable<String> filenames, @Nonnull final Predicate<Path> filter,
			@Nullable final Path rootDirectory) throws IOException {
		for(final String filename : filenames) {
			final Path file = directory.resolve(requireNonNull(filename));
			if(exists(file) && filter.test(file)) {
				return Optional.of(file);
			}
		}
		if(directory.equals(rootDirectory)) { //if we have reached the root directory (if specified), don't go any higher
			return Optional.empty();
		}
		final Path parentDirectory = directory.getParent();
		if(parentDirectory == null) { //if we ran out of parent directories
			checkArgument(rootDirectory == null, "Directory `%s` was not in given root directory `%s`.", directory, rootDirectory);
			return Optional.empty(); //if no root directory was specified, that's OK; we just didn't find the file
		}
		return findAncestorFileByName(parentDirectory, filenames, filter, rootDirectory);
	}

	//### Backup

	/**
	 * Determines the backup file path to use for the file at the given path without a rolling policy. The backup file will be in the same directory as the given
	 * path; and the filename will be in the form <code>filename.ext.bak</code>, i.e. with a <code>bak</code> extension added.
	 * @implSpec This is a convenience method that delegates to {@link #getBackupPath(Path, long)} with the value <code>1</code> for <var>maxBackupCount</var>.
	 * @param path The path of the file to back up.
	 * @return The path to the backup file to use.
	 */
	public static Path getBackupPath(@Nonnull final Path path) { //TODO move to Paths?
		return getBackupPath(path, 1);
	}

	/**
	 * Determines the rolling, numbered backup file path to use for the file at the given path. The backup file will be in the same directory as the given path;
	 * and the filename is determined in the following manner:
	 * <ol>
	 * <li>If <var><code>maxBackupCount</code></var> is <code>1</code>, the backup filename will be in the form <code>filename.ext.bak</code>, i.e. with a
	 * <code>bak</code> extension added.</li>
	 * <li>If <var><code>maxBackupCount</code></var> is greater than <code>1</code>, the backup filename will be in the form <code>filename.ext.1.bak</code>, i.e.
	 * with a <code>1.bak</code> extension added.</li>
	 * </ol>
	 * 
	 * @param path The path of the file to back up.
	 * @param maxBackupCount The maximum number of rolling backup files to use.
	 * 
	 * @return The path to the backup file to use.
	 * @throws IllegalArgumentException if <var><code>maxBackupCount</code></var> is zero or negative.
	 */
	public static Path getBackupPath(@Nonnull final Path path, @Nonnegative final long maxBackupCount) { //TODO move to Paths?
		checkArgumentNotNull(path, "The path to the file cannot be null.");
		checkArgument(java.nio.file.Files.exists(path) && !java.nio.file.Files.isDirectory(path),
				"The provided path is referring to a directory or doesn't exist.");
		checkArgument(maxBackupCount > 0, "The maximum number of rolling backup files to be used must be greater than zero.");

		Path backupFile = null;

		if(maxBackupCount == 1) {
			backupFile = addFilenameExtension(path, BACKUP_EXTENSION); //if the backups are not numbered, we return the simple backup path.
		} else {
			backupFile = addFilenameExtension(path, LATEST_NUMBERED_BACKUP_EXTENSION); //if the backup file is numbered, we return the path for the latest one.
		}

		return backupFile;
	}

	/**
	 * Backs up a given file without a rolling policy. If the backup filename does not exist, the indicated file will simply be copied to the backup file
	 * destination. If the backup file destination exists, it will be overwritten.
	 * @implSpec This is a convenience method that delegates to {@link #backupFile(Path, long)} with the value <code>1</code> for <var>maxBackupCount</var>.
	 * @param path The path of the file to back up.
	 * @return The path to the backup file used.
	 * @throws IllegalArgumentException if <var><code>maxBackupCount</code></var> is zero or negative.
	 * @throws IOException If an error occurs while rolling the backup files.
	 * 
	 * @see #backupFile(Path, long)
	 */
	public static Path backupFile(@Nonnull final Path path) throws IOException {
		return backupFile(path, 1);
	}

	/**
	 * Backs up a given file using a rolling, numbered backup file determined by {@link #getBackupPath(Path, long)}. If the backup filename does not exist, the
	 * indicated file will simply be copied to the backup file destination. If <var><code>maxBackupCount</code></var> is <code>1</code> and the backup file
	 * destination exists, it will be overwritten. If <var><code>maxBackupCount</code></var> is greater than <code>1</code> and the backup file destination
	 * exists, <code>filename.ext.<var>maxBackupCount</var>.bak</code> will be deleted (if it exists) and each backup file
	 * <code>filename.ext.<var>number</var>.bak</code> in the sequence will be renamed to <code>filename.ext.<var>number+1</var>.bak</code> (if it exists) down to
	 * and including <var>1</var>, and then the indicated file will be copied to the backup filename, which is <code>filename.ext.<var>1</var>.bak</code>.
	 * 
	 * <p>
	 * If it's a rolling backup and one of the backup files are manually erased, the blank space that will be left between them will be rolled aswell, until the
	 * blank space take the place of number <var><code>maxBackupCount</code></var> and then disappears.
	 * </p>
	 * 
	 * @param path The path of the file to back up.
	 * @param maxBackupCount The maximum number of rolling backup files to use.
	 * 
	 * @return The path to the backup file used.
	 * @throws IllegalArgumentException if <var><code>maxBackupCount</code></var> is zero or negative.
	 * @throws IOException If an error occurs while rolling the backup files.
	 */
	public static Path backupFile(@Nonnull final Path path, @Nonnegative final long maxBackupCount) throws IOException {
		checkArgumentNotNull(path, "The path to the file cannot be null.");
		checkArgument(java.nio.file.Files.exists(path) && !java.nio.file.Files.isDirectory(path),
				"The provided path is referring to a directory or doesn't exist.");
		checkArgument(maxBackupCount > 0, "The maximum number of rolling backup files to be used must be greater than zero.");

		if(maxBackupCount > 1) {
			rollBackupFiles(path, maxBackupCount); //if there's an existent backup for the given file we need to apply the rolling policy.
		}

		return java.nio.file.Files.copy(path, getBackupPath(path, maxBackupCount), StandardCopyOption.REPLACE_EXISTING);
	}

	/**
	 * Rolls the backup files, adding one to the index of each backup file, in the end, there will be a free space on the first index of the backup files, and if
	 * the number of backup files is <var>maxBackupCount</var> the oldest one (the one with the end <code>.<var>maxBackupCount</var>.bak</code>) will be
	 * discarded.
	 * 
	 * @param path The path of the file to roll the backup files.
	 * @param maxBackupCount The maximum number of rolling backup files to use.
	 * 
	 * @throws IOException If an error occurs while rolling the backup files.
	 */
	private static void rollBackupFiles(@Nonnull final Path path, @Nonnegative final long maxBackupCount) throws IOException {
		checkArgumentNotNull(path, "The path to the file cannot be null.");
		checkArgument(java.nio.file.Files.exists(path) && !java.nio.file.Files.isDirectory(path),
				"The provided path is referring to a directory or doesn't exist.");
		checkArgument(maxBackupCount > 1, "The maximum number of rolling backup files to be used must be greater than one.");

		for(long i = maxBackupCount - 1; i >= 1; i--) {
			final Path sourceBackupFile = addFilenameExtension(path, NUMBERED_BACKUP_EXTENSION_TEMPLATE.apply(i));

			if(java.nio.file.Files.exists(sourceBackupFile)) {
				final Path destinationBackupFile = addFilenameExtension(path, NUMBERED_BACKUP_EXTENSION_TEMPLATE.apply(i + 1));

				java.nio.file.Files.move(sourceBackupFile, destinationBackupFile, StandardCopyOption.REPLACE_EXISTING);
			}
		}

	}

	/**
	 * Opens or creates a file after first creating a backup without a rolling policy of the file if it exists, returning an output stream that may be used to
	 * write bytes to the file. The maximum number of backups used on this method will be 1.
	 * @implSpec This is a convenience method that delegates to {@link #newOutputStreamWithBackup(Path, long, OpenOption...)} with the value <code>1</code> for
	 *           <var>maxBackupCount</var>.
	 * @param path The path of the file to back up.
	 * @param options The options specifying how the file is opened.
	 * @return The new {@link OutputStream} after the creation of a backup for the given file.
	 * @throws IllegalArgumentException if <var><code>maxBackupCount</code></var> is negative.
	 * @throws IOException If an error occurs while creating the backup file.
	 * 
	 * @see #newOutputStreamWithBackup(Path, long, OpenOption...)
	 */
	public static OutputStream newOutputStreamWithBackup(@Nonnull final Path path, final OpenOption... options) throws IOException {
		return newOutputStreamWithBackup(path, 1, options);
	}

	/**
	 * Opens or creates a file after first creating a backup of the file if it exists, returning an output stream that may be used to write bytes to the file. If
	 * <var><code>maxBackupCount</code></var> is greater than zero, a backup will first be created using {@link #backupFile(Path, long)}.
	 * 
	 * @param path The path of the file to back up.
	 * @param maxBackupCount The maximum number of rolling backup files to use.
	 * @param options The options specifying how the file is opened.
	 * 
	 * @return The new {@link OutputStream} after the creation of a backup for the given file.
	 * @throws IllegalArgumentException if <var><code>maxBackupCount</code></var> is negative.
	 * @throws IOException If an error occurs while creating the backup file.
	 */
	public static OutputStream newOutputStreamWithBackup(@Nonnull final Path path, @Nonnegative final long maxBackupCount, final OpenOption... options)
			throws IOException {
		backupFile(path, maxBackupCount);
		return java.nio.file.Files.newOutputStream(path, options);
	}

}
