package com.garretwilson.util.zip;

import java.io.*;
import java.net.*;
import java.net.URI;
import java.util.*;
import java.util.zip.*;
import com.garretwilson.io.*;
import com.garretwilson.net.*;
import com.garretwilson.util.Debug;

/**Facilitates access to a zip file.
	Keeps track of zip contents by zip entry name as a URI as if each zip entry
	were unarchived in the current location of the zip file.
	<p>This class cannot descend from <code>ZipFile</code> because a
	<code>ZipFile</code> cannot be arbitrarily opened and closed.</p>
@author Garret Wilson
@see ZipFile
*/
public class ZipManager implements URIInputStreamable
{

	/**The file containing the zipped information.*/
	private final File file;

		/**@return The file containing the zipped information.*/
		public File getFile() {return file;}

	/**The base URI to use in determining zip entry URIs.*/
	private final URI baseURI;

		/**@return The base URI to use in determining zip entry URIs.*/
		public URI getBaseURI() {return baseURI;}

	/**A map for storing zip entries, each keyed to a URI of what their filename
		would be if uncompressed.*/
	private final Map<URI, ZipEntry> zipEntryMap=new HashMap();

		/**@return A map for storing zip entries, each keyed to a URI of what their
			filename would be if uncompressed.*/
		protected Map<URI, ZipEntry> getZipEntryMap() {return zipEntryMap;}

		/**Gets the zip entry that corresponds to the given URI.
		@param uri The URI for which a URI should be returned.
		@return The zip entry corresponding to the given URI, or <code>null</code>
			if there is no corresponding zip entry.
		*/
		public ZipEntry getZipEntry(final URI uri)
		{
			return zipEntryMap.get(uri);  //get the zip entry keyed to the given URI
		}

		/**@return An iterator to all zip entries in the zip file in an undefined order.
		*/
		public Iterator<ZipEntry> getZipEntryIterator() {return zipEntryMap.values().iterator();}

	/**The number of callers that have grabbed the zip file.*/
	private int holdCount=0;

	/**The zip file that allows access to the zipped information. This has a
		non-<code>null</code> when the zip file is held (the reference count is one
		or more.
	*/
	private ZipFile zipFile=null;

		/**Opens the zip file if needed and increases the reference count to
		  indicate the zip file should be held open. Every call to this method
			should eventually be followed by a single call to
			<code>releaseZipFile</code>.
			<p>This method is thread-safe.</p>
		@return ZipFile An object allowing access to the zip file's contents.
		@exception IOException Thrown if there is an error opening the zip file.
		@see #releaseZipFile
		*/
		public synchronized ZipFile grabZipFile() throws IOException
		{
			if(zipFile==null) //if there is no zip file
			{
			  zipFile=new ZipFile(getFile()); //create a new zip file object from the file holding the zipped information
			}
			++holdCount;  //show that one more caller is holding the zip file
			return zipFile; //return the open zip file that allows access to the zip contents
		}

		/**Releases the hold on the zip file. When no callers hold the zip file
		  (the hold count reaches zero), the zip file is closed and the zip file
			variable is set to <code>null</code>.
			<p>This method is thread-safe.</p>
		@exception IOException Thrown if there is an error closing the zip file.
		*/
		public synchronized void releaseZipFile() throws IOException
		{
			--holdCount;  //decrease the hold count
			if(holdCount==0)  //if there are no more holds on the zip file
			{
				zipFile.close();  //close the zip file
				zipFile=null; //release our reference to the zip file
			}
		}

	/**Constructs a zip manager to manage entries in a zip file stored in the
		specified file.
	@param zippedFile The file in which the zipped information is stored.
	@param baseURI The base URI to use in determining zip entry URIs.
	@exception IOException Thrown if there is an error accessing the given zip
		file.
	*/
	public ZipManager(final File zippedFile, final URI baseURI) throws IOException
	{
		file=zippedFile;  //store the file we're using
		this.baseURI=baseURI; //store the URI to use when calculating zip entry URIs
		loadZipEntries(); //load the zip entries
	}

	/**Loads and maps the entries of the zip file.
		For each zip entry, a URI is created for the canonical filename of the
		entry as if it were extracted with its current path under the directory
		of the zip file.
	@exception IOException Thrown if there is an error opening and reading from
		the zip file.
	*/
	protected void loadZipEntries() throws IOException
	{
		final Map<URI, ZipEntry> zipEntryMap=getZipEntryMap(); //get the map of zip entries
		zipEntryMap.clear();  //remove all entries from the zip entry map
		final ZipFile zipFile=new ZipFile(getFile()); //open the zip file for reading
		try
		{
			final Enumeration<? extends ZipEntry> zipEntryEnumeration=zipFile.entries();	//get an enumeration of all the entries in the zip file
			while(zipEntryEnumeration.hasMoreElements())	//while there is another zip entry left
			{
				final ZipEntry zipEntry=zipEntryEnumeration.nextElement();	//get a reference to this zip entry
				try
				{
Debug.trace("storing zip entry: ", zipEntry.getName()); //G***del
Debug.trace("storing zip entry under: ", getURI(zipEntry)); //G***del
					zipEntryMap.put(getURI(zipEntry), zipEntry);	//store this zip entry in the map, keyed to its URI
/*G***fix in the bigger picture
				if(packageHRef==null && zipEntryName.endsWith(".opf")) 	//if this zip file claims to be a package file and we haven't found a package file
					packageHRef=zipEntryName;	//show that this is the first package file we found, so we'll take it to be the package file for the zip file
*/
				}
				catch(final IllegalArgumentException illegalArgumentException)	//if one of the zip entries has an invalid filename
				{
					final IOException ioException=new IOException(illegalArgumentException.getMessage());	//create an I/O exception from the error
					ioException.initCause(ioException);	//show the cause of the syntax exception
					throw ioException;	//throw the I/O exception representing the syntax exceptoin
				}
			}
		}
		finally
		{
			zipFile.close();  //always close the zip file
		}
	}

	public void close() throws IOException //G***fix; comment
	{
		holdCount=0;  //G**testing; fix for rapid closing
		if(zipFile!=null) //if we have a zip file
		{
			zipFile.close();  //close the zip file
			zipFile=null; //release our reference to the zip file
		}
	}

	/**Opens the publication. If the publication is stored in a zip file, this
		involves opening the zip file. For other types of storage, no action may
		be taken.
	@exception IOException Thrown if there is an error opening the publication.
	*/
/*G***del
	public void open() throws IOException
	{
		if(!isOpen())	//if we're not already open
		{
			open=true;	//show that the publication is open
			if(getStorageType()==ZIP_STORAGE)	//if files are stored in a zip file
			{
				final ZipFile zipFile=new ZipFile(getPublicationURL().getFile());	//create a new zip file from which to read the files
					//G***maybe check here for a problem opening the zip file
				setZipFile(zipFile);	//make a note of which zip file we're using so it can be accessed later (e.g. during processing of an XML file)
			}
		}
	}
*/
	/**Creates a URI from the file location, based on the zip file's URI.
	@param href The location, either a URI or a filename, of the file.
	@return A URI representing the specified file.
	@exception IllegalArgumentException if the given string violates RFC&nbsp;2396.
	@see #getZipFileURI
	@see URIUtilities
	*/
	public URI getURI(final String href)
	{
		return getBaseURI().resolve(href);	//resolve the file location against the base URI to yield a full URI
	}

	/**Creates a URI to represent the given zip entry, based upon the zip entry's
		path relative to the URI of the zip file itself.
	@param zipEntry The zip entry for which a URI should be returned.
	@return A URI representing the specified zip entry.
	@exception IllegalArgumentException if the the zip entry does not have
		a valid filename (a URI cannot be constructed from the filename).
	*/
	public URI getURI(final ZipEntry zipEntry)
	{
		return getURI(zipEntry.getName()); //create a URI from the zip entry name relative to the URI of the zip file itself
	}

	/**Returns an input stream from given URI.
		If the URI can be matched with a zip file entry, an input stream to that
		entry will be returned.
		The input stream will be left open and should be closed after use.
	@param uri A complete URI to a file.
	@return An input stream to the contents of the file represented by the given URI.
	@exception FileNotFoundException Thrown if the file referenced by the URI
		could not be located.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public InputStream getInputStream(final URI uri) throws FileNotFoundException, IOException
	{
		final ZipFile zipFile=grabZipFile();  //grab the zip file, which may include opening the zip file
		try
		{
			final ZipEntry zipEntry=(ZipEntry)getZipEntryMap().get(uri);	//get the zip entry represented by this URI
		  if(zipEntry!=null)  //if the file is in the zip file
			{
Debug.trace("Found zip entry: ", zipEntry.getName());	//G***del
//G***del				Debug.assert(getZipFile()!=null, "OEBPublication zip file unexpectedly closed.");
				//G***check to make sure the file is really in the zip file, and throw a FileNotFoundException if not
				return zipFile.getInputStream(zipEntry);	//get an input stream to the zip entry represented by this URI
			}
/*G***fix in the big OEB publication picture
			else if(URLConstants.HTTP_PROTOCOL.equals(url.getProtocol()))  //if the URL is not in the zip file, but it uses the HTTP protocol G***this is here so that the XML parse can get external DTDs and such from the web, but it would probably be better to place it in a higher-level method
		  {
				return url.openConnection().getInputStream();	//try to open a connection to the URL and return an input stream to that connection
		  }
*/
			else  //if we can't find the file
			{
				throw new FileNotFoundException(uri+" cannot be found in zip file "+getFile()); //throw an exception G***i18n
			}
		}
		finally
		{
;//G***fix this; maybe we can have the input stream release the zip file when it is closed			releaseZipFile(); //always release the zip file so that it can be closed if needed
		}
	}

}