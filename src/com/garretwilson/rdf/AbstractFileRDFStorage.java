package com.garretwilson.rdf;

import java.io.*;
import java.net.*;
import com.garretwilson.io.*;
import org.w3c.dom.*;

/**A modifiable object that knows how to store and retrieve itself as RDF in a
	file.
<p>This class creates needed directories before storing the information.</p>
<p>All writing is done to a temporary file and then copied to the storage file
	to mitigate data corruption and loss. Optionally, a backup file can be
	automatically created and used to recover from corrupted files.</p>  
<p>Bound properties:</p>
<ul>
	<li><code>Modifiable.MODIFIED_PROPERTY</code> ("modified")</li>
</ul>
@author Garret Wilson
@see Modifiable#MODIFIED_PROPERTY
*/
public abstract class AbstractFileRDFStorage extends AbstractRDFStorage 
{

	/**Whether backup files should be used.*/
	private final boolean backupUsed;
	
		/**@return Whether backup files should be used.*/
		public boolean isBackupUsed() {return backupUsed;}	

	/**Storage file location constructor that does not use backups.
	@param storageFile The file where the RDF should be stored.

	*/
	public AbstractFileRDFStorage(final File storageFile)
	{
		this(storageFile, false);	//default to not using backups
	}

	/**Storage file location and backup constructor.
	@param storageFile The file where the RDF should be stored.
	@param useBackup <code>true</code> if backup files should be used to mitigate
		and recover from data corruption.
	*/
	public AbstractFileRDFStorage(final File storageFile, final boolean useBackup)
	{
		super(storageFile.toURI());	//save the URI of the file
		backupUsed=useBackup;	//save whether we should use backup files
	}

	/**Returns an input stream for the given URI.
	The calling class has the responsibility for closing the input stream.
	@param uri A URI to a resource.
	@return An input stream to the contents of the resource represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public InputStream getInputStream(final URI uri) throws IOException
	{
		return new FileInputStream(new File(uri));	//create and return an input stream to the file
	}

	/**Returns an output stream for the given URI.
	The calling class has the responsibility for closing the output stream.
	@param uri A URI to a resource.
	@return An output stream to the contents of the resource represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public OutputStream getOutputStream(final URI uri) throws IOException
	{
		return new FileOutputStream(new File(uri));	//create and return an output stream to the file
	}
	
	/**Checks to see if the storage file exists. If the file does not exist, yet
		a backup file exists, the backup file will be moved to the original file
		location. If this method returns true, there will be a file located at
		<code>file</code>.
	<p>If backup file use is not enabled, this version simply checks to see if
		the storage file exists.</p>
	@return <code>true</code> if the storage file existed or exists now after
		moving the backup file, else <code>false</code> if neither file exists.
	@exception IOException Thrown if the backup file cannot be moved.
	@see #isBackupUsed
	*/
	public boolean exists() throws IOException
	{
		final File storageFile=new File(getStorageURI());	//get the file to represent the URI
		if(isBackupUsed())	//if backup files are used
		{
			final File backupFile=Files.getBackupFile(storageFile);  //get the expected backup file
			return Files.checkExists(storageFile, backupFile);	//check to see if the file exists; if not, try to use the backup file instead
		}
		else	//if backup file use is not enabled
		{
			return storageFile.exists();	//just return whether the file exists, without checking for a backup file if it doesn't
		}
	}
	
	/**Stores the RDF information at the storage URI.
	A temporary file and optionally a backup file is used to mitigate data loss.
	<p>Classes may override this method and provide last-minute modifications of
		the XML document tree before it is serialized.</p>
	@param document The XML document that contains the RDF data to store
	@exception IOException Thrown if there is a problem storing the information.
	@see #getStorageURI()
	@see #isBackupUsed
	*/
	protected void store(final Document document) throws IOException
	{
		final File storageFile=new File(getStorageURI());	//get the file to use for storage
		final File directory=storageFile.getParentFile();	//get the directory of the file
		if(!directory.exists() || !directory.isDirectory())	//if the directory doesn't exist as a directory
		{
			Files.mkdirs(directory);	//create the directory
		}
		final File tempFile=Files.getTempFile(storageFile);  //get a temporary file to write to
		final File backupFile=isBackupUsed() ? Files.getBackupFile(storageFile) : null;  //get a backup file, if we should create a backup, or null if we shouldn't
		store(document, tempFile.toURI());	//store the document in the temporary file
		Files.moveFile(tempFile, storageFile, backupFile); //move the temp file to the normal file, creating a backup if necessary
	}

	/**Retrieves the information from RDF stored at the given URI.
	This version attempts to locate a backup copy if the requested file does not
	exist.
	@param uri The URI at which the information is be stored
	@exception IOException Thrown if there is a problem retrieving the information.
	@see #isBackupUsed
	*/
	public void retrieve(final URI uri) throws IOException
	{
		if(isBackupUsed())	//if backup files are used
		{
			final File storageFile=new File(uri);	//get the file to represent the URI
			final File backupFile=Files.getBackupFile(storageFile);  //get the expected backup file G***can't we just remove this parameter and use the default?
			Files.checkExists(storageFile, backupFile);	//check to see if the file exists; if not, try to use the backup file instead
		}
		super.retrieve(uri);	//attempt to retrieve the file normally		
	}	
}