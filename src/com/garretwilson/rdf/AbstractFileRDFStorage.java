package com.garretwilson.rdf;

import java.io.*;
import java.net.*;
import com.garretwilson.io.*;
import com.garretwilson.rdf.*;
import org.w3c.dom.*;

/**A modifiable object that knows how to store and retrieve itself as RDF in a
	file.
<p>This class creates needed directories before storing the information.</p>
<p>All writing is done to a temporary file and then copied to the storage file
	to mitigate data corruption and loss. Optionally, a backup file can be
	automatically created and used to recover from corrupted files.</p>  
<p>Bound properties:</p>
<ul>
	<li><code>Modifiable.MODIFIED_PROPERTY_NAME</code> ("modified")</li>
</ul>
@author Garret Wilson
@see Modifiable#MODIFIED_PROPERTY_NAME
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
			FileUtilities.mkdirs(directory);	//create the directory
		}
		final File tempFile=FileUtilities.getTempFile(storageFile);  //get a temporary file to write to
		final File backupFile=isBackupUsed() ? FileUtilities.getBackupFile(storageFile) : null;  //get a backup file, if we should create a backup, or null if we shouldn't
		store(document, tempFile.toURI());	//store the document in the temporary file
		FileUtilities.moveFile(tempFile, storageFile, backupFile); //move the temp file to the normal file, creating a backup if necessary
	}
	
/*G***fix retrieve
 		if(useBackup) //if we should use a backup file if the file doesn't exist
			FileUtilities.checkExists(file);  //see if we can use a backup file if the file doesn't exist
		final InputStream inputStream=new BufferedInputStream(new FileInputStream(file));  //create a buffered input stream for the file
		try
		{
		  return retrieve(inputStream, file, type); //read and return the object, passing the file as the source of the information
		}
		finally
		{
		  inputStream.close(); //always close the input stream
		}
	}
*/
	
}