package com.garretwilson.beans;

//G***bring back when Java 1.4 beans supports long term persistence import java.beans.*;
import java.io.*;

/**Utilities to manipulate and process JavaBeans.
@author Garret Wilson
*/
public class BeanUtilities
{

	/**Default constructor.*/
	public BeanUtilities() {}

	/**Upgrades an encoder to support encoding of other non-JavaBean classes.
		New classes supported are:
		<ul>
		  <li><code>File</code> - Constructed by <code>File.getAbsolutePath</code></li>
		</ul>
	*/
/*G***fix
	public static void upgradeEncoder(final Encoder encoder)
	{
		//File
		encoder.setPersistenceDelegate(File.class, new DefaultPersistenceDelegate(new String[]{"absolutePath"}));
	}
*/

	/**Constructs an <code>XMLEncoder</code> and upgrades it to support other
		non-JavaBean classes.
	@param out The stream to which the XML representation of the objects will be sent.
	@return A new <code>XMLEncoder</code> which supports encoding of other other
		non-JavaBean classes.
	@see XMLEncoder
	@see #upgradeEncoder
	*/
/*G***fix
	public static XMLEncoder createUpgradedXMLEncoder(OutputStream out)
	{
		final XMLEncoder xmlEncoder=new XMLEncoder(out);  //create a new XML encoder
		upgradeEncoder(xmlEncoder); //upgrade the encoder
		return xmlEncoder;  //return the new upgraded encoder
	}
*/

	/**Writes the given JavaBean to the file using long-term XML-encoded persistence.
	@param object The object to store.
	@param file The file in which the object should be stored.
	@exception FileNotFoundException Thrown if the specified file is invalid.
	*/
/*G***fix
	public static void xmlEncode(final Object object, final File file) throws FileNotFoundException
	{
		xmlEncode(object, file, false); //store the object without creating a backup
	}
*/

	/**Writes the given JavaBean to the file using long-term XML-encoded persistence.
		If a backup is created, its filename is formed by adding a ".backup"
		extension to the filename.
	@param object The object to store.
	@param file The file in which the object should be stored.
	@param createBackup Whether existing files should be saved in a backup file.
	@exception FileNotFoundException Thrown if the specified file is invalid.
	*/
/*G***fix
	public static void xmlEncode(final Object object, final File file, final boolean createBackup) throws FileNotFoundException
	{
//G***it would be better to write to a temporary file and only copy if the write was successful
		if(createBackup && file.exists())	//if we should make a backup, and the file exists
		{
			final File backupFile=new File(file.toString()+".backup");	//create a file with the same name with a ".backup" appended G***use a constant here
			if(backupFile.exists())	//if the backup file exists
				backupFile.delete();		//delete the backup file
			file.renameTo(backupFile);	//rename the file to the backup file G***should we copy instead here?
		}
			//create an output stream to the file, make it buffered, and create an XML encoder to write to the stream
		final XMLEncoder xmlEncoder=createUpgradedXMLEncoder(new BufferedOutputStream(new FileOutputStream(file)));
		try
		{
			xmlEncoder.writeObject(object);  //write the object to the file
		}
		finally
		{
			xmlEncoder.close(); //always close the encoder
		}
	}
*/

	/**Reads the given JavaBean to the file using long-term XML-encoded persistence.
	@param file The file in which the object is stored.
	@exception FileNotFoundException Thrown if the specified file does not exist.
	*/
/*G***fix
	public static Object xmlDecode(final File file) throws FileNotFoundException
	{
		//create a buffered input stream for the file, and construct an XML decoder that uses it
		final XMLDecoder xmlDecoder=new XMLDecoder(new BufferedInputStream(new FileInputStream(file)));
		try
		{
		  return xmlDecoder.readObject(); //read and return the object
		}
		finally
		{
		  xmlDecoder.close(); //always close the XML decoder
		}
	}
*/

}