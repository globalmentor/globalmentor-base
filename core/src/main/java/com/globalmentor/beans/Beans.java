/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.beans;

import java.beans.*;
import java.io.*;

/**
 * Utilities to manipulate and process JavaBeans.
 * @author Garret Wilson
 * @deprecated to be removed in favor of Ploop. This class seems to be more about persistence than beans in general.
 */
@Deprecated
public class Beans {

	/** Default constructor. */
	public Beans() {
	}

	/**
	 * Upgrades an encoder to support encoding of other non-JavaBean classes. New classes supported are:
	 * <ul>
	 * <li><code>File</code> - Constructed by <code>File.getAbsolutePath</code></li>
	 * </ul>
	 * @param encoder The encoder to be upgraded.
	 */
	public static void upgradeEncoder(final Encoder encoder) {
		//File
		encoder.setPersistenceDelegate(File.class, new DefaultPersistenceDelegate(new String[] {"absolutePath"}));
	}

	/**
	 * Constructs an <code>XMLEncoder</code> and upgrades it to support other non-JavaBean classes.
	 * @param out The stream to which the XML representation of the objects will be sent.
	 * @return A new <code>XMLEncoder</code> which supports encoding of other other non-JavaBean classes.
	 * @see XMLEncoder
	 * @see #upgradeEncoder(Encoder)
	 */
	public static XMLEncoder createUpgradedXMLEncoder(OutputStream out) {
		final XMLEncoder xmlEncoder = new XMLEncoder(out); //create a new XML encoder
		upgradeEncoder(xmlEncoder); //upgrade the encoder
		return xmlEncoder; //return the new upgraded encoder
	}

	/**
	 * Writes the given JavaBean to the file using long-term XML-encoded persistence.
	 * @param object The object to store.
	 * @param file The file in which the object should be stored.
	 * @throws FileNotFoundException Thrown if the specified file is invalid.
	 */
	public static void xmlEncode(final Object object, final File file) throws FileNotFoundException {
		xmlEncode(object, file, false); //store the object without creating a backup
	}

	/**
	 * Writes the given JavaBean to the file using long-term XML-encoded persistence. If a backup is created, its filename is formed by adding a ".backup"
	 * extension to the filename.
	 * @param object The object to store.
	 * @param file The file in which the object should be stored.
	 * @param createBackup Whether existing files should be saved in a backup file.
	 * @throws FileNotFoundException Thrown if the specified file is invalid.
	 */
	public static void xmlEncode(final Object object, final File file, final boolean createBackup) throws FileNotFoundException {
		//TODO it would be better to write to a temporary file and only copy if the write was successful
		if(createBackup && file.exists()) { //if we should make a backup, and the file exists
			final File backupFile = new File(file.toString() + ".backup"); //create a file with the same name with a ".backup" appended TODO use a constant here
			if(backupFile.exists()) //if the backup file exists
				backupFile.delete(); //delete the backup file
			file.renameTo(backupFile); //rename the file to the backup file TODO should we copy instead here?
		}
		//create an output stream to the file, make it buffered, and create an XML encoder to write to the stream
		try (final XMLEncoder xmlEncoder = createUpgradedXMLEncoder(new BufferedOutputStream(new FileOutputStream(file)))) {
			xmlEncoder.writeObject(object); //write the object to the file
		}
	}

	/**
	 * Reads the given JavaBean to the file using long-term XML-encoded persistence.
	 * @param file The file in which the object is stored.
	 * @throws FileNotFoundException Thrown if the specified file does not exist.
	 * @return The object retrieved by the given file.
	 */
	public static Object xmlDecode(final File file) throws FileNotFoundException {
		//create a buffered input stream for the file, and construct an XML decoder that uses it
		try (final XMLDecoder xmlDecoder = new XMLDecoder(new BufferedInputStream(new FileInputStream(file)))) {
			return xmlDecoder.readObject(); //read and return the object
		}
	}

}
