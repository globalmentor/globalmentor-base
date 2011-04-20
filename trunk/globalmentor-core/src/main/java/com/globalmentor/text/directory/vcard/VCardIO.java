/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text.directory.vcard;

import java.io.*;
import java.net.*;
import java.util.Set;

import com.globalmentor.io.*;
import com.globalmentor.text.directory.*;

import static com.globalmentor.collections.Sets.*;
import static com.globalmentor.io.Charsets.*;
import static com.globalmentor.text.directory.vcard.VCard.*;
import static java.util.Collections.emptySet;

/**
 * Class for loading and saving a a vCard <code>text/directory</code> profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
 * "vCard MIME Directory Profile".
 * @author Garret Wilson
 * @see VCard
 */
public class VCardIO implements IO<VCard>
{

	/** The names of contact lines that should be reduced to single content lines upon serialization. */
	private Set<String> serializationSingleValueNames = emptySet();

	/** @return The names of contact lines that should be reduced to a single value in single content lines upon serialization. */
	public Set<String> getSerializationSingleValueNames()
	{
		return serializationSingleValueNames;
	}

	/**
	 * Sets the names of contact lines that should be reduced to a single value in a single content lines upon serialization.
	 * @param singleValueNames The names of contact lines that should be reduced to a single value in a single content lines.
	 */
	public void setSerializationSingleValueNames(final Set<String> singleValueNames)
	{
		this.serializationSingleValueNames = immutableSetOf(singleValueNames);
	}

	/**
	 * Sets the names of contact lines that should be reduced to a single value in a single content lines upon serialization.
	 * @param singleValueNames The names of contact lines that should be reduced to a single value in a single content lines.
	 */
	public void setSerializationSingleValueNames(final String... singleValueNames)
	{
		this.serializationSingleValueNames = immutableSetOf(singleValueNames);
	}
	
	/** The profile to handle vCards. */
	protected final static VCardProfile VCARD_PROFILE = new VCardProfile();

	/** {@inheritDoc} */
	public VCard read(final InputStream inputStream, final URI baseURI) throws IOException
	{
		final DirectoryProcessor directoryProcessor = new DirectoryProcessor(); //create a new directory processor
		directoryProcessor.registerProfile(VCARD_PROFILE_NAME, VCARD_PROFILE); //register the vCard profile with the vCard processor
		final Reader reader = new InputStreamReader(inputStream, UTF_8_CHARSET); //assume the vCard is stored in UTF-8
		final Directory directory = directoryProcessor.processDirectory(reader, baseURI); //process the directory
		//TODO del Log.trace("parsed directory: ", directory);
		if(!(directory instanceof VCard)) //if the directory is not a VCard
		{
			throw new IOException("Directory " + directory.getDisplayName() + " is not a vCard."); //TODO i18n
		}
		return (VCard)directory; //cast the directory to a vCard and return it 
	}

	/** {@inheritDoc} */
	public void write(final OutputStream outputStream, final URI baseURI, final VCard object) throws IOException
	{
		final ContentLine[] contentLines = VCardProfile.createContentLines(object); //create content lines from the vCard
		final DirectorySerializer directorySerializer = new DirectorySerializer(); //create a new directory serializer
		directorySerializer.setSingleValueNames(getSerializationSingleValueNames()); //set the single-value names, if any
		directorySerializer.registerProfile(VCARD_PROFILE_NAME, VCARD_PROFILE); //register the vCard profile with the vCard serializer
		final Writer writer = new OutputStreamWriter(outputStream, UTF_8_CHARSET); //write the vCard using UTF-8
		directorySerializer.serializeContentLines(contentLines, writer); //serialize the content lines of the vCard TODO maybe allow the serializer to find a profile and convert to content lines automatically
		writer.flush(); //flush all our output, because the calling class will close the input stream, not the writer we created		
	}

}
