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

import com.globalmentor.io.*;
import com.globalmentor.text.CharacterEncoding;
import com.globalmentor.text.directory.*;
import static com.globalmentor.text.directory.vcard.VCard.*;

/**
 * Class for loading and saving a a vCard <code>text/directory</code> profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
 * "vCard MIME Directory Profile".
 * @author Garret Wilson
 * @see VCard
 */
public class VCardIO implements IO<VCard>
{

	/** The profile to handle vCards. */
	protected final static VCardProfile VCARD_PROFILE = new VCardProfile();

	/** {@inheritDoc} */
	public VCard read(final InputStream inputStream, final URI baseURI) throws IOException
	{
		final DirectoryProcessor directoryProcessor = new DirectoryProcessor(); //create a new directory processor
		directoryProcessor.registerProfile(VCARD_PROFILE_NAME, VCARD_PROFILE); //register the vCard profile with the vCard processor
		final Reader reader = new InputStreamReader(inputStream, CharacterEncoding.UTF_8); //assume the vCard is stored in UTF-8
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
		directorySerializer.registerProfile(VCARD_PROFILE_NAME, VCARD_PROFILE); //register the vCard profile with the vCard serializer
		final Writer writer = new OutputStreamWriter(outputStream, CharacterEncoding.UTF_8); //write the vCard using UTF-8
		directorySerializer.serializeContentLines(contentLines, writer); //serialize the content lines of the vCard TODO maybe allow the serializer to find a profile and convert to content lines automatically
		writer.flush(); //flush all our output, because the calling class will close the input stream, not the writer we created		
	}

}
