package com.garretwilson.text.directory.vcard;

import java.io.*;
import java.net.*;
import com.garretwilson.io.*;
import com.garretwilson.text.CharacterEncodingConstants;
import com.garretwilson.text.directory.*;

/**Class for loading and saving a a vCard <code>text/directory</code>
	profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
@see VCard
*/
public class VCardIOKit extends AbstractIOKit<VCard>
{

	/**The profile to handle vCards.*/
	protected final static VCardProfile VCARD_PROFILE=new VCardProfile();

	/**Default constructor.*/
	public VCardIOKit()
	{
		this(null, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public VCardIOKit(final URIInputStreamable uriInputStreamable)
	{
		this(uriInputStreamable, null);
	}

	/**URI output stream locator constructor.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public VCardIOKit(final URIOutputStreamable uriOutputStreamable)
	{
		this(null, uriOutputStreamable);
	}

	/**Full constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public VCardIOKit(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable)
	{
		super(uriInputStreamable, uriOutputStreamable);	//construct the parent class
	}

	/**Loads a model from an input stream.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the content, or <code>null</code> if no base
		URI is available.
	@throws IOException Thrown if there is an error reading the data.
	*/ 
	public VCard load(final InputStream inputStream, final URI baseURI) throws IOException
	{
		final DirectoryProcessor directoryProcessor=new DirectoryProcessor();	//create a new directory processor
		directoryProcessor.registerProfile(VCardProfile.VCARD_PROFILE_NAME, VCARD_PROFILE);	//register the vCard profile with the vCard processor
		final Reader reader=new InputStreamReader(inputStream, CharacterEncodingConstants.UTF_8);	//assume the vCard is stored in UTF-8
		final Directory directory=directoryProcessor.processDirectory(reader, baseURI);	//process the directory
//G***del Debug.trace("parsed directory: ", directory);
		if(directory instanceof VCard)	//if this directory is a vCard
		{
//		G***del		final VCard vcard=VCardProfile.createVCard(directory);	//create a new VCard from the directory
			return (VCard)directory;	//cast the directory to a vCard and return it as the model 
//G***del			displayName=vcard.getDisplayName();	//save the display name so that we won't lose it TODO allow this to be edited							
		}
		else	//if this directory is not a vCard
		{
			throw new IOException("Directory "+directory.getDisplayName()+" is not a vCard.");	//G***i18n
		}
	}
	
	/**Saves a model to an output stream.
	@param model The model the data of which will be written to the given output stream.
	@param outputStream The output stream to which to write the model content.
	@throws IOException Thrown if there is an error writing the model.
	*/
	public void save(final VCard model, final OutputStream outputStream) throws IOException
	{
//G***del		vcard.setDisplayName(displayName);	//add the display name we saved
		write(model, outputStream);	//write the vCard to the output stream
	}

	/**Writes a vCard to an output stream.
	@param vcard The vCard which will be written to the given output stream.
	@param outputStream The output stream to which to write the vCard.
	@throws IOException Thrown if there is an error writing the vCard.
	*/
	public static void write(final VCard vcard, final OutputStream outputStream) throws IOException	//TODO probably remove this, as save() allows the same thing
	{
		final ContentLine[] contentLines=VCardProfile.createContentLines(vcard);	//create content lines from the vCard
		final DirectorySerializer directorySerializer=new DirectorySerializer();	//create a new directory serializer
		directorySerializer.registerProfile(VCardProfile.VCARD_PROFILE_NAME, VCARD_PROFILE);	//register the vCard profile with the vCard serializer
		final Writer writer=new OutputStreamWriter(outputStream, CharacterEncodingConstants.UTF_8);	//write the vCard using UTF-8
		directorySerializer.serializeContentLines(contentLines, writer);	//serialize the content lines of the vCard G***maybe allow the serializer to find a profile and convert to content lines automatically
		writer.flush();	//flush all our output, because the calling class will close the input stream, not the writer we created		
	}
}
