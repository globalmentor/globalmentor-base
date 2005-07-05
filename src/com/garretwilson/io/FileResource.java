package com.garretwilson.io;

import java.io.File;
import java.net.URI;
import java.util.Collections;
import java.util.List;

import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.model.DefaultResource;

/**A resource accessible by a file.
The file may or may not have the same URI as the resource.
@author Garret Wilson
*/
public class FileResource extends DefaultResource
{

	/**The file this resource represents.*/
	private final File file;

		/**@return The file this resource represents.*/
		public File getFile() {return file;}

	/**Constructs a resource with a file.
	The reference URI will be set to the reference URI of the file.
	@param file The file this resource represents.
	@exception NullPointerException if the given file or reference URI is <code>null</code>.
	*/
	public FileResource(final File file)
	{
		this(file, file.toURI());	//construct the resource using the file's URI
	}

	/**Constructs a resource with a file and a reference URI.
	@param file The file this resource represents.
	@param referenceURI The reference URI for the new resource.
	@exception NullPointerException if the given file or reference URI is <code>null</code>.
	*/
	public FileResource(final File file, final URI referenceURI)
	{
		super(referenceURI);	//construct the parent class TODO fix null check in parent class
		this.file=checkNull(file, "File cannot be null.");	//save the file
	}

	/**Retrieves an list of child resources of this resource.
	@return A list of child resources.
	*/
	public List<FileResource> getChildResources()
	{
		return Collections.emptyList();	//TODO implement
	}

}
