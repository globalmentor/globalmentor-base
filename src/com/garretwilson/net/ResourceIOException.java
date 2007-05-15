package com.garretwilson.net;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;

import com.garretwilson.net.http.HTTPForbiddenException;
import com.garretwilson.net.http.HTTPNotFoundException;
import com.garretwilson.net.http.HTTPPreconditionFailedException;

import static com.garretwilson.lang.ObjectUtilities.*;

/**A class for resource-related I/O errors, agnostic of the I/O protocol being used.
In most case a subclass more specific to the error should be used.
@author Garret Wilson
*/
public class ResourceIOException extends IOException
{

	/**The URI of the resource to which the exception is related.*/
	private final URI resourceURI;

		/**@return The URI of the resource to which the exception is related.*/
		public URI getResourceURI() {return resourceURI;}

	/**Constructs a new exception for the specified resource URI.
	@param resourceURI The URI of the resource to which the exception is related.
	@exception NullPointerException if the given resource URI is <code>null</code>.
	*/
	public ResourceIOException(final URI resourceURI)
	{
		this(resourceURI, (String)null);	//construct the exception with the resource URI and no message
	}

	/**Constructs a new exception with the specified resource URI and detail message.
	@param resourceURI The URI of the resource to which the exception is related.
	@param message The detail message.
	@exception NullPointerException if the given resource URI is <code>null</code>.
	*/
	public ResourceIOException(final URI resourceURI, final String message)
	{
		this(resourceURI, message, null);	//construct the class with no cause
	}

	/**Constructs a new exception with the specified resource URI and cause, along with a detail message derived from the cause.
	@param resourceURI The URI of the resource to which the exception is related.
	@param cause The cause, or <code>null</code> to indicate the cause is nonexistent or unknown.
	@exception NullPointerException if the given resource URI is <code>null</code>.
	*/
	public ResourceIOException(final URI resourceURI, final Throwable cause)
	{
		this(resourceURI, cause!=null ? cause.toString() : null, cause);	//create an exception with a generated detail message
	}

	/**Constructs a new exception with the specified resource URI, detail message, and cause.
	@param resourceURI The URI of the resource to which the exception is related.
	@param message The detail message.
	@param cause The cause, or <code>null</code> to indicate the cause is nonexistent or unknown.
	@exception NullPointerException if the given resource URI is <code>null</code>.
	*/
	public ResourceIOException(final URI resourceURI, final String message, final Throwable cause)
	{
		super(message);	//construct the parent class
		initCause(cause);	//indicate the source of this exception
		this.resourceURI=checkInstance(resourceURI, "Resource URI cannot be null.");	//save the resource URI
	}

	/**Translates a given error into a resource I/O exception.
	If the exception is already a {@link ResourceIOException} it is returned unmodified.
	This version makes the following translations:
	<dl>
		<dt>{@link FileNotFoundException}</dt> <dd>{@link ResourceNotFoundException}</dd>
		<dt>{@link HTTPForbiddenException}</dt> <dd>{@link ResourceForbiddenException}</dd>
		<dt>{@link HTTPNotFoundException}</dt> <dd>{@link ResourceNotFoundException}</dd>
		<dt>{@link HTTPPreconditionFailedException}</dt> <dd>{@link ResourceStateException}</dd>
	</dl>
	@param throwable The error which should be translated to a resource I/O exception.
	@param resourceURI The URI of the resource to which the exception is related.
	@return A resource I/O exception based upon the given throwable.
	*/
	public static ResourceIOException toResourceIOException(final Throwable throwable, final URI resourceURI) 
	{
		if(throwable instanceof ResourceIOException)	//resource I/O exception
		{
			return (ResourceIOException)throwable;	//cast the throwable to a resource I/O exception
		}
			//file exceptions
		else if(throwable instanceof FileNotFoundException)
		{
			return new ResourceNotFoundException(resourceURI, throwable);
		}
			//HTTP exceptions
		else if(throwable instanceof HTTPForbiddenException)
		{
			return new ResourceForbiddenException(resourceURI, throwable);
		}
		else if(throwable instanceof HTTPNotFoundException)
		{
			return new ResourceNotFoundException(resourceURI, throwable);
		}
		else if(throwable instanceof HTTPPreconditionFailedException)
		{
			return new ResourceStateException(resourceURI, throwable);
		}
			//default
		else
		{
			return new ResourceIOException(resourceURI, throwable);	//create a default resource I/O exception
		}
	}
}
