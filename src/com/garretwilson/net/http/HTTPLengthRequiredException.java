package com.garretwilson.net.http;

import static com.garretwilson.net.http.HTTPConstants.*;

/**Indicates that the server refuses to accept the request without a defined Content-Length. 
Corresponds to HTTP status code 411.
@author Garret Wilson
*/
public class HTTPLengthRequiredException extends HTTPClientErrorException
{

	/**Constructs a new exception.*/
	public HTTPLengthRequiredException()
	{
		this((String)null);	//construct the exception with no message
	}

	/**Constructs a new exception with the specified detail message.
	@param message The detail message.
	*/
	public HTTPLengthRequiredException(final String message)
	{
		this(message, null);	//construct the class with no cause
	}

	/**Constructs a new exception with the specified cause, along with a detail message derived from the cause.
	@param cause The cause, or <code>null</code> to indicate the cause is nonexistent or unknown.
	*/
	public HTTPLengthRequiredException(final Throwable cause)
	{
		this(cause!=null ? cause.toString() : null, cause);	//create an exception with a generated detail message
	}

	/**Constructs a new exception with the specified detail message, and cause.
	@param message The detail message.
	@param cause The cause, or <code>null</code> to indicate the cause is nonexistent or unknown.
	*/
	public HTTPLengthRequiredException(final String message, final Throwable cause)
	{
		super(SC_LENGTH_REQUIRED, message, cause);	//construct the parent class
	}
}