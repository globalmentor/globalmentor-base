package com.garretwilson.text;

/**An unchecked illegal argument exception to indicate that an argument was not in the correct format or did not have the correct checksums.
@author Garret Wilson
*/
public class ArgumentSyntaxException extends IllegalArgumentException
{
	/**Constructs a new exception with no detail message.*/
	public ArgumentSyntaxException()
	{
		super();
	}

	/**Constructs a new exception with the specified detail message. 
	@param message The detail message.
	*/
	public ArgumentSyntaxException(final String message)
	{
		super(message);
	}

	/**Constructs a new exception with the specified detail message and cause.
	@param message The detail message (which is saved for later retrieval by the {@link Throwable#getMessage()} method).
	@param cause The cause (which is saved for later retrieval by the {@link Throwable#getCause()} method), or <code>null</code> if the cause is nonexistent or unknown.
	*/
	public ArgumentSyntaxException(final String message, final Throwable cause)
	{
		super(message, cause);
	}
 
	/**Constructs a new exception with the specified cause and a detail message of <code>(cause==null ? null : cause.toString())</code>.
	@param cause The cause (which is saved for later retrieval by the {@link Throwable#getCause()} method), or <code>null</code> if the cause is nonexistent or unknown.
	*/
	public ArgumentSyntaxException(final Throwable cause)
	{
		super(cause);
	}
}
