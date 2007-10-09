package com.garretwilson.util;

/**Checked exception indicating that there was an error with data being processed.
@author Garret Wilson
*/
public class DataException extends Exception
{
	/**Constructs a new exception with no detail message.*/
	public DataException()
	{
		super();
	}

	/**Constructs a new exception with the specified detail message. 
	@param message The detail message.
	*/
	public DataException(final String message)
	{
		super(message);
	}

	/**Constructs a new exception with the specified detail message and cause.
	@param message The detail message (which is saved for later retrieval by the {@link Throwable#getMessage()} method).
	@param cause The cause (which is saved for later retrieval by the {@link Throwable#getCause()} method), or <code>null</code> if the cause is nonexistent or unknown.
	*/
	public DataException(final String message, final Throwable cause)
	{
		super(message, cause);
	}
 
	/**Constructs a new exception with the specified cause and a detail message of <code>(cause==null ? null : cause.toString())</code>.
	@param cause The cause (which is saved for later retrieval by the {@link Throwable#getCause()} method), or <code>null</code> if the cause is nonexistent or unknown.
	*/
	public DataException(final Throwable cause)
	{
		super(cause);
	}
}
