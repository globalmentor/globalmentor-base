package com.garretwilson.text;

/**Generic checked exception thrown to indicate that a string could not be parsed.
@author Garret Wilson
*/
public class SyntaxException extends Exception
{
	
	/**The input string.*/
	private final String input; 

		/**@return The input string.*/
		public String getInput() {return input;}
		
	/**The index into the input string of the position at which the parse error occurred, or -1 if the position is not known.*/
	private final int index;

		/**@return The index into the input string of the position at which the parse error occurred, or -1 if the position is not known.*/
		public int getIndex() {return index;}

	/**Input string constructor with a default message.
	@param input The input string.
	@exception NullPointerException if the given input string is <code>null</code>.
	*/
	public SyntaxException(final String input)
	{
		this(input, (String)null);	//construct the class for a default message
	}

	/**Input string and message constructor.
	A message will be constructed including the given message, if any, or the given message of the cause, if any.
	@param input The input string.
	@param message An explanation of why the input string could not be parsed, or <code>null</code> if a default message should be used.
	@exception NullPointerException if the given input string is <code>null</code>.
	*/
	public SyntaxException(final String input, final String message)
	{
		this(input, -1, message);	//construct the class with an unknown index
	}

	/**Input string and index constructor with a default message.
	@param input The input string.
	@param index The index into the input string of the position at which the parse error occurred, or -1 if the position is not known.
	@exception NullPointerException if the given input string is <code>null</code>.
	@exception IllegalArgumentException if the given index is less than -1.
	*/
	public SyntaxException(final String input, final int index)
	{
		this(input, index, (String)null);	//construct the class for a default message
	}

	/**Input string, index, and message constructor.
	A message will be constructed including the given message, if any, or the given message of the cause, if any.
	@param input The input string.
	@param index The index into the input string of the position at which the parse error occurred, or -1 if the position is not known.
	@param message An explanation of why the input string could not be parsed, or <code>null</code> if a default message should be used.
	@exception NullPointerException if the given input string is <code>null</code>.
	@exception IllegalArgumentException if the given index is less than -1.
	*/
	public SyntaxException(final String input, final int index, final String message)
	{
		this(input, index, message, null);	//construct the exception with no cause
	}

	/**Input and existing exception constructor.
	@param input The input string.
	@param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	@exception NullPointerException if the given input string is <code>null</code>.
	*/
	public SyntaxException(final String input, final Throwable cause)
	{
		this(input, -1, null, cause);	//construct the class with an unknown message and index
	}

	/**Input string, index, and cause constructor with a default message.
	@param input The input string.
	@param index The index into the input string of the position at which the parse error occurred, or -1 if the position is not known.
	@param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	@exception NullPointerException if the given input string is <code>null</code>.
	@exception IllegalArgumentException if the given index is less than -1.
	*/
	protected SyntaxException(final String input, final int index, final Throwable cause)
	{
		this(input, index, null, cause);	//construct the class for a default message		
	}

	/**Input string, index, message, and cause constructor.
	A message will be constructed including the given message, if any, or the given message of the cause, if any.
	@param input The input string.
	@param index The index into the input string of the position at which the parse error occurred, or -1 if the position is not known.
	@param message An explanation of why the input string could not be parsed, or <code>null</code> if a default message should be used.
	@param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	@exception NullPointerException if the given input string is <code>null</code>.
	@exception IllegalArgumentException if the given index is less than -1.
	*/
	protected SyntaxException(final String input, final int index, final String message, final Throwable cause)
	{
		super(ArgumentParseException.createMessage(input, index, message!=null ? message : (cause!=null && cause.getMessage()!=null ? cause.getMessage() : null)), cause);	//construct the parent class with the message and the cause
		this.input=input;	//save the input
		this.index=index;	//save the index		
	}

}
