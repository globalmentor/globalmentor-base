package com.garretwilson.text;

/**Generic checked exception thrown to indicate that a string could not be parsed.
Allows convenient indication of optional cause exception as well as optional indication of a reason message.
@author Garret Wilson
*/
public class SyntaxException extends Exception
{
	
	/**The input string.*/
	private final String input; 

		/**@return The input string.*/
		public String getInput() {return input;}
		
	/**The index into the input string of the position at which the parse error
		occurred, or -1 if the position is not known.
	*/
	private final int index;

		/**@return The index into the input string of the position at which the parse error
			occurred, or -1 if the position is not known.
		*/
		public int getIndex() {return index;}

	/**Constructs a syntax exception from an input string and a reason.
	@param input The input string.
	@param reason An explanation of why the input string could not be parsed.
	*/
	public SyntaxException(final String input, final String reason)
	{
		this(input, reason, -1);	//construct the class with an unknown index
	}

	/**Constructs a syntax exception from an input string, a reason, and an index.
	@param input The input string.
	@param reason An explanation of why the input string could not be parsed.
	@param index The index into the input string of the position at which the
		parse error occurred, or -1 if the position is not known.
	*/
	public SyntaxException(final String input, final String reason, final int index)
	{
		this(input, reason, index, null);	//construct the exception with no cause
	}

	/**Constructs a syntax exception from an existing exception.
	@param input The input string.
	@param reason An explanation of why the input string could not be parsed.
	@param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	*/
	public SyntaxException(final String input, final Throwable cause)
	{
		this(input, null, -1, cause);	//construct the class with an unknown reason and index
	}

	/**Constructs a syntax exception from an input string, a reason, and an index.
	A reason and/or a cause must be provided.
	@param input The input string.
	@param reason An explanation of why the input string could not be parsed.
	@param index The index into the input string of the position at which the
		parse error occurred, or -1 if the position is not known.
	@param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	*/
	protected SyntaxException(final String input, final String reason, final int index, final Throwable cause)
	{
		super(reason!=null ? reason : cause.getMessage(), cause);	//construct the parent class with the reason and the cause
		if(input==null)	//if no input was provided
			throw new NullPointerException("Missing input string.");	//indicate that we only received null
		if(index<-1)	//if the index is less than negative one, the "unknown index" value
			throw new IllegalArgumentException();	//report the error
		this.input=input;	//save the input
		this.index=index;	//save the index		
	}

	/**@return An explanation of why the input string could not be parsed.*/
	public String getReason()
	{
		return super.getMessage();	//return the reason, which we stored in the super class message (don't call this version of getMessage(), because this version calls getReason() and would result in infinite recursion)
	}
	
	/**@return A string explaining the syntax exception, in the form of
		"<em>reason</em>: <em>input</em> at index <em>index</em>".
	*/ 
	public String getMessage()
	{
		final StringBuilder stringBuilder=new StringBuilder(this.getReason());	//create a string builder with the reason
		if(getInput()!=null && getInput().length()>0)	//if we know the input
		{
			stringBuilder.append(':').append(' ').append(getInput());	//append the input
		}
		if(getIndex()>=0)	//if we know the index
		{
			stringBuilder.append(" at index ").append(getIndex());	//append the index
		}
		return stringBuilder.toString();	//return the constructed message
	}
}
