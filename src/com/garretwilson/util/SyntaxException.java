package com.garretwilson.util;

/**Generic checked exception thrown to indicate that a string could not be
	parsed. 
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
		super(reason);	//construct the parent class with the reason
		if(input==null || reason==null)	//if neither input nor a reason were provided
			throw new NullPointerException();	//indicate that we only received null
		if(index<-1)	//if the index is less than negative one, the "unknown index" value
			throw new IllegalArgumentException();	//report the error
		this.input=input;	//save th input
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
		final StringBuffer stringBuffer=new StringBuffer(this.getReason());	//create a string buffer with the reason
		stringBuffer.append(':').append(' ').append(getInput());	//append the input
		if(getIndex()>=0)	//if we know the index
		{
			stringBuffer.append(" at index ").append(getIndex());	//append the index
		}
		return stringBuffer.toString();	//return the constructed message
	}
}
