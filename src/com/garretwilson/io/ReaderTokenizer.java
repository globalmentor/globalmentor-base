package com.garretwilson.io;

import java.io.Reader;
import java.io.IOException;
import java.util.*;
import com.garretwilson.util.SyntaxException;

import static com.garretwilson.text.CharacterConstants.*;

/**Tokenizes input from a reader, recognizing groups.
All text within a group will be ignored when delimiting tokens, except that
	group delimiters are checked for matching.
@author Garret Wilson
*/
public class ReaderTokenizer implements Iterator<String>
{

	/**The default delimiter characters: whitespace.
	@see CharacterConstants#WHITESPACE_CHARS
	*/
	protected final static String DEFAULT_DELIMITERS=WHITESPACE_CHARS;

	/**The default beginning group characters: "([{".*/
	protected final static String DEFAULT_GROUP_BEGINS="([{";

	/**The default ending group characters: ")]}".*/
	protected final static String DEFAULT_GROUP_ENDS=")]}";

	/**The source of the input characters.*/
	private final Reader reader;

		/**@return The source of the input characters.*/
		protected Reader getReader() {return reader;}

	/**The characters delimiting tokens.*/
	private String delimiters;

		/**@return The characters delimiting tokens.*/
		public String getDelimiters() {return delimiters;}

	/**The valid group beginning characters.*/
	private final String groupBegins;

		/**@return The valid group beginning characters.*/
		public String getGroupBegins() {return groupBegins;}

	/**The valid group ending characters.*/
	private final String groupEnds;

		/**@return The valid group ending characters, matching to beginning characters.*/
		public String getGroupEnds() {return groupEnds;}

	/**We'll use the string builder as a stack to keep track of our group depth.*/
	private final StringBuilder groupStackStringBuilder=new StringBuilder();

		/**Pushes a group onto the stack.
		@param groupBegin The character representing the start of the group.
		*/
		protected void pushGroup(final char groupBegin)
		{
			groupStackStringBuilder.append(groupBegin);	//append the group beginning character
		}

		/**Pops a group from the stack and returns the group beginning character.
		@return The character used to begin the group.
		@throws StringIndexOutOfBoundsException {@inheritDoc}
		@exception EmptyStackException if there are no groups left.
		*/
		protected char popGroup() throws EmptyStackException
		{
			final int groupDepth=getGroupDepth();	//see how many groups there are
			if(groupDepth>0)	//if we have groups
			{
				final char groupBegin=groupStackStringBuilder.charAt(groupDepth-1);	//get the group beginning character
				groupStackStringBuilder.deleteCharAt(groupDepth-1);	//delete the group beginning character
				return groupBegin;	//return the group beginning character
			}
			else	//if we are out of groups
			{
				throw new EmptyStackException();	//show that there are no more groups
			}
		}

		/**@return The number of nested groups currently being processed.*/
		protected int getGroupDepth()
		{
			return groupStackStringBuilder.length();
		}

	/**The next primed token, or <code>null</code> if there is no
			next token or the token has not been primed.
	*/
	private String primedToken=null;

		/**@return The next primed token, or <code>null</code> if there is no
			next token or the token has not been primed.
		*/
//G***del		protected String getPrimedToken() {return nextToken;}

	/**Reader constructor with default token delimiters and group delimiters.
	@param reader The input characters to tokenize.
	@param delimiters The delimiter characters.
	@param groupBegins The valid group beginning characters.
	@param groupEnds The valid group ending characters, matching to beginning characters.
	@see #DEFAULT_DELIMITERS
	@see #DEFAULT_GROUP_BEGINS
	@see #DEFAULT_GROUP_ENDS
	*/
	public ReaderTokenizer(final Reader reader)
	{
		this(reader, DEFAULT_DELIMITERS);
	}

	/**Token delimiter constructor with default group delimiters.
	@param reader The input characters to tokenize.
	@param delimiters The delimiter characters.
	@param groupBegins The valid group beginning characters.
	@param groupEnds The valid group ending characters, matching to beginning characters.
	@see #DEFAULT_GROUP_BEGINS
	@see #DEFAULT_GROUP_ENDS
	*/
	public ReaderTokenizer(final Reader reader, final String delimiters)
	{
		this(reader, delimiters, DEFAULT_GROUP_BEGINS, DEFAULT_GROUP_ENDS);
	}

	/**Delimiter and group constructor.
	@param reader The input characters to tokenize.
	@param delimiters The delimiter characters.
	@param groupBegins The valid group beginning characters.
	@param groupEnds The valid group ending characters, matching to beginning characters.
	*/
	public ReaderTokenizer(final Reader reader, final String delimiters, final String groupBegins, final String groupEnds)
	{
		this.reader=reader;
		this.delimiters=delimiters;
		this.groupBegins=groupBegins;
		this.groupEnds=groupEnds;
	}

	/**@return <code>true</code> if there are more tokens.*/
	public boolean hasNext()
	{
		return primeToken()!=null;	//prime a token and see if there is one available
	}

	/**@return The next token available.
	@exception NoSuchElementException if there are no more tokens.
	*/
	public String next()
	{
		final String token=primeToken();	//prime the next token
		if(token!=null)	//if there is a token available
		{
			primedToken=null;	//show that we've used the primed token
			return token;	//return the token
		}
		else	//if there is no token available
		{
			throw new NoSuchElementException();	//show that there are no more tokens
		}
	}

	/**Removes the last token from the underlying collection.
	This method is not supported, and an
		<code>UnsupportedOperationException</code> is always thrown.
	@exception UnsupportedOperationException if the the remove operation is
		not supported .
	@exception IllegalStateException if the <code>next()</code> method has not
		yet been called, or the <code>remove</code> method has already
		been called after the last call to the <code>next</code>
		method.
	*/
	public void remove()
	{
		throw new UnsupportedOperationException();
	}
	
	/**If there is no token primed, attempts to retrieve the next token.
	@return Either the token that was already primed or, if there is no token
		already, the token newly primed by this method. If no token is available,
	 this method returns <code>null</code>.
	*/
	public String primeToken()
	{
		if(primedToken==null)	//if there is no token primed
		{
			final Reader reader=getReader();	//get a reference to our reader
			final String delimiters=getDelimiters();	//get our delimiters
			final String groupBegins=getGroupBegins();	//get our group beginning delimiters
			final String groupEnds=getGroupEnds();	//get our group ending delimiters
			final StringBuilder stringBuilder=new StringBuilder();	//create a string builder to build our token
			try
			{
				int value;	//this will keep track of each character we read
				while((value=reader.read())>=0)	//read the next character; while there are more characters
				{
					final char character=(char)value;	//cast the character value to a char
					if(getGroupDepth()==0 && delimiters.indexOf(character)>=0)	//only look at delimiters when we're not in a group
					{
						if(stringBuilder.length()>0)	//if we've already started a token, return the token; otherwise, just ignore the delimiter
						{
							break;	//we found a delimiter, so we've finished a token
						}
					}
					else	//if this is not a delimiter, or we're inside the group, we'll keep the character
					{
						if(groupBegins.indexOf(character)>=0)	//if this is a group beginning
						{
							pushGroup(character);	//push the group onto the stack
						}
						else	//if this is not a group beginning
						{
							final int groupEndCharIndex=groupEnds.indexOf(character);	//see if this is a group ending character
							if(groupEndCharIndex>=0)	//if this is a group ending
							{
								try
								{
									final char groupBegin=popGroup();	//pop a group from the stack
									//TODO check to make sure groupBegins[groupEndCharIndex]==groupBegin and do something interesting
								}
								catch(EmptyStackException emptyStackException)
								{
									//TODO do something interesting with the empty stack error
								}
							}
						}
						stringBuilder.append(character);	//append the character
					}
				}
			}
			catch(IOException ioException)
			{
				//TODO do something interesting with the I/O exception
			}
			primedToken=stringBuilder.length()>0 ? stringBuilder.toString() : null;	//store the token, or null if we didn't collect any characters
		}
		return primedToken;	//return the waiting token
	}

}