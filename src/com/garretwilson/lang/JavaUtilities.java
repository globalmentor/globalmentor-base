package com.garretwilson.lang;

import java.beans.Introspector;

import static com.garretwilson.lang.StringBuilderUtilities.*;
import static com.garretwilson.lang.JavaConstants.*;

/**Various utilities to assist programming with language-specific Java features.
@author Garret Wilson
*/
public class JavaUtilities
{

	/**This class cannot be publicly instantiated.*/
	private JavaUtilities() {}

//G***del	public static Object ensureSafeCast(

/*G***del if can't use
	public final static assertArgumentInstanceOf(final Object argument1, final Object argument2) throws IllegalArgumentException
	{
		instanceof
		if(!applicationPanel instanceof TextApplicationPanel)	//if this isn't the correct type of application panel
			throw new IllegalArgumentException("Expected "+TextApplicationPanel.)
*/

	/**Compares one object with another, taking into account that one or both
		objects may be <code>null</code>. If one object is <code>null</code> but
		the other is not, the <code>null</code> object is considered to be less
		than the non-<code>null</code> object.
	@param comparable1 The first object to compare.
	@param comparable2 The second object to compare.
	@return A negative integer, zero, or a positive integer if the first object
		is less than, equal to, or greater than the specified annotation,
		respectively, with a <code>null</code> considered less than a
		non-<code>null</code> value.
	@see Comparable#compareTo
	*/
	public final static int compareTo(final Comparable comparable1, final Comparable comparable2)
	{
		if(comparable1!=null && comparable2!=null) //if both objects are non-null
			return comparable1.compareTo(comparable2); //compare the objects
		else if(comparable1==comparable2)  //if both objects are null (we know at this point that one object is null, so if the objects are equal then both are null)
			return 0; //the objects are equal
		else  //if one objects is null and the other isn't
			return comparable1==null ? -1 : 1;  //the null object is lower
	}

	/**Constructs a proper name from the given name by capitalizing the first
		letter of the name
	@param name The name to convert to a proper name.
	@return A proper name appropriate for the given name.
	@see #getVariableName
	*/
	public static String getProperName(final String name)
	{
		if(!Character.isUpperCase(name.charAt(0)))  //if the first letter is not in uppercase
		{
			final StringBuffer stringBuffer=new StringBuffer(name); //create a new string buffer containing the name
			stringBuffer.setCharAt(0, Character.toUpperCase(stringBuffer.charAt(0))); //make sure the first letter is in uppercase
			return stringBuffer.toString(); //convert the string buffer to a string and return it
		}
		else  //if the first letter is already in uppercase
			return name;  //return the name itself; it's already a proper name
	}

	/**Constructs a variable name from the given name by decapitalizing the first
		letter of the name, unless the name begins with two capital letters.
	@param name The name to convert to a variable name.
	@return A variable name appropriate for the given name.
	@see Introspector.decapitalize
	@see #getProperName
	*/
	public static String getVariableName(final String name)
	{
		return Introspector.decapitalize(name); //decapitalize the name and return it
	}

	/**Illagal variable characters to be replaced when creating a valid variable name.*/
	private final static String ILLEGAL_VARIABLE_NAME_CHARACTERS=""+PACKAGE_SEPARATOR+INTERNAL_CLASS_SEPARATOR;

	/**Creates a safe Java variable name by replacing all illegal
	 	characters with the underscore ('_') character.
	This version only checks for the '.' and '$' characters.
	@param string A string to convert to a variable
	*/ 
	public static String createVariableName(final String string)
	{
		final StringBuilder stringBuilder=new StringBuilder(string);
		replace(stringBuilder, ILLEGAL_VARIABLE_NAME_CHARACTERS, '_');	//replace every '.' and '$' with '_'
		return stringBuilder.toString();
	}

}