package com.garretwilson.country.us;

import static com.garretwilson.lang.IntegerUtilities.*;
import static com.garretwilson.lang.Objects.*;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.garretwilson.lang.IntegerUtilities;
import com.garretwilson.text.ArgumentSyntaxException;

/**An ABA Routing Transit Number (RTN) that identifies financial institutions within the United States.
<p>A RTN is is a nine-digit number.</p>
<p>A RTN is accepted in the form "AAAGGSSSS" or "AAA-GG-SSSS".
@see <a href="http://en.wikipedia.org/wiki/Routing_number">Wikipedia: Routing Transit Number</a>
@see <a href="http://www.brainjar.com/js/validation/">Brain Jar.com: Validation Algorithms</a>
@author Garret Wilson
*/
public class RTN
{

	/**The institution category of the RTN, based upon its first two digits.*/
	public enum Category
	{
		US_GOVERNMENT(00, false),
		PRIMARY(01, true),
		THRIFT(21, true),
		ELECTRONIC(61, true),
		TRAVELERS_CHEQUE(80, false);

		/**The base ID of this category.
		Some categories may not have more than one category ID besides the base.
		*/
		private final int baseID;

			/**Returns the base ID of this category.
			Some categories may not have more than one category ID besides the base.
			@return The base ID of this category.
			*/
			public int getBaseID() {return baseID;}

		/**Whether this category represents a range of category IDs corresponding to federal reserve districts.*/
		private final boolean isFed;

			/**@return Whether this category represents a range of category IDs corresponding to federal reserve districts.*/
			public boolean isFed() {return isFed;}

		/**Base ID constructor.
		@param baseID The base ID of this category.
		@param isFed Whether this category represents a range of category IDs corresponding to federal reserve districts.
		*/
		Category(final int baseID, final boolean isFed)
		{
			this.baseID=baseID;
			this.isFed=isFed;
		}
	}

	/**The length of a routing transit number.*/
	public final static int LENGTH=9;

	/**A regular expression pattern matching a RTN in the form "XXXXXXXXX".
	The first matching group matches the first two category/federal reserve digits. 
	*/
	public final static Pattern PATTERN=Pattern.compile("(\\d{2})\\d{7}");

	/**The weights used in calculating the checksum of an RTN.*/
	public final static int[] CHECKSUM_WEIGHTS=new int[]{3, 7, 1, 3, 7, 1, 3, 7, 1}; 

	/**The ID value indicating the category and optionally reserve bank.*/
	private final int categoryID;

	/**@return The institution category of the RTN.*/
	public Category getCategory()
	{
		final Category category=getCategory(categoryID);	//get the category corresponding to our category ID
		assert category!=null : "Stored category ID should always be recognized.";
		return category;	//return the category
	}

	/**Determines the The institution category of a category ID.
	@param categoryID The category ID.
	@return The category corresponding to the category ID, or <code>null</code> if no matching category could be found.*/
	protected static Category getCategory(final int categoryID)
	{
		final int federalReserveCount=FederalReserveDistrict.values().length;	//find out how many federal reserve districts there are
		for(final Category category:Category.values())	//for each possible category
		{
			final int baseID=category.getBaseID();	//get the category base ID
			if(category.isFed())	//if this is a range category
			{
				if(categoryID>=baseID && categoryID<baseID+federalReserveCount)	//if the category ID is within the range
				{
					return category;	//we've found the category
				}
			}
			else	//if this is not a range category
			{
				if(categoryID==baseID)	//if this category ID matches the base ID
				{
					return category;	//we've found the category					
				}
			}
		}
		return null;	//indicate that we couldn't find a matching category
	}

	/**@return The federal reserve district of the institution identified by the RTN, or <code>null</code> if this RTN is not associated with a federal reserve district.*/
	public FederalReserveDistrict getFederalReserveDistrict()
	{
		final Category category=getCategory();	//get the category of this RTN
		return category.isFed() ? FederalReserveDistrict.values()[categoryID-getCategory().getBaseID()] : null;	//if this is a ranged category, find the federal reserve district based upon its offset within the range 
	}
	
	/**The value of the routing transit number.*/
	private final int value;

		/**@return The value of the routing transit number.*/
		public int getValue() {return value;}

	/**RTN value constructor.
	@param rtn The routing transit number value.
	@exception IllegalArgumentException if the resulting string is longer than nine digits.
	@exception IllegalArgumentException if the given RTN is zero.
	@exception ArgumentSyntaxException if the first two digits are not recognized as a valid category ID.
	*/
	public RTN(final int rtn) throws ArgumentSyntaxException
	{
		this(IntegerUtilities.toString(checkRange(rtn, 1, 999999999), 10, LENGTH));	//create a string from the RTN value
	}
		
	/**Character sequence constructor.
	@param rtn An RTN character sequence in the form "XXXXXXXXX".
	@exception NullPointerException if the given character sequence is <code>null</code>.
	@exception ArgumentSyntaxException if the character sequence is not in the form "XXXXXXXXX".
	@exception ArgumentSyntaxException if the character sequence is "000000000".
	@exception ArgumentSyntaxException if the character sequence does not have a valid checksum.
	@exception ArgumentSyntaxException if the first two digits are not recognized as a valid category ID.
	*/
	public RTN(final CharSequence rtn) throws ArgumentSyntaxException
	{
		final Matcher matcher=PATTERN.matcher(checkInstance(rtn, "Routing transit number cannot be null."));	//create a matcher from the RTN pattern
		if(matcher.matches())	//if the RTN matches the pattern
		{
			categoryID=Integer.parseInt(matcher.group(1));	//extract the category ID value
			if(getCategory(categoryID)==null)	//if we don't recognize the category
			{
				throw new ArgumentSyntaxException("Unknown category ID: "+matcher.group(1));
			}
			value=Integer.parseInt(matcher.group(0));	//extract the entire value
			if(value==0)	//if the value is zero
			{
				throw new ArgumentSyntaxException("Illegal routing transit number: "+rtn);
			}
			long checksum=0;	//create a checksum to validate the RTN
			for(int i=0; i<LENGTH; ++i)	//for each digit
			{
				checksum+=CHECKSUM_WEIGHTS[i]*(rtn.charAt(i)-'0');	//multiply the value if this digit with the weight and add it to the checksum
			}
			if(checksum%10!=0)	//if the checksum isn't a multiple of 10
			{
				throw new ArgumentSyntaxException("Routing transit number "+rtn+" has invalid checksum.");
			}
		}
		else	//if the RTN doesn't match the pattern
		{
			throw new ArgumentSyntaxException("RTN "+rtn+" is not a valid routing transit number in the form \"XXXXXXXXX\".");
		}
	}

	/**@return A hash code representing this object.*/
	public int hashCode()
	{
		return getValue();	//return the value itself
	}

	/**Determines if this object is equivalent to another object.
	This method considers another object equivalent if it is another RTN with the same value.
	@return <code>true</code> if the given object is an equivalent RTN.
	*/
	public boolean equals(final Object object)
	{
		return object instanceof RTN && getValue()==((RTN)object).getValue();	//if the object is an RTN, compare values
	}

	/**Compares this object with the specified object for order.
	@param rtn The object to be compared.
	@return A negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	*/
	public int compareTo(final RTN rtn)
	{
		return getValue()-rtn.getValue();	//compare values
	}

	/**Returns a string representation of this routing transit number.
	@return A string in the form "XXXXXXXXX" representing this routing transit number.
	*/
	public String toString()
	{
		return IntegerUtilities.toString(getValue(), 10, LENGTH);	//create a string from the RTN value
	}

}
