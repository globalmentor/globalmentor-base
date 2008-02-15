package com.garretwilson.iso.idcard;

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Objects.*;

import com.garretwilson.math.Luhn;
import com.garretwilson.text.ArgumentSyntaxException;
import static com.globalmentor.java.Characters.*;
import com.globalmentor.java.Integers;
import com.globalmentor.java.Objects;

/**Primary Account Number (PAN) of an identification card as defined in ISO/IEC 7812-1:2000(E),
	"Identification cards � Identification of issuers � Part 1: Numbering system".
@see <a href="http://en.wikipedia.org/wiki/ISO_7812">Wikipedia: ISO 7812</a>
@author Garret Wilson
*/
public class PAN implements Comparable<PAN>
{

	/**The length of the Issuer Identification Number (IIN).*/
	public final static int IIN_LENGTH=6;

	/**The maximum length of the individual account identification.*/
	public final static int MAX_IAD_LENGTH=12;

	/**The minimum length of a Primary Account Number (PAN), including the Issuer Identification Number (IIN) plus a check digit.*/
	protected final static int MIN_PAN_LENGTH=IIN_LENGTH+1;

	/**The Major Industry Identifier (MII).*/
	private final int mii;

		/**@return The Major Industry Identifier (MII).*/
		public int getMII() {return mii;}

		/**@return The single-digit Major Industry Identifier (MII) character.*/
		public char getMIICharacter() {return (char)('0'+getMII());}
		
	/**The Issuer Identification Number (IIN).*/
	private final int iin;

		/**@return The Issuer Identification Number (IIN).*/
		public int getIIN() {return iin;}

		/**@return The six-digit Issuer Identification Number (IIN) string beginning with the single-digit Major Industry Identifier (MII) character.*/
		public String getIINString() {return Integers.toString(getIIN(), 10, IIN_LENGTH);}

	/**The original length of the individual acount identification.*/
	private final int individualAccountIDLength;
		
	/**The individual account identification.*/
	private final int individualAccountID;

		/**@return The individual account identification.*/
		public int getIndividualAccountID() {return individualAccountID;}

		/**@return The individual account identification string, with a maximum of {@value #MAX_IAD_LENGTH} digits.*/
		public String getIndividualAccountIDString() {return Integers.toString(getIndividualAccountID(), 10, individualAccountIDLength);}

	/**The check digit value for the number.*/
	private final int checkDigit;

		/**@return The check digit value for the number.*/
		public int getCheckDigit() {return checkDigit;}

		/**@return The check digit character for the number.*/
		public char getCheckDigitCharacter() {return (char)('0'+checkDigit);}

	/**The value of the primary account number.*/
	private final long value;

		/**@return The value of the primary account number.*/
		public long getValue() {return value;}

	/**The calculated hash code.*/
	private final int hashCode;

	/**String constructor.
	@param pan The string representation of the primary account number.
	@exception NullPointerException if the given primary account number string is <code>null</code>.
	@exception SyntaxException if the string is less than {@value #MIN_PAN_LENGTH} in length, one of the components of the account number is not valid, and/or if the components do not match the ending check digit.
	*/
/*TODO fix
	public PrimaryAccountNumber(final String pan) throws SyntaxException
	{
		checkInstance(pan, "Primary account number string cannot be null.");
		final int length=pan.length();	//get the length of the PAN
		if(length<MIN_PAN_LENGTH)	//if the string is too short to be a PAN
		{
			throw new SyntaxException(pan, "Primary account number string must be at least "+MIN_PAN_LENGTH+" digits in length.");
		}
		final String iin=pan.substring(0, IIN_LENGTH);	//get the IIN
		final int checkDigitIndex=length-1;	//the check digit should always be last
		final String iai=pan.substring(IIN_LENGTH, checkDigitIndex);	//everything after the IIN and before the check digit is the individual account identification
		final char checkDigit=pan.charAt(checkDigitIndex);	//get the check digit
		this(iin, iai, chechDigit);
	}
*/

	/**Account number constructor.
	This constructor is not reliable if a PAN has an IIN with one or more leading zeros. 
	@param pan The primary account number value.
	@exception ArgumentSyntaxException if the string form of the PAN is less than {@value #MIN_PAN_LENGTH} in length, one of the components of the account number is not valid, and/or if the components do not match the ending check digit.
	*/
	public PAN(final int pan) throws ArgumentSyntaxException
	{
		this(Integer.toString(pan));	//create a string from the PAN value
	}
		
	/**Character sequence constructor.
	@param pan The primary account number representation.
	@exception NullPointerException if the given primary account number string is <code>null</code>.
	@exception ArgumentSyntaxException if the string is less than {@value #MIN_PAN_LENGTH} in length, one of the components of the account number is not valid, and/or if the components do not match the ending check digit.
	*/
	public PAN(final CharSequence pan) throws ArgumentSyntaxException
	{
		this(checkMinLength(checkInstance(pan, "Primary account number cannot be null."), MIN_PAN_LENGTH).subSequence(0, IIN_LENGTH),	//split out the individual components
				pan.subSequence(IIN_LENGTH, pan.length()-1),
				pan.charAt(pan.length()-1));
	}

	/**Component constructor.
	@param iin The Issuer Identification Number (IIN); six digits beginning with the Major Industry Identifier (MII). 
	@param iai The individual account identification; a maximum of 12 digits.
	@param checkDigit The check digit.
	@exception NullPointerException if the given IIN and/or individual account identification is <code>null</code>.
	@exception ArgumentSyntaxException if the given IIN is not {@value #IIN_LENGTH} digits in length and/or is not composed solely of Latin digits.
	@exception ArgumentSyntaxException if the given individual account identification is more than {@value #MAX_IAD_LENGTH} digits in length and/or is not composed solely of Latin digits.
	@exception ArgumentSyntaxException if the given check digit is not a Latin digit.
	@exception ArgumentSyntaxException if the provided check digit is not the appropriate check digit for this primary account number.
	*/
	public PAN(final CharSequence iin, final CharSequence iai, final char checkDigit) throws ArgumentSyntaxException
	{
		checkInstance(iin, "IIN cannot be null.");
		if(!isLatinDigits(iin))	//if the IIN is not solely latin digits
		{
			throw new ArgumentSyntaxException("IIN must be composed entirely of Latin digits: "+iin);			
		}
		if(iin.length()!=IIN_LENGTH)	//if the IIN is not six digits long
		{
			throw new ArgumentSyntaxException("IIN must be six digits in length: "+iin);
		}
		checkInstance(iai, "Individual account identification cannot be null.");
		if(!isLatinDigits(iai))	//if the individual account identification is not solely latin digits
		{
			throw new ArgumentSyntaxException("IIN must be composed entirely of Latin digits: "+iin);			
		}
		if(iai.length()>MAX_IAD_LENGTH)	//if the individual account identification is over 12 digits long
		{
			throw new ArgumentSyntaxException("IIN must be six digits in length: "+iin);
		}
		if(!isLatinDigit(checkDigit))	//if the check digit is not a Latin digit
		{
			throw new ArgumentSyntaxException("Check digit "+checkDigit+" must be a Latin digit.");						
		}
		final String baseDigits=new StringBuilder().append(iin).append(iai).toString();	//get the base digits to checksum
		if(Luhn.getCheckDigit(baseDigits)!=checkDigit)	//if the number doesn't match its check digit
		{
			throw new ArgumentSyntaxException("Check digit "+checkDigit+" is not appropriate for digits: "+baseDigits);
		}
		this.iin=Integer.parseInt(iin.toString());	//save the IIN
		mii=iin.charAt(0)-'0';	//save the value represented by the first character of the IIN as the MII
		final String iaiString=iai.toString();	//get the individual account identification string
		this.individualAccountIDLength=iaiString.length();	//save the original length of the individual account identification
		this.individualAccountID=Integer.parseInt(iaiString);	//save the individual account identification
		this.checkDigit=checkDigit-'0';	//save the check digit value
		this.value=Long.parseLong(toString());	//save the long value of the PAN
		this.hashCode=Objects.hashCode(getIIN(), getIndividualAccountID());	//a PAN is uniquely identified by its IIN and IAI
	}

	/**@return A hash code representing this object.*/
	public int hashCode()
	{
		return hashCode;	//return the precalculated hash code
	}

	/**Determines if this object is equivalent to another object.
	This method considers another object equivalent if it is another PAN with the same IIN and IAI.
	@return <code>true</code> if the given object is an equivalent PAN.
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof PAN)	//if the other object is a PAN
		{
			final PAN pan=(PAN)object;	//get the other object as a PAN
			return getIIN()==pan.getIIN() && getIndividualAccountID()==pan.getIndividualAccountID();	//compare IIN and IAI
		}
		else	//if the other object is not a PAN
		{
			return false;	//the objects aren't equal
		}
	}

	/**Compares this object with the specified object for order.
	@param pan The object to be compared.
	@return A negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	*/
	public int compareTo(final PAN pan)
	{
		final long value1=value;	//get this value
		final long value2=pan.getValue();	//get the other value
		return value1<value2 ? -1 : value1==value2 ? 0 : 1;	//compare values, as we can't be sure a simple subtraction will yield a value within the supported range of integer (check for equality last, becuase it's more likely that the numbers will not be equal, so almost 50% of the time we'll save one comparison)
	}

	/**Returns a string representation of this primary account number.
	@return A string in the form IIN + individual account identification + check digit.
	*/
	public String toString()
	{
		return getIINString()+getIndividualAccountIDString()+getCheckDigitCharacter();	//IIN + individual account ID + check digit
	}

}
