package com.garretwilson.text.directory.vcard;

/**An object representing the "EMAIL" type of a vCard <code>text/directory</code>
	profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
*/
public class Email
{

	/**Indicates no email type is specified.*/
	public final static int NO_EMAIL_TYPE=0;
	/**An internet email address.*/
	public final static int INTERNET_EMAIL_TYPE=1<<0;
	/**An X.400 addressing type.*/
	public final static int X400_EMAIL_TYPE=1<<1;
	/**A preferred-use email address.*/
	public final static int PREFERRED_EMAIL_TYPE=1<<3;

	/**The default email type.*/
	public final static int DEFAULT_EMAIL_TYPE=INTERNET_EMAIL_TYPE;

	/**The email address.*/
	private String address;

		/**@return The email address.*/
		public String getAddress() {return address;}

		/**Sets the email address.
		@param address The new email address.
		*/
		public void setAddress(final String address) {this.address=address;}

	/**The email addressing type.*/
	private int emailType;

		/**@return The email addressing type; defaults to
			<code>INTERNET_EMAIL_TYPE</code>.
		*/
		public int getEmailType() {return emailType;}

		/**Sets the email addressing type.
		@param emailType The email addressing type, one or more of the
			<code>XXX_EMAIL_TYPE</code> constants ORed together.
		*/
		public void setEmailType(final int emailType) {this.emailType=emailType;}

	/**Email address constructor with default telephone type of <code>INTERNET_EMAIL_TYPE</code>.
	@param emailAddress The email address.
	*/
	public Email(final String emailAddress)
	{
		this(emailAddress, DEFAULT_EMAIL_TYPE);	//construct an email with the default email type
	}

	/**Email address and type constructor.
	@param telephoneNumber The telephone number from which values should be used
		for initialization.
	@param emailType The email addressing type, one of the
		<code>XXX_EMAIL_TYPE</code> constants.
	*/
	public Email(final String emailAddress, final int emailType)
	{
		setAddress(emailAddress);
		setEmailType(emailType);
	}

	/**The string for separating the components of the string representation of
		the EMAIL type.
	*/
	protected final static String EMAIL_TYPE_SEPARATOR=", ";

	/**@return A string to represent the email addressing type.*/
	public String getEmailTypeString()
	{
		return getEmailTypeString(getEmailType());	//return a string for our email type
	}

	/**Constructs a string to represent the given email addressing type.
	@param emailType The intended use, one or more of the
		<code>XXX_EMAIL_TYPE</code> constants ORed together.
	 */
	public static String getEmailTypeString(final int emailType)	//G***i18n
	{
		final StringBuffer stringBuffer=new StringBuffer();
		if((emailType&PREFERRED_EMAIL_TYPE)!=0)
		{
			if(stringBuffer.length()>0)
				stringBuffer.append(EMAIL_TYPE_SEPARATOR);
			stringBuffer.append("preferred");
		}		
		if((emailType&INTERNET_EMAIL_TYPE)!=0)
		{
			if(stringBuffer.length()>0)
				stringBuffer.append(EMAIL_TYPE_SEPARATOR);
			stringBuffer.append("Internet");
		}		
		if((emailType&X400_EMAIL_TYPE)!=0)
		{
			if(stringBuffer.length()>0)
				stringBuffer.append(EMAIL_TYPE_SEPARATOR);
			stringBuffer.append("X.400");
		}		
		return stringBuffer.toString();		
	}

	/**@return A string representation of the email address.*/
	public String toString()
	{
		return getAddress()+" ("+getEmailTypeString()+")";	//return the telephone type appended to the telephone number 
	}
}
