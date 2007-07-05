package com.garretwilson.mail;

import com.garretwilson.util.StringTemplate;

/**Constant values for mail.
@author Garret Wilson
*/
public class MailConstants
{
	/**The property that contains whether mail debug is turned on.*/
	public final static String MAIL_DEBUG_PROPERTY="mail.debug";
	/**The property that contains the mail from address.*/
	public final static String MAIL_FROM_PROPERTY="mail.from";
	/**The property that contains the mail host name.*/
	public final static String MAIL_HOST_PROPERTY="mail.host";
	/**The template for "mail-<var>protocol</var>.auth".*/
	public final static StringTemplate MAIL_PROTOCOL_AUTH_PROPERTY_TEMPLATE=new StringTemplate("mail.", StringTemplate.STRING_PARAMETER, ".auth");
	/**The template for "mail-<var>protocol</var>.host".*/
	public final static StringTemplate MAIL_PROTOCOL_HOST_PROPERTY_TEMPLATE=new StringTemplate("mail.", StringTemplate.STRING_PARAMETER, ".host");
	/**The template for "mail-<var>protocol</var>.password".*/
	public final static StringTemplate MAIL_PROTOCOL_PASSWORD_PROPERTY_TEMPLATE=new StringTemplate("mail.", StringTemplate.STRING_PARAMETER, ".password");
	/**The template for "mail-<var>protocol</var>.port".*/
	public final static StringTemplate MAIL_PROTOCOL_PORT_PROPERTY_TEMPLATE=new StringTemplate("mail.", StringTemplate.STRING_PARAMETER, ".port");
	/**The template for "mail-<var>protocol</var>.user".*/
	public final static StringTemplate MAIL_PROTOCOL_USER_PROPERTY_TEMPLATE=new StringTemplate("mail.", StringTemplate.STRING_PARAMETER, ".user");
	/**The property that specifies the protocol for mail transport.*/
	public final static String MAIL_TRANSPORT_PROTOCOL_PROPERTY="mail.transport.protocol";

}