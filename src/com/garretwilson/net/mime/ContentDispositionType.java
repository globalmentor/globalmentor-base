package com.garretwilson.net.mime;

/**Represents a MIME content disposition type as defined in
	<a href="http://www.rfc-editor.org/rfc/rfc1806.txt">RFC 1806</a>,
	"Communicating Presentation Information in Internet Messages: The Content-Disposition Header".
@author Garret Wilson
@see javax.mail.internet.ContentDisposition
@see http://www.rfc-editor.org/rfc/rfc1806.txt
@author Garret Wilson
*/
public enum ContentDispositionType
{

	/**Indicates a bodypart is intended to be displayed automatically upon display of the message.*/	
	inline,

	/**Indicates a bodypart is separate from the main body of the mail message.*/	
	attachment

}
