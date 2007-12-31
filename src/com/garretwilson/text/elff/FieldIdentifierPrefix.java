package com.garretwilson.text.elff;

import static com.garretwilson.lang.Objects.*;

/**The prefix of a field identifier of an individual field in an entry of the Extended Log File Format (ELFF).
@author Garret Wilson
@see <a href="file:///D:/reference/W3C/ELFF/WD-logfile.html">W3C Extended Log File Format</a>
*/
public enum FieldIdentifierPrefix
{

	/**Client information.*/
	CLIENT("c"),
	/**Server information.*/
	SERVER("s"),
	/**Remote information.*/
	REMOTE("r"),
	/**Client-to-server information.*/
	CLIENT_SERVER("cs"),
	/**Server-to-client information.*/
	SERVER_CLIENT("sc"),
	/**Server-to-remote server information.*/
	SERVER_REMOTE_SERVER("sr"),
	/**Remote-server-to-server information.*/
	REMOTE_SERVER_SERVER("rs"),
	/**WebTrends DCS prefix.*/
	DCS("dcs"),
	/**Application-specific information.*/
	APPLICATION_SPECIFIC("x");

	/**The literal ID character sequence used to format this prefix.*/
	private final String id;

		/**@return The literal ID character sequence used to format this prefix.*/
		public String getID() {return id;}

	/**Prefix ID constructor.
	@param id The literal ID character sequence used to format this prefix.
	@exception NullPointerException if the given ID is <code>null</code>.
	*/
	private FieldIdentifierPrefix(final String id)
	{
		this.id=checkInstance(id, "ID cannot be null.");	//save the ID
	}
}