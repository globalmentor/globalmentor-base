package com.garretwilson.text.elff;

import static com.garretwilson.lang.ObjectUtilities.checkInstance;

import java.util.Date;

/**An individual field in an entry of the Extended Log File Format (ELFF).
A field is an identifier with a specified type.
@param <T> The type of Java value the field accepts.
@author Garret Wilson
@see <a href="file:///D:/reference/W3C/ELFF/WD-logfile.html">W3C Extended Log File Format</a>
*/
public class Field<T> extends FieldIdentifier
{

	/**The date at which the transaction completed.*/
	public final static Field<Date> DATE_FIELD=new Field<Date>("date", FieldType.DATE);

	/**The time at which the transaction completed.*/
	public final static Field<Date> TIME_FIELD=new Field<Date>("time", FieldType.TIME);

	/**The time taken for transaction to complete in seconds.*/
	public final static Field<Double> TIME_TAKEN_FIELD=new Field<Double>("time-taken", FieldType.FIXED);
	
	/**The bytes transferred.*/
	public final static Field<Integer> BYTES_FIELD=new Field<Integer>("bytes", FieldType.INTEGER);

	/**Whether a cache hit occurred; 0 indicates a cache miss.*/
	public final static Field<Integer> CACHED_FIELD=new Field<Integer>("cached", FieldType.INTEGER);

	/**The client IP address.*/
	public final static Field<String> CLIENT_IP_FIELD=new Field<String>(FieldIdentifierPrefix.CLIENT, "ip", FieldType.ADDRESS);

	/**The client's username to the server.*/
	public final static Field<String> CLIENT_SERVER_USERNAME_FIELD=new Field<String>(FieldIdentifierPrefix.CLIENT_SERVER, "username", FieldType.STRING);
	
	/**The host accessed by the client.*/
	public final static Field<String> CLIENT_SERVER_HOST_FIELD=new Field<String>(FieldIdentifierPrefix.CLIENT_SERVER, "host", FieldType.STRING);

	/**The method used by the client to the server.*/
	public final static Field<String> CLIENT_SERVER_METHOD_FIELD=new Field<String>(FieldIdentifierPrefix.CLIENT_SERVER, "method", FieldType.STRING);

	/**The URI path accessed by the client.*/
	public final static Field<String> CLIENT_SERVER_URI_STEM_FIELD=new Field<String>(FieldIdentifierPrefix.CLIENT_SERVER, "uri-stem", FieldType.STRING);

	/**The query string accessed by the client.*/
	public final static Field<String> CLIENT_SERVER_URI_QUERY_FIELD=new Field<String>(FieldIdentifierPrefix.CLIENT_SERVER, "uri-query", FieldType.STRING);

	/**The HTTP response status returned to the client.*/
	public final static Field<Integer> SERVER_CLIENT_STATUS_FIELD=new Field<Integer>(FieldIdentifierPrefix.SERVER_CLIENT, "status", FieldType.INTEGER);

	/**The bytes transferred from server to client.*/
	public final static Field<Integer> CLIENT_SERVER_BYTES_FIELD=new Field<Integer>(FieldIdentifierPrefix.SERVER_CLIENT, "bytes", FieldType.INTEGER);

	/**The version of the client accessing the server.*/
	public final static Field<String> CLIENT_SERVER_VERSION_FIELD=new Field<String>(FieldIdentifierPrefix.CLIENT_SERVER, "version", FieldType.STRING);

	/**The user-agent header of the client accessing the server.*/
	public final static Field<String> CLIENT_SERVER_USER_AGENT_HEADER_FIELD=new Field<String>(FieldIdentifierPrefix.CLIENT_SERVER, "User-Agent", true, FieldType.STRING);

	/**The cookie header of the client accessing the server.*/
	public final static Field<String> CLIENT_SERVER_COOKIE_HEADER_FIELD=new Field<String>(FieldIdentifierPrefix.CLIENT_SERVER, "Cookie", true, FieldType.STRING);
	
	/**The referrer header of the client accessing the server.*/
	public final static Field<String> CLIENT_SERVER_REFERER_HEADER_FIELD=new Field<String>(FieldIdentifierPrefix.CLIENT_SERVER, "Referer", true, FieldType.STRING);
	
	/**The WebTrends DCS identification field.*/
	public final static Field<String> DCS_ID_FIELD=new Field<String>(FieldIdentifierPrefix.DCS, "id", FieldType.STRING);

	/**The type of value stored in the field.*/
	private final FieldType type;

		/**@return The type of value stored in the field.*/
		public FieldType getType() {return type;}

	/**Identifier constructor with no prefix.
	@param identifier Identifier of information transferred between parties defined by the value prefix.
	@param type The type of value stored in the field.
	@exception NullPointerException if the given type and/or identifier is <code>null</code>.
	*/
	public Field(final String identifier, final FieldType type)
	{
		this(identifier, false, type);	//construct the field identifier specifying that it is not a header
	}

	/**Identifier/header constructor with no prefix.
	@param identifier Identifier of information transferred between parties defined by the value prefix, or of an HTTP header if this field identifier represents a header.
	@param isHeader <code>true</code> if the identifier represents an HTTP header, else <code>false</code> for normal identifiers.
	@param type The type of value stored in the field.
	@exception NullPointerException if the given type and/or identifier is <code>null</code>.
	*/
	public Field(final String identifier, final boolean isHeader, final FieldType type)
	{
		this(null, identifier, isHeader, type);	//construct the field identifier with no prefix
	}

	/**Prefix and identifier constructor.
	@param prefix The identifier prefix, or <code>null</code> if there is no prefix.
	@param identifier Identifier of information transferred between parties defined by the value prefix.
	@param type The type of value stored in the field.
	@exception NullPointerException if the given type and/or identifier is <code>null</code>.
	*/
	public Field(final FieldIdentifierPrefix prefix, final String identifier, final FieldType type)
	{
		this(prefix, identifier, false, type);	//construct the field identifier specifying that it is not a header
	}

	/**Prefix and identifier/header constructor.
	@param prefix The identifier prefix, or <code>null</code> if there is no prefix.
	@param identifier Identifier of information transferred between parties defined by the value prefix, or of an HTTP header if this field identifier represents a header.
	@param isHeader <code>true</code> if the identifier represents an HTTP header, else <code>false</code> for normal identifiers.
	@param type The type of value stored in the field.
	@exception NullPointerException if the given type and/or identifier is <code>null</code>.
	*/
	public Field(final FieldIdentifierPrefix prefix, final String identifier, final boolean isHeader, final FieldType type)
	{
		super(prefix, identifier, isHeader);	//construct the parent class
		this.type=checkInstance(type, "Type cannot be null.");
	}
}
