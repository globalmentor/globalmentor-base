package com.garretwilson.net;

import static com.garretwilson.net.URIConstants.PORT_SEPARATOR;

import com.globalmentor.util.DefaultComparableNamed;

/**Encapsulates a host and an optional port.
@author Garret Wilson
*/
public class Host extends DefaultComparableNamed<String>
{

	/**The port of the host, or <code>-1</code> if no port is specified.*/
	private final int port;

		/**@return The port of the host, or <code>-1</code> if no port is specified.*/
		public int getPort() {return port;}

	/**Constructs a host from a string.
	@param host A host and optional port in the form <code><var>hostname</var>[:<var>port</var>]</var>.
	@exception IllegalArgumentException if the given host string is not syntactically correct.
	*/
	public Host(final String host) throws IllegalArgumentException
	{
		this(getName(host), getPort(host));	//extract the hostname and optional port and construct the class
	}

	/**Constructs a host from a name and optional port.
	@param name A hostname.
	@param port The port of the host, or <code>-1</code> if no port should be specified.
	*/
	public Host(final String name, final int port)
	{
		super(name);	//construct the parent class with the hostname
		this.port=port;	//save the port
	}

	/**Retrieves a hostname from a string containing a host and optional port.
	@param host The host string to parse.
	@return The name of the host.
	*/
	public static String getName(final String host)
	{
		final int portDelimiterIndex=host.indexOf(PORT_SEPARATOR);	//get the index of the port separator
		return portDelimiterIndex>=0 ? host.substring(0, portDelimiterIndex) : host;	//return the hostname unless there is no port, in which case the entire string is the hostname
	}

	/**Retrieves a port from a string containing a host and optional port.
	@param host The host string to parse.
	@return The port of the host, or <code>-1</code> if no port is specified.
	@exception IllegalArgumentException if the given port is not a valid integer.
	*/
	public static int getPort(final String host) throws NumberFormatException
	{
		final int portDelimiterIndex=host.indexOf(PORT_SEPARATOR);	//get the index of the port separator
		return portDelimiterIndex>=0 ? Integer.valueOf(host.substring(portDelimiterIndex+1)) : -1;	//return the port unless there is no port, in which case return -1
	}

	/**@return A string representation of the host in the form <code><var>hostname</var>[:<var>port</var>]</var>.*/
	public String toString()
	{
		final StringBuilder stringBuilder=new StringBuilder(getName());	//host
		final int port=getPort();	//get the port
		if(port>=0)	//if a port is given
		{
			stringBuilder.append(PORT_SEPARATOR).append(port);	//:port
		}
		return stringBuilder.toString();	//return the string we constructed
	}

	/**If <code>object</code> is another <code>Host</code>, compares the
		hostname and port.
	@param object The object with which to compare this host; should be
		another <code>Host</code>.
	@return <code>true<code> if this host equals a host specified in
		<code>object</code>.
	@see #getName
	@see #getPort
	*/
	public boolean equals(Object object)
	{
		if(object instanceof Host)	//if we're being compared with another host
		{
			return super.equals(object) && getPort()==((Host)object).getPort();	//compare names and ports
		}
		else	//if the object is not a host
		{
			return false;
		}
	}

}
