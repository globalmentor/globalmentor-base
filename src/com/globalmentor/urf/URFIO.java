package com.globalmentor.urf;

import java.io.*;
import java.net.*;

import com.garretwilson.io.IO;

/**Support for reading or writing a particular type stored in URF.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@param <T> The type to read and write.
@author Garret Wilson
*/
public interface URFIO<T> extends IO<T>
{

	/**Reads a resource from an input stream using an existing URF instance.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given URF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	*/ 
	public T read(final URF urf, final InputStream inputStream, final URI baseURI) throws IOException;

	/**Registers a resource factory to be used to create resources with a type from the specified namespace. If a resource factory is already registered for this namespace, it will be replaced.
	@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
	@param factory The resource factory that will be used to create resources of types from this namespace.
	*/
	public void registerResourceFactory(final URI typeNamespaceURI, final URFResourceFactory factory);

	/**Removes the resource factory being used to create resources with a type from the specified namespace. If there is no resource factory registered for this namespace, no action will be taken.
	@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
	*/
	public void unregisterResourceFactory(final URI typeNamespaceURI);

}
