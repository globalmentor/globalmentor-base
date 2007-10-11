package com.garretwilson.urf.ploop;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.net.*;

import com.garretwilson.urf.*;
import com.garretwilson.util.DataException;

/**Class for loading and saving a type of Java object to and from a TURF instance.
@param <T> The type to read and write.
@author Garret Wilson
*/
public class PLOOPTURFIO<T> extends AbstractTURFIO<T>
{

	/**Class constructor.
	@param objectClass The class representing the type of object being loaded and saved.
	@exception NullPointerException if the given class is <code>null</code>.
	*/
	public PLOOPTURFIO(final Class<T> objectClass)
	{
		super(objectClass);	//construct the parent class
	}

	/**Reads an object from an input stream using an existing URF instance.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given URF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public T read(final URF urf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		readURF(urf, inputStream, baseURI);	//read URF from the input stream
		final PLOOPURFProcessor ploopProcessor=new PLOOPURFProcessor();	//create a new PLOOP processor
		final T object;
		try
		{
			object=ploopProcessor.getObject(urf, getObjectClass());	//create and retrieve the object from the RDF instance
		}
		catch(final DataException dataException)	//if the data was incorrect
		{
			throw (IOException)new IOException(dataException.getMessage()).initCause(dataException);	//convert the exception into an IO exception			
		}
		catch(final InvocationTargetException invocationTargetException)	//if a Java contructor threw an exception
		{
			throw (IOException)new IOException(invocationTargetException.getMessage()).initCause(invocationTargetException);	//convert the exception into an IO exception			
		}
		if(object==null)	//if there is no resource
		{
			throw new IOException("No object found with type "+getObjectClass()+".");
		}
		return object;	//return the object
	}

	/**Writes an object to an output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param object The object to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public void write(final OutputStream outputStream, final URI baseURI, final T object) throws IOException
	{
/*TODO convert to URF
		final PLOOPRDFGenerator rdfGenerator=new PLOOPRDFGenerator();	//create a new PLOOP RDF generator
		final RDFResource resource;
		try
		{
			resource=rdfGenerator.generateRDFResource(object);	//generate the RDF resource
		}
		catch(final InvocationTargetException invocationTargetException)	//if a Java contructor throw an exception
		{
			throw (IOException)new IOException(invocationTargetException.getMessage()).initCause(invocationTargetException);	//convert the exception into an IO exception			
		}
		writeRDFResource(outputStream, baseURI, resource);	//write the generated RDF resource
*/
	}

}
