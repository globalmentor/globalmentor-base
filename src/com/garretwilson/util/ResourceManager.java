package com.garretwilson.util;

import java.lang.ref.*;
import java.net.URL;
import java.util.*;

/**Manages resources bundled with an application.
	Loads resources from the class loader of a given class.
	This class keeps weak references to the resources it loads so that they may
	be reused if they have not been garbage collected. This class does have a
	small overhead of references to resources that are no longer used and have
	been garbage collected.
@author Garret Wilson
*/
public class ResourceManager
{

	/**The map of references to resources, each keyed to a filename.*/
	protected static final Map resourceReferenceMap=new HashMap();

	/**The class used for loading resources.*/
	protected final Class loaderClass;

	/**Class constructor.
	@param resourceLoaderClass The class used for loading resources.
	*/
	public ResourceManager(final Class resourceLoaderClass)
	{
		loaderClass=resourceLoaderClass;  //save the class so that we can use it later to load things
	}

	/**Loads a resource. The filename may either be the name of a file
		stored in the same path as the loader class, or the full path to the
		resource file.
		<p>In most cases, a subclass should not override this method, but instead
		override <code>getResource(URL)</code>, which this method calls.</p>
	@param filename The filename of the resource.
	@return An object representing the resource.
	*/
	public Object getResource(final String filename)
	{
		Object resource=null; //we'll try to get the resource if we already have it loaded
		final Reference resourceReference=(Reference)resourceReferenceMap.get(filename); //see if we have a resource reference
		if(resourceReference!=null) //if we have a resource reference
		{
			resource=resourceReference.get(); //get the resource stored in the reference
		}
		if(resource==null)  //if we haven't loaded this resource yet, or the resource has been garage collected
		{
			final URL resourceURL=loaderClass.getResource(filename);  //get the URL of the resource
			resource=getResource(resourceURL);  //load the resource from the URL
			resourceReferenceMap.put(filename, new WeakReference(resource));  //store a weak reference to the resource
		}
		return resource;  //return the resource that we loaded or already had loaded
	}

	/**Retrieves an object from a URL. Each subclass may override this method
		to do custom loading of the resource.
	@param url The URL of the resource.
	@return The resource, loaded from the URL.
	*/
	protected Object getResource(final URL url)
	{
return null;  //G***create default functionality to simply return a byte array
	}

}