/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.util;

import java.lang.ref.*;
import java.net.URL;
import java.util.*;

/**
 * Manages resources bundled with an application. Loads resources from the class loader of a given class. This class keeps weak references to the resources it
 * loads so that they may be reused if they have not been garbage collected. This class does have a small overhead of references to resources that are no longer
 * used and have been garbage collected.
 * @author Garret Wilson
 * @deprecated
 */
public class ResourceManager {

	/** The map of references to resources, each keyed to a filename. */
	protected static final Map resourceReferenceMap = new HashMap();

	/** The class used for loading resources. */
	protected final Class loaderClass;

	/**
	 * Class constructor.
	 * @param resourceLoaderClass The class used for loading resources.
	 */
	public ResourceManager(final Class resourceLoaderClass) {
		loaderClass = resourceLoaderClass; //save the class so that we can use it later to load things
	}

	/**
	 * Loads a resource. The filename may either be the name of a file stored in the same path as the loader class, or the full path to the resource file.
	 * <p>
	 * In most cases, a subclass should not override this method, but instead override <code>getResource(URL)</code>, which this method calls.
	 * </p>
	 * @param filename The filename of the resource.
	 * @return An object representing the resource.
	 */
	public Object getResource(final String filename) {
		Object resource = null; //we'll try to get the resource if we already have it loaded
		final Reference resourceReference = (Reference)resourceReferenceMap.get(filename); //see if we have a resource reference
		if(resourceReference != null) { //if we have a resource reference
			resource = resourceReference.get(); //get the resource stored in the reference
		}
		if(resource == null) { //if we haven't loaded this resource yet, or the resource has been garage collected
			final URL resourceURL = loaderClass.getResource(filename); //get the URL of the resource
			resource = getResource(resourceURL); //load the resource from the URL
			resourceReferenceMap.put(filename, new WeakReference(resource)); //store a weak reference to the resource
		}
		return resource; //return the resource that we loaded or already had loaded
	}

	/**
	 * Retrieves an object from a URL. Each subclass may override this method to do custom loading of the resource.
	 * @param url The URL of the resource.
	 * @return The resource, loaded from the URL.
	 */
	protected Object getResource(final URL url) {
		return null; //TODO create default functionality to simply return a byte array
	}

}