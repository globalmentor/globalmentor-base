/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.io;

import java.net.URI;

import com.globalmentor.event.EventListenerManager;
import com.globalmentor.model.Model;

/**A model that keeps track of a base URI and knows how to access input streams
	based upon URIs.
@author Garret Wilson
*/
public abstract class URIAccessibleModel extends DefaultURIAccessible implements Model
{

	/**Whether the object has been modified; the default is not modified.*/
//TODO del if not needed	private boolean modified=false;

		/**@return Whether the object been modified.*/
//TODO del if not needed		public boolean isModified() {return modified;}

		/**Sets whether the object has been modified.
		This is a bound property.
		@param newModified The new modification status.
		*/
/*TODO del if not needed
		public void setModified(final boolean newModified)
		{
			final boolean oldModified=modified; //get the old modified value
			if(oldModified!=newModified)  //if the value is really changing
			{
				modified=newModified; //update the value
					//show that the modified property has changed
				firePropertyChange(MODIFIED_PROPERTY, Boolean.valueOf(oldModified), Boolean.valueOf(newModified));
			}
		}
*/

	/**The manager of the registered event listeners.*/
	private final EventListenerManager eventListenerManager=new EventListenerManager();
	
		/**@return The manager of the registered event listeners.*/
		public EventListenerManager getEventListenerManager() {return eventListenerManager;}

	/**The base URI of the model, or <code>null</code> if unknown.*/
	private final URI baseURI;
	
		/**@return The base URI of the model, or <code>null</code> if unknown.*/
		public URI getBaseURI() {return baseURI;}

	/**Default constructor.*/
	public URIAccessibleModel()
	{
		this((URI)null);
	}

	/**Base URI constructor.
	@param baseURI The base URI, or <code>null</code> if unknown.
	*/
	public URIAccessibleModel(final URI baseURI)
	{
		this(baseURI, null, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URIInputStreamable uriInputStreamable)
	{
		this(null, uriInputStreamable, null);
	}

	/**URI output stream locator constructor.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URIOutputStreamable uriOutputStreamable)
	{
		this(null, uriOutputStreamable);
	}

	/**Base URI and input stream locator constructor.
	@param baseURI The base URI, or <code>null</code> if unknown.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URI baseURI, final URIInputStreamable uriInputStreamable)
	{
		this(baseURI, uriInputStreamable, null);
	}

	/**Base URI and output stream locator constructor.
	@param baseURI The base URI, or <code>null</code> if unknown.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URI baseURI, final URIOutputStreamable uriOutputStreamable)
	{
		this(baseURI, null, uriOutputStreamable);
	}

	/**Full constructor.
	@param baseURI The base URI, or <code>null</code> if unknown.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URI baseURI, final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable)
	{
		super(uriInputStreamable, uriOutputStreamable);
		this.baseURI=baseURI;	//save the base URI
	}
}
