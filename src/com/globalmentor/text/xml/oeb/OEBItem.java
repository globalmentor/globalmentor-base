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

package com.globalmentor.text.xml.oeb;

import javax.mail.internet.ContentType;

/**Class which represents an OEB item (OEB document, image, etc.) found in
the manifest section of an OEB package.
@author Garret Wilson
@see OEBPublication
*/
public class OEBItem	//TODO del class when new XPackage stuff works
{

	/**The ID of this item.*/
	private String ID;

		/**@return The ID of this item.*/
		public String getID() {return ID;}

	/**The filename of this item.*/
	private String HRef;

		/**@return The filename of this item.*/
		public String getHRef() {return HRef;}

	/**The media type of this item.*/
	private ContentType mediaType;

		/**@return The media type of this item.*/
		public ContentType getMediaType() {return mediaType;}


	/**The item to be used as a fallback, or <code>null</code> for no fallback.*/
	private OEBItem Fallback=null;

		/**@return The item to be used as a fallback, or <code>null</code> for no fallback.*/
		public OEBItem getFallback() {return Fallback;}

		/**Sets the item to be used as a fallback. This method can only be called by
		  other classes in this package.
		@param newFallback The new item to use as a fallback, or <code>null</code>
			if this item should have no fallback.
		*/
		void setFallback(final OEBItem newFallback) {Fallback=newFallback;}

	/**Constructor for creating an OEB item with no fallback.
	@param id The ID of this item.
	@param href The filename of this item.
	@param newMediaType The media type of this item.
	*/
	public OEBItem(final String id, final String href, final ContentType newMediaType)
	{
		this(id, href, newMediaType, null); //do the default construction with a null fallback
	}

	/**Constructor.
	@param id The ID of this item.
	@param href The filename of this item.
	@param newMediaType The media type of this item.
	@param fallback The fallback item, or <code>null</code> for no fallback.
	*/
	public OEBItem(final String id, final String href, final ContentType newMediaType, final OEBItem fallback)
	{
		ID=id;	//set the ID
		HRef=href;	//set the href
		mediaType=newMediaType;	//set the media type
		Fallback=fallback;	//set the fallback
	}

	/**@return A string representation of this OEBItem.*/
	public String toString()
	{
		return "OEBItem [id: "+getID()+" href: "+getHRef()+" media-type: "+getMediaType()+" fallback: "+(getFallback()!=null ? getFallback().getID() : "none")+"]";	//create a string representation of this item and return it
	}

}
