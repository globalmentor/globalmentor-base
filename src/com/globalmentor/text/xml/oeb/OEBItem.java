package com.globalmentor.text.xml.oeb;

//G***del import java.io.InputStream;
import java.awt.Image;
import java.awt.Toolkit;
//G***del if not needed import java.awt.image.ImageObserver;
import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
/*G***bring back as needed
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;
import java.net.MalformedURLException;
import java.net.URL;
import org.w3c.dom.DOMException;
import com.garretwilson.util.StringManipulator;
*/
import javax.mail.internet.ContentType;
import javax.sound.sampled.*;
//G***del when works import com.garretwilson.awt.ImageUtilities;
//G***del import com.garretwilson.io.InputStreamUtilities;
import com.garretwilson.sound.sampled.SampledSoundUtilities;
import com.globalmentor.text.xml.XMLDocument;
import com.globalmentor.text.xml.XMLProcessor;
import com.globalmentor.text.xml.XMLReader;
import com.globalmentor.text.xml.stylesheets.css.XMLCSSProcessor;
import com.globalmentor.util.Debug;

//G***del class when new XPackage stuff works

/**Class which represents an OEB item (OEB document, image, etc.) found in
the manifest section of an OEB package.
@see OEBPublication
*/
public class OEBItem
{

	/**The publication that owns this item. G***eventually remove this and have the calling program determine the URL, for example*/
//G***del	private OEBPublication Publication;

		/**@return The publication that owns this item.*/
//G***del		public OEBPublication getPublication() {return Publication;}

	/**The ID of this item.*/
	private String ID;

		/**@return The ID of this item.*/
		public String getID() {return ID;}

	/**The filename of this item.*/
	private String HRef;

		/**@return The filename of this item.*/
		public String getHRef() {return HRef;}

		/**@return The full URL to this item.
		@exception MalformedURLException Thrown if the URL of the data to load is invalid.
		@see OEBPublication#getURL
		@see #getHRef
		@see #getPublication
		*/
/*G***fix; transfer to publication
		public URL getURL() throws MalformedURLException
		{
			return getPublication().getURL(getHRef());
		}
*/

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

	/**The ID of the item to be used as a fallback.
	@see OEBItem#getID
	*/
//G***del	when works private String Fallback;

		/**@return The ID of this item's fallback item.
		@see OEBItem#getID
		*/
//G***del when works		public String getFallback() {return Fallback;}

	/**The data of this object (a document, an image, etc.) or <code>null</code>
		if the item has not yet been loaded.*/
//G***del	private Object Data=null;

		/**@return Whether or not this item has been loaded.
		@see OEBItem#getData
		*/
//G***del		public boolean isLoaded() {return Data!=null;}  //G***del if getData() is removed

	/**Constructor for creating an OEB item with no fallback.
//G***del	@param publication The publication which owns this item.
	@param id The ID of this item.
	@param href The filename of this item.
	@param newMediaType The media type of this item.
	*/
	public OEBItem(/*G***del final OEBPublication publication, */final String id, final String href, final ContentType newMediaType)
	{
		this(/*G***del publication, */id, href, newMediaType, null); //do the default construction with a null fallback
	}

	/**Constructor.
//G***del	@param publication The publication which owns this item.
	@param id The ID of this item.
	@param href The filename of this item.
	@param newMediaType The media type of this item.
	@param fallback The fallback item, or <code>null</code> for no fallback.
	*/
	public OEBItem(/*G***del final OEBPublication publication, */final String id, final String href, final ContentType newMediaType, final OEBItem fallback)
	{
//G***del		Publication=publication;	//set the owner publication
		ID=id;	//set the ID G***maybe make setXXX() functions for these
		HRef=href;	//set the href
		mediaType=newMediaType;	//set the media type
		Fallback=fallback;	//set the fallback
	}

	/**@return A string representation of this OEBItem.*/
	public String toString()
	{
		return "OEBItem [id: "+getID()+" href: "+getHRef()+" media-type: "+getMediaType()+" fallback: "+(getFallback()!=null ? getFallback().getID() : "none")+"]";	//create a string representation of this item and return it
	}

	/**Opens an input stream to the item. This may be an input stream created
		directly from a URL, or if the publication is inside a zip file, the URL
		will be matched with a zip file entry and an input stream to that entry will
		be returned. The input stream will be open and should be closed after use.
	@return An input stream to the contents of the item.
	@exception MalformedURLException Thrown if the URL of the item is invalid.
	@exception IOException Thrown if an I/O error occurred.
	@see OEBPublication#getInputStream
	@see #getURL
	*/
/*G***del
	public InputStream getInputStream() throws MalformedURLException, IOException
	{
		final URL itemURL=getURL();	//get a complete URL to the HRef location
		return getPublication().getInputStream(itemURL);	//create an input stream to the item and return it
	}
*/

}
