package com.garretwilson.text.xml.oeb;

import java.io.*;
/*G***bring back as needed
import java.util.Vector;
*/
import java.util.*;
import java.util.zip.*;
import java.net.MalformedURLException;
import java.net.URL;
/*G***bring back as needed
import org.w3c.dom.DOMException;
import com.garretwilson.util.StringManipulator;
*/
import com.garretwilson.io.InputStreamLocator;
import com.garretwilson.io.MediaType;
import com.garretwilson.net.URLConstants;
import com.garretwilson.net.URLUtilities;
import com.garretwilson.text.xml.*; //G***del all this when everything below is converted to strictly DOM
import com.garretwilson.rdf.*;
import com.garretwilson.util.Debug;
import org.w3c.dom.*;

//G***del all the XMLUndefinedEntityReferenceException throws when we don't need them anymore, in favor of XMLWellFormednessException

//G***fix all the namespaces (e.g. getAttributeNS()); probably add a feature to the XMLProcessor to turn off namespace compliance, but probably go ahead and use namespaces here

/**Class which represents an entire OEB publication. Implements
	<code>InputStreamLocator</code> so that this class can locate files for the
	XML processor in the event that the files are in a zip file, for instance.
@see InputStreamLocator
@see OEBConstants
*/
public class OEBPublication extends DefaultRDFResource /*G***del implements InputStreamLocator, OEBConstants*/
{

	/**Whether or not the publication is open. This value may not reflect any
		resources being open if the publication is not stored in a zip file.
	*/
//G***del	private boolean open=false;

	/**@return Whether or not the publication is open. This value may not reflect
		any	resources actually being open if the publication is not stored in a zip
		file.
	*/
//G***del	public boolean isOpen() {return open;}

	//Storage types
	/**StorageType: Files are stored separately, uncompresed.*/
//G***del	public final static short SEPARATE_FILE_STORAGE=0;
	/**StorageType: Files are stored in a single .zip file.*/
//G***del	public final static short ZIP_STORAGE=1;
	/**StorageType: Files are stored in a single .oeb file.*/
//G***del	public final static short OEB_STORAGE=2;

	/**The method used to store the files of this publication.*/
//G***del	private short StorageType=SEPARATE_FILE_STORAGE;

		/**Returns whether the OEB files are stored separately or in one file.
		@return The method used to store the files of this publication.
		*/
//G***del		public short getStorageType() {return StorageType;}

		/**Sets the method used to store the files of this publication.
		@param storageType The storage type, one of the constants defined in this class.
		*/
//G***del		public void setStorageType(final short storageType) {StorageType=storageType;}

	/**The URL of the package or the main oeb/zip file.*/
//G***del	private URL PublicationURL=null;

		/**@return The URL of the package or the main oeb/zip file.*/
//G***del		public URL getPublicationURL() {return PublicationURL;}

		/**Sets the URL of the package or the main oeb/zip file. Sets the storage
			type to match the URL.
		@param url The URL of this publication.
		@see #setStorageType
		*/
/*G***del
		public void setPublicationURL(final URL url)
		{
			PublicationURL=url;	//set the URL of the publication
			final String filename=url.getFile();	//get a reference to the URL's file component
			if(filename.endsWith(".opf"))	//if this is a package file G***use a constant here
				setStorageType(SEPARATE_FILE_STORAGE);	//show that the publication is stored in several files
			else if(filename.endsWith(".zip"))	//if this is a zip file G***use a constant here
				setStorageType(ZIP_STORAGE);	//show that the publication is stored in one zip file
			else if(filename.endsWith(".oebzip"))	//if this is a zip file G***use a constant here
				setStorageType(ZIP_STORAGE);	//show that the publication is stored in one zip file
			else if(filename.endsWith(".oeb"))	//if this is an oeb file G***use a constant here
				setStorageType(OEB_STORAGE);	//show that the publication is stored in one OEB file
			else	//if we don't know how the file is stored G***fix
				setStorageType(SEPARATE_FILE_STORAGE);	//G***fix for unknown file types
		}
*/

	/**The zip file in which the publication is stored, if a zip file is used.*/
//G***del	private ZipFile Zip=null;

		/**@return The zip file in which the publication is stored, if a zip file is used.*/
//G***del		public ZipFile getZipFile() {return Zip;}

		/**Sets the zip file in which the publication is stored.
		@param zipFile The zip file to use.
		*/
//G***del		protected void setZipFile(final ZipFile zipFile) {Zip=zipFile;}

	/**A map for storing zip entries, each keyed by a URL of what their filename
		would be if uncompressed.*/
//G***del	private Map ZipMap=new HashMap();

		/**@return A map for storing zip entries, each keyed by a URL of what their
			filename would be if uncompressed.*/
//G***del		protected Map getZipMap() {return ZipMap;}

	/**The XML processor used to process XML files.*/
//G***del	private XMLProcessor Processor=null;

		/**@return The XML processor used to process XML files.*/
//G***del		public XMLProcessor getXMLProcessor() {return Processor;}

		/**Sets the processor to be used to process all XML files.
		@param processor The XML processor to use.
		*/
//G***del		protected void setXMLProcessor(final XMLProcessor processor) {Processor=processor;}

	/**The URL of the package.*/
//G***del if we don't need	private URL PackageURL=null;

		/**@return The URL of the package, which may be the same as the publication URL.
		@see #getPublicationURL*/
//G***del if we don't need		protected URL getPackageURL() {return PackageURL;}

		/**Sets the URL of the package, which may be the same as the publication URL.
		@param url The URL of this package.
		@see #setPublicationURL
		*/
//G***del if we don't need		protected void setPackageURL(final URL url) {PackageURL=url;}

	/**The package XML document.*/
//G***del	private XMLDocument Package=null;

		/**@return The package XML document.*/
//G***del		public XMLDocument getPackage() {return Package;}

		/**Sets the package XML document.
		@param newPackage The package XML document.
		*/
//G***del		protected void setPackage(final XMLDocument newPackage) {Package=newPackage;}

	/**The map of manifest items, keyed by ID.
	@see OEBItem
	*/
//G***del	private Map ManifestMap=new HashMap();

		/**@return The map that holds the items in the manifest, keyed by ID.
		@see OEBItem
		*/
//G***del		protected Map getManifestMap() {return ManifestMap;}

		/**Sets a new manifest map.
		@param manifestMap The new manifest map.
		@see OEBItem
		*/
//G***del		protected void setManifestMap(final Map manifestMap) {ManifestMap=manifestMap;}

		/**@return A read-only interator of all available manifest items.*/
/*G***del
		public Iterator getManifestIterator()
		{
			return Collections.unmodifiableCollection(getManifestMap().values()).iterator();  //create an unmodifiable iterator to the collection of manifest items
		}
*/

	/**The map of manifest items, keyed by fully qualified URL.
	@see OEBItem
	*/
//G***del	private Map manifestURLMap=new HashMap();

		/**@return The map that holds the items in the manifest, keyed by fully qualified URL.
		@see OEBItem
		*/
//G***del		protected Map getManifestURLMap() {return manifestURLMap;}

		/**Sets a new manifest map keyed by URL.
		@param newManifestURLMap The new manifest map keyed by URL.
		@see OEBItem
		*/
//G***del		private void setManifestURLMap(final Map newManifestURLMap) {manifestURLMap=newManifestURLMap;}

	/**Puts the specified item in the manifest. If there is already an item in the
		manifest with the same ID, it will be replaced. A URL created from the
		item's HRef will be stored in a separate map.
	@param item The item to add to the manifest.
	@see OEBItem#getID
	*/
/*G***del
	public void putManifestItem(final OEBItem item)
	{
		getManifestMap().put(item.getID(), item);	//put the item in the manifest map
		try
		{
			final URL itemURL=getURL(item.getHRef());	//get a complete URL to the HRef location
			getManifestURLMap().put(itemURL, item);	//store the item in the manifest URL map as well, keyed to its full URL
		}
		catch(MalformedURLException e)	//if we had trouble getting the URL
		{
			Debug.error(e);	//that's not a big problem
		}
	}
*/

	/**Returns an item in the manifest from its ID.
	@param id The ID to search for.
	@return The item from the manifest with the specified ID, or
		<code>null</code> if there is no item with that ID.
	@see OEBItem#getID
	*/
/*G***del
	public OEBItem getManifestItemByID(final String id)
	{
		return (OEBItem)getManifestMap().get(id);	//lookup the item from the manifest map
	}
*/

	/**Returns an item in the manifest from a fully qualified URL.
	@param url The URL to search for.
	@return The item from the manifest with a matching fully qualified URL, or
		<code>null</code> if there is no match.
	@see OEBItem#getHRef
	*/
/*G***del
	public OEBItem getManifestItemByURL(final URL url)
	{
		return (OEBItem)getManifestURLMap().get(url);	//lookup the item from the manifest URL map
	}
*/

	/**Returns an item in the manifest from a string with a relative or absolute reference.
	@param href The relative or absolute reference to the item, which will be
		converted to an absolute URL in order to compare it with each item's fully
		qualified URL.
	@return The item whose fully qualified URL matches the fully qualified
		version of the specified href, or <code>null</code> if there is no match.
	@see #getManifestItemByURL
	@see OEBItem#getHRef
	*/
/*G***del
	public OEBItem getManifestItemByHRef(final String href)
	{
//G***del Debug.trace("Inside OEBPublication.getManifestItemByHRef() for "+href);	//G***del
		try
		{
Debug.trace("Getting absolute URL");
			final URL absoluteURL=getURL(href);	//get the fully qualified URL from the href
//G***del Debug.trace("OEBPublication.getManifestItemByHRef() absolute URL "+absoluteURL);	//G***del
Debug.trace("Getting manifest item by URL");
			return getManifestItemByURL(absoluteURL);	//get a fully qualified URL from the href and use it to lookup an item
		}
		catch(MalformedURLException e)	//if there is an error with the URL
		{
			Debug.error(e);	//log the error
			return null;	//that simply means we can't find the item
		}
	}
*/

	/**The list of spine items.
	@see OEBItem
	*/
//G***del	private List SpineList=new ArrayList();

		/**@return The list that holds the items in the spine.
		@see OEBItem
		*/
//G***del		public List getSpineList() {return SpineList;}

		/**Sets a new spine list.
		@param spineList The new spine list.
		@see OEBItem
		*/
//G***del		protected void setSpineList(final List spineList) {SpineList=spineList;}

	/**Adds the specified item to the spine if it doesn't exist already.
	@param item The item to add to the spine.
	@see OEBItem#getID
	*/
/*G***del
	public void addSpineItem(final OEBItem item)
	{
	  getSpineList().add(item);	//add the item to the spine list
	}
*/

	/**Inserts the specified item to the spine at the given index.
	@param index The index at which to add the spine item.
	@param item The item to add to the spine.
	@see OEBItem#getID
	*/
/*G***del
	public void addSpineItem(final int index, final OEBItem item)
	{
		getSpineList().add(index, item);	//add the item to the spine list at the given index
	}
*/

	/**The list of guides, in the order they are specified in the publication.
	@see OEBGuide
	*/
	private List guideList=new ArrayList();

		/**@return The list of guides, in the order they are specified in the publication.
		@see OEBGuide
		*/
		protected List getGuideList() {return guideList;}

		/**Sets a new list to hold guides.
		@param newGuideList The new list to hold guides.
		@see OEBGuide
		*/
		private void setGuideList(final List newGuideList) {guideList=newGuideList;}

	/**Adds a guide to the list of guides.
	@param oebGuide The guide to add.
	*/
	public void addGuide(final OEBGuide oebGuide)
	{
		getGuideList().add(oebGuide);
	}

	/**Adds a guide to the list of guides at the specified index.
	@param index The index at which the guide should be added.
	@param oebGuide The guide to add.
	*/
	public void addGuide(final int index, final OEBGuide oebGuide)
	{
		getGuideList().add(index, oebGuide);
	}

	/**@return An interator of all available guides.*/
	public Iterator getGuideIterator()
	{
		return getGuideList().iterator(); //return an iterator of the guides
	}

	/**The publication title.*/
//G***del	private String Title="";

		/**@return The publication title.*/
//G***del		public String getTitle() {return Title;}

		/**Sets the publication title.
		@param title The publication title.
		*/
//G***del		public void setTitle(final String title) {Title=title;}

	/**The list of creators.
	@see #DCCreator
	*/
	private List CreatorList=new ArrayList();

		/**@return The list of creators.
		@see #DCCreator
		*/
		public List getCreatorList() {return CreatorList;}

		/**Sets the list of creators..
		@param creatorList The list of creators.
		@see #DCCreator
		*/
		protected void setTitle(final List creatorList) {CreatorList=creatorList;}


	/**Constructs a publication with a reference URI.
	@param referenceURI The reference URI for the new publication.
	@exception IllegalArgumentException Thrown if the provided reference URI is
		<code>null</code>.
	*/
	protected OEBPublication(final String referenceURI) throws IllegalArgumentException
	{
		super(referenceURI);  //construct the parent class
	}

	/**Constructor that specifies the location of the publication.
	@param publicationURL The URL of the package or the main oeb/zip file.
	*/
/*G***del
	public OEBPublication(final URL publicationURL)
	{
		super("urn:fakeid");  //G***fix; delete this constructor eventually
//G***del System.out.println("Creating OEB Publication.");	//G***del
		setPublicationURL(publicationURL);	//set the publication URL
		setXMLProcessor(new XMLProcessor(this));	//create a new XML processor and store a reference to it; tell it that *we* want to determine where it gets its files from, because they may be from a zip file or other archive
//G***del System.out.println("Created OEB Publication.");	//G***Del
	}
*/

	/**Creates a URL from the file location, based on the publication's URL.
	@param href The location, either a URL or a filename, of the file.
	@return A URL representing the specified file.
	@exception IOException Thrown if an I/O error occurred.
	@exception MalformedURLException Thrown if the filename is not a valid filename or URL name.
//G***del	@exception FileNotFoundException Thrown if the given file could not be found.
	@see #getPublicationURL
	@see URLUtilities
	*/
/*G***del
	public URL getURL(final String href) throws MalformedURLException
	{
		return URLUtilities.createURL(getPublicationURL(), href);	//create a URL based upon the location of the package and the given file location
	}
*/

	/**Retrieves the URL of an item.
	@param oebItem The OEB item to which a URL should be returned.
	@return The full URL to the OEB item.
	@exception MalformedURLException Thrown if the URL of the data to load is invalid.
	@see OEBPublication#getURL
	@see OEBItem#getHRef
	*/
/*G***dle
	public URL getURL(final OEBItem oebItem) throws MalformedURLException
	{
		return getURL(oebItem.getHRef());
	}
*/

	/**Returns an input stream from given URL. This may be an input stream created
		directly from a URL, or if the publication is inside a zip file, the URL
		will be matched with a zip file entry and an input stream to that entry will
		be returned. If a URL using the HTTP protocol is not found inside a zip file,
		it will be opened directly from the literal location specified by the URL.
		The input stream will be open and should be closed after use.
	@param url A complete URL to a file.
	@return An input stream to the contents of the file represented by the given URL.
	@exception FileNotFoundException Thrown if the file referenced by the URL
		could not be located.
	@exception IOException Thrown if an I/O error occurred.
	*/
/*G***del
	public InputStream getInputStream(final URL url) throws FileNotFoundException, IOException
	{
Debug.trace("Getting input stream for URL: ", url);	//G***del
		if(getStorageType()==SEPARATE_FILE_STORAGE)	//if files are stored in separate files
		{
//G***del Debug.trace("Opening connection for actual URL: "+url);	//G***del
			return url.openConnection().getInputStream();	//try to open a connection to the URL and return an input stream to that connection
		}
		else if(getStorageType()==ZIP_STORAGE)	//if files are stored in a zip file
		{
//G***del Debug.trace("Looking in zip storage for URL: "+url);	//G***del
			final ZipEntry zipEntry=(ZipEntry)getZipMap().get(url.toString());	//get the zip entry represented by this URL (its string representation, not the actual URL object)
		  if(zipEntry!=null)  //if the file is in the zip file G***what if it's not in the manifest?
			{

	Debug.trace("Found zip entry: ", zipEntry.getName());	//G***del
	//G***del Debug.trace("Zip file is: "+(getZipFile()==null ? "null" : "not null"));	//G***del

				Debug.assert(getZipFile()!=null, "OEBPublication zip file unexpectedly closed.");
				//G***check to make sure the file is really in the zip file, and throw a FileNotFoundException if not
				return getZipFile().getInputStream(zipEntry);	//get an input stream to the zip entry represented by this URL
			}
			else if(URLConstants.HTTP_PROTOCOL.equals(url.getProtocol()))  //if the URL is not in the zip file, but it uses the HTTP protocol G***this is here so that the XML parse can get external DTDs and such from the web, but it would probably be better to place it in a higher-level method
		  {
				return url.openConnection().getInputStream();	//try to open a connection to the URL and return an input stream to that connection
		  }
			else  //if we can't find the file
			{
				throw new FileNotFoundException(url+" cannot be found."); //throw an exception G***i18n
			}
		}
		else	//G***fix for other storage types
		{
			Debug.error("OEBPublication.getInputStream() can't handle storage type of: "+getStorageType());
			return null;	//G***fix
		}
	}
*/

	/**Opens an input stream to an item. This may be an input stream created
		directly from a URL, or if the publication is inside a zip file, the URL
		will be matched with a zip file entry and an input stream to that entry will
		be returned. The input stream will be open and should be closed after use.
	@param oebItem The item for which an input stream should be returned.
	@return An input stream to the contents of the item.
	@exception MalformedURLException Thrown if the URL of the item is invalid.
	@exception IOException Thrown if an I/O error occurred.
	@see #getURL
	*/
/*G***del
	public InputStream getInputStream(final OEBItem oebItem) throws MalformedURLException, IOException
	{
		final URL itemURL=getURL(oebItem);	//get a complete URL to the item
		return getInputStream(itemURL);	//create an input stream to the item and return it
	}
*/

/*G***fix
	protected loadItem(final String id, final String href, final String mediaType)
	{

	}
*/

	/**Loads the OEB package from a package file.
//G***del	@param packageReader A reader with the package information.
	@param packageHRef A filename or URL to the package file.
//G***declare DOM exceptions being thrown
G***comment IOException
	*/
/*G***del
	protected void loadPackage(final String packageHRef) throws IOException
	{

//G***del System.out.println("Getting ready to load package from href: "+packageHRef);	//G***del
//G***del when works		try	//G***testing
		{
//G***del Debug.trace("OEBPackage.loadPackage() getting URL.");	//G***del
			final URL packageURL=getURL(packageHRef);	//get the URL of the package
	//G***del System.out.println("Getting ready to load package from URL: "+packageURL);	//G***del
//G***del when works			final XMLReader packageReader=new XMLReader(getInputStream(packageURL), packageURL);	//create an input stream from the URL and create an XML reader from that
	//G***fix		final XMLReader packageReader=new XMLReader(getPublicationURL());	//create an XML reader from the publication URL and read the package
//G***del Debug.trace("OEBPackage.loadPackage() getting input stream.");	//G***del
			final InputStream packageInputStream=getInputStream(packageURL);	//get an input stream to the package
			try
			{
//G***del when works				setPackage(getXMLProcessor().parseDocument(packageReader));	//parse the package
//G***fix				packageInputStream=getInputStream(packageURL);	//get an input stream to the package
//G***del Debug.trace("OEBPackage.loadPackage() setting package.");	//G***del
				setPackage(getXMLProcessor().parseDocument(packageInputStream, packageURL));	//parse the package
			}
			finally
			{
//G***del Debug.trace("OEBPackage.loadPackage() closing input stream.");	//G***del
				packageInputStream.close();	//close the stream to the package, whatever happened
//G***fix soon				packageReader.close();	//close the package reader, whatever happens
			}
				//G***do a normalize() somewhere here
		}

		//<package>
		final XMLElement packageRoot=getPackage().getDocumentXMLElement();	//get the root of the package
		//XPath: /metadata/dc-metadata/ *
		final XMLNodeList dcMetadataElementList=(XMLNodeList)XPath.evaluateLocationPath(packageRoot,
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_METADATA+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_METADATA_DC_METADATA+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+XPath.WILDCARD_CHAR);
//G***del when works		final XMLNodeList dcMetadataElementList=(XMLNodeList)packageRoot.getElementsByTagPath(new String[]{PKG_ELEMENT_METADATA, PKG_ELEMENT_METADATA_DC_METADATA, "*"});	//get all the DC metadata items
		for(int i=0; i<dcMetadataElementList.getLength(); ++i)	//look at each DC metadata element
		{
			final XMLElement dcMetadataElement=(XMLElement)dcMetadataElementList.item(i);	//get a reference to this DC metadata element
			final String dcMetadataElementName=dcMetadataElement.getNodeName();	//get the name of this DC metadata element
			//<package><metadata><dc-metadata><dc:Title>
			if(dcMetadataElementName.equals(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TITLE))	//if this is the dc:Title element
				setTitle(dcMetadataElement.getText());	//set the title of the publication
			//<package><metadata><dc-metadata><dc:Creator>
			else if(dcMetadataElementName.equals(PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CREATOR))	//if this is the dc:Creator element
				getCreatorList().add(new DCCreator(dcMetadataElement.getText(), dcMetadataElement.getAttributeNS(null, PKG_METADATA_DC_METADATA_DC_CREATOR_ATTRIBUTE_ROLE)));	//create a new creator object and add it to our list
		}

				final Map fallbackMap=new HashMap();  //create a map to be used for storing references to fallbacks
		//XPath: /manifest/item
		final NodeList manifestElementList=(NodeList)XPath.evaluateLocationPath(packageRoot,
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_MANIFEST+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_MANIFEST_ITEM);
		for(int i=0; i<manifestElementList.getLength(); ++i)	//look at each manifest element
		{
			final Element itemElement=(Element)manifestElementList.item(i);	//get a reference to this item in the manifest
//G***del Debug.trace("Found manifest item element: "+itemElement.getAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_ID));	//G***del
//G***del System.out.println("Found manifest item element: "+itemElement.getAttribute(PKG_MANIFEST_ITEM_ATTRIBUTE_ID));	//G***del
			//create a new OEB item without a fallback
			final OEBItem oebItem=new OEBItem(itemElement.getAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_ID), itemElement.getAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_HREF), new MediaType(itemElement.getAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_MEDIA_TYPE)));
			Debug.assert(oebItem!=null, "Invalid OEB Item: "+itemElement.getAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_ID)); //G***del
			putManifestItem(oebItem); //add the item to our manifest map
			if(itemElement.hasAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_FALLBACK)) //if the element has a fallback attribute
				fallbackMap.put(oebItem, itemElement.getAttributeNS(null, PKG_MANIFEST_ITEM_ATTRIBUTE_FALLBACK)); //put the fallback ID in the map, keyed to the item
//G***del					getManifestMap().put(itemElement.getAttribute(PKG_MANIFEST_ITEM_ATTRIBUTE_ID), new OEBItem(this, itemElement.getAttribute(PKG_MANIFEST_ITEM_ATTRIBUTE_ID), itemElement.getAttribute(PKG_MANIFEST_ITEM_ATTRIBUTE_HREF), itemElement.getAttribute(PKG_MANIFEST_ITEM_ATTRIBUTE_MEDIA_TYPE), itemElement.getAttribute(PKG_MANIFEST_ITEM_ATTRIBUTE_FALLBACK)));
		}
			//resolve all the fallbacks
		final Iterator manifestIterator=getManifestMap().values().iterator(); //get an iterator to iterate through the manifest items
		while(manifestIterator.hasNext()) //while there are more items in the manifest
		{
			final OEBItem oebItem=(OEBItem)manifestIterator.next(); //get the next OEB item
			final String fallbackID=(String)fallbackMap.get(oebItem); //get the fallback ID for this OEB item
			if(fallbackID!=null)  //if there is a fallback ID
			{
				final OEBItem fallbackItem=getManifestItemByID(fallbackID); //get the item the fallback ID references
				Debug.assert(fallbackItem!=null, "Invalid fallback ID: "+fallbackID); //G***fix with a real error message
				oebItem.setFallback(fallbackItem);  //set the resolved fallback item
			}
		}
		//XPath: /spine/itemref
		final NodeList spineElementList=(NodeList)XPath.evaluateLocationPath(packageRoot,
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_SPINE+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_SPINE_ITEMREF);
		for(int i=0; i<spineElementList.getLength(); ++i)	//look at each spine element
		{
			final Element itemElement=(Element)spineElementList.item(i);	//get a reference to this item in the spine
//G***del Debug.trace("Found spine element: "+itemElement.getAttributeNS(null, PKG_SPINE_ITEMREF_ATTRIBUTE_IDREF));	//G***del
			//use this item's idref attribute to find the appropriate item in the map and add it to the spine list
			final OEBItem manifestItem=getManifestItemByID(itemElement.getAttributeNS(null, PKG_SPINE_ITEMREF_ATTRIBUTE_IDREF));
//G***del when works					final Object manifestItem=getManifestMap().get(itemElement.getAttribute(PKG_SPINE_ITEMREF_ATTRIBUTE_IDREF));
//G***del or bring back					Debug.assert(manifestItem!=null, "Could not get manifest item "+itemIndex+" with idref attribute \""+itemElement.getAttribute(PKG_SPINE_ITEMREF_ATTRIBUTE_IDREF)+"\"");
			Debug.assert(manifestItem!=null, "Missing spine element: "+itemElement.getAttributeNS(null, PKG_SPINE_ITEMREF_ATTRIBUTE_IDREF)); //G***fix with a real error message
			addSpineItem(manifestItem);	//add the item to the spine list
//G***del					getSpineList().add(manifestItem);	//add the item to the spine list
		}


		//XPath: /guide/reference
		final NodeList guideElementList=(NodeList)XPath.evaluateLocationPath(packageRoot,
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_GUIDE+
			XPath.LOCATION_STEP_SEPARATOR_CHAR+PKG_ELEMENT_GUIDE_REFERENCE);
		for(int i=0; i<guideElementList.getLength(); ++i)	//look at each guide element
		{
			final Element referenceElement=(Element)guideElementList.item(i);	//get a reference to this reference in the guide
		  final String type=referenceElement.getAttributeNS(null, PKG_GUIDE_REFERENCE_ATTRIBUTE_TYPE);  //get the guide type
		  final String title=referenceElement.getAttributeNS(null, PKG_GUIDE_REFERENCE_ATTRIBUTE_TITLE);  //get the guide title
		  final String href=referenceElement.getAttributeNS(null, PKG_GUIDE_REFERENCE_ATTRIBUTE_HREF);  //get the guide href
//G***del Debug.trace("found guide type: "+type+" title: "+title+" href: "+href);
			//create a new OEB guide
			final OEBGuide oebGuide=new OEBGuide(type, title, href); //create a new guide
			addGuide(oebGuide); //add this guide to our list
		}
	}
*/

	/**Loads an OEB publication from a package file.
	@param fileLocation The location, either a URL or a filename, of the package file.
	*/
/*G***fix
	public void load(final String fileLocation)
	{
		load(getInputStream(fileLocation));	//get an input stream for this location and read it
	}
*/

	/**Loads an OEB publication from a package file.
//G***del	@param reader A reader with the package information.
	*/
	//G***add error reporting to this code
/*G***del
	public void load() throws IOException
	{
		setPackage(null);	//unload any package we already have
		getManifestMap().clear(); //remove all the manifest items
		getManifestURLMap().clear(); //remove all the manifest items keyed by URL
		getSpineList().clear(); //remove all the items in the spine
		getGuideList().clear(); //remove all the guides from our list of guides

		String packageHRef=null;	//we'll store an HRef to our package when we find out where it is; right now, we don't have a clue
	//G***check the type of file the URL is, here
//G***del System.out.println("Getting ready to load publication.");
		open();	//open the publication
		if(getStorageType()==ZIP_STORAGE)	//if files are stored in a zip file
		{
			final Enumeration zipEntries=getZipFile().entries();	//get an enumeration of all the entries in the zip file
			while(zipEntries.hasMoreElements())	//while there is another zip entry left
			{
				final ZipEntry zipEntry=(ZipEntry)zipEntries.nextElement();	//get a reference to this zip entry
				final String zipEntryName=zipEntry.getName();	//get a reference to the name of the zip entry
Debug.trace("storing zip entry: "+zipEntryName+" under key: "+URLUtilities.createURL(getPublicationURL(), zipEntryName).toString());	//G***del
				getZipMap().put(URLUtilities.createURL(getPublicationURL(), zipEntryName).toString(), zipEntry);	//store this zip entry in the map, keyed to its full URL (its string representation, not the actual URL object)
				if(packageHRef==null && zipEntryName.endsWith(".opf")) 	//if this zip file claims to be a package file and we haven't found a package file
					packageHRef=zipEntryName;	//show that this is the first package file we found, so we'll take it to be the package file for the zip file
			}
		}
		else if(getStorageType()==SEPARATE_FILE_STORAGE)	//if files are stored in separate files
			packageHRef=getPublicationURL().toString();	//we know that the URL of the publication should be the same as that of the package
		//G***check for other storage types here
		//G***throw an exeception if we can't find a package href

//G***del		final XMLReader packageReader=new XMLReader(getPublicationURL());	//create an XML reader from the publication URL and read the package
//G***del Debug.trace("Getting ready to load package.");
//G***del when works		loadPackage(packageReader);	//load the package file
		loadPackage(packageHRef);	//load the package file
//G***del System.out.println("Loaded package: loaded? "+(getPackage()!=null));


//G***fix System.out.println("Adding the default OEB stylesheet.");	//G***del
//G***fix				xmlDocument.getStyleSheetList().add(new DefaultOEBCSSStyleSheet());	//G***put this somewhere else; perhaps in an OEBEditorKit
						//G***do a normalize() somewhere here
//G***fix System.out.println("Ready to parse stylesheets.");	//G***del
//G***fix				final XMLCSSProcessor cssProcessor=new XMLCSSProcessor();	//G***testing
//G***fix				cssProcessor.parseStyles(xmlDocument);	//G***testing
//G***fix				cssProcessor.applyxStyles(xmlDocument);	//G***testing

//G***fix				final XMLElement xmlRoot=xmlDocument.getDocumentXMLElement();	//get the root of the document G***change to DOM
	}
*/


	/**Opens the publication. If the publication is stored in a zip file, this
		involves opening the zip file. For other types of storage, no action may
		be taken.
	@exception IOException Thrown if there is an error opening the publication.
	*/
/*G***del
	public void open() throws IOException
	{
		if(!isOpen())	//if we're not already open
		{
			open=true;	//show that the publication is open
			if(getStorageType()==ZIP_STORAGE)	//if files are stored in a zip file
			{
				final ZipFile zipFile=new ZipFile(getPublicationURL().getFile());	//create a new zip file from which to read the files
					//G***maybe check here for a problem opening the zip file
				setZipFile(zipFile);	//make a note of which zip file we're using so it can be accessed later (e.g. during processing of an XML file)
			}
		}
	}
*/

	/**Opens the publication, returning whether the publication was already open.
	@return <code>true</code> if the publication was already open.
	@exception IOException Thrown if there is an error opening the publication.
	@see #open
	*/
/*G***del
	public boolean openReturnPreviousOpenStatus() throws IOException
	{
		final boolean publicationWasOpen=isOpen();	//see if the publication is open
		if(!publicationWasOpen)	//if the publication isn't open
			open();	//open the publication
		return publicationWasOpen;	//return whether or not we were already open
	}
*/

	/**Close the publication. This is only really necessary if the publication was
		read from a zip file, and even then once there were no references to the
		publication the zip file would have been closed anyway. However, it is
		still recommended that this function always be explicitly called when finished
		with a publication.
	@except IOException Thrown if an I/O error occurs.
	*/
/*G***del
	public void close() throws IOException
	{
		if(getZipFile()!=null)	//if we have a zip file
		{
			getZipFile().close();	//close the zip file
			setZipFile(null);	//unreference the zip file, freeing it for garbage collection
//G***del Debug.trace("Nulling zip file.");	//G***del
		}
		open=false;	//show that the publication is closed
	}
	//G***add code later that allows the items to dynamically re-open the zip file when they need to
*/


	/**Class that encapsulates Dublin Core creator information in a publication.*/
	public class DCCreator
	{

//G***create constant strings for the roles

		/**The role played by the creator.*/
		private String Role="";

			/**@return The role played by the creator.*/
			public String getRole() {return Role;}

			/**Sets the role played by the creator.
			@param role The role played by the creator.
			*/
			public void setRole(final String role) {Role=role;}

		/**The name of the creator.*/
		private String Name="";

			/**@return The name of the creator.*/
			public String getName() {return Name;}

			/**Sets the name of the creator.
			@param name The name of the creator.
			*/
			public void setName(final String name) {Name=name;}


		/**Constructor that specifies a name and a role.
		@param name The name of the creator.
		@param role The role played by the creator.
		*/
		public DCCreator(final String name, final String role)
		{
			setName(name);	//set the name of the creator
			setRole(role);	//set the role of the creator
		}

	}

}
