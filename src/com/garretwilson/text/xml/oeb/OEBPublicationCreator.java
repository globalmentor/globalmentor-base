package com.garretwilson.text.xml.oeb;

import java.awt.Image;
import java.awt.Toolkit;
import java.io.*;
import java.net.*;
import java.text.MessageFormat;
import java.util.*;
import com.garretwilson.awt.ImageUtilities;
import com.garretwilson.io.*;
import com.garretwilson.lang.*;
import com.garretwilson.net.*;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.dublincore.*;
import com.garretwilson.rdf.xpackage.*;
import com.garretwilson.text.*;
import com.garretwilson.text.xml.*;
import com.garretwilson.text.xml.stylesheets.XMLStyleSheetConstants;
import com.garretwilson.text.xml.xhtml.*;
import com.garretwilson.util.*;
import org.w3c.dom.*;
import org.w3c.dom.traversal.*;

/**Creates an OEB publication by gathering all source files from a directory.
	If the tidy option is turned on, each document in the manifest will be loaded,
	tidied, and saved after the original copy is renamed with a ".backup" extension.
	<ul>
	  <li>All manifest items of type GIF are given fallback items of type PNG if
			a PNG file with the same name exists.</li>
	</ul>
	<p>Options:</p>
	<ul>
		<li><code>OUTPUT_DIR_OPTION</code>:
			<ul>
				<li>A <code>File</code> specifying the output directory or <code>null</code>
			  to use the same directory as the source.</li>
			</ul>
		</li>
	</ul>
//G***fix	with an option: Images are loaded and the width and height of their corresponding elements are updated.
@author Garret Wilson
*/
public class OEBPublicationCreator extends TextUtilities implements OEBConstants, OEB2Constants, DCConstants, JavaConstants, FileConstants
{

	/**The English string indicating that the author is not specified.*/
//G***del	protected final static String AUTHOR_NOT_SPECIFIED="(author not specified)";  //G***i18n

	/**A <code>File</code> specifying the output directory or <code>null</code>
		to use the same directory as the source.*/
//G***del if not needed	public final static String OUTPUT_DIR_OPTION="outputDir";

//G***del		/**Default to .*/
//G***del		public final static boolean NORMALIZE_STYLE_CLASSES_OPTION_DEFAULT=true;

	/**Whether the resulting OEB files should be stored in a ZIP file.*/
	public final static String ZIP_OPTION="zip";

		/**Default to not zipping the output.*/
		public final static boolean ZIP_OPTION_DEFAULT=false;

	/**The location of a preface, if any.*/
	public final static String PREFACE_LOCATION_OPTION="preface";

		/**Default to not including a preface.*/
		public final static String PREFACE_LOCATION_OPTION_DEFAULT=null;

	/**The location of a title page, if any.*/
	public final static String TITLE_PAGE_LOCATION_OPTION="titlePage";

		/**Default to not including a title page.*/
		public final static String TITLE_PAGE_LOCATION_OPTION_DEFAULT=null;

	/**Whether only metadata should be gathered, rather than actual conversion.*/
//G***del	public final static String METADATA_ONLY_OPTION="metadataOnly";

		/**Default to not just gathering metadata.*/
//G***del		public final static boolean METADATA_ONLYP_OPTION_DEFAULT=false;

	/**The title of the work, if any.*/
//G***del	public final static String TITLE_OPTION="title";

		/**Default to not having a title.*/
//G***del		public final static String TITLE_OPTION_DEFAULT=null;

	/**The author of the work, if any.*/
//G***del	public final static String AUTHOR_OPTION="author";

		/**Default to not having an author.*/
//G***del		public final static String AUTHOR_OPTION_DEFAULT=null;

	/**The publisher option.*/
	public final static String PUBLISHER_OPTION="publisher";

		/**Default to not including a publisher.*/
		public final static String PUBLISHER_OPTION_DEFAULT=null;

	/**The rights statement option.*/
	public final static String RIGHTS_OPTION="rights";

		/**Default to not including a rights statement.*/
		public final static String RIGHTS_OPTION_DEFAULT=null;

	/**The ID prefix option.*/
//G***del	public final static String ID_PREFIX_OPTION="idPrefix";

		/**Default to a generic ID prefix.*/
//G***del		public final static String ID_PREFIX_OPTION_DEFAULT="urn:file:";

	/**The output directory.*/
	private File outputDir=null;

		/**@return The output directory, or <code>null</code> if the default output
		  directory should be used.*/
		public File getOutputDir() {return outputDir;}

		/**Sets the output directory.
		@param newOutputDir The directory to use for writing files.
		*/
		public void setOutputDir(final File newOutputDir) {outputDir=newOutputDir;}

	/**The context URL.*/
	private URL contextURL=null;

		/**@return The context URL.*/
		public URL getContextURL() {return contextURL;}

		/**Sets the context URL.
		@param newContextURL The URL that serves as a context for relative locations.
		*/
		public void setContextURL(final URL newContextURL) {contextURL=newContextURL;}

	/**The URL of the local title page, if available.*/
	private URL titlePageURL=null;

		/**@return The URL of the local title page, or <code>null</code> if not available.*/
		public URL getTitlePageURL() {return titlePageURL;}

		/**Sets the title page URL.
		@param newTitlePageURL The URL of the local title page.
		*/
		public void setTitlePageURL(final URL newTitlePageURL) {titlePageURL=newTitlePageURL;}

	/**The URL of the local preface, if available.*/
	private URL prefaceURL=null;

		/**@return The URL of the local preface, or <code>null</code> if not available.*/
		public URL getPrefaceURL() {return prefaceURL;}

		/**Sets the preface URL.
		@param newPrefaceURL The URL of the preface page.
		*/
		public void setPrefaceURL(final URL newPrefaceURL) {prefaceURL=newPrefaceURL;}

/*G***del
		final File oebDocumentFile; //we'll find out where to store the document
//G***del Debug.trace("text URL file: ", filename);  //G***del
		if(getOutputDir()!=null) //if an output directory was specified
		{
				//store the file in the output directory G***use a constant
			oebDocumentFile=FileUtilities.changeExtension(new File(getOutputDir(), URLUtilities.getFileName(textURL)), "html");
		}
		else  //if an output directory was not specified
		{
			//G***make sure a file: protocol was specified
			oebDocumentFile=FileUtilities.changeExtension(URLUtilities.getFile(textURL), "html");  //create a file from the URL G***use a constant
		}
*/

	/**Whether this is a Project Gutenberg EText.*/
	private boolean isProjectGutenbergEText=false;

		/**@return Whether this is a Project Gutenberg EText.*/
		protected boolean isProjectGutenbergEText() {return isProjectGutenbergEText;}

		/**Sets whether this is a Project Gutenberg EText.
		@param newProjectGutenbergEText Whether this text is from Project Gutenberg.
		*/
		protected void setProjectGutenbergEText(final boolean newProjectGutenbergEText) {isProjectGutenbergEText=newProjectGutenbergEText;}

	/**The ID of the work.*/
	private String referenceURI=null;

		/**@return The ID of the work, or <code>null</code> if the ID is not
		  known.
		*/
		public String getReferenceURI() {return referenceURI;}

		/**Sets the ID of the work.
		@param newReferenceURI The ID of the work.
		*/
		public void setReferenceURI(final String newReferenceURI) {referenceURI=newReferenceURI;}

	/**The title of the work.*/
	private String title=null;

		/**@return The title of the work, or <code>null</code> if the title is not
		  known.
		*/
		public String getTitle() {return title;}

		/**Sets the title of the work.
		@param newTitle The title of the work.
		*/
		public void setTitle(final String newTitle) {title=newTitle;}

	/**The author of the work.*/
	private String author=null;

		/**@return The author of the work, or <code>null</code> if the author is not
		  known.
		*/
		public String getAuthor() {return author;}

		/**Sets the author of the work.
		@param newAuthor The author of the work.
		*/
		public void setAuthor(final String newAuthor) {author=newAuthor;}

	/**The description of the work.*/
	private String description=null;

		/**@return The descrdiption of the work, or <code>null</code> if there is
		  no description is not known.
		*/
		public String getDescription() {return description;}

		/**Sets the description of the work.
		@param newDescription The description of the work.
		*/
		public void setDescription(final String newDescription) {description=newDescription;}

	/**The publisher of the work.*/
	private String publisher=null;

		/**@return The publisher of the work, or <code>null</code> if the publisher
		  is not known.
		*/
		public String getPublisher() {return publisher;}

		/**Sets the publisehr of the work.
		@param newPublisher The publisher of the work.
		*/
		public void setPublisher(final String newPublisher) {publisher=newPublisher;}

	/**The rights statement of the work.*/
	private String rights=null;

		/**@return The rights statement of the work, or <code>null</code> if the
		  rights statement is not known known.
		*/
		public String getRights() {return rights;}

		/**Sets the rights statement of the work.
		@param newRights The rights statement of the work.
		*/
		public void setRights(final String newRights) {rights=newRights;}

	/**A static application/java media type for quick reference when comparing media types.*/
	protected final static MediaType APPLICATION_JAVA_MEDIA_TYPE=new MediaType(MediaType.APPLICATION, MediaType.JAVA);

	/**A static image/gif media type for quick reference when comparing media types.*/
	protected final static MediaType IMAGE_GIF_MEDIA_TYPE=new MediaType(MediaType.IMAGE, MediaType.GIF);

	/**A static image/png media type for quick reference when comparing media types.*/
	protected final static MediaType IMAGE_PNG_MEDIA_TYPE=new MediaType(MediaType.IMAGE, MediaType.PNG);

	/**A static text/html media type for quick reference when comparing media types.*/
	protected final static MediaType TEXT_HTML_MEDIA_TYPE=new MediaType(MediaType.TEXT, MediaType.HTML);

	/**Whether we should load and tidy each OEB document.*/
	private boolean tidy=false;

		/**@return Whether we should load and tidy each OEB document.*/
		public boolean isTidy() {return tidy;}

		/**Sets whether we should load and tidy each OEB document.
		@param newTidy Whether we should automatically tidy non-well-formed OEB documents.
		*/
		public void setTidy(final boolean newTidy)
		{
			tidy=newTidy; //update the new tidy status
			xmlProcessor=null;  //remove our XML processor so that it will be created again with the correct tidy status
		}

	/**Whether we should zip the resulting OEB files.*/
	private boolean zip=false;

		/**@return Whether we should zip the resulting OEB files.*/
		public boolean isZip() {return zip;}

		/**Sets whether we should zip the resulting OEB files.
		@param newZip Whether we should zip the resulting OEB files.
		*/
		public void setZip(final boolean newZip)
		{
			zip=newZip; //update the new zip status
		}

	/**The location of a preface.*/
	private String prefaceLocation=null;

		/**@return The location of a preface, or <code>null</code> if there should
		  be no preface.
		*/
		public String getPrefaceLocation() {return prefaceLocation;}

		/**Sets the location of a preface.
		@param newPrefaceLocation The location of a preface, or <code>null</code>
		  if there should be no preface.
		*/
		public void setPrefaceLocation(final String newPrefaceLocation)
		{
			prefaceLocation=newPrefaceLocation; //update the new preface location
		}

	/**The location of a title page.*/
	private String titlePageLocation=null;

		/**@return The location of a title page, or <code>null</code> if there should
		  be no title page.
		*/
		public String getTitlePageLocation() {return titlePageLocation;}

		/**Sets the location of a title page.
		@param newTitlePageLocation The location of a title page, or <code>null</code>
		  if there should be no title page.
		*/
		public void setTitlePageLocation(final String newTitlePageLocation)
		{
			titlePageLocation=newTitlePageLocation; //update the new title page location
		}

	/**The RDF data model describing this publication.*/
	private RDF rdf=null;

		/**@return The RDF data model describing this publication.*/
		public RDF getRDF()
		{
			if(rdf==null)  //if there is no RDF data model
			{
				rdf=new RDF();  //create new RDF
				final OEBUtilities oebUtilities=new OEBUtilities(); //create a new OEB utility object
				rdf.registerResourceFactory(OEB1_PACKAGE_NAMESPACE_URI, oebUtilities);  //register a factory for OEB 1.x package resources
				rdf.registerResourceFactory(OEB2_PACKAGE_NAMESPACE_URI, oebUtilities);  //register a factory for OEB 2.x package resources
			}
			return rdf;  //return the RDF data model
		}

	/**The list of stylesheet references.*/
	protected List styleSheetReferenceList=new ArrayList();

		/**Adds a stylesheet reference to the list of references.
		@param href The reference to the stylesheet file.
		*/
		public void addStyleSheetReference(final String href)
		{
			styleSheetReferenceList.add(href);  //add this reference
		}

		/**Adds a list of stylesheet references to the list of references.
		@param hrefList The list of references to stylesheet files.
		*/
		public void addStyleSheetReferences(final List hrefList)
		{
			styleSheetReferenceList.addAll(hrefList); //add the contenst of the list to our stylesheets list
		}

	/**The XML processor used for reading OEB files.*/
	private XMLProcessor xmlProcessor=null;

		/**Retrieves the XML processor, creating one if one has not been created.
		@return The XML processor.
		*/
		public XMLProcessor getXMLProcessor()
		{
			if(xmlProcessor==null)  //if there is no XML processor
			{
				xmlProcessor=new XMLProcessor();	//create a new XML processor
				xmlProcessor.setTidy(isTidy()); //turn tidy on or off in our XML processor
				if(isTidy())  //if tidy is turned on
				{
						//show that we should always use the OEB external ID
					xmlProcessor.setTidyDocumentTypeExternalID(OEB101_DOCUMENT_PUBLIC_ID, OEB101_DOCUMENT_SYSTEM_ID);
				}
			}
			return xmlProcessor;  //return the XML processor
		}

	/**The XML serializer used for writing OEB files.*/
	private XMLSerializer xmlSerializer=null;

		/**Retrieves the XML serializer, creating one if one has not been created.
		@return The XML serializer.
		*/
		public XMLSerializer getXMLSerializer()
		{
			if(xmlSerializer==null)  //if there is no XML serializer
				xmlSerializer=new XMLSerializer(getOptions());  //create a class to serialize XML, using whatever options are set
			return xmlSerializer;  //return the XML serializer
		}

//G***probably eventuallly make this a non-static class so that a common XML processor can be used, instead of creating one in each function
//G***note that this would also allow a tidy variable to be set, so that documents could be tidied and manifest items gathered in the same iteration, rather than loading documents twice

	/**The properties which govern how we behave.*/
	protected Properties options;

		/**Sets the options that govern how OEB publication creation will occur. These
			options are used locally and are also passed to other objects such as
			<code>XMLSerializer</code>. The specific options passed are therefore not
			only applicable to the <code>OEBPublicationCreator</code> object, but may
			contain options meant for other classes used by this class.
			The options are copied before being stored, so changing the original
			properties later will not affect the behavior here.
		@param newOptions The options that govern how this class behaves, or
			<code>null</code> if options should be reset.
		*/
		public void setOptions(final Properties newOptions)
		{
			//clone the options and store them, or if null was passed just create default options
			options=newOptions!=null ? (Properties)newOptions.clone() : new Properties();
			setZip(PropertyUtilities.getBooleanProperty(options, ZIP_OPTION, ZIP_OPTION_DEFAULT));
			setPrefaceLocation(options.getProperty(PREFACE_LOCATION_OPTION, PREFACE_LOCATION_OPTION_DEFAULT));
			setTitlePageLocation(options.getProperty(TITLE_PAGE_LOCATION_OPTION, TITLE_PAGE_LOCATION_OPTION_DEFAULT));
			setPublisher(options.getProperty(PUBLISHER_OPTION, PUBLISHER_OPTION_DEFAULT));
			setRights(options.getProperty(RIGHTS_OPTION, RIGHTS_OPTION_DEFAULT));
//G***del			setIDPrefix(options.getProperty(ID_PREFIX_OPTION, RIGHTS_OPTION_DEFAULT));
//G***del			setMetadataOnly(PropertyUtilities.getBooleanProperty(options, METADATA_ONLY_OPTION, METADATA_ONLY_OPTION_DEFAULT));
		}

		/**@return The options that govern the behavior of this class.*/
		protected Properties getOptions() {return options;}

	/**Default constructor.*/
  public OEBPublicationCreator()
	{
		this(null); //create a publication creator with default options
	}

	/**Constructs a publication creator with specific options.
	@param newOptions The options that govern how this class behaves.
	*/
  public OEBPublicationCreator(final Properties newOptions)
	{
		setOptions(newOptions); //set the options
	}

	/**Searches a directory for relevant files and returns an OEB publication
		based on those files. Optionally tidies each OEB document and gathers all
		references to files.
	@param directory The base directory in which the publication files are located.
	@exception MalformedURLException Thrown if there is an error creating a URL
		from the directory.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	public OEBPublication createPublicationFromDirectory(final File directory) throws MalformedURLException, IOException
	{
/*G***fix new publication
Debug.trace("OEBPublicationCreator.createPublication() directory: ", directory);
		final File canonicalDirectory=directory.getCanonicalFile(); //convert the directory into a canonical directory (i.e. "c:\\temp\\.\\.\\." will be changed to "c:\\temp")
			//create a new OEB publication using the directory as the publication URL; this will later be changed to the actual name of the OEB publication file
		final OEBPublication publication=new OEBPublication(canonicalDirectory.toURL());
		gatherManifestItems(publication, canonicalDirectory); //gather the manifest items from this directory
		processManifestItems(publication);  //process the manifest items
		return publication; //return the publication we created
*/
		return null;  //G***fix
	}

	/**Creates a publication with the given OEB file.
	Optionally tidies each OEB document and gathers all references to files.
	@param oebFile The OEB file which should be included in the publication.
	@exception MalformedURLException Thrown if there is an error creating a URL
		from the directory.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	public OEBPublication createPublicationFromOEBFile(final File oebFile) throws MalformedURLException, IOException
	{
/*G***fix new publication
Debug.trace("OEBPublicationCreator.createPublication() file: ", oebFile);
		final File canonicalDirectory=oebFile.getCanonicalFile(); //convert the directory into a canonical directory (i.e. "c:\\temp\\.\\.\\." will be changed to "c:\\temp")
			//create a new OEB publication using the directory as the publication URL; this will later be changed to the actual name of the OEB publication file
		final OEBPublication publication=new OEBPublication(canonicalDirectory.toURL());
		final String fileRelativePath=oebFile.getName();  //get the filename of the OEB item G***probably later make this relative to the publication; now we're assuming it's in the same directory as the publication; probably add another parameter with the publication name, both here and above
		  //create an OEB item with no fallback for this document; use the URL for an ID, after converting it to an XML name
	  final OEBItem oebItem=new OEBItem(publication, XMLUtilities.createName(fileRelativePath), fileRelativePath, OEB10_DOCUMENT_MEDIA_TYPE);
		publication.putManifestItem(oebItem); //add the item to the publication's manifest G***change to gathering this manifest item
		publication.addSpineItem(oebItem);  //add the item to the spine
		processManifestItems(publication);  //process the manifest items
//G***maybe add		sortSpineItems(publication);  //sort the publication's spine
		return publication; //return the publication we created
*/
		return null;  //G***fix
	}

	/**Creates a publication with the given OEB document.
	  Optionally tidies each OEB document and gathers all references to files. G***why "optionally?"
	@param oebDocumentURL The URL to the OEB document which should be included
		in the publication.
	@param referenceURI The identifying URI of the publication.
	@param outputDir The output directory to use for writing files, or
		<code>null</code> if the default should be used.
	@return A new publication constructed from the OEB document.
	@exception MalformedURLException Thrown if there is an error creating a URL
		from the directory. G***fix comment
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	public OEBPublication createPublicationFromOEBDocument(final URL oebDocumentURL, final String referenceURI, final File outputDir) throws MalformedURLException, IOException
	{
		if(outputDir!=null) //if an output directory is specified
			setOutputDir(outputDir.getCanonicalFile());  //set the output directory G***should we make sure this directory exists, and create it if  not?
Debug.trace("OEBPublicationCreator.createPublication() document: ", oebDocumentURL);
			//create a new OEB publication using the OEB document URL as the publication URL; this will later be changed to the actual name of the OEB publication file
		final OEBPublication publication=(OEBPublication)OEBUtilities.createOEBPublication(getRDF(), referenceURI); //create a publication
			//store the publication reference URI as a Dublin Core identifier property
		RDFUtilities.addProperty(getRDF(), publication, DCMI11_ELEMENTS_NAMESPACE_URI, DC_IDENTIFIER_PROPERTY_NAME, publication.getReferenceURI());


//G***fix; moved to text converter		final RDFResource oebItem=gatherReference(publication, oebDocumentURL, oebDocumentURL.getFile(), OEB10_DOCUMENT_MEDIA_TYPE);  //add this document to the manifest
/*G***fix
		processManifestItems(publication);  //process the manifest items
		sortSpineItems(publication);  //sort the publication's spine
*/
Debug.trace("OEBPublicationCreator zip option: ", new Boolean(isZip())); //G***del
		return publication; //return the publication we created
	}












	/**Creates a publication from the given text document. The resulting OEB
		document is saved.
	  Tidies each OEB document and gathers all references to files. If the file
		is a Project Gutenberg text file, it is processed as such, and the
		title and author propertiers, if located, are added as metadata.
	@param textURL The URL to the tetx document which should be included
		in the publication.
	@param referenceURI The identifying URI of the publication.
	@param encoding The encoding to use when reading the text file.
	@param outputDir The output directory to use for writing files, or
		<code>null</code> if the default should be used.
	@return A new publication constructed from the text document.
	@exception MalformedURLException Thrown if there is an error creating a URL
		from the directory. G***fix comment
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	public OEBPublication createPublicationFromText(final URL textURL, String referenceURI, final String encoding, final File outputDir) throws MalformedURLException, IOException
	{
Debug.trace("creating publication from text file: ", textURL);  //G***del
		if(outputDir!=null) //if an output directory is specified
			setOutputDir(outputDir.getCanonicalFile());  //set the output directory G***should we make sure this directory exists, and create it if  not?
		final Document document=OEBUtilities.createDefaultOEB1Document();  //create a default OEB document
Debug.trace("created default OEB document");  //G***del
		final InputStream textInputStream=textURL.openConnection().getInputStream();		//connect to the URL and get an input stream G***check for proxying and such
		try
		{
				//G***maybe use a shared XHTMLCreator
		  new XHTMLCreator().createXHTMLFromText(document, textInputStream, encoding);  //convert the text to XHTML
			document.normalize(); //normalize the document so that all the consecutive text regions will be combined
//G***del Debug.trace("converted text to XHTML: ", XMLUtilities.toString(document));  //G***del
			//G***del XMLUtilities.printTree(itemDocument, Debug.getOutput()); //G***testing
		}
		finally
		{
			textInputStream.close();  //always close the input stream
		}
Debug.trace("checking for PG"); //G***del
//G***del		String title=null;  //assume we won't find a title
//G***del		String author=null; //assume we won't find an author
		Locale languageLocale=null; //assume we won't find a language G***fix to be consistent with title and author
		final DocumentFragment pgHeaderFragment;  //we'll attempt to find a Project Gutenberg header, if applicable
		setProjectGutenbergEText(PGUtilities.isProjectGutenbergEText(document)); //see if this document is a Project Gutenberg EText
		if(isProjectGutenbergEText()) //if this document is a Project Gutenberg EText
		{
Debug.trace("is PG EText"); //G***del
//G***fix				new XHTMLTidier().tidy(document); //G***testing
			pgHeaderFragment=PGUtilities.extractHeader(document);  //extract the Project Gutenberg header
			if(pgHeaderFragment!=null)  //if we found a header
			{
				referenceURI=PGUtilities.getID(referenceURI); //convert this URI into a Project Gutenberg identifier by removing the version number
Debug.trace("found PG header"); //G***del
				setTitle(PGUtilities.getTitle(pgHeaderFragment));  //get the title
//G***del					Debug.notify("Title: "+title);  //G***del
				setAuthor(PGUtilities.getAuthor(pgHeaderFragment, getTitle()));  //get the author
//G***del					Debug.notify("Author: "+author);  //G***del
				setDescription(PGUtilities.getDescription(pgHeaderFragment));  //get the description
Debug.trace("description: ", getDescription()); //G***del
				final String displayLanguage=PGUtilities.getLanguage(pgHeaderFragment);  //get the language
Debug.trace("display language: ", displayLanguage); //G***del
				languageLocale=LocaleUtilities.createDisplayLanguageLocale(displayLanguage); //get the locale for this language
Debug.trace("display locale: ", languageLocale); //G***del
			}
			PGUtilities.extractFooter(document);  //extract the Project Gutenberg footer, if it is available
		}
		else  //if this is not a Project Gutenberg EText
		{
			pgHeaderFragment=null;  //show that there is no Project Gutenberg header
		}
		if(getTitle()==null)  //if we haven't found a title so far
			setTitle(getTitle(document)); //try to get the title from the text
		if(getAuthor()==null) //if we haven't found an author so far
		  setAuthor(getAuthor(document)); //try to get the author from the text
		setReferenceURI(referenceURI);  //save the reference URI in case we need it later




//G***fix				new XHTMLTidier().tidy(document); //G***testing

	if(isTidy())  //if we should tidy
	{
				//G***eventually use the tidy() method, and remove the addStylesheetReferences() call which will then be unecessary
		final XHTMLTidier xhtmlTidier=new XHTMLTidier(getOptions());  //create a new XHTML tidier G***use a common XHTML tidier
		xhtmlTidier.setTitle(getTitle()); //tell the tidier the title, if we know it
		xhtmlTidier.tidy(document/*G***fix, outputFile*/);  //tidy the document, specifying where any CSS file should be written to
	}

		addStylesheetReferences(document);  //add any stylesheet references to the document
		final File oebDocumentFile; //we'll find out where to store the document
/*G***del
Debug.trace("text URL: ", textURL); //G***del
Debug.trace("text URL path: ", textURL.getPath()); //G***del
Debug.trace("text URL ref: ", textURL.getRef()); //G***del
*/
//G***del Debug.trace("text URL file: ", filename);  //G***del
		if(getOutputDir()!=null) //if an output directory was specified
		{
				//store the file in the output directory G***use a constant
			oebDocumentFile=FileUtilities.changeExtension(new File(getOutputDir(), URLUtilities.getFileName(textURL)), "html");
		}
		else  //if an output directory was not specified
		{
			//G***make sure a file: protocol was specified
			oebDocumentFile=FileUtilities.changeExtension(URLUtilities.getFile(textURL), "html");  //create a file from the URL G***use a constant
		}
Debug.trace("using file for document: ", oebDocumentFile);
		  //G***testing
		setContextURL(oebDocumentFile.toURL()); //show that the location of the document will be the context URL
			//create a publication from our new OEB document
		final OEBPublication publication=createPublicationFromOEBDocument(oebDocumentFile.toURL(), referenceURI, getOutputDir());
		if(getTitle()!=null) //if we have a title
		  DCUtilities.addTitle(getRDF(), publication, getTitle()); //add the title to the publication
		if(getAuthor()!=null) //if we have an author
		  DCUtilities.addCreator(getRDF(), publication, getAuthor()); //add the author to the publication
		if(getDescription()!=null) //if we have a description
		  DCUtilities.addDescription(getRDF(), publication, getDescription()); //add the description to the publication
		if(languageLocale!=null) //if we have a language
		  DCUtilities.addLanguage(getRDF(), publication, languageLocale); //add the language to the publication
		DCUtilities.addDate(getRDF(), publication, new Date());  //add the current date and time
		DCUtilities.addSource(getRDF(), publication, "file:"+URLUtilities.getFileName(textURL));  //add the text filename as the source G***use a constant
		if(isProjectGutenbergEText) //if we got this text from Project Gutenberg
		{
			DCUtilities.addContributor(getRDF(), publication, "Project Gutenberg");  //add Project Gutenberg as a contributor G***use a constant
		}
		if(getPublisher()!=null)  //if we have a publisher
		  DCUtilities.addPublisher(getRDF(), publication, getPublisher()); //add the publisher to the publication
		if(getRights()!=null)  //if we have a rights statement
		  DCUtilities.addRights(getRDF(), publication, getRights()); //add the rights statement to the publication
		if(pgHeaderFragment!=null)  //if we have a Project Gutenberg header, write it to its own file
		{
			final Document pgHeaderDocument=OEBUtilities.createOEB1Document(pgHeaderFragment);  //create a document from the header fragment
		  final Element bodyElement=XHTMLUtilities.getBodyElement(pgHeaderDocument);  //get the body of the document
				//create a header element and add it to the body
			final Element headerElement=XMLUtilities.createElement(pgHeaderDocument, bodyElement.getNamespaceURI(), XHTMLConstants.ELEMENT_H2, "Information from the Original Project Gutenberg EText");  //G***use a constant
		  bodyElement.insertBefore(headerElement, bodyElement.getFirstChild()); //insert the header element as the first element in the body
/*G***del
		  final String pgHeaderDocumentFilename=FileUtilities.changeExtension(
					new File(FileUtilities.removeExtension(URLUtilities.getFileName(textURL))+"-pgheader"),
					"html").getName();  //create a filename for the header document, in the form "filename-pgheader.html" G***use constants here
*/
		  final String pgHeaderDocumentFilename="projectgutenbergheader.html";  //create a filename for the header document G***use constants here
			final File pgHeaderDocumentFile; //we'll find out where to store the header document
			if(getOutputDir()!=null) //if an output directory was specified
			{
					//store the file in the output directory
				pgHeaderDocumentFile=new File(getOutputDir(), pgHeaderDocumentFilename);
			}
			else  //if an output directory was not specified
			{
				pgHeaderDocumentFile=new File(oebDocumentFile, pgHeaderDocumentFilename);  //create a file from the OEB file G***does this really work?
			}
			write(pgHeaderDocument, pgHeaderDocumentFile); //write the header document
				//add this document to the manifest without adding it to the spine
		  gatherReference(publication, pgHeaderDocumentFile.toURL(), pgHeaderDocumentFile.toString(), OEB10_DOCUMENT_MEDIA_TYPE, false);
		}
			//add a preface if needed
		final String prefaceLocation=getPrefaceLocation();  //see if we were given a preface to add
		if(prefaceLocation!=null) //if we were given a preface to add
		{
		  final URL prefaceURL=useTemplate(URLUtilities.createURL(null, prefaceLocation));  //use the given preface template
		  setPrefaceURL(prefaceURL);  //save the URL to the preface
				//add the preface to the manifest without adding it to the spine
		  final RDFResource prefaceResource=gatherReference(publication, prefaceURL, prefaceURL.getFile(), OEB10_DOCUMENT_MEDIA_TYPE, false);
				//add the preface as the first element in the spine
		  XPackageUtilities.getOrganization(publication).add(getRDF(), prefaceResource, 1);
		}
			//add a title page if needed
		final String titlePageLocation=getTitlePageLocation();  //see if we were given a title page to add
		if(titlePageLocation!=null) //if we were given a title page to add
		{
		  final URL titlePageURL=useTemplate(URLUtilities.createURL(null, titlePageLocation));  //use the given template
		  setTitlePageURL(titlePageURL);  //save the URL to the title page
				//add the title page to the manifest without adding it to the spine
		  final RDFResource titlePageResource=gatherReference(publication, titlePageURL, titlePageURL.getFile(), OEB10_DOCUMENT_MEDIA_TYPE, false);
				//add the title page  as the first element in the spine
		  XPackageUtilities.getOrganization(publication).add(getRDF(), titlePageResource, 1);
		}
			//add the stylesheets to the publication
		  //G***fix to gather the stylesheet references into the manifest automatically through gatherReferences()
		final Iterator stylesheetReferenceIterator=styleSheetReferenceList.iterator();  //get an iterator to look through the stylesheet references
		while(stylesheetReferenceIterator.hasNext())  //while there are more references
		{
			final String href=(String)stylesheetReferenceIterator.next(); //get the next reference
				//add the stylesheet to the manifest without adding it to the spine
		  gatherReference(publication, URLUtilities.createURL(getContextURL(), href), href, OEB10_CSS_MEDIA_TYPE, false);
		}
//G***del		if(isTidy()) //if we're tidying G***fix with some other type of test
		gatherGuides(publication, document);  //gather guides from the document
		write(document, oebDocumentFile); //write the document after we've done everything else, because we might need to modify it along the way
		  //fix; tidy; make it work with createOEBPublication(), which will need it added back at some point
		gatherReference(publication, oebDocumentFile.toURL(), oebDocumentFile.toURL().getFile(), OEB10_DOCUMENT_MEDIA_TYPE);  //add this document to the manifest
		return publication; //return the publication
	}

	/**Searches a directory for relevant files and returns an OEB publication
		based on those files.
	@param publication The publication to which the manifest items should be added.
	@param directory The base directory in which the publication files are located.
	@exception MalformedURLException Thrown if there is an error creating a URL
		for an item.
	@exception IOException Thrown if there is an error reading files from the
		directory.
	*/
	protected void gatherManifestItems(final OEBPublication publication, final File directory) throws MalformedURLException, IOException
	{
/*G***fix new publication

		final URL directoryURL=directory.toURL(); //create a URL from the directory, so that we can create a relative path for each file
//G***del Debug.trace("Directory URL: "+directoryURL);
//G***del		final URL directoryURL=directory.toURL(); //create a URL from the directory, so that we can create relative URLs from each file
		final File[] fileList=directory.listFiles(new FileFilter()
			  {
*/
					/**@return <code>true</code> if the pathname ends in ".htm" or ".html".*/
/*G***fix new publication
					public boolean accept(File pathname)
					{
						return pathname.toString().endsWith(".htm") || pathname.toString().endsWith(".html");
					}
			  }); //get a list of HTML files
		for(int i=0; i<fileList.length; ++i)  //look at each file in the list
		{
			final File file=fileList[i].getCanonicalFile();  //get a reference to this file, after changing it to a canonical file
Debug.trace("looking at publication file: ", file);
		  final URL fileURL=file.toURL(); //convert the file to a URL so that we can create a relative path with a correct href format
		  final String fileRelativePath=URLUtilities.getRelativePath(directoryURL, fileURL);  //create a relative path for the file
Debug.trace("Relative path: ", fileRelativePath);  //G***fix
		  final MediaType mediaType=OEB10_DOCUMENT_MEDIA_TYPE;  //assume this is an OEB document G***remove this variable and use the value directly
		  //create an OEB item with no fallback for this document; use the URL for an ID, after converting it to an XML name
//G***del		  final OEBItem oebItem=new OEBItem(publication, XMLUtilities.createName(fileURL.toString()), fileURL.toString(), mediaType);
		  final OEBItem oebItem=new OEBItem(publication, XMLUtilities.createName(fileRelativePath), fileRelativePath, mediaType);
//G***del		  final OEBItem oebItem=new OEBItem(publication, XMLUtilities.createName(file.getName()), file.toString(), mediaType);
			publication.putManifestItem(oebItem); //add the item to the publication's manifest
			publication.addSpineItem(oebItem);  //add the item to the spine
		}
		sortSpineItems(publication);  //sort the publication's spine
*/
	}

	/**Iterates through the manifest items, optionally tidies the document, and
		gathers references.
	@param publication The publication whose manifest documents should be processed.
	@see #isTidy
	@exception MalformedURLException Thrown if there is an error creating a URL
		from an item.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected void processManifestItems(final OEBPublication publication) throws MalformedURLException, IOException
	{
//G***del		final XMLProcessor xmlProcessor=getXMLProcessor();	//get the XML processor
/*G***del
		xmlProcessor.setTidy(isTidy()); //turn tidy on or off in our XML processor
		if(isTidy())  //if tidy is turned on
		{
				//show that we should always use the OEB external ID
			xmlProcessor.setTidyDocumentTypeExternalID(OEB101_DOCUMENT_PUBLIC_ID, OEB101_DOCUMENT_SYSTEM_ID);
		}
*/
//G***del		final XMLSerializer xmlSerializer=getXMLSerializer();  //get the object to serialize XML

/*G***fix new publication

		{ //gather references and tidy the manifest items
				//create a separate list of the items in the manifest, so that when adding items to the manifest we won't conflict with our iterator
			final List manifestList=new ArrayList(publication.getManifestMap().values());
			final Iterator manifestIterator=manifestList.iterator(); //get an iterator to iterate through the manifest items
	//G***del when works		final Iterator manifestIterator=publication.getManifestMap().values().iterator(); //get an iterator to iterate through the manifest items
			while(manifestIterator.hasNext()) //while there are more items in the manifest
			{
				final OEBItem oebItem=(OEBItem)manifestIterator.next(); //get the next OEB item
				processManifestItem(publication, oebItem);  //process this item
			}
		}
		{ //prepare the fallback items for each manifest item
			final List manifestList=new ArrayList(publication.getManifestMap().values()); //create a separate list of the items in the manifest, so that when adding items to the manifest we won't conflict with our iterator
			final Iterator manifestIterator=manifestList.iterator(); //get an iterator to iterate through the manifest items
			while(manifestIterator.hasNext()) //while there are more items in the manifest
			{
				final OEBItem oebItem=(OEBItem)manifestIterator.next(); //get the next OEB item
				gatherFallbacks(publication, oebItem);  //gather fallbacks for this item, if needed
			}
		}
*/
	}

	/**Gathers references and optionally tidies this OEB item, if applicable.
	@param publication The publication tow which the OEB item belongs.
	@param oebItem The item being processed.
	@see #isTidy
	@exception MalformedURLException Thrown if there is an error creating a URL
		from the item.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected void processManifestItem(final OEBPublication publication, final OEBItem oebItem) throws MalformedURLException, IOException
	{
/*G***fix new publication
		if(oebItem.getMediaType().equals(OEB10_DOCUMENT_MEDIA_TYPE)) //if this is an OEB document
		{
			final URL itemURL=oebItem.getURL(); //get the URL to the item
Debug.trace("Looking at item URL: ", itemURL);
Debug.trace("Output directory: ", getOutputDir());
				  //G***un-indent
				final File outputFile;  //we'll store here a reference to the file to write
				final File outputDir=getOutputDir();  //get the output directory if one is specified
				if(outputDir!=null) //if an output directory was specified
				{
Debug.trace("publication URL: ", publication.getPublicationURL());  //G***del
Debug.trace("item URL: ", itemURL);  //G***del
					final String fileRelativePath=URLUtilities.getRelativePath(publication.getPublicationURL(), itemURL); //get the file's relative path
Debug.trace("File relative path: ", fileRelativePath);  //G***del
				  outputFile=new File(outputDir, fileRelativePath); //the file will be relative to the original URL, yet in our output directory
				}
				else  //if an output directory was not specified
				{
				//G***what if we're not tidying; shouldn't we still gather references?
					outputFile=new File(itemURL.getPath());  //create a file from the URL
				}


			final InputStream itemInputStream=itemURL.openConnection().getInputStream();		//connect to the URL and get an input stream
			Document itemDocument;  //we'll store the document here
			try
			{
				itemDocument=getXMLProcessor().parseDocument(itemInputStream, itemURL);	//parse the document
//G***del XMLUtilities.printTree(itemDocument, Debug.getOutput()); //G***testing
			}
			finally
			{
				itemInputStream.close();  //always close the input stream
			}
			//G***do a normalize() somewhere here
			if(isTidy())  //if we should tidy this item
			{
				tidyDocument(publication, itemURL, itemDocument, outputFile); //tidy the document
			}
Debug.trace("ready to gatherReferences() for item: ", oebItem.getHRef());
				//Gather items referenced in this document, which will also make
				//  changes in the document if tidy is turned on, such as updating
				//  image dimensions.
			gatherReferences(publication, itemURL, itemDocument);
		}
*/
	}

	/**Searches through an OEB document and retrieves the items that should go in
		the manifest. This includes embedded images, applets, and local link targets.
		Currently, the following elements are searched:
		<ul>
		  <li><code>&lt;a&gt;</code></li>
		  <li><code>&lt;img&gt;</code></li>
		  <li><code>&lt;object&gt;</code></li>
		</ul>
	@param publication The publication to which the item belongs.
	@param itemURL The URL of the item which provides a context for relative hrefs.
	@param itemDocument The document to be searched for references. This object
		should also implement the <code>DocumentTraversal</code> interface.
//G***del if not needed	@param documentTraversal The interface for creating traversal objects.
//G***del if not needed	@exception MalformedURLException Thrown if there is a problem creating a URL to a particular item.
//G***del if not needed	@exception IOException Thrown if there is an error reading or writing to files.
	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	@exception IOException Thrown if there is an error reading or writing to a file.
	@see DocumentTraversal
	*/
	protected void gatherReferences(final OEBPublication publication, final URL itemURL, final Document itemDocument) throws MalformedURLException, IOException //G***del if not needed throws MalformedURLException, IOException
	{
/*G***fix new publication
		//G***switch this new syntax back to XMLCSSProcessor
		final Iterator styleSheetProcessingInstructionIterator=XMLUtilities.getNodesByName(itemDocument, Node.PROCESSING_INSTRUCTION_NODE, XMLStyleSheetConstants.XML_STYLESHEET_PROCESSING_INSTRUCTION, false).iterator();  //get a list of all the style processing instructions in the document
		while(styleSheetProcessingInstructionIterator.hasNext())  //while there are more stylesheet references
		{
			final XMLProcessingInstruction styleSheetLink=(XMLProcessingInstruction)styleSheetProcessingInstructionIterator.next();	//get a reference to this child node G***just get a ProcessingInstruction and use an XMLUtility to get the value
			final String type=styleSheetLink.getPseudoAttributeValue(XMLStyleSheetConstants.TYPE_ATTRIBUTE);	//get the value of the type attribute, if it is present G***use a ProcessingInstruction coupled with an XMLUtilities
			final String href=styleSheetLink.getPseudoAttributeValue(XMLStyleSheetConstants.HREF_ATTRIBUTE);	//get the value of the href attribute, if it is present G***use a ProcessingInstruction coupled with an XMLUtilities
			final MediaType mediaType=new MediaType(type);  //create a new media type from the type
		  gatherReference(publication, itemURL, href, mediaType); //gather references for this stylesheet G*** change the href to be relative to the item document
		}
		final Element rootElement=itemDocument.getDocumentElement();  //get the root element
		NodeIterator nodeIterator=((DocumentTraversal)itemDocument).createNodeIterator(rootElement, NodeFilter.SHOW_ELEMENT, null, false); //create a node walker to traverse over every node
		Node node;
		while((node=nodeIterator.nextNode())!=null)  //while we haven't reached the last node
		{
//G***del Debug.trace("Node: "+node.getNodeName()+" namespace URI: "+node.getNamespaceURI()); //G***testing
		  final Element element=(Element)node;  //cast the node to an element; elements are all we asked for
		  final String elementName=element.getNodeName(); //get the name of the element G***fix for namespaces
//G***replace this next part with XHTMLUtilities.isApplet()
		  if(XHTMLUtilities.isLinkElement(element.getNamespaceURI(), element)) //if this is a link element G***pass the real XHTML namespace
			{
				final String href=XHTMLUtilities.getLinkElementHRef(element.getNamespaceURI(), element);  //get the link element's href
				if(href!=null)  //if the anchor has an href
					gatherReference(publication, itemURL, href);  //gather references for this href G*** change the href to be relative to the item document
			}
		  else if(elementName.equals(ELEMENT_OBJECT)) //if this is an object element
			{
Debug.trace("gatherReferences() found object.");
				final String codeType=element.getAttributeNS(null, ELEMENT_OBJECT_ATTRIBUTE_CODETYPE); //see if there is a code type attribute
				if(codeType.length()!=0)  //if the object has a code type
				{
Debug.trace("found codetype: ", codeType);
					final MediaType mediaType=new MediaType(codeType); //create a media type from the given type
Debug.trace("media type: ", mediaType);
					if(mediaType.equals(APPLICATION_JAVA_MEDIA_TYPE))  //if this is a java applet
					{
Debug.trace("determined media type: ", mediaType);
						final String classID=element.getAttributeNS(null, ELEMENT_OBJECT_ATTRIBUTE_CLASSID); //see if there is a class ID attribute
						if(classID.length()!=0) //if there is a class ID attribute
						{
Debug.trace("found class ID: ", classID);
								//G***put this code in a common location
							final String javaPrefix="java:";  //the prefix the classid, as a URI, will probably have G***use a constant here
							final String classPostfix=".class"; //the ending postfix the classid URI may have G***use a constant here
							String className=StringUtilities.trimBeginning(classID, javaPrefix);  //remove the "java:" prefix if present
							className=StringUtilities.trimEnd(className, classPostfix);  //remove the ".class" postfix if present
								//replace '.' with '/' and append ".class"
							final String href=className.replace(PACKAGE_SEPARATOR, PATH_SEPARATOR).concat(String.valueOf(EXTENSION_SEPARATOR)).concat(CLASS_EXTENSION);
							gatherReference(publication, itemURL, href, mediaType);  //gather references for this class
							continue; //continue with the next item
						}
					}
				}
				MediaType mediaType=null; //we'll see if a media type is specified so we can use it when gathering the reference
				final String type=element.getAttributeNS(null, ELEMENT_OBJECT_ATTRIBUTE_TYPE); //see if there is a type attribute
				if(type.length()!=0)  //if the object has a code type
				{
					mediaType=new MediaType(type); //create a media type from the given type
				}
				final String data=element.getAttributeNS(null, ELEMENT_OBJECT_ATTRIBUTE_DATA); //see if there is a data attribute, since there is no type specified
				if(data.length()!=0)  //if the object has a data attribute
				{
					gatherReference(publication, itemURL, data, mediaType);  //gather references for this object, passing a media type if we found one
//G***del					continue; //continue with the next item
				}
			}
*/


/*G***fix
Debug.trace("preparing to gather info for element: "+element.getNodeName());
			  //if this element represents an image G***fix for namespace; currently this takes any namespace
			if(XHTMLUtilities.isImageElement(element.getNamespaceURI(), element))
			{
Debug.trace("element is an image");
					//get the reference to the image G***fix for namespace; currently this takes any namespace
				final String href=XHTMLUtilities.getImageElementHRef(element.getNamespaceURI(), element);
				if(href!=null)  //if there is a valid image reference
				{
					gatherReference(publication, href);  //gather a reference to this image
					if(isTidy())  //if we're tidying the document
				  {
Debug.trace("OEBPublicationCreator tidying image: "+href);
Debug.trace("Old image dimensions, width: "+element.getAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_WIDTH)+" height: "+element.getAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_HEIGHT));
						final URL imageURL=publication.getURL(href);  //get the full URL to the image
System.out.println("Loading image dimensions for: "+imageURL);  //G***del
						final Toolkit toolkit=Toolkit.getDefaultToolkit(); //get the default toolkit
						final Image image=toolkit.createImage(imageURL);  //load the image
						Debug.assert(image!=null, "Couldn't load image at "+imageURL+".");
						ImageUtilities.loadImage(image);  //make sure the image is loaded
						element.setAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_WIDTH, String.valueOf(image.getWidth(null)));  //G***testing
						element.setAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_HEIGHT, String.valueOf(image.getHeight(null)));  //G***testing
Debug.trace("New image dimensions, width: "+element.getAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_WIDTH)+" height: "+element.getAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_HEIGHT));
				  }
				}
			}
//G***del if not needed			continue; //continue with the next item
*/
//G***fix		}
	}


	/**Adds a particular reference with an unknown media type to the publication
		manifest if it doesn't exist there already.
		Only references to files local to the publication will be added.
	@param publication The publication to which the item belongs.
	@param contextURL The URL to which the href is relative.
	@param href The reference to the item.
	@return The item created to represent the reference or the item that already
		appeared in the manifext, or <code>null</code> if the item was not added and
		did not already appear in the manifest.
G***fix outputDir
	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected RDFResource gatherReference(final OEBPublication publication, final URL contextURL, final String href) throws MalformedURLException, IOException
	{
		return gatherReference(publication, contextURL, href, null); //gather the reference without specifying a media type
	}

	/**Adds a particular reference to the publication manifest, if it doesn't
		exist there already.
	  Only references to files local to the publication and files that are not
		above the publication URL in the directory hierarchy will be added.
	@param publication The publication to which the item belongs.
	@param contextURL The URL to which the href is relative.
	@param href The reference to the item.
	@param mediaType The reference media type, if known, or <code>null</code> if
		the media type is undetermined.
	@return The item created to represent the reference or the item that already
		appeared in the manifext, or <code>null</code> if the item was not added and
		did not already appear in the manifest.
	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected RDFResource gatherReference(final OEBPublication publication, final URL contextURL, final String href, MediaType mediaType) throws MalformedURLException, IOException
	{
			//gather the reference, specifying that the reference should be added to the spine as well
		return gatherReference(publication, contextURL, href, mediaType, true);
	}

	/**Adds a particular reference with an unknown media type to the publication
		manifest if it doesn't exist there already.
		Only references to files local to the publication will be added.
	@param publication The publication to which the item belongs.
	@param contextURL The URL to which the href is relative.
	@param href The reference to the item.
	@param shouldAddToSpine Whether the reference should be added to the spine
		as well.
	@return The item created to represent the reference or the item that already
		appeared in the manifext, or <code>null</code> if the item was not added and
		did not already appear in the manifest.
G***fix outputDir
	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected RDFResource gatherReference(final OEBPublication publication, final URL contextURL, final String href, final boolean shouldAddToSpine) throws MalformedURLException, IOException
	{
		return gatherReference(publication, contextURL, href, null, shouldAddToSpine); //gather the reference without specifying a media type
	}

	/**Adds a particular reference to the publication manifest, if it doesn't
		exist there already.
	  Only references to files local to the publication and files that are not
		above the publication URL in the directory hierarchy will be added.
	@param publication The publication to which the item belongs.
	@param contextURL The URL to which the href is relative.
	@param href The reference to the item.
	@param mediaType The reference media type, if known, or <code>null</code> if
		the media type is undetermined.
	@param shouldAddToSpine Whether the reference should be added to the spine
		as well.
	@return The item created to represent the reference or the item that already
		appeared in the manifext, or <code>null</code> if the item was not added and
		did not already appear in the manifest.
	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected RDFResource gatherReference(final OEBPublication publication, final URL contextURL, final String href, MediaType mediaType, final boolean shouldAddToSpine) throws MalformedURLException, IOException
	{
Debug.trace("Here in gatherReference(), looking at href: ", href); //G***del
		RDFResource oebItem=null; //we'll try to gather a reference, and if we do we'll store the corresponding OEB item here
		try
		{
				//get the path of the file relative to the publication
//G***del		  final String publicationHRef=URLUtilities.getRelativePath(contextURL, URLUtilities.createURL(contextURL, href));
//G***del		  final String publicationHRef=URLUtilities.getRelativePath(publication.getPublicationURL(), URLUtilities.createURL(contextURL, href));
//G***make sure this works; see why we originally had to re-relativize this to the publication URL
//G***del	Debug.trace("publicationHRef: ", publicationHRef);  //G***del
				//get the path of the file relative to the publication
		  oebItem=XPackageUtilities.getManifestItemByLocationHRef(publication, contextURL, href);
//G***del			oebItem=publication.getManifestItemByHRef(publicationHRef); //see if this item is already in the manifest
			if(oebItem==null) //if this item is not already in the manifest
			{
	Debug.trace("item not in manifest.");  //G***del
//G***del				final URL url=URLUtilities.createURL(publication.getPublicationURL(), publicationHRef); //create a URL for the href, allowing for the href to be a relative path (although we don't yet know if it was) G***shouldn't we really use the URLUtilities here?
				final URL url=URLUtilities.createURL(contextURL, href); //create a URL for the href, allowing for the href to be a relative path (although we don't yet know if it was)
//G***del	Debug.trace("src URL: "+url+" relative to "+publication.getPublicationURL());
	Debug.trace("src URL: "+url+" relative to "+contextURL);
	//G***important fix		  if(URLConstants.FILE_PROTOCOL.equals(url.getProtocol()))  //if this is a local file being gathered into the manifest
				if(URLUtilities.exists(url))  //if a file exists at this URL
				{
					final File file=new File(url.getPath());  //create a file from the URL
	Debug.trace("File exists? ", new Boolean(file.exists()));  //G***del
	Debug.trace("Is directory? ", new Boolean(file.isDirectory()));  //G***del
					final boolean protocolsMatch=contextURL.getProtocol().equals(url.getProtocol()); //see if the protocols match
	Debug.trace("protocols match: ", new Boolean(protocolsMatch));  //G***del
					final boolean hostsMatch=contextURL.getHost().equals(url.getHost()); //see if the hosts match
	Debug.trace("hosts match: ", new Boolean(hostsMatch));  //G***del
					final boolean isLocalFile=protocolsMatch && hostsMatch; //see if the file is a local file
	//G***fix				if(file.exists() && !file.isDirectory())  //if the file exists, but it isn't a directory G***make sure this catches local file references
					if(isLocalFile)  //if this is a local file G***important check
					{
						String hrefRelativePath; //we'll set the relative path, if we can find it
						hrefRelativePath=URLUtilities.getRelativePath(contextURL, url);  //try to create a relative path for the reference G***why do we need to do this yet again?
	Debug.trace("href relative path: ", hrefRelativePath);  //G***de
						final String itemURI=createURI(publication.getReferenceURI(), hrefRelativePath);  //create an ID from the relative path
//G***del						final String itemID=XMLUtilities.createName(hrefRelativePath);  //create an ID from the relative path
						if(XPackageUtilities.getManifestItem(publication, itemURI)==null)  //if there isn't an item already in the manifest with this ID G***this could in some strange circumstances prevent two different paths from being stored, if the slash conversion to underline matches a filename with underlines in the same locations
						{
	Debug.trace("no manifest items with URI: ", itemURI);
//G***fix										final File srcFile=new File(src); //create a file object to represent the image source
//G***fix										final MediaType mediaType=FileUtilities.getMediaType(srcFile);  //try to see which of media type the image is
//G***fix										Debug.assert(mediaType!=null, "\""+srcFile+"\" has unknown media type.");  //G***put in better error handling here
							if(mediaType==null) //if no media type is given
							{
								mediaType=URLUtilities.getMediaType(url);  //try to see which of media type the reference is by examining the URL
								if(mediaType.equals(TEXT_HTML_MEDIA_TYPE))  //if this is the "text/html" media type
									mediaType=OEB10_DOCUMENT_MEDIA_TYPE;  //assume it's really the OEB document media type
							}
							Debug.assert(mediaType!=null, "\""+url+"\" has unknown media type.");  //G***put in better error handling here
							if(mediaType!=null) //if we have a media type
							{
	Debug.trace("found media type");
									//create a new OEB item to go in the manifest to represent this object or link targe
								oebItem=XPackageUtilities.createXPackageResource(getRDF(), itemURI);
//G***del								oebItem=new OEBItem(publication, itemID, hrefRelativePath, mediaType);
								XPackageUtilities.addLocation(getRDF(), oebItem, hrefRelativePath);  //add the relative href to the item
								XPackageUtilities.addContentType(getRDF(), oebItem, mediaType); //add the content type we determined
								XPackageUtilities.getManifest(publication).add(getRDF(), oebItem); //add the item to the publication's manifest
								  //if this is an OEB document, and we should add it to the spine
								if(shouldAddToSpine && mediaType.equals(OEB10_DOCUMENT_MEDIA_TYPE))
								  XPackageUtilities.getOrganization(publication).add(getRDF(), oebItem);  //add the item to the spine as well
								if(getOutputDir()!=null)  //if we have an output directory where files should be copied
								{
	Debug.trace("output dir: ", getOutputDir());
									final File outputFile=new File(getOutputDir(), hrefRelativePath); //create an output file for the file
	Debug.trace("output file: ", outputFile);
									final URL outputURL=outputFile.toURL(); //create a URL from the output file
	Debug.trace("output URL: ", outputURL);
	Debug.trace("original URL: ", url);
									if(!outputURL.equals(url))  //if the output file isn't the same as the file we started with
									{
	Debug.trace("URLs are different");  //G***del
										try
										{
												//make sure all subdirectories have been created
											if(outputFile.getParentFile().exists() || outputFile.getParentFile().mkdirs())  //G***probably put this in a FileUtilities
											{
	Debug.trace("Created directories");  //G***del
													//if we're tidying and this is an OEB document, it will be copied automatically;
													//  otherwise, we'll need to copy it ourselves
												if(!isTidy() || !mediaType.equals(OEB10_DOCUMENT_MEDIA_TYPE))
												{
	Debug.trace("ready to copy file");  //G***del
													NetworkUtilities.copy(url, outputFile); //copy the URL to the output directory
												}
											}
										}
										catch(IOException ioException)  //if we can't copy the source URL to the destination file
										{
											Debug.error(ioException); //G***fix
										}
									}
								}
//G***fix								processManifestItem(publication, oebItem);  //process this manifest item
							}
						}
					}
				}
				else  //if the URL doesn't exist
				{
					Debug.warn("File not found at "+url+" referenced from "+contextURL);
				}
			}
		}
		catch(MalformedURLException e)  //if there was an error creating a relative path, the URL is probably an absolute path
		{
				Debug.warn(e);  //G***fix
			//we currently ignore files that are not stored locally G***what should we do with these?
		}
		return oebItem; //return the item we found or created, or null if we did neither
	}

	/**Checks to make sure the given OEB item has a supported MIME type. If not,
		a fallback item is searched for.
		G***add capability to convert .gif to .png
		Currently, the following items are checked:
		<ul>
		  <li>GIF files are given PNG fallback items of the same name, if such PNG
				files exist.</li>
		</ul>
	@param publication The publication to which the item belongs.
	@param oebItem The item being processed.
//G***del if not needed	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	*/
	protected void gatherFallbacks(final OEBPublication publication, final OEBItem oebItem) throws MalformedURLException //G***del if not needed throws MalformedURLException, IOException
	{
/*G***fix new publication
		final MediaType mediaType=oebItem.getMediaType(); //get the item's media type
		if(IMAGE_GIF_MEDIA_TYPE.equals(mediaType))  //if this item is a GIF
		{
			if(oebItem.getFallback()==null) //if this item has no fallback
			{
			  final URL itemURL=oebItem.getURL(); //get the URL to the item
				if(URLConstants.FILE_PROTOCOL.equals(itemURL.getProtocol()))  //if this is a local file being gathered into the manifest
				{
//G***fix to see if the file exists					final File file=new File(url.getPath());  //create a file from the URL
				  final File file=new File(itemURL.getPath());  //create a file from the URL
					final File fallbackFile=FileUtilities.changeExtension(file, "png"); //create another file with a reference to a PNG file G***use a constant here
				  if(!fallbackFile.exists())  //G***testing
						Debug.notify("Fallback file does not exist: "+fallbackFile); //G***fix better with a warning

					final File hrefFile=new File(oebItem.getHRef());  //create a file from the href
					final File fallbackHRefFile=FileUtilities.changeExtension(hrefFile, "png"); //create another href with a reference to a PNG file G***use a constant here
					final String fallbackHRef=fallbackHRefFile.toString();  //convert the file to an href

//G***fix or del						hrefRelativePath=URLUtilities.getRelativePath(publication.getPublicationURL(), url);  //try to create a relative path for the reference
//G***fix or del						final String itemID=XMLUtilities.createName(hrefRelativePath);  //create an ID from the relative path
//G***fix or del						if(publication.getManifestItemByID(itemID)==null)  //if there isn't an item already in the manifest with this ID G***this could in some strange circumstances prevent two different paths from being stored, if the slash conversion to underline matches a filename with underlines in the same locations
//G***fix or del						{
						//create the fallback item
					final OEBItem fallbackItem=new OEBItem(publication, XMLUtilities.createName(fallbackHRef), fallbackHRef, IMAGE_PNG_MEDIA_TYPE);
					publication.putManifestItem(fallbackItem); //add the fallback item to the publication's manifest
					oebItem.setFallback(fallbackItem);  //set the new fallback item as the OEB item's fallback
				}
			}
		}
*/
	}

	/**Tidies an OEB file and adds any stylesheet references. Internal stylesheets
		may be extracted and stored in a separate file, and other parts of the
		document, such as tables of content, may be likewise extracted and stored
		as separate files.
	@param publication The publication that contains the document.
	@param itemURL The URL of the item which provides a context for relative hrefs.
	@param itemDocument The document to tidy.
	@param outputFile The destination location to write the tidied file.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected void tidyDocument(final OEBPublication publication, final URL itemURL, final Document itemDocument, final File outputFile) throws IOException
	{
		final XHTMLTidier xhtmlTidier=new XHTMLTidier(getOptions());  //create a new XHTML tidier G***use a common XHTML tidier
		xhtmlTidier.setTitle(getTitle()); //tell the tidier the title, if we know it
		xhtmlTidier.tidy(itemDocument, outputFile);  //tidy the document, specifying where any CSS file should be written to

//G***use new convenience method to add stylesheet references

		final Iterator stylesheetReferenceIterator=styleSheetReferenceList.iterator();  //get an iterator to look through the stylesheet references
		while(stylesheetReferenceIterator.hasNext())  //while there are more references
		{
			final String href=(String)stylesheetReferenceIterator.next(); //get the next reference
			XMLUtilities.addStyleSheetReference(itemDocument, href, OEB10_CSS_MEDIA_TYPE);  //add the stylesheet to the document G***eventually change to text/css
		}

		//G***see if we should extract the TOC or not
		extractTOC(publication, itemURL, itemDocument, outputFile); //G***testing
/*G***del
		final File tocFile=new File(FileUtilities.changeExtension(outputFile, "toc.html").getCanonicalPath());  //create a file indicating a table of contents with an .html extension G***use a constant here
		extractTOC(publication, itemDocument, tocFile); //G***testing
*/


Debug.trace("New tidy file: ", outputFile);  //G***del
		final File backupFile=new File(outputFile.getParent(), outputFile.getName()+FileConstants.EXTENSION_SEPARATOR+"backup"); //create the backup file object G***use a constant here G***use a standard extension changing routine
		if(backupFile.exists()) //if the  backup file exists
			backupFile.delete();  //delete the backup file
		if(!outputFile.exists() || outputFile.renameTo(backupFile)) //try to rename the file to a backup file; if we succeeded (or if the file didn't exist in the first place, meaning we're transferring the file from another location
		{
Debug.trace("Ready to write file to: ", outputFile);
			final OutputStream outputStream=new BufferedOutputStream(new FileOutputStream(outputFile)); //create an output stream to the original filename
			try
			{
				getXMLSerializer().serialize(itemDocument, outputStream);	//serialize the document to the output stream
				outputStream.flush(); //flush the output stream
			}
/*G***del when works
			catch(Exception e)
			{
				Debug.error(e); //G***del
			}
*/
			finally
			{
				outputStream.close(); //close the stream we were writing to
			}
		}
	}

	//G***comment
	protected void extractTOC(final OEBPublication publication, final URL itemURL, final Document itemDocument, final File itemFile) throws IOException
	{
/*G***fix new publication

		final File tocFile=new File(FileUtilities.changeExtension(itemFile, "toc.html").getCanonicalPath());  //create a file indicating a table of contents with an .html extension G***use a constant here

Debug.trace("inside extracttoc with file: ", tocFile);  //G***del
		final DocumentFragment tocDocumentFragment=extractElementsByClass(itemDocument, "MsoTo"); //extract any table of contents G***use a constant here
			//G***make sure stylsheet references are carried over
		if(tocDocumentFragment!=null)  //if we found a table of contents
		{
			final Document tocDocument=OEBUtilities.createDefaultOEB1Document();  //create a default OEB document
			//get a reference to the body element
			final Element bodyElement=(Element)XPath.getNode(tocDocument, XPath.LOCATION_STEP_SEPARATOR_CHAR+ELEMENT_BODY);  //G***what about namespaces?
			Debug.assert(bodyElement!=null, "Missing body element");  //we should always have a body element starting out
		  tocDocument.importNode(tocDocumentFragment, true); //import the entire table of contents document fragment
			bodyElement.appendChild(tocDocumentFragment); //append the table of contents document fragment to the new document
//G***del			XMLUtilities.printTree(tocDocument, System.out);  //G***testing
//G***del			final File tocFile=new File(FileUtilities.changeExtension(outputFile, "css").getCanonicalPath());  //create a file with a .css extension G***use a constant here
			XMLUtilities.appendText(bodyElement, "\n");	//append a newline to end the content of the body element
			XMLUtilities.appendText(tocDocument.getDocumentElement(), "\n");	//append a newline to end the content of the html element

		  convertInternalLinks(tocDocument, itemFile.getName());  //G***testing


				//create an output file stream for writing the table of contents
			final OutputStream tocOutputStream=new BufferedOutputStream(new FileOutputStream(tocFile));
			try
			{
				getXMLSerializer().serialize(tocDocument, tocOutputStream);	//serialize the document to the output stream
				tocOutputStream.flush(); //flush the output stream
Debug.trace("Ready to gather reference for toc file: ", tocFile.getName());
				final OEBItem tocItem=gatherReference(publication, itemURL, tocFile.getName(), OEB10_DOCUMENT_MEDIA_TYPE);  //G***fix; testing
//G***del if not needed				publication.addSpineItem(0, tocItem);  //add the item to the first of the spine
				publication.addSpineItem(0, tocItem);  //add the item to the first of the spine
				final OEBGuide tocGuide=new OEBGuide(OEBGuide.TOC, "Table of Contents", tocItem.getHRef()); //create a new guide for the table of contents G***i18n
				publication.addGuide(0, tocGuide); //add this guide to the beginning of our list
			}
			finally
			{
				tocOutputStream.close(); //always close the file
			}

		}
*/
	}

	/**Extracts all elements that contain a certain class attribute and returns
		all elements and their interspersed nodes as a document fragment.
	@param itemDocument The document from which the elements should be extracted.
	@param classSubstring Represents a substring to match (case insensitive) the
		class attribute of the elements to extract.
	@return A document fragment containing the extracted elements and all
		interspersed nodes.
	*/
	protected static DocumentFragment extractElementsByClass(final Document itemDocument, final String classSubstring)
	{
/*G***fix new publication
		int startChildIndex=-1; //we don't yet know where we will start
		int endChildIndex=-1; //we don't yet know where we will end
		final Element rootElement;  //we'll find a root element from which to search
		//get a reference to the body element
		final Element bodyElement=(Element)XPath.getNode(itemDocument, XPath.LOCATION_STEP_SEPARATOR_CHAR+ELEMENT_BODY);  //G***what about namespaces?
//G***del		Debug.assert(bodyElement!=null, "Missing body element");  //we should always have a body element starting out
		  //G***see if this is really OEB/XHTML, and only get the body element if so
		rootElement=bodyElement!=null ? bodyElement : itemDocument.getDocumentElement();  //use the body as the root, or if there is no body use the root element of the document
		final NodeList childNodeList=rootElement.getChildNodes(); //get a list of child nodes
		final int childCount=childNodeList.getLength(); //see how many child nodes there are
		for(int i=0; i<childCount; ++i) //look at each of the children
		{
			final Node childNode=childNodeList.item(i); //get a reference tot his child
Debug.trace("Looking for TOC element: ", childNode.toString());
			if(childNode.getNodeType()==childNode.ELEMENT_NODE) //if this is an element
			{
				final Element childElement=(Element)childNode;  //get a reference to this element
			  final String classAttributeValue=childElement.getAttributeNS(null, XHTMLConstants.ATTRIBUTE_CLASS);  //get the class attribute
Debug.trace("looking at class attribute value: ", classAttributeValue); //G***del
				if(StringUtilities.indexOfIgnoreCase(classAttributeValue, classSubstring)>=0) //if this contains the class substring in any case
				{
Debug.trace("found TOC class"); //G***del
					if(startChildIndex<0)  //if we haven't yet found a starting index
					{
						startChildIndex=i; //show where we'll start the extraction
					}
				}
				else  //if this element should not be extracted
				{
					if(startChildIndex>=0)  //if we had found a starting index
					{
						endChildIndex=i; //show that it's not ended
						break;  //stop looking for more elements to extract
					}
				}
			}
		}
		if(startChildIndex>=0) //if we've started an extraction but didn't find its end
		{
			if(endChildIndex<0) //if we didn't find the end of the extraction
			  endChildIndex=childCount;  //we'll extract all the elements
Debug.trace("Found TOC element to extract: ", endChildIndex-startChildIndex); //G***del
			return XMLUtilities.extractChildren(rootElement, startChildIndex, endChildIndex);  //extract and return the children G***what about the DOMException?
		}
		else  //if we didn't find anything to extract
			return null;  //show that there's nothing to extract
*/
		return null;  //G***fix
	}

	/**Searches a document for all links that are internal (i.e. begin with '#')
		and converts them to external links to the specified external reference,
		correctly retaining the internal link to the new references.
		This is useful if a fragment of a document has been extracted from the
		original document &mdash; the external reference would then be a reference
		to the original document.
	@param document The document to check. This object should also implement the
		<code>DocumentTraversal</code> interface.
	@param externalHRef The external reference, without any fragment, to the new
		document.
	@see DocumentTraversal
	 */
	protected static void convertInternalLinks(final Document document, final String externalHRef)
	{
		final Element rootElement=document.getDocumentElement();  //get the root element
		NodeIterator nodeIterator=((DocumentTraversal)document).createNodeIterator(rootElement, NodeFilter.SHOW_ELEMENT, null, false); //create a node walker to traverse over every node
		Node node;
		while((node=nodeIterator.nextNode())!=null)  //while we haven't reached the last node
		{
		  final Element element=(Element)node;  //cast the node to an element; elements are all we asked for
		  if(XHTMLUtilities.isLinkElement(element.getNamespaceURI(), element)) //if this is a link element G***pass the real XHTML namespace
			{
				final String href=XHTMLUtilities.getLinkElementHRef(element.getNamespaceURI(), element);  //get the link element's href
				if(href!=null)  //if the link has an href
				{
					if(href.length()>0 && href.charAt(0)==URLConstants.FRAGMENT_SEPARATOR_CHAR) //if this link is an internal reference
					{
					  XHTMLUtilities.setLinkElementHRef(element.getNamespaceURI(), element, externalHRef+href); //prepend the local reference with the external reference
					}
				}
			}
		}
	}


	/**Sorts the items in the publication spine by hrefs.
	@param publication The publication that needs its spine sorted.
	*/
	protected static void sortSpineItems(final OEBPublication publication)
	{
/*G***fix
		Collections.sort(publication.getSpineList(), new Comparator() //sort the spine by item HRefs
			  {
			    public int compare(Object o1, Object o2) {return ((OEBItem)o1).getHRef().compareTo(((OEBItem)o2).getHRef());}
			  });
*/
	}

	/**Adds any stylesheet references that an XML document should have.
	@param document The document to which stylesheet references should be added
	*/
	protected void addStylesheetReferences(final Document document)
	{
		final Iterator stylesheetReferenceIterator=styleSheetReferenceList.iterator();  //get an iterator to look through the stylesheet references
		while(stylesheetReferenceIterator.hasNext())  //while there are more references
		{
			final String href=(String)stylesheetReferenceIterator.next(); //get the next reference
			XMLUtilities.addStyleSheetReference(document, href, OEB10_CSS_MEDIA_TYPE);  //add the stylesheet to the document G***eventually change to text/css
		}
	}

	/**Gathers the guides for a publication from a given document.
	@param publication The OEB publication to which the guides references
		will be added.
	@param document The document that contains headings to be turned into guides.
	*/
	protected void gatherGuides(final OEBPublication publication, final Document document)
	{
				//G***should we gather the title and preface guides somewhere else?
		try //gather the title page
		{
			final URL titlePageURL=getTitlePageURL(); //get the URL to the title page
			if(titlePageURL!=null)  //if we have a title page
			{
					//try to create a relative path for the reference
				final String href=URLUtilities.getRelativePath(getContextURL(), titlePageURL);
//G***del				final OEBGuide guide=new OEBGuide("title-page", getTitle()!=null ? getTitle()+" (Title Page)" : "Title Page", href); //G***use constants; i18n
				final OEBGuide guide=new OEBGuide("title-page", "Title Page", href); //G***use constants; i18n
				publication.addGuide(guide);  //add the guide
			}
		}
		catch(MalformedURLException malformedURLException)
		{
			Debug.warn(malformedURLException);  //G***fix
		}
		try //gather the preface
		{
			final URL prefaceURL=getPrefaceURL(); //get the URL to the preface
			if(prefaceURL!=null)  //if we have a preface
			{
					//try to create a relative path for the reference
				final String href=URLUtilities.getRelativePath(getContextURL(), prefaceURL);
				final OEBGuide guide=new OEBGuide("preface", "Preface", href); //G***use constants; i18n
				publication.addGuide(guide);  //add the guide
			}
		}
		catch(MalformedURLException malformedURLException)
		{
			Debug.warn(malformedURLException);  //G***fix
		}
		int guideCount=0; //we'll keep track of the number of guides we have
		final Element rootElement=document.getDocumentElement();  //get the root element
		NodeIterator nodeIterator=((DocumentTraversal)document).createNodeIterator(rootElement, NodeFilter.SHOW_ELEMENT, null, false); //create a node walker to traverse over every element
		Node node;
		while((node=nodeIterator.nextNode())!=null)  //while we haven't reached the last node
		{
//G***del Debug.trace("Node: "+node.getNodeName()+" namespace URI: "+node.getNamespaceURI()); //G***testing
		  final Element element=(Element)node;  //cast the node to an element; elements are all we asked for
		  final String elementName=element.getNodeName(); //get the name of the element G***fix for namespaces
				//see if this is an HTML heading G***have some convenience method; use constants; fix for namespaces
			final boolean isHTMLHeading=elementName.length()==2 && Character.toLowerCase(elementName.charAt(0))=='h';  //G***use a constant
			if(isHTMLHeading) //if this is an HTML heading
			{
					//get the class attribute, if it's defined
				final String elementClass=element.getAttributeNS(null, "class");
					//see if this is a significant heading
				final boolean isSignificant=StringUtilities.indexOfIgnoreCase(elementClass, "significant")>=0;  //G***use a constant
				final String text=XMLUtilities.getText(element, true).trim(); //get the text of the element
				final boolean hasLetterOrDigit=StringUtilities.hasLetterOrDigit(text);  //see if the text has a letter or digit
				final int headingType=getHeadingType(text); //see what type of heading this is
					//make sure there is a letter or digit in the text
					//if this heading is marked as significant, or if we know that it's significant based upon its heading level
				if(hasLetterOrDigit && (isSignificant || headingType<=MAX_SIGNIFICANT_HEADING))
				{
					++guideCount; //show that we'll add another guide
					String id=XMLUtilities.getDefinedAttributeNS(element, null, "id");  //get the ID attribute G***use a constant
					if(id==null)  //if this heading doesn't have an ID attribute
					{
						id="guide"+guideCount;  //create an ID using this guide number G***use a constant
						element.setAttributeNS(null, "id", id); //add the ID to the attribute G***use a constant
					}
						//create an href to the element within the document
					final String href=URLUtilities.getFileName(getContextURL())+'#'+id; //G***pass the href; do something better than getContextURL(); use a constant for '#'
						//get the text of this element, collapsing all whitespace into single spaces
					final String elementText=StringUtilities.collapseEveryChar(XMLUtilities.getText(element, true), WHITESPACE_CHARS, " ");
						//making sure it's not too long
					final String shortText=StringUtilities.truncate(elementText, 32);  //G***use a constant
						//remove everything but the first line and trim it
					final String line=StringUtilities.truncateChar(shortText, EOL_CHARS).trim();
						//if we removed part of the string, indicate as much
					final String title=line.length()==elementText.length() ? line : line+"..."; //G***use a constant; should we use a real ellipsis?
					final String guideType; //we'll decide what type of guide this is, based upon whether this is a heading
						//see what type of heading this is, and use the appropriate guide type, if possible
					switch(getHeadingType(elementText))
					{
						case CONTENTS_HEADING:
							guideType="toc";  //G***use a constant
							break;
						case PREFACE_HEADING:
							guideType="preface";  //G***use a constant
							break;
						case FOREWORD_HEADING:
							guideType="foreword";  //G***use a constant
							break;
						case BIBLIOGRAPHY_HEADING:
							guideType="bibliography";  //G***use a constant
							break;
						case GLOSSARY_HEADING:
							guideType="glossary";  //G***use a constant
							break;
						case INDEX_HEADING:
							guideType="index";  //G***use a constant
							break;
						default:  //for all other headings, make up a type string
							guideType="other."+id;  //G***use a constant; i18n
							break;
					}
						//create a guide from the first line of this element
					final OEBGuide guide=new OEBGuide(guideType, title, href);
					publication.addGuide(guide);  //add the guide
				}
			}
		}
	}

	/**Loads the given template, does replacements, and copies the template to
		the appropriate location.
	@param templateURL The location of the template.
	@return The URL of the formatted template copy.
	@exception IOException Thrown if there is an error accessing or copying the
		template.
	*/
	protected URL useTemplate(final URL templateURL) throws IOException
	{
		final String filename=getContextURL()!=null //if we have a context URL, get the filename
				? FileUtilities.removeExtension(URLUtilities.getFileName(getContextURL()))
				: null;
		  //the title (replacement parameter {0})
		final String title=getTitle()!=null
				? getTitle()  //get the title if we can
				: (filename!=null
						? filename  //if not, use the filename if we can
						: (getReferenceURI()!=null ? getReferenceURI() : "(Untitled)"));  //lastly, use the reference URI G***use a constants
		  //the author (replacement parameter {1})
		final String author=getAuthor()!=null ? getAuthor() : "";
		  //"by" (replacement parameter {2})
		final String by=getAuthor()!=null ? "by" : "";
		  //a URL to more information about the book (replacement parameter {3})
		final String infoURL=isProjectGutenbergEText()  //if this is a Project Gutenberg work
			  ? "projectgutenbergheader.html" //point to the header we created G***use a constant
				: URLUtilities.getFileName(getContextURL());  //otherwise, just point to the main content G***does this method always work?
		  //read the template as a string, assuming UTF-8 encoding
		final String templateString=URLUtilities.readString(templateURL, CharacterEncodingConstants.UTF_8);
			//store the title and author in the preface, making sure all the content is valid for XML content
	  final String formattedString=MessageFormat.format(templateString,
			  new Object[]{
						  XMLUtilities.createValidContent(title),
							XMLUtilities.createValidContent(author),
							XMLUtilities.createValidContent(by),
							XMLUtilities.createValidContent(infoURL)}); //G***use constants
	  final String templateFilename=URLUtilities.getFileName(templateURL);  //get the filename of the template
		final File formattedTemplateFile; //we'll find out where to store the formatted template
		if(getOutputDir()!=null) //if an output directory was specified
		{
			formattedTemplateFile=new File(getOutputDir(), templateFilename);  //store the file in the output directory
		}
		else  //if an output directory was not specified
		{
				//create a file from the context URL G***what if the context URL is not a file?
			formattedTemplateFile=URLUtilities.getFile(URLUtilities.createURL(getContextURL(), templateFilename));
		}
			//write the resulting file
		FileUtilities.write(formattedTemplateFile, formattedString.getBytes(CharacterEncodingConstants.UTF_8));
		return formattedTemplateFile.toURL(); //return a URL to the resulting file
	}

	/**Writes the specified XML document to the given file. The output is formatted.
	@param document The document to write.
	@param file The file into which to write the document's contents.
	@exception IOException Thrown if there is an error writing to the file.
	*/
	public static void write(final Document document, final File file) throws IOException
	{
		final OutputStream outputStream=new BufferedOutputStream(new FileOutputStream(file)); //create an output stream
		try
		{
			final Properties serializeOptions=new Properties(); //create properties for the serialization options
			PropertyUtilities.setProperty(serializeOptions, XMLSerializer.FORMAT_OUTPUT_OPTION, true);  //show that we should format the output
			new XMLSerializer(serializeOptions).serialize(document, outputStream);	//serialize the document to the output stream using the default output encoding
		}
		finally
		{
			outputStream.close(); //always close the output stream
		}
	}

	/**Creates a reference URI of an item from the given href and URI of the
		publication.
	@param publication URI The identifying URI of the publication.
	@param href The relative location of the item.
	@return An identifying URI of the item.
	*/
	public static String createURI(final String publicationURI, final String href)
	{
		return publicationURI+URIConstants.FRAGMENT_SEPARATOR+href; //create a URI in the form publicationURI#href
		//G***del final String itemID=XMLUtilities.createName(hrefRelativePath);  //create an ID from the relative path
	}

	/**Tries to determine the title from the the first few elements of the
		XHTML document.
	@param xhtmlDocument The document to check for a title.
	@return The title, or <code>null</code> if a title could not be found.
	*/
	public static String getTitle(final Document xhtmlDocument)
	{
//G***del		String title=null;  //we'll try to find a title
Debug.trace("getting normal title");  //G***del
		final String BY="by"; //G***put elsewhere
		final Element bodyElement=XHTMLUtilities.getBodyElement(xhtmlDocument);  //get the body of the document
		final NodeList childNodes=bodyElement.getChildNodes();  //get the list of child nodes
		for(int i=0; i<25 && i<childNodes.getLength(); ++i) //look at each child node
		{
//G***del			String lastTitle=null;  //we'll store here every title we find, in case we need it later
			final Node childNode=childNodes.item(i);  //get this child node
			if(childNode.getNodeType()==Node.ELEMENT_NODE)  //if this is an element
			{
				final Element childElement=(Element)childNode;  //get a reference to this element
				final String text=XMLUtilities.getText(childElement, true).trim(); //get the trimmed text of the element
//G***del if not needed				final int lineCount=new StringTokenizer(text, EOL_CHARS).countTokens(); //see how many lines there are
Debug.trace("looking at text: ", text);  //G***del
				  //Michael Hart shouldn't be in the title, or even near it (e.g. Project Gutenberg plboss10.txt)
				if(StringUtilities.indexOfIgnoreCase(text, "Michael S. Hart")<0 //G*** these are last-minute hacks specifically for plboss10.txt
				  && StringUtilities.indexOfIgnoreCase(text, "405 West")<0
				  && StringUtilities.indexOfIgnoreCase(text, "Urbana, IL")<0)
				{
						//if the text is in uppercase or if it's a title, assume it's the book title if it has at least one letter in it
	//G***fix				if(StringUtilities.isUpperCase(text) || TextUtilities.getHeadingType(text)!=TextUtilities.NO_HEADING) //G***probably put this routine in some common area
					if(StringUtilities.hasLetter(text) &&
							(StringUtilities.isUpperCase(text) || isTitleHeading(text))) //G***probably put this routine in some common area
					{
	Debug.trace("This is a title: ", text);  //G***del
						String title=text;  //G***fix, tidy
							//if this sting starts with "by" and has a period in it, we'll assume it's really an author name (although this test is precarious) (e.g. jjknd10.txt)
						if(!StringUtilities.startsWithIgnoreCase(title.trim(), "by") || title.indexOf('.')<0)
						{
		//G***del System.out.println("looking at title: "+title);
									//see if "version" appears with a space after it (e.g. not "second version" as in bible11.txt)
							final int versionIndex=StringUtilities.indexOfIgnoreCase(title, "version "); //G***testing
		//G***del System.out.println("version index: "+versionIndex);
							if(versionIndex>=0)
								title=title.substring(0, versionIndex); //G***testing
		//G***del System.out.println("new title: "+title);
								//trim the title of certain delimiters, and then collapse the whitespace
							title=PGUtilities.tidyTitle(title);  //G***use a common method, not in PGUtilities
		Debug.trace("after tidying: ", title);  //G***del
							if(isTitleOrAuthor(title)) //if we have valid title text
								return title; //assume that's the title
		/*G***del
							else  //if this isn't a valid title
								lastTitle=title;  //save it in
		*/
						}
					}
				}
				int byIndex=StringUtilities.indexOfIgnoreCase(text, BY);  //see if "by" is in this string
				while(byIndex>0)  //if we found "by" and it's not at the first of the line
				{
Debug.trace("found by index: ", byIndex); //G***del
				  if(CharacterUtilities.isWhitespace(text.charAt(byIndex-1))  //and it's preceded by whitespace
							&& byIndex+BY.length()<text.length()  //and it's not at the end of the line
							&& CharacterUtilities.isWhitespace(text.charAt(byIndex+BY.length())))  //and whitespace comes after it
					{
Debug.trace("found by in text: ", text);  //G***del
							//make sure this isn't "donated by", "scanned by", etc. G***this is duplicated in getTitle() and getAuthor(); combine
						if((StringUtilities.indexOfIgnoreCase(text, "donated")<0 || StringUtilities.indexOfIgnoreCase(text, "donated")>byIndex)  //G***use constants
								&& (StringUtilities.indexOfIgnoreCase(text, "contributed")<0 || StringUtilities.indexOfIgnoreCase(text, "contributed")>byIndex)
								&& (StringUtilities.indexOfIgnoreCase(text, "created")<0 || StringUtilities.indexOfIgnoreCase(text, "created")>byIndex)
								&& (StringUtilities.indexOfIgnoreCase(text, "contributed")<0 || StringUtilities.indexOfIgnoreCase(text, "contributed")>byIndex)
								&& (StringUtilities.indexOfIgnoreCase(text, "created")<0 || StringUtilities.indexOfIgnoreCase(text, "created")>byIndex)
								&& (StringUtilities.indexOfIgnoreCase(text, "delimited")<0 || StringUtilities.indexOfIgnoreCase(text, "delimited")>byIndex)
								&& (StringUtilities.indexOfIgnoreCase(text, "etext")<0 || StringUtilities.indexOfIgnoreCase(text, "etext")>byIndex)
								&& (StringUtilities.indexOfIgnoreCase(text, "prepared")<0 || StringUtilities.indexOfIgnoreCase(text, "prepared")>byIndex)
								&& (StringUtilities.indexOfIgnoreCase(text, "proofread")<0 || StringUtilities.indexOfIgnoreCase(text, "proofread")>byIndex)
								&& (StringUtilities.indexOfIgnoreCase(text, "scanned")<0 || StringUtilities.indexOfIgnoreCase(text, "scanned")>byIndex)
								&& (StringUtilities.indexOfIgnoreCase(text, "typed in")<0 || StringUtilities.indexOfIgnoreCase(text, "typed in")>byIndex))
						{
								//get the title and trim it of certain delimiters, and then collapse the whitespace
							final String title=PGUtilities.tidyTitle(text.substring(0, byIndex));  //G***use a common method, not in PGUtilities
							if(isTitleOrAuthor(title)) //if we have valid title text
								return title; //assume that's the title
						}
					}
					byIndex=StringUtilities.indexOfIgnoreCase(text, BY, byIndex+BY.length());  //keep searching after the occurrence
				}
//G***del					//if we didn't find "XXX by XXX"
			}
		}
		return null;  //we couldn't find a title
	}

	/**Tries to determine the author from the the first few elements of the
		XHTML document.
	@param xhtmlDocument The document to check for an author.
	@return The author, or <code>null</code> if an author could not be found.
	*/
	public static String getAuthor(final Document xhtmlDocument)
	{
Debug.trace("finding normal text author");  //G***del
//G***del		final String pgAuthor=PGUtilities.getAuthor()
		final String BY="by"; //G***put elsewhere
		boolean nextLineIsAuthor=false; //something might make us think at some point that the next line is the author
		final Element bodyElement=XHTMLUtilities.getBodyElement(xhtmlDocument);  //get the body of the document
		final NodeList childNodes=bodyElement.getChildNodes();  //get the list of child nodes
		for(int i=0; i<20 && i<childNodes.getLength(); ++i) //look at each child node
		{
			final Node childNode=childNodes.item(i);  //get this child node
			if(childNode.getNodeType()==Node.ELEMENT_NODE)  //if this is an element
			{
				final Element childElement=(Element)childNode;  //get a reference to this element
				final String text=XMLUtilities.getText(childElement, true).trim(); //get the trimmed text of the element
Debug.trace("looking for normal author in text: ", text);
				if(nextLineIsAuthor)  //if we told ourselves that the next line will be the author
				{
Debug.trace("we think this line is the author");
						//get the author and trim it of certain delimiters, and then collapse the whitespace
					final String author=PGUtilities.tidyAuthor(text);  //G***use a common method, not in PGUtilities
Debug.trace("checking next line author: ", author);
					if(isTitleOrAuthor(author)) //if we have valid author text
						return author; //assume that's the author
				}
//G***del				int byIndex=PGUtilities.getByIndex(text);  //see if "by" is in this string G***use a common routine somewhere else
				int byIndex=StringUtilities.indexOfIgnoreCase(text, BY);  //see if "by" is in this string
				while(byIndex>=0)  //if we found "by"
				{
Debug.trace("byIndex: "+byIndex);
//G***del System.out.println("is whitespace after: "+CharacterUtilities.isWhitespace(text.charAt(byIndex+BY.length())));
						//and it's at the first of the line or preceded by whitespace
				  if((byIndex==0 || CharacterUtilities.isWhitespace(text.charAt(byIndex-1)))
						&& (byIndex+BY.length()==text.length()
							  || (byIndex+BY.length()<text.length()
								&& Character.isWhitespace(text.charAt(byIndex+BY.length()))))) //if there is whitespace after "by"
/*G***del
							&& byIndex+BY.length()<text.length()  //and it's not at the end of the line
							&& CharacterUtilities.isWhitespace(text.charAt(byIndex+BY.length())))  //and whitespace comes after it
*/
					{
Debug.trace("by is a word");  //G***del
						if(BY.equalsIgnoreCase(text.trim()))  //if this is just "by" by itself
						{
Debug.trace("is by by itself: ", text); //G***del
							nextLineIsAuthor=true;  //the next line is the author
						}
						else
						{
Debug.trace("checking to see if this is an acceptable by");  //G***del
							boolean isAcceptableBy=true;  //assume this by is OK
								//words that cannot appear before "by"
							final String[] UNACCEPTABLE_BY_PREFIXES={"donated", "contributed", "created", //G***use constants
									"delimited", "etext", "prepared", "proofread", "scanned", "typed in",
									"made available"};
							for(int phraseWordIndex=UNACCEPTABLE_BY_PREFIXES.length-1; phraseWordIndex>=0; --phraseWordIndex) //look at each string
							{
								final String word=UNACCEPTABLE_BY_PREFIXES[phraseWordIndex];  //get this word
									//find the first time this word apears before "by"
								final int wordIndex=StringUtilities.lastIndexOfIgnoreCase(text, word, byIndex-1);
								if(wordIndex>=0)  //if the word appears before "by"
								{
										//for "etext", we don't care if "etext by" appears as long as it is "Project Gutenberg etext by" (e.g. jjstg10.txt), not "etext by"
									if(!"etext".equals(word) || StringUtilities.charIndexOf(text, "Project Gutenberg")<byIndex)
									{
											//if there is just whitespace between the word and "by" (e.g. "donated by"), this is unacceptable
										if(StringUtilities.notCharIndexOf(text, WHITESPACE_CHARS, wordIndex+word.length())>=byIndex)
										{
Debug.trace("unacceptable because of: ", word); //G***del
											isAcceptableBy=false; //don't accept this phrase
											break;  //stop looking for an unacceptable phrase; we just found one
										}
									}
								}
							}
							if(isAcceptableBy)  //if this "by" is acceptable
							{
/*G***del
							//make sure this isn't "donated by", "scanned by", etc. G***this is duplicated in getTitle() and getAuthor(); combine
						else if(StringUtilities.indexOfIgnoreCase(text, "donated")<0  //G***use constants
								&& StringUtilities.indexOfIgnoreCase(text, "contributed")<0
								&& StringUtilities.indexOfIgnoreCase(text, "created")<0
								&& StringUtilities.indexOfIgnoreCase(text, "delimited")<0
								&& StringUtilities.indexOfIgnoreCase(text, "etext")<byIndex-16  //G***hack for jjclk10.txt
								&& StringUtilities.indexOfIgnoreCase(text, "prepared")<0
								&& StringUtilities.indexOfIgnoreCase(text, "proofread")<0
								&& StringUtilities.indexOfIgnoreCase(text, "scanned")<0
								&& StringUtilities.indexOfIgnoreCase(text, "typed in")<0)
						{
*/
								final String authorText=text.substring(byIndex+BY.length());  //get everything after "by"
	Debug.trace("got author text: ", authorText); //G***del
								if(StringUtilities.charIndexOf(authorText, EOL_CHARS)<0 //if everything's on a single line
												//or if the other lines are just numbers and hyphens (hack for hrlnd10.txt)
										|| StringUtilities.charIndexOf(StringUtilities.trim(authorText, WHITESPACE_CHARS+"0123456789-"), EOL_CHARS)<0)
								{
										//get the author and trim it of certain delimiters, and then collapse the whitespace
									final String author=PGUtilities.tidyAuthor(authorText);  //G***use a common method, not in PGUtilities
									if(isTitleOrAuthor(author)  //if we have valid author text
											&& !StringUtilities.startsWithIgnoreCase(author, "author")  //if this wasn't "by author" G***use a constant
											&& !StringUtilities.startsWithIgnoreCase(author, "the author")  //if this wasn't "by the author" G***use a constant
											&& !StringUtilities.startsWithIgnoreCase(author, "his wife")  //if this wasn't "by his wife" (e.g. slanr10.txt) G***use a constant
											&& !StringUtilities.startsWithIgnoreCase(author, "electronic")  //if this wasn't "by electronic mail" G***use a constant
											&& !StringUtilities.startsWithIgnoreCase(author, "email")  //if this wasn't "by email" G***use a constant
											&& !StringUtilities.startsWithIgnoreCase(author, "e-mail"))  //if this wasn't "by e-mail" G***use a constant
									{
	Debug.trace("is author: ", authorText); //G***del
										return author; //assume that's the author
									}
									else if(BY.equalsIgnoreCase(author))  //if this is just "by" by itself G***how did this ever work? we removed by already
									{
	Debug.trace("is by by itself: ", authorText); //G***del
										nextLineIsAuthor=true;  //the next line is the author
									}
								}
							}
						}
					}
					byIndex=StringUtilities.indexOfIgnoreCase(text, BY, byIndex+BY.length());  //keep searching after the occurrence
				}
/*G***del; not needed
				final String trimmedText=StringUtilities.trim(text, "*"); //trim the text of asterisks
					//if this text starts with a Project Gutenberg indicator (last-minute addition for jjstg10.txt) G***it would be better not to have Project Gutenberg-specific checks in this class
				if(StringUtilities.startsWithIgnoreCase(text, "Project Gutenberg etext of")==0)
				{
					final String author=
				}
*/
			}
		}
		return null;  //we couldn't find an author
	}

	/**Determines if the given string is a valid title or author.
	@param string The string to check.
	@return <code>false</code> if the given string cannot be a title or an author.
	*/
	protected static boolean isTitleOrAuthor(final String string)
	{
//G***del Debug.trace("is title or author? ", string);
			//if the string doesn't have the correct number of characters (this was 64, but huxbr10.txt needs larger for "NOTE ON THE RESEMBLANCES AND DIFFERENCES IN THE STRUCTURE AND THE DEVELOPMENT OF THE BRAIN IN MAN AND APES")
		if(string.length()==0 || string.length()>=128)
			return false; //this isn't valid
				//if the word "copyright" appears in the string
		if(StringUtilities.indexOfIgnoreCase(string, "copyright")>=0) //G***use a constant
			return false; //this isn't valid
				//if the string only has punctuation G***should we eventually add dependent punctuation to the general punctuation string?
		if(!StringUtilities.hasLetterOrDigit(string)) //if there are no letters or digits in the string
			return false; //this isn't valie
		return true;  //the string passed all our tests
	}


}