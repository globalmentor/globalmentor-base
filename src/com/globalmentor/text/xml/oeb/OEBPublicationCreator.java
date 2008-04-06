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

import java.io.*;
import java.net.*;
import java.text.MessageFormat;
import java.util.*;
import javax.mail.internet.ContentType;

import com.globalmentor.io.*;
import static com.globalmentor.io.ContentTypes.*;
import com.globalmentor.java.*;
import static com.globalmentor.java.Characters.*;
import com.globalmentor.net.*;
import static com.globalmentor.net.URIs.*;

import com.globalmentor.rdf.*;
import com.globalmentor.rdf.dublincore.*;
import com.globalmentor.rdf.xpackage.*;
import com.globalmentor.text.*;
import com.globalmentor.text.xml.*;
import static com.globalmentor.text.xml.xhtml.XHTML.*;
import static com.globalmentor.text.xml.oeb.OEB.*;
import com.globalmentor.text.xml.xhtml.*;
import com.globalmentor.util.*;

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
//TODO fix	with an option: Images are loaded and the width and height of their corresponding elements are updated.
@author Garret Wilson
@deprecated
*/
public class OEBPublicationCreator	//TODO update this class to work with XEbook; this class may not even function properly anymore
{

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

	/**The publisher option.*/
	public final static String PUBLISHER_OPTION="publisher";

		/**Default to not including a publisher.*/
		public final static String PUBLISHER_OPTION_DEFAULT=null;

	/**The rights statement option.*/
	public final static String RIGHTS_OPTION="rights";

		/**Default to not including a rights statement.*/
		public final static String RIGHTS_OPTION_DEFAULT=null;

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

	/**Whether this is a Project Gutenberg EText.*/
	private boolean isProjectGutenbergEText=false;

		/**@return Whether this is a Project Gutenberg EText.*/
		protected boolean isProjectGutenbergEText() {return isProjectGutenbergEText;}

		/**Sets whether this is a Project Gutenberg EText.
		@param newProjectGutenbergEText Whether this text is from Project Gutenberg.
		*/
		protected void setProjectGutenbergEText(final boolean newProjectGutenbergEText) {isProjectGutenbergEText=newProjectGutenbergEText;}

	/**The ID of the work.*/
	private URI referenceURI=null;

		/**@return The ID of the work, or <code>null</code> if the ID is not
		  known.
		*/
		public URI getReferenceURI() {return referenceURI;}

		/**Sets the ID of the work.
		@param newReferenceURI The ID of the work.
		*/
		public void setReferenceURI(final URI newReferenceURI) {referenceURI=newReferenceURI;}

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
	protected final static ContentType APPLICATION_JAVA_MEDIA_TYPE=getContentTypeInstance(APPLICATION_PRIMARY_TYPE, ContentTypeConstants.JAVA_SUBTYPE);

	/**A static image/gif media type for quick reference when comparing media types.*/
	protected final static ContentType IMAGE_GIF_MEDIA_TYPE=getContentTypeInstance(ContentTypes.IMAGE_PRIMARY_TYPE, ContentTypeConstants.GIF_SUBTYPE);

	/**A static image/png media type for quick reference when comparing media types.*/
	protected final static ContentType IMAGE_PNG_MEDIA_TYPE=getContentTypeInstance(ContentTypes.IMAGE_PRIMARY_TYPE, ContentTypeConstants.PNG_SUBTYPE);

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

//TODO probably eventually make this a non-static class so that a common XML processor can be used, instead of creating one in each function
//TODO note that this would also allow a tidy variable to be set, so that documents could be tidied and manifest items gathered in the same iteration, rather than loading documents twice

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
			setZip(PropertiesUtilities.getBooleanProperty(options, ZIP_OPTION, ZIP_OPTION_DEFAULT));
			setPrefaceLocation(options.getProperty(PREFACE_LOCATION_OPTION, PREFACE_LOCATION_OPTION_DEFAULT));
			setTitlePageLocation(options.getProperty(TITLE_PAGE_LOCATION_OPTION, TITLE_PAGE_LOCATION_OPTION_DEFAULT));
			setPublisher(options.getProperty(PUBLISHER_OPTION, PUBLISHER_OPTION_DEFAULT));
			setRights(options.getProperty(RIGHTS_OPTION, RIGHTS_OPTION_DEFAULT));
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
/*TODO fix new publication
Debug.trace("OEBPublicationCreator.createPublication() directory: ", directory);
		final File canonicalDirectory=directory.getCanonicalFile(); //convert the directory into a canonical directory (i.e. "c:\\temp\\.\\.\\." will be changed to "c:\\temp")
			//create a new OEB publication using the directory as the publication URL; this will later be changed to the actual name of the OEB publication file
		final OEBPublication publication=new OEBPublication(canonicalDirectory.toURL());
		gatherManifestItems(publication, canonicalDirectory); //gather the manifest items from this directory
		processManifestItems(publication);  //process the manifest items
		return publication; //return the publication we created
*/
		return null;  //TODO fix
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
/*TODO fix new publication
Debug.trace("OEBPublicationCreator.createPublication() file: ", oebFile);
		final File canonicalDirectory=oebFile.getCanonicalFile(); //convert the directory into a canonical directory (i.e. "c:\\temp\\.\\.\\." will be changed to "c:\\temp")
			//create a new OEB publication using the directory as the publication URL; this will later be changed to the actual name of the OEB publication file
		final OEBPublication publication=new OEBPublication(canonicalDirectory.toURL());
		final String fileRelativePath=oebFile.getName();  //get the filename of the OEB item TODO probably later make this relative to the publication; now we're assuming it's in the same directory as the publication; probably add another parameter with the publication name, both here and above
		  //create an OEB item with no fallback for this document; use the URL for an ID, after converting it to an XML name
	  final OEBItem oebItem=new OEBItem(publication, XMLUtilities.createName(fileRelativePath), fileRelativePath, OEB10_DOCUMENT_MEDIA_TYPE);
		publication.putManifestItem(oebItem); //add the item to the publication's manifest TODO change to gathering this manifest item
		publication.addSpineItem(oebItem);  //add the item to the spine
		processManifestItems(publication);  //process the manifest items
//TODO maybe add		sortSpineItems(publication);  //sort the publication's spine
		return publication; //return the publication we created
*/
		return null;  //TODO fix
	}

	/**Creates a publication with the given OEB document.
	  Optionally tidies each OEB document and gathers all references to files. TODO why "optionally?"
	@param oebDocumentURL The URL to the OEB document which should be included
		in the publication.
	@param referenceURI The identifying URI of the publication.
	@param outputDir The output directory to use for writing files, or
		<code>null</code> if the default should be used.
	@return A new publication constructed from the OEB document.
	@exception MalformedURLException Thrown if there is an error creating a URL
		from the directory. TODO fix comment
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	public OEBPublication createPublicationFromOEBDocument(final URL oebDocumentURL, final URI referenceURI, final File outputDir) throws MalformedURLException, IOException
	{
		if(outputDir!=null) //if an output directory is specified
			setOutputDir(outputDir.getCanonicalFile());  //set the output directory TODO should we make sure this directory exists, and create it if  not?
Debug.trace("OEBPublicationCreator.createPublication() document: ", oebDocumentURL);
		final RDF rdf=getRDF();	//get the RDF data model
			//create a new OEB publication using the OEB document URL as the publication URL; this will later be changed to the actual name of the OEB publication file
		final OEBPublication publication=new OEBPublication(referenceURI);	//create a new OEB publication
		publication.setRDF(rdf);	//set the publication's RDF data model
		rdf.addResource(publication);	//add the resource to the RDF data model
			//store the publication reference URI as a Dublin Core identifier property
		RDFDublinCore.addIdentifier(publication, publication.getURI().toString());
//TODO del when works		RDFUtilities.addProperty(getRDF(), publication, DCMI11_ELEMENTS_NAMESPACE_URI, DC_IDENTIFIER_PROPERTY_NAME, publication.getReferenceURI());
//TODO fix; moved to text converter		final RDFResource oebItem=gatherReference(publication, oebDocumentURL, oebDocumentURL.getFile(), OEB10_DOCUMENT_MEDIA_TYPE);  //add this document to the manifest
/*TODO fix
		processManifestItems(publication);  //process the manifest items
		sortSpineItems(publication);  //sort the publication's spine
*/
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
	@exception URISyntaxException Thrown if an invalid reference was discovered.
	@exception MalformedURLException Thrown if there is an error creating a URL
		from the directory. TODO fix comment
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	public OEBPublication createPublicationFromText(final URL textURL, URI referenceURI, final String encoding, final File outputDir) throws URISyntaxException, MalformedURLException, IOException
	{
		if(outputDir!=null) //if an output directory is specified
			setOutputDir(outputDir.getCanonicalFile());  //set the output directory TODO should we make sure this directory exists, and create it if  not?
		final Document document=OEB.createDefaultOEB1Document();  //create a default OEB document
		final InputStream textInputStream=textURL.openConnection().getInputStream();		//connect to the URL and get an input stream TODO check for proxying and such
		try
		{
				//TODO maybe use a shared XHTMLCreator
		  new XHTMLCreator().createXHTMLFromText(document, textInputStream, encoding);  //convert the text to XHTML
			document.normalize(); //normalize the document so that all the consecutive text regions will be combined
		}
		finally
		{
			textInputStream.close();  //always close the input stream
		}
		Locale languageLocale=null; //assume we won't find a language TODO fix to be consistent with title and author
		final DocumentFragment pgHeaderFragment;  //we'll attempt to find a Project Gutenberg header, if applicable
		setProjectGutenbergEText(ProjectGutenbergXHTMLTidier.isProjectGutenbergEText(document)); //see if this document is a Project Gutenberg EText
		if(isProjectGutenbergEText()) //if this document is a Project Gutenberg EText
		{
//TODO fix				new XHTMLTidier().tidy(document); //TODO testing
			pgHeaderFragment=ProjectGutenbergXHTMLTidier.extractHeader(document);  //extract the Project Gutenberg header
			if(pgHeaderFragment!=null)  //if we found a header
			{
				referenceURI=URI.create(ProjectGutenbergXHTMLTidier.getID(referenceURI.toString())); //convert this URI into a Project Gutenberg identifier by removing the version number TODO fix better for URI
				setTitle(ProjectGutenbergXHTMLTidier.getTitle(pgHeaderFragment));  //get the title
				setAuthor(ProjectGutenbergXHTMLTidier.getAuthor(pgHeaderFragment, getTitle()));  //get the author
				setDescription(ProjectGutenbergXHTMLTidier.getDescription(pgHeaderFragment));  //get the description
				final String displayLanguage=ProjectGutenbergXHTMLTidier.getLanguage(pgHeaderFragment);  //get the language
				languageLocale=Locales.createDisplayLanguageLocale(displayLanguage); //get the locale for this language
			}
			ProjectGutenbergXHTMLTidier.extractFooter(document);  //extract the Project Gutenberg footer, if it is available
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
//TODO fix				new XHTMLTidier().tidy(document); //TODO testing

	if(isTidy())  //if we should tidy
	{
				//TODO eventually use the tidy() method, and remove the addStylesheetReferences() call which will then be unecessary
		final XHTMLTidier xhtmlTidier=new XHTMLTidier(getOptions());  //create a new XHTML tidier TODO use a common XHTML tidier
		xhtmlTidier.setTitle(getTitle()); //tell the tidier the title, if we know it
		xhtmlTidier.tidy(document/*TODO fix, outputFile*/);  //tidy the document, specifying where any CSS file should be written to
	}

		addStylesheetReferences(document);  //add any stylesheet references to the document
		final File oebDocumentFile; //we'll find out where to store the document
		if(getOutputDir()!=null) //if an output directory was specified
		{
				//store the file in the output directory
			oebDocumentFile=Files.changeExtension(new File(getOutputDir(), URLs.getFileName(textURL)), "html");	//TODO use a constant
		}
		else  //if an output directory was not specified
		{
			//TODO make sure a file: protocol was specified
			oebDocumentFile=Files.changeExtension(URLs.getFile(textURL), "html");  //create a file from the URL TODO use a constant
		}
		  //TODO testing
		setContextURL(oebDocumentFile.toURL()); //show that the location of the document will be the context URL
			//create a publication from our new OEB document
		final OEBPublication publication=createPublicationFromOEBDocument(oebDocumentFile.toURL(), referenceURI, getOutputDir());
		if(getTitle()!=null) //if we have a title
		  RDFDublinCore.addTitle(publication, getTitle()); //add the title to the publication
		if(getAuthor()!=null) //if we have an author
		  RDFDublinCore.addCreator(publication, getAuthor()); //add the author to the publication
		if(getDescription()!=null) //if we have a description
		  RDFDublinCore.addDescription(publication, getDescription()); //add the description to the publication
		if(languageLocale!=null) //if we have a language
		  RDFDublinCore.addLanguage(publication, languageLocale); //add the language to the publication
		RDFDublinCore.setDate(publication, new Date());  //add the current date and time
		RDFDublinCore.addSource(publication, "file:"+URLs.getFileName(textURL));  //add the text filename as the source TODO use a constant
		if(isProjectGutenbergEText) //if we got this text from Project Gutenberg
		{
			RDFDublinCore.addContributor(publication, "Project Gutenberg");  //add Project Gutenberg as a contributor TODO use a constant
		}
		if(getPublisher()!=null)  //if we have a publisher
		  RDFDublinCore.addPublisher(publication, getPublisher()); //add the publisher to the publication
		if(getRights()!=null)  //if we have a rights statement
		  RDFDublinCore.addRights(publication, getRights()); //add the rights statement to the publication
		if(pgHeaderFragment!=null)  //if we have a Project Gutenberg header, write it to its own file
		{
			final Document pgHeaderDocument=OEB.createOEB1Document(pgHeaderFragment);  //create a document from the header fragment
		  final Element bodyElement=getBodyElement(pgHeaderDocument);  //get the body of the document
				//create a header element and add it to the body
			final Element headerElement=XML.createElementNS(pgHeaderDocument, bodyElement.getNamespaceURI(), ELEMENT_H2, "Information from the Original Project Gutenberg EText");  //TODO use a constant
		  bodyElement.insertBefore(headerElement, bodyElement.getFirstChild()); //insert the header element as the first element in the body
		  final String pgHeaderDocumentFilename="projectgutenbergheader.html";  //create a filename for the header document TODO use constants here
			final File pgHeaderDocumentFile; //we'll find out where to store the header document
			if(getOutputDir()!=null) //if an output directory was specified
			{
					//store the file in the output directory
				pgHeaderDocumentFile=new File(getOutputDir(), pgHeaderDocumentFilename);
			}
			else  //if an output directory was not specified
			{
				pgHeaderDocumentFile=new File(oebDocumentFile, pgHeaderDocumentFilename);  //create a file from the OEB file TODO does this really work?
			}
			write(pgHeaderDocument, pgHeaderDocumentFile); //write the header document
				//add this document to the manifest without adding it to the spine
		  gatherReference(publication, pgHeaderDocumentFile.toURL(), pgHeaderDocumentFile.toString(), OEB10_DOCUMENT_MEDIA_TYPE, false);
		}
			//add a preface if needed
		final String prefaceLocation=getPrefaceLocation();  //see if we were given a preface to add
		if(prefaceLocation!=null) //if we were given a preface to add
		{
		  final URL prefaceURL=useTemplate(URLs.createURL(null, prefaceLocation));  //use the given preface template
		  setPrefaceURL(prefaceURL);  //save the URL to the preface
				//add the preface to the manifest without adding it to the spine
		  final RDFResource prefaceResource=gatherReference(publication, prefaceURL, prefaceURL.getFile(), OEB10_DOCUMENT_MEDIA_TYPE, false);
				//add the preface as the first element in the spine
		  publication.getSpine().add(0, prefaceResource);
		}
			//add a title page if needed
		final String titlePageLocation=getTitlePageLocation();  //see if we were given a title page to add
		if(titlePageLocation!=null) //if we were given a title page to add
		{
		  final URL titlePageURL=useTemplate(URLs.createURL(null, titlePageLocation));  //use the given template
		  setTitlePageURL(titlePageURL);  //save the URL to the title page
				//add the title page to the manifest without adding it to the spine
		  final RDFResource titlePageResource=gatherReference(publication, titlePageURL, titlePageURL.getFile(), OEB10_DOCUMENT_MEDIA_TYPE, false);
				//add the title page as the first element in the spine
		  publication.getSpine().add(0, titlePageResource);
		}
			//add the stylesheets to the publication
		  //TODO fix to gather the stylesheet references into the manifest automatically through gatherReferences()
		final Iterator stylesheetReferenceIterator=styleSheetReferenceList.iterator();  //get an iterator to look through the stylesheet references
		while(stylesheetReferenceIterator.hasNext())  //while there are more references
		{
			final String href=(String)stylesheetReferenceIterator.next(); //get the next reference
				//add the stylesheet to the manifest without adding it to the spine
		  gatherReference(publication, URLs.createURL(getContextURL(), href), href, OEB10_CSS_MEDIA_TYPE, false);
		}
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
/*TODO fix new publication

		final URL directoryURL=directory.toURL(); //create a URL from the directory, so that we can create a relative path for each file
		final File[] fileList=directory.listFiles(new FileFilter()
			  {
*/
					/**@return <code>true</code> if the pathname ends in ".htm" or ".html".*/
/*TODO fix new publication
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
Debug.trace("Relative path: ", fileRelativePath);  //TODO fix
		  final MediaType mediaType=OEB10_DOCUMENT_MEDIA_TYPE;  //assume this is an OEB document TODO remove this variable and use the value directly
		  //create an OEB item with no fallback for this document; use the URL for an ID, after converting it to an XML name
		  final OEBItem oebItem=new OEBItem(publication, XMLUtilities.createName(fileRelativePath), fileRelativePath, mediaType);
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
/*TODO fix new publication

		{ //gather references and tidy the manifest items
				//create a separate list of the items in the manifest, so that when adding items to the manifest we won't conflict with our iterator
			final List manifestList=new ArrayList(publication.getManifestMap().values());
			final Iterator manifestIterator=manifestList.iterator(); //get an iterator to iterate through the manifest items
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
/*TODO fix new publication
		if(oebItem.getMediaType().equals(OEB10_DOCUMENT_MEDIA_TYPE)) //if this is an OEB document
		{
			final URL itemURL=oebItem.getURL(); //get the URL to the item
Debug.trace("Looking at item URL: ", itemURL);
Debug.trace("Output directory: ", getOutputDir());
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
			}
			finally
			{
				itemInputStream.close();  //always close the input stream
			}
			//TODO do a normalize() somewhere here
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
	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	@exception IOException Thrown if there is an error reading or writing to a file.
	@see DocumentTraversal
	*/
	protected void gatherReferences(final OEBPublication publication, final URL itemURL, final Document itemDocument) throws MalformedURLException, IOException
	{
/*TODO fix new publication
		//TODO switch this new syntax back to XMLCSSProcessor
		final Iterator styleSheetProcessingInstructionIterator=XMLUtilities.getNodesByName(itemDocument, Node.PROCESSING_INSTRUCTION_NODE, XMLStyleSheetConstants.XML_STYLESHEET_PROCESSING_INSTRUCTION, false).iterator();  //get a list of all the style processing instructions in the document
		while(styleSheetProcessingInstructionIterator.hasNext())  //while there are more stylesheet references
		{
			final XMLProcessingInstruction styleSheetLink=(XMLProcessingInstruction)styleSheetProcessingInstructionIterator.next();	//get a reference to this child node TODO just get a ProcessingInstruction and use an XMLUtility to get the value
			final String type=styleSheetLink.getPseudoAttributeValue(XMLStyleSheetConstants.TYPE_ATTRIBUTE);	//get the value of the type attribute, if it is present TODO use a ProcessingInstruction coupled with an XMLUtilities
			final String href=styleSheetLink.getPseudoAttributeValue(XMLStyleSheetConstants.HREF_ATTRIBUTE);	//get the value of the href attribute, if it is present TODO use a ProcessingInstruction coupled with an XMLUtilities
			final MediaType mediaType=new MediaType(type);  //create a new media type from the type
		  gatherReference(publication, itemURL, href, mediaType); //gather references for this stylesheet TODO change the href to be relative to the item document
		}
		final Element rootElement=itemDocument.getDocumentElement();  //get the root element
		NodeIterator nodeIterator=((DocumentTraversal)itemDocument).createNodeIterator(rootElement, NodeFilter.SHOW_ELEMENT, null, false); //create a node walker to traverse over every node
		Node node;
		while((node=nodeIterator.nextNode())!=null)  //while we haven't reached the last node
		{
		  final Element element=(Element)node;  //cast the node to an element; elements are all we asked for
		  final String elementName=element.getNodeName(); //get the name of the element TODO fix for namespaces
//TODO replace this next part with XHTMLUtilities.isApplet()
		  if(XHTMLUtilities.isLinkElement(element.getNamespaceURI(), element)) //if this is a link element TODO pass the real XHTML namespace
			{
				final String href=XHTMLUtilities.getLinkElementHRef(element.getNamespaceURI(), element);  //get the link element's href
				if(href!=null)  //if the anchor has an href
					gatherReference(publication, itemURL, href);  //gather references for this href TODO change the href to be relative to the item document
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
					if(mediaType.match(APPLICATION_JAVA_MEDIA_TYPE))  //if this is a java applet
					{
Debug.trace("determined media type: ", mediaType);
						final String classID=element.getAttributeNS(null, ELEMENT_OBJECT_ATTRIBUTE_CLASSID); //see if there is a class ID attribute
						if(classID.length()!=0) //if there is a class ID attribute
						{
Debug.trace("found class ID: ", classID);
								//TODO put this code in a common location
							final String javaPrefix="java:";  //the prefix the classid, as a URI, will probably have TODO use a constant here
							final String classPostfix=".class"; //the ending postfix the classid URI may have TODO use a constant here
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
				}
			}
*/


/*TODO fix
Debug.trace("preparing to gather info for element: "+element.getNodeName());
			  //if this element represents an image TODO fix for namespace; currently this takes any namespace
			if(XHTMLUtilities.isImageElement(element.getNamespaceURI(), element))
			{
Debug.trace("element is an image");
					//get the reference to the image TODO fix for namespace; currently this takes any namespace
				final String href=XHTMLUtilities.getImageElementHRef(element.getNamespaceURI(), element);
				if(href!=null)  //if there is a valid image reference
				{
					gatherReference(publication, href);  //gather a reference to this image
					if(isTidy())  //if we're tidying the document
				  {
Debug.trace("OEBPublicationCreator tidying image: "+href);
Debug.trace("Old image dimensions, width: "+element.getAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_WIDTH)+" height: "+element.getAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_HEIGHT));
						final URL imageURL=publication.getURL(href);  //get the full URL to the image
						final Toolkit toolkit=Toolkit.getDefaultToolkit(); //get the default toolkit
						final Image image=toolkit.createImage(imageURL);  //load the image
						Debug.assert(image!=null, "Couldn't load image at "+imageURL+".");
						ImageUtilities.loadImage(image);  //make sure the image is loaded
						element.setAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_WIDTH, String.valueOf(image.getWidth(null)));  //TODO testing
						element.setAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_HEIGHT, String.valueOf(image.getHeight(null)));  //TODO testing
Debug.trace("New image dimensions, width: "+element.getAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_WIDTH)+" height: "+element.getAttributeNS(null, ELEMENT_IMG_ATTRIBUTE_HEIGHT));
				  }
				}
			}
*/
//TODO fix		}
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
TODO fix outputDir
	@exception URISyntaxException Thrown if an invalid reference was discovered.
	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected RDFResource gatherReference(final OEBPublication publication, final URL contextURL, final String href) throws URISyntaxException, MalformedURLException, IOException
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
	@exception URISyntaxException Thrown if an invalid reference was discovered.
	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected RDFResource gatherReference(final OEBPublication publication, final URL contextURL, final String href, ContentType mediaType) throws URISyntaxException, MalformedURLException, IOException
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
TODO fix outputDir
	@exception URISyntaxException Thrown if an invalid reference was discovered.
	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected RDFResource gatherReference(final OEBPublication publication, final URL contextURL, final String href, final boolean shouldAddToSpine) throws URISyntaxException, MalformedURLException, IOException
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
	@exception URISyntaxException Thrown if an invalid reference was discovered.
	@exception MalformedURLException Thrown if a an invalid reference is discovered.
	@exception IOException Thrown if there is an error reading or writing to a file.
	*/
	protected RDFResource gatherReference(final OEBPublication publication, final URL contextURL, final String href, ContentType mediaType, final boolean shouldAddToSpine) throws URISyntaxException, MalformedURLException, IOException
	{
		RDFResource oebItem=null; //we'll try to gather a reference, and if we do we'll store the corresponding OEB item here
		try
		{
				//get the path of the file relative to the publication
//TODO make sure this works; see why we originally had to re-relativize this to the publication URL
				//get the path of the file relative to the publication
		  oebItem=null;	//TODO fix with URF
//TODO fix with URF		  oebItem=XPackageUtilities.getManifestItemByLocationHRef(publication, contextURL.toURI(), href);
			if(oebItem==null) //if this item is not already in the manifest
			{
				final URL url=URLs.createURL(contextURL, href); //create a URL for the href, allowing for the href to be a relative path (although we don't yet know if it was)
	Debug.trace("src URL: "+url+" relative to "+contextURL);
	//TODO important fix		  if(URLConstants.FILE_PROTOCOL.equals(url.getProtocol()))  //if this is a local file being gathered into the manifest
				if(URLs.exists(url))  //if a file exists at this URL
				{
					final File file=new File(url.getPath());  //create a file from the URL
					final boolean protocolsMatch=contextURL.getProtocol().equals(url.getProtocol()); //see if the protocols match
					final boolean hostsMatch=contextURL.getHost().equals(url.getHost()); //see if the hosts match
					final boolean isLocalFile=protocolsMatch && hostsMatch; //see if the file is a local file
	//TODO fix				if(file.exists() && !file.isDirectory())  //if the file exists, but it isn't a directory TODO make sure this catches local file references
					if(isLocalFile)  //if this is a local file TODO important check
					{
						String hrefRelativePath; //we'll set the relative path, if we can find it
						hrefRelativePath=URLs.getRelativePath(contextURL, url);  //try to create a relative path for the reference TODO why do we need to do this yet again?
						final URI itemURI=createURI(publication.getURI(), hrefRelativePath);  //create an ID from the relative path
//TODO fix with URF						if(XPackageUtilities.getManifestItem(publication, itemURI)==null)  //if there isn't an item already in the manifest with this ID TODO this could in some strange circumstances prevent two different paths from being stored, if the slash conversion to underline matches a filename with underlines in the same locations
						if(true)	//TODO fix with URF
						{
	Debug.trace("no manifest items with URI: ", itemURI);
//TODO fix										final File srcFile=new File(src); //create a file object to represent the image source
//TODO fix										final MediaType mediaType=FileUtilities.getMediaType(srcFile);  //try to see which of media type the image is
//TODO fix										Debug.assert(mediaType!=null, "\""+srcFile+"\" has unknown media type.");  //TODO put in better error handling here
							if(mediaType==null) //if no media type is given
							{
								mediaType=URLs.getMediaType(url);  //try to see which of media type the reference is by examining the URL
								if(mediaType.match(XHTML_CONTENT_TYPE))  //if this is the "application/xhtml+xml" media type
									mediaType=OEB10_DOCUMENT_MEDIA_TYPE;  //assume it's really the OEB document media type
							}
							assert mediaType!=null : "\""+url+"\" has unknown media type.";  //TODO put in better error handling here
							if(mediaType!=null) //if we have a media type
							{
	Debug.trace("found media type");
									//create a new OEB item to go in the manifest to represent this object or link targe
								oebItem=getRDF().locateResource(itemURI);
								XPackage.addLocation(oebItem, hrefRelativePath);  //add the relative href to the item
/*TODO fix with URF
								Marmot.addContentType(oebItem, mediaType); //add the content type we determined
								Marmot.getContents(publication).add(oebItem); //add the item to the publication's manifest
*/
								  //if this is an OEB document, and we should add it to the spine
								if(shouldAddToSpine && mediaType.match(OEB10_DOCUMENT_MEDIA_TYPE))
								{
								  publication.getSpine().add(oebItem);  //add the item to the spine as well
								}
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
	//TODO fix									try
										{
												//make sure all subdirectories have been created
											if(outputFile.getParentFile().exists() || outputFile.getParentFile().mkdirs())  //TODO probably put this in a FileUtilities
											{
													//if we're tidying and this is an OEB document, it will be copied automatically;
													//  otherwise, we'll need to copy it ourselves
												if(!isTidy() || !mediaType.match(OEB10_DOCUMENT_MEDIA_TYPE))
												{
//TODO replace with a generalized copy method													NetworkUtilities.copy(url, outputFile); //copy the URL to the output directory
												}
											}
										}
/*TODO fix
										catch(IOException ioException)  //if we can't copy the source URL to the destination file
										{
											Debug.error(ioException); //TODO fix
										}
*/
									}
								}
//TODO fix								processManifestItem(publication, oebItem);  //process this manifest item
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
				Debug.warn(e);  //TODO fix
			//we currently ignore files that are not stored locally TODO what should we do with these?
		}
		return oebItem; //return the item we found or created, or null if we did neither
	}

	/**Checks to make sure the given OEB item has a supported MIME type. If not,
		a fallback item is searched for.
		TODO add capability to convert .gif to .png
		Currently, the following items are checked:
		<ul>
		  <li>GIF files are given PNG fallback items of the same name, if such PNG
				files exist.</li>
		</ul>
	@param publication The publication to which the item belongs.
	@param oebItem The item being processed.
	*/
	protected void gatherFallbacks(final OEBPublication publication, final OEBItem oebItem) throws MalformedURLException
	{
/*TODO fix new publication
		final MediaType mediaType=oebItem.getMediaType(); //get the item's media type
		if(IMAGE_GIF_MEDIA_TYPE.equals(mediaType))  //if this item is a GIF
		{
			if(oebItem.getFallback()==null) //if this item has no fallback
			{
			  final URL itemURL=oebItem.getURL(); //get the URL to the item
				if(URLConstants.FILE_PROTOCOL.equals(itemURL.getProtocol()))  //if this is a local file being gathered into the manifest
				{
//TODO fix to see if the file exists					final File file=new File(url.getPath());  //create a file from the URL
				  final File file=new File(itemURL.getPath());  //create a file from the URL
					final File fallbackFile=FileUtilities.changeExtension(file, "png"); //create another file with a reference to a PNG file TODO use a constant here
				  if(!fallbackFile.exists())  //TODO testing
						Debug.notify("Fallback file does not exist: "+fallbackFile); //TODO fix better with a warning

					final File hrefFile=new File(oebItem.getHRef());  //create a file from the href
					final File fallbackHRefFile=FileUtilities.changeExtension(hrefFile, "png"); //create another href with a reference to a PNG file TODO use a constant here
					final String fallbackHRef=fallbackHRefFile.toString();  //convert the file to an href

//TODO fix or del						hrefRelativePath=URLUtilities.getRelativePath(publication.getPublicationURL(), url);  //try to create a relative path for the reference
//TODO fix or del						final String itemID=XMLUtilities.createName(hrefRelativePath);  //create an ID from the relative path
//TODO fix or del						if(publication.getManifestItemByID(itemID)==null)  //if there isn't an item already in the manifest with this ID TODO this could in some strange circumstances prevent two different paths from being stored, if the slash conversion to underline matches a filename with underlines in the same locations
//TODO fix or del						{
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
		final XHTMLTidier xhtmlTidier=new XHTMLTidier(getOptions());  //create a new XHTML tidier TODO use a common XHTML tidier
		xhtmlTidier.setTitle(getTitle()); //tell the tidier the title, if we know it
		xhtmlTidier.tidy(itemDocument, outputFile);  //tidy the document, specifying where any CSS file should be written to

//TODO use new convenience method to add stylesheet references

		final Iterator stylesheetReferenceIterator=styleSheetReferenceList.iterator();  //get an iterator to look through the stylesheet references
		while(stylesheetReferenceIterator.hasNext())  //while there are more references
		{
			final String href=(String)stylesheetReferenceIterator.next(); //get the next reference
			XML.addStyleSheetReference(itemDocument, href, OEB10_CSS_MEDIA_TYPE);  //add the stylesheet to the document TODO eventually change to text/css
		}

		//TODO see if we should extract the TOC or not
		extractTOC(publication, itemURL, itemDocument, outputFile); //TODO testing
		final File backupFile=new File(outputFile.getParent(), outputFile.getName()+FileConstants.EXTENSION_SEPARATOR+"backup"); //create the backup file object TRODO use a constant here TODO use a standard extension changing routine
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
			finally
			{
				outputStream.close(); //close the stream we were writing to
			}
		}
	}

	//TODO comment
	protected void extractTOC(final OEBPublication publication, final URL itemURL, final Document itemDocument, final File itemFile) throws IOException
	{
/*TODO fix new publication

		final File tocFile=new File(FileUtilities.changeExtension(itemFile, "toc.html").getCanonicalPath());  //create a file indicating a table of contents with an .html extension TODO use a constant here

		final DocumentFragment tocDocumentFragment=extractElementsByClass(itemDocument, "MsoTo"); //extract any table of contents TODO use a constant here
			//TODO make sure stylsheet references are carried over
		if(tocDocumentFragment!=null)  //if we found a table of contents
		{
			final Document tocDocument=OEBUtilities.createDefaultOEB1Document();  //create a default OEB document
			//get a reference to the body element
			final Element bodyElement=(Element)XPath.getNode(tocDocument, XPath.LOCATION_STEP_SEPARATOR_CHAR+ELEMENT_BODY);  //TODO what about namespaces?
			Debug.assert(bodyElement!=null, "Missing body element");  //we should always have a body element starting out
		  tocDocument.importNode(tocDocumentFragment, true); //import the entire table of contents document fragment
			bodyElement.appendChild(tocDocumentFragment); //append the table of contents document fragment to the new document
			XMLUtilities.appendText(bodyElement, "\n");	//append a newline to end the content of the body element
			XMLUtilities.appendText(tocDocument.getDocumentElement(), "\n");	//append a newline to end the content of the html element

		  convertInternalLinks(tocDocument, itemFile.getName());  //TODO testing

				//create an output file stream for writing the table of contents
			final OutputStream tocOutputStream=new BufferedOutputStream(new FileOutputStream(tocFile));
			try
			{
				getXMLSerializer().serialize(tocDocument, tocOutputStream);	//serialize the document to the output stream
				tocOutputStream.flush(); //flush the output stream
Debug.trace("Ready to gather reference for toc file: ", tocFile.getName());
				final OEBItem tocItem=gatherReference(publication, itemURL, tocFile.getName(), OEB10_DOCUMENT_MEDIA_TYPE);  //TODO fix; testing
				publication.addSpineItem(0, tocItem);  //add the item to the first of the spine
				final OEBGuide tocGuide=new OEBGuide(OEBGuide.TOC, "Table of Contents", tocItem.getHRef()); //create a new guide for the table of contents TODO i18n
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
/*TODO fix new publication
		int startChildIndex=-1; //we don't yet know where we will start
		int endChildIndex=-1; //we don't yet know where we will end
		final Element rootElement;  //we'll find a root element from which to search
		//get a reference to the body element
		final Element bodyElement=(Element)XPath.getNode(itemDocument, XPath.LOCATION_STEP_SEPARATOR_CHAR+ELEMENT_BODY);  //TODO what about namespaces?
		  //TODO see if this is really OEB/XHTML, and only get the body element if so
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
				if(StringUtilities.indexOfIgnoreCase(classAttributeValue, classSubstring)>=0) //if this contains the class substring in any case
				{
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
			return XMLUtilities.extractChildren(rootElement, startChildIndex, endChildIndex);  //extract and return the children TODO what about the DOMException?
		}
		else  //if we didn't find anything to extract
			return null;  //show that there's nothing to extract
*/
		return null;  //TODO fix
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
		  if(isLinkElement(element.getNamespaceURI(), element)) //if this is a link element TODO pass the real XHTML namespace
			{
				final String href=getLinkElementHRef(element.getNamespaceURI(), element);  //get the link element's href
				if(href!=null)  //if the link has an href
				{
					if(href.length()>0 && href.charAt(0)==FRAGMENT_SEPARATOR) //if this link is an internal reference
					{
					  setLinkElementHRef(element.getNamespaceURI(), element, externalHRef+href); //prepend the local reference with the external reference
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
/*TODO fix
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
			XML.addStyleSheetReference(document, href, OEB10_CSS_MEDIA_TYPE);  //add the stylesheet to the document TODO eventually change to text/css
		}
	}

	/**Gathers the guides for a publication from a given document.
	@param publication The OEB publication to which the guides references
		will be added.
	@param document The document that contains headings to be turned into guides.
	*/
	protected void gatherGuides(final OEBPublication publication, final Document document)
	{
				//TODO should we gather the title and preface guides somewhere else?
		try //gather the title page
		{
			final URL titlePageURL=getTitlePageURL(); //get the URL to the title page
			if(titlePageURL!=null)  //if we have a title page
			{
					//try to create a relative path for the reference
				final String href=URLs.getRelativePath(getContextURL(), titlePageURL);
				final OEBGuide guide=new OEBGuide("title-page", "Title Page", href); //TODO use constants; i18n
				publication.addGuide(guide);  //add the guide
			}
		}
		catch(MalformedURLException malformedURLException)
		{
			Debug.warn(malformedURLException);  //TODO fix
		}
		try //gather the preface
		{
			final URL prefaceURL=getPrefaceURL(); //get the URL to the preface
			if(prefaceURL!=null)  //if we have a preface
			{
					//try to create a relative path for the reference
				final String href=URLs.getRelativePath(getContextURL(), prefaceURL);
				final OEBGuide guide=new OEBGuide("preface", "Preface", href); //TODO use constants; i18n
				publication.addGuide(guide);  //add the guide
			}
		}
		catch(MalformedURLException malformedURLException)
		{
			Debug.warn(malformedURLException);  //TODO fix
		}
		int guideCount=0; //we'll keep track of the number of guides we have
		final Element rootElement=document.getDocumentElement();  //get the root element
		NodeIterator nodeIterator=((DocumentTraversal)document).createNodeIterator(rootElement, NodeFilter.SHOW_ELEMENT, null, false); //create a node walker to traverse over every element
		Node node;
		while((node=nodeIterator.nextNode())!=null)  //while we haven't reached the last node
		{
		  final Element element=(Element)node;  //cast the node to an element; elements are all we asked for
		  final String elementName=element.getNodeName(); //get the name of the element TODO fix for namespaces
				//see if this is an HTML heading TODO have some convenience method; use constants; fix for namespaces
			final boolean isHTMLHeading=elementName.length()==2 && Character.toLowerCase(elementName.charAt(0))=='h';  //TODO use a constant
			if(isHTMLHeading) //if this is an HTML heading
			{
					//get the class attribute, if it's defined
				final String elementClass=element.getAttributeNS(null, "class");
					//see if this is a significant heading
				final boolean isSignificant=Strings.indexOfIgnoreCase(elementClass, "significant")>=0;  //TODO use a constant
				final String text=XML.getText(element, true).trim(); //get the text of the element
				final boolean hasLetterOrDigit=CharSequences.containsLetterOrDigit(text);  //see if the text has a letter or digit
				final int headingType=Prose.getHeadingType(text); //see what type of heading this is
					//make sure there is a letter or digit in the text
					//if this heading is marked as significant, or if we know that it's significant based upon its heading level
				if(hasLetterOrDigit && (isSignificant || headingType<=Prose.MAX_SIGNIFICANT_HEADING))
				{
					++guideCount; //show that we'll add another guide
					String id=XML.getDefinedAttributeNS(element, null, "id");  //get the ID attribute TODO use a constant
					if(id==null)  //if this heading doesn't have an ID attribute
					{
						id="guide"+guideCount;  //create an ID using this guide number TODO use a constant
						element.setAttributeNS(null, "id", id); //add the ID to the attribute TODO use a constant
					}
						//create an href to the element within the document
					final String href=URLs.getFileName(getContextURL())+'#'+id; //TODO pass the href; do something better than getContextURL(); use a constant for '#'
						//get the text of this element, collapsing all whitespace into single spaces
					final String elementText=Strings.collapseEveryChar(XML.getText(element, true), WHITESPACE_CHAR_STRING, " ");
						//making sure it's not too long
					final String shortText=Strings.truncate(elementText, 32);  //TODO use a constant
						//remove everything but the first line and trim it
					final String line=Strings.truncateChar(shortText, EOL_CHAR_STRING).trim();
						//if we removed part of the string, indicate as much
					final String title=line.length()==elementText.length() ? line : line+"..."; //TODO use a constant; should we use a real ellipsis?
					final String guideType; //we'll decide what type of guide this is, based upon whether this is a heading
						//see what type of heading this is, and use the appropriate guide type, if possible
					switch(Prose.getHeadingType(elementText))
					{
						case Prose.CONTENTS_HEADING:
							guideType="toc";  //TODO use a constant
							break;
						case Prose.PREFACE_HEADING:
							guideType="preface";  //TODO use a constant
							break;
						case Prose.FOREWORD_HEADING:
							guideType="foreword";  //TODO use a constant
							break;
						case Prose.BIBLIOGRAPHY_HEADING:
							guideType="bibliography";  //TODO use a constant
							break;
						case Prose.GLOSSARY_HEADING:
							guideType="glossary";  //TODO use a constant
							break;
						case Prose.INDEX_HEADING:
							guideType="index";  //TODO use a constant
							break;
						default:  //for all other headings, make up a type string
							guideType="other."+id;  //TODO use a constant; i18n
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
				? Files.removeExtension(URLs.getFileName(getContextURL()))
				: null;
		  //the title (replacement parameter {0})
		final String title=getTitle()!=null
				? getTitle()  //get the title if we can
				: (filename!=null
						? filename  //if not, use the filename if we can
						: (getReferenceURI()!=null ? getReferenceURI().toString() : "(Untitled)"));  //lastly, use the reference URI TODO use a constants
		  //the author (replacement parameter {1})
		final String author=getAuthor()!=null ? getAuthor() : "";
		  //"by" (replacement parameter {2})
		final String by=getAuthor()!=null ? "by" : "";
		  //a URL to more information about the book (replacement parameter {3})
		final String infoURL=isProjectGutenbergEText()  //if this is a Project Gutenberg work
			  ? "projectgutenbergheader.html" //point to the header we created TODO use a constant
				: URLs.getFileName(getContextURL());  //otherwise, just point to the main content TODO does this method always work?
		  //read the template as a string, assuming UTF-8 encoding
		final String templateString=URLs.readString(templateURL, CharacterEncoding.UTF_8);
			//store the title and author in the preface, making sure all the content is valid for XML content
	  final String formattedString=MessageFormat.format(templateString,
			  new Object[]{
						  XML.createValidContent(title),
							XML.createValidContent(author),
							XML.createValidContent(by),
							XML.createValidContent(infoURL)}); //TODO use constants
	  final String templateFilename=URLs.getFileName(templateURL);  //get the filename of the template
		final File formattedTemplateFile; //we'll find out where to store the formatted template
		if(getOutputDir()!=null) //if an output directory was specified
		{
			formattedTemplateFile=new File(getOutputDir(), templateFilename);  //store the file in the output directory
		}
		else  //if an output directory was not specified
		{
				//create a file from the context URL TODO what if the context URL is not a file?
			formattedTemplateFile=URLs.getFile(URLs.createURL(getContextURL(), templateFilename));
		}
			//write the resulting file
		Files.write(formattedTemplateFile, formattedString.getBytes(CharacterEncoding.UTF_8));
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
			PropertiesUtilities.setProperty(serializeOptions, XMLSerializer.FORMAT_OUTPUT_OPTION, true);  //show that we should format the output
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
	public static URI createURI(final URI publicationURI, final String href)
	{
		return URI.create(publicationURI.toString()+FRAGMENT_SEPARATOR+href); //create a URI in the form publicationURI#href TODO fix to check the runtime exception this might throw---and make sure spaces in the fragment won't hurt anything
	}

	/**Tries to determine the title from the the first few elements of the
		XHTML document.
	@param xhtmlDocument The document to check for a title.
	@return The title, or <code>null</code> if a title could not be found.
	*/
	public static String getTitle(final Document xhtmlDocument)
	{
		final String BY="by"; //TODO put elsewhere
		final Element bodyElement=getBodyElement(xhtmlDocument);  //get the body of the document
		final NodeList childNodes=bodyElement.getChildNodes();  //get the list of child nodes
		for(int i=0; i<25 && i<childNodes.getLength(); ++i) //look at each child node
		{
			final Node childNode=childNodes.item(i);  //get this child node
			if(childNode.getNodeType()==Node.ELEMENT_NODE)  //if this is an element
			{
				final Element childElement=(Element)childNode;  //get a reference to this element
				final String text=XML.getText(childElement, true).trim(); //get the trimmed text of the element
				  //Michael Hart shouldn't be in the title, or even near it (e.g. Project Gutenberg plboss10.txt)
				if(Strings.indexOfIgnoreCase(text, "Michael S. Hart")<0 //TODO these are last-minute hacks specifically for plboss10.txt
				  && Strings.indexOfIgnoreCase(text, "405 West")<0
				  && Strings.indexOfIgnoreCase(text, "Urbana, IL")<0)
				{
						//if the text is in uppercase or if it's a title, assume it's the book title if it has at least one letter in it
	//TODO fix				if(StringUtilities.isUpperCase(text) || TextUtilities.getHeadingType(text)!=TextUtilities.NO_HEADING) //TODO probably put this routine in some common area
					if(CharSequences.containsLetter(text) &&
							(CharSequences.isUpperCase(text) || Prose.isTitleHeading(text))) //TODO probably put this routine in some common area
					{
						String title=text;  //TODO fix, tidy
							//if this sting starts with "by" and has a period in it, we'll assume it's really an author name (although this test is precarious) (e.g. jjknd10.txt)
						if(!Strings.startsWithIgnoreCase(title.trim(), "by") || title.indexOf('.')<0)
						{
									//see if "version" appears with a space after it (e.g. not "second version" as in bible11.txt)
							final int versionIndex=Strings.indexOfIgnoreCase(title, "version "); //TODO testing
							if(versionIndex>=0)
								title=title.substring(0, versionIndex); //TODO testing
								//trim the title of certain delimiters, and then collapse the whitespace
							title=ProjectGutenbergXHTMLTidier.tidyTitle(title);  //TODO use a common method, not in PGUtilities
							if(isTitleOrAuthor(title)) //if we have valid title text
								return title; //assume that's the title
						}
					}
				}
				int byIndex=Strings.indexOfIgnoreCase(text, BY);  //see if "by" is in this string
				while(byIndex>0)  //if we found "by" and it's not at the first of the line
				{
				  if(Characters.isWhitespace(text.charAt(byIndex-1))  //and it's preceded by whitespace
							&& byIndex+BY.length()<text.length()  //and it's not at the end of the line
							&& Characters.isWhitespace(text.charAt(byIndex+BY.length())))  //and whitespace comes after it
					{
							//make sure this isn't "donated by", "scanned by", etc. TODO this is duplicated in getTitle() and getAuthor(); combine
						if((Strings.indexOfIgnoreCase(text, "donated")<0 || Strings.indexOfIgnoreCase(text, "donated")>byIndex)  //TODO use constants
								&& (Strings.indexOfIgnoreCase(text, "contributed")<0 || Strings.indexOfIgnoreCase(text, "contributed")>byIndex)
								&& (Strings.indexOfIgnoreCase(text, "created")<0 || Strings.indexOfIgnoreCase(text, "created")>byIndex)
								&& (Strings.indexOfIgnoreCase(text, "contributed")<0 || Strings.indexOfIgnoreCase(text, "contributed")>byIndex)
								&& (Strings.indexOfIgnoreCase(text, "created")<0 || Strings.indexOfIgnoreCase(text, "created")>byIndex)
								&& (Strings.indexOfIgnoreCase(text, "delimited")<0 || Strings.indexOfIgnoreCase(text, "delimited")>byIndex)
								&& (Strings.indexOfIgnoreCase(text, "etext")<0 || Strings.indexOfIgnoreCase(text, "etext")>byIndex)
								&& (Strings.indexOfIgnoreCase(text, "prepared")<0 || Strings.indexOfIgnoreCase(text, "prepared")>byIndex)
								&& (Strings.indexOfIgnoreCase(text, "proofread")<0 || Strings.indexOfIgnoreCase(text, "proofread")>byIndex)
								&& (Strings.indexOfIgnoreCase(text, "scanned")<0 || Strings.indexOfIgnoreCase(text, "scanned")>byIndex)
								&& (Strings.indexOfIgnoreCase(text, "typed in")<0 || Strings.indexOfIgnoreCase(text, "typed in")>byIndex))
						{
								//get the title and trim it of certain delimiters, and then collapse the whitespace
							final String title=ProjectGutenbergXHTMLTidier.tidyTitle(text.substring(0, byIndex));  //TODO use a common method, not in PGUtilities
							if(isTitleOrAuthor(title)) //if we have valid title text
								return title; //assume that's the title
						}
					}
					byIndex=Strings.indexOfIgnoreCase(text, BY, byIndex+BY.length());  //keep searching after the occurrence
				}
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
		final String BY="by"; //TODO put elsewhere
		boolean nextLineIsAuthor=false; //something might make us think at some point that the next line is the author
		final Element bodyElement=getBodyElement(xhtmlDocument);  //get the body of the document
		final NodeList childNodes=bodyElement.getChildNodes();  //get the list of child nodes
		for(int i=0; i<20 && i<childNodes.getLength(); ++i) //look at each child node
		{
			final Node childNode=childNodes.item(i);  //get this child node
			if(childNode.getNodeType()==Node.ELEMENT_NODE)  //if this is an element
			{
				final Element childElement=(Element)childNode;  //get a reference to this element
				final String text=XML.getText(childElement, true).trim(); //get the trimmed text of the element
Debug.trace("looking for normal author in text: ", text);
				if(nextLineIsAuthor)  //if we told ourselves that the next line will be the author
				{
Debug.trace("we think this line is the author");
						//get the author and trim it of certain delimiters, and then collapse the whitespace
					final String author=ProjectGutenbergXHTMLTidier.tidyAuthor(text);  //TODO use a common method, not in PGUtilities
Debug.trace("checking next line author: ", author);
					if(isTitleOrAuthor(author)) //if we have valid author text
						return author; //assume that's the author
				}
				int byIndex=Strings.indexOfIgnoreCase(text, BY);  //see if "by" is in this string
				while(byIndex>=0)  //if we found "by"
				{
Debug.trace("byIndex: "+byIndex);
						//and it's at the first of the line or preceded by whitespace
				  if((byIndex==0 || Characters.isWhitespace(text.charAt(byIndex-1)))
						&& (byIndex+BY.length()==text.length()
							  || (byIndex+BY.length()<text.length()
								&& Character.isWhitespace(text.charAt(byIndex+BY.length()))))) //if there is whitespace after "by"
					{
						if(BY.equalsIgnoreCase(text.trim()))  //if this is just "by" by itself
						{
							nextLineIsAuthor=true;  //the next line is the author
						}
						else
						{
							boolean isAcceptableBy=true;  //assume this by is OK
								//words that cannot appear before "by"
							final String[] UNACCEPTABLE_BY_PREFIXES={"donated", "contributed", "created", //TODO use constants
									"delimited", "etext", "prepared", "proofread", "scanned", "typed in",
									"made available"};
							for(int phraseWordIndex=UNACCEPTABLE_BY_PREFIXES.length-1; phraseWordIndex>=0; --phraseWordIndex) //look at each string
							{
								final String word=UNACCEPTABLE_BY_PREFIXES[phraseWordIndex];  //get this word
									//find the first time this word apears before "by"
								final int wordIndex=Strings.lastIndexOfIgnoreCase(text, word, byIndex-1);
								if(wordIndex>=0)  //if the word appears before "by"
								{
										//for "etext", we don't care if "etext by" appears as long as it is "Project Gutenberg etext by" (e.g. jjstg10.txt), not "etext by"
									if(!"etext".equals(word) || CharSequences.charIndexOf(text, "Project Gutenberg")<byIndex)
									{
											//if there is just whitespace between the word and "by" (e.g. "donated by"), this is unacceptable
										if(CharSequences.notCharIndexOf(text, WHITESPACE_CHAR_STRING, wordIndex+word.length())>=byIndex)
										{
											isAcceptableBy=false; //don't accept this phrase
											break;  //stop looking for an unacceptable phrase; we just found one
										}
									}
								}
							}
							if(isAcceptableBy)  //if this "by" is acceptable
							{
								final String authorText=text.substring(byIndex+BY.length());  //get everything after "by"
								if(CharSequences.charIndexOf(authorText, EOL_CHAR_STRING)<0 //if everything's on a single line
												//or if the other lines are just numbers and hyphens (hack for hrlnd10.txt)
										|| CharSequences.charIndexOf(Strings.trim(authorText, WHITESPACE_CHAR_STRING+"0123456789-"), EOL_CHAR_STRING)<0)
								{
										//get the author and trim it of certain delimiters, and then collapse the whitespace
									final String author=ProjectGutenbergXHTMLTidier.tidyAuthor(authorText);  //TODO use a common method, not in PGUtilities
									if(isTitleOrAuthor(author)  //if we have valid author text
											&& !Strings.startsWithIgnoreCase(author, "author")  //if this wasn't "by author" TODO use a constant
											&& !Strings.startsWithIgnoreCase(author, "the author")  //if this wasn't "by the author" TODO use a constant
											&& !Strings.startsWithIgnoreCase(author, "his wife")  //if this wasn't "by his wife" (e.g. slanr10.txt) TODO use a constant
											&& !Strings.startsWithIgnoreCase(author, "electronic")  //if this wasn't "by electronic mail" TODO use a constant
											&& !Strings.startsWithIgnoreCase(author, "email")  //if this wasn't "by email" TODO use a constant
											&& !Strings.startsWithIgnoreCase(author, "e-mail"))  //if this wasn't "by e-mail" TODO use a constant
									{
										return author; //assume that's the author
									}
									else if(BY.equalsIgnoreCase(author))  //if this is just "by" by itself TODO how did this ever work? we removed by already
									{
										nextLineIsAuthor=true;  //the next line is the author
									}
								}
							}
						}
					}
					byIndex=Strings.indexOfIgnoreCase(text, BY, byIndex+BY.length());  //keep searching after the occurrence
				}
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
			//if the string doesn't have the correct number of characters (this was 64, but huxbr10.txt needs larger for "NOTE ON THE RESEMBLANCES AND DIFFERENCES IN THE STRUCTURE AND THE DEVELOPMENT OF THE BRAIN IN MAN AND APES")
		if(string.length()==0 || string.length()>=128)
			return false; //this isn't valid
				//if the word "copyright" appears in the string
		if(Strings.indexOfIgnoreCase(string, "copyright")>=0) //TODO use a constant
			return false; //this isn't valid
				//if the string only has punctuation TODO should we eventually add dependent punctuation to the general punctuation string?
		if(!CharSequences.containsLetterOrDigit(string)) //if there are no letters or digits in the string
			return false; //this isn't valie
		return true;  //the string passed all our tests
	}


}