package com.garretwilson.swing.text.xml;

import java.awt.Color;
//G***del import java.awt.Component;  //G***del when loading routines are placed elsewhere
import java.awt.Font;
import java.awt.font.TextAttribute;
import java.awt.GraphicsEnvironment;
import java.awt.Image;  //G***del when loading routines are placed elsewhere
//G***del import java.awt.MediaTracker;  //G***del when loading routines are placed elsewhere
import java.awt.Toolkit;  //G***del when loading routines are placed elsewhere
import java.lang.ref.*;
import java.util.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
/*G***bring back as needed
import java.net.URL;
import java.net.URLEncoder;
*/
import java.net.MalformedURLException;
import java.io.*;
import java.text.MessageFormat;
import javax.sound.sampled.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.undo.UndoableEdit;

/*G***bring back as needed
import javax.swing.Icon;
import javax.swing.ImageIcon;
*/
//G***fix maybe import org.w3c.dom.*;
import org.w3c.dom.Attr;
import org.w3c.dom.css.CSSStyleDeclaration;
import org.w3c.dom.stylesheets.StyleSheet;
//G***del when works import com.garretwilson.awt.ImageUtilities;
import com.garretwilson.io.*;
import com.garretwilson.lang.JavaConstants;
import com.garretwilson.net.URIUtilities;
import com.garretwilson.rdf.RDF;  //G***move
import com.garretwilson.swing.event.ProgressEvent;
import com.garretwilson.swing.event.ProgressListener;
import com.garretwilson.swing.text.DocumentConstants;
import com.garretwilson.swing.text.DocumentUtilities;
import com.garretwilson.swing.text.SwingTextUtilities;
import com.garretwilson.text.CharacterConstants;
import com.garretwilson.text.xml.XMLConstants;
import com.garretwilson.text.xml.XMLDOMImplementation;
import com.garretwilson.text.xml.XMLText; //G***remove these in favor of W3C DOM
import com.garretwilson.text.xml.XMLElement;
import com.garretwilson.text.xml.XMLNode;
import com.garretwilson.text.xml.XMLUtilities;
import com.garretwilson.text.xml.oeb.OEBConstants;  //G***move
import com.garretwilson.text.xml.stylesheets.XMLStyleSheetConstants;
import com.garretwilson.text.xml.stylesheets.XMLStyleSheetDescriptor;
import com.garretwilson.text.xml.stylesheets.XMLStyleSheetList;
import com.garretwilson.text.xml.stylesheets.css.AbstractXMLCSSStylesheetApplier;
import com.garretwilson.text.xml.stylesheets.css.XMLCSSConstants;
import com.garretwilson.text.xml.stylesheets.css.XMLCSSValue;
import com.garretwilson.text.xml.stylesheets.css.XMLCSSPrimitiveValue;
import com.garretwilson.text.xml.stylesheets.css.XMLCSSProcessor;
import com.garretwilson.text.xml.stylesheets.css.XMLCSSStyleDeclaration;
import com.garretwilson.text.xml.stylesheets.css.XMLCSSSelector; //G***del when fully switched to DOM
import com.garretwilson.text.xml.stylesheets.css.XMLCSSStyleRule; //G***del when fully switched to DOM
import com.garretwilson.text.xml.stylesheets.css.XMLCSSUtilities; //G***maybe move
import com.garretwilson.text.xml.xhtml.XHTMLConstants;  //G***move
import com.garretwilson.text.xml.xhtml.XHTMLUtilities;
import com.garretwilson.sound.sampled.SampledSoundUtilities;
import com.garretwilson.swing.text.Bidi;
import com.garretwilson.swing.text.xml.css.XMLCSSStyleUtilities;
import com.garretwilson.swing.text.xml.css.XMLCSSStyleContext;
import com.garretwilson.swing.text.xml.xhtml.XHTMLSwingTextUtilities;
import com.garretwilson.util.Debug;
import com.garretwilson.util.NameValuePair;
//G***del when works import com.garretwilson.swing.text.xml.css.XMLCSSSimpleAttributeSet;

import com.garretwilson.lang.StringUtilities;	//G***del when we can
import com.garretwilson.lang.StringBufferUtilities;	//G***del if we don't need

import org.w3c.dom.*;
import org.w3c.dom.css.*; //G***maybe move elsewhere
import org.w3c.dom.stylesheets.StyleSheetList;

/**A document that models XML.
	Implements <code>URIInputStreamable</code>, as this class knows how to
	retrieve streams to URIs.
@see com.garretwilson.text.xml.XMLProcessor
@see com.garretwilson.text.xml.XMLDocument
@author Garret Wilson
*/
public class XMLDocument extends DefaultStyledDocument implements URIInputStreamable
{

	/**The task of applying a stylesheet.*/
	public final static String APPLY_STYLESHEET_TASK="applyStylesheet";

	/**The character used to mark the end of an element so that caret positioning
		will work correctly at the end of block views.
	*/
//G***fix	final static char ELEMENT_END_CHAR=CharacterConstants.ZERO_WIDTH_NO_BREAK_SPACE_CHAR;	
		//G***fix; the ZWNBSP seems to make Swing want to break a line early or something
//G***fix final static char ELEMENT_END_CHAR=CharacterConstants.ZERO_WIDTH_SPACE_CHAR;	

//G***fix final static char ELEMENT_END_CHAR='\n';	

final static char ELEMENT_END_CHAR=CharacterConstants.ZERO_WIDTH_SPACE_CHAR;	
final static String ELEMENT_END_STRING=String.valueOf(ELEMENT_END_CHAR);	
//G***fix final static char ELEMENT_END_CHAR=CharacterConstants.ZERO_WIDTH_NO_BREAK_SPACE_CHAR;	
//G***fix	final static char ELEMENT_END_CHAR=CharacterConstants.PARAGRAPH_SIGN_CHAR;	

	/**The list of progress event listeners.*/
	private EventListenerList progressListenerList=new EventListenerList();

	/**The object that applies stylesheets to the document.*/
	private final SwingXMLCSSStylesheetApplier stylesheetApplier=new SwingXMLCSSStylesheetApplier();

		/**@return The object that applies stylesheets to the document.*/
		protected SwingXMLCSSStylesheetApplier getStylesheetApplier() {return stylesheetApplier;}

	/**A map of all full element target IDs (URI+#+id).*/
	private Map linkTargetMap=new HashMap();

		/**Gets the element associated with the target ID.
		@param targetID The full target ID (URI+#+element ID) of the element.
		@return The element to which the target ID refers, or <code>null</code> if
			there is no element associated with the target ID.
		*/
		public Element getLinkTarget(final String targetID)
		{
			return (Element)linkTargetMap.get(targetID);	//return whatever element is associated with the target ID
		}

		/**Associates a target ID with an element.
		@param targetID The full target ID (URI+#+element ID) of the element.
		@param element The element to which the target ID refers.
		*/
		public void setLinkTarget(final String targetID, final Element element)
		{
			linkTargetMap.put(targetID, element);	//put the element in the map, keyed to the target ID
		}

	/**The access to input streams via URIs, if one exists.*/
	private URIInputStreamable uriInputStreamable=null;

		/**@return The access to input streams via URIs, or <code>null</code> if
		  none exists.
		*/
		public URIInputStreamable getURIInputStreamable() {return uriInputStreamable;}

		/**Sets the object for accessing input streams.
		@param newURIInputStreamable The object that allows acces to input streams
			via URIs.
		*/
		public void setURIInputStreamable(final URIInputStreamable newURIInputStreamable) {uriInputStreamable=newURIInputStreamable;}

	/**A map of references to resources that have been loaded.*/
	private final Map resourceReferenceMap=new HashMap();

	/**Returns a cached resource identified by the URI, if the object's memory
		has not been reclaimed.
	@param resourceURI The URI of the requested resource.
	@return The resource, if it has been cached and is still referenced in the
		JVM, or <code>null</code> if the resource's memory has been reclaimed or the
		object has never been cached.
	*/
	protected Object getCachedResource(final URI resourceURI)
	{
		final Reference resourceReference=(Reference)resourceReferenceMap.get(resourceURI); //return a reference to the cached resource, if available
		if(resourceReference!=null) //if we found a reference to the resource
		{
			final Object resource=resourceReference.get();  //get the resource itself
			if(resource!=null)  //if we still have the resource cached
				return resource;  //return the resource
			else
				resourceReferenceMap.remove(resourceURI);  //remove the reference from the cache, since it is no longer useful
		}
		return null;  //show that either the object wasn't cached, or its memory has been reclaimed
	}

	/**Stores a resource as a reference in the cache. The resource will only
		stay in the cache until the JVM decides its needs to reclaim its memory.
	@param resourceURI The URI of the resource being cached.
	@param resource The resource to cache.
	*/
	protected void putCachedResource(final URI resourceURI, final Object resource)
	{
			//store the resource in the map as a soft reference
	  resourceReferenceMap.put(resourceURI, new SoftReference(resource));
	}

	/**@return The RDF data model where metadata is stored, or <code>null</code>
		if there is no metadata document.*/
	public RDF getRDF()
	{
		return DocumentUtilities.getRDF(this);  //retrieve the RDF property value
	}

	/**Sets the RDF data model where metadata is stored.
	@param rdf The RDF data model.
	*/
	public void setRDF(final RDF rdf)
	{
		DocumentUtilities.setRDF(this, rdf);  //set the RDF
	}

//G***fix	private Map resourceMap

	private Font CODE2000_FONT=null;  //G***testing; make final

	//G***fix; comment
	protected String[] sortedAvailableFontFamilyNameArray;

	/**Default constructor.*/
	public XMLDocument()
	{
		super(new XMLCSSStyleContext());	//construct the parent class, specifying our own type of style context that knows how to deal with CSS attributes
//G***fix		setProperty(AbstractDocument.I18NProperty);  //G***testing i18n
//G***fix		putProperty("i18n", Boolean.TRUE);  //G***testing i18n
Debug.trace("Document i18n property: ", getProperty("i18n")); //G***testing i18n

		//get the list of available font family names
		sortedAvailableFontFamilyNameArray=GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
		Arrays.sort(sortedAvailableFontFamilyNameArray);  //sort the array of font family names


		//G***testing
/*G***fix
Debug.trace("loading code2000");
		try
		{
			final InputStream code2000InputStream=new BufferedInputStream(new FileInputStream("Code2000.ttf"));
			try
			{
				CODE2000_FONT=Font.createFont(Font.TRUETYPE_FONT, code2000InputStream);
			}
			finally
			{
				code2000InputStream.close();
			}
		}
		catch(Exception e)
		{
			Debug.error(e);
//G***			CODE2000_FONT=null;
		}
*/
	}
//G***fix	{
//G***fix		this(new GapContent(BUFFER_SIZE_DEFAULT)/*G***fix, new StyleSheet()*/);
//G***fix	}


		/**
		 * Constructs an html document with the default content
		 * storage implementation and the given style/attribute
		 * storage mechanism.
		 *
		 * @param styles the styles
		 */
/*G***fix
		public HTMLDocument(StyleSheet styles) {
	this(new GapContent(BUFFER_SIZE_DEFAULT), styles);
		}
*/




	/**Creates the root element to be used to represent the default document
		structure. G***make this somehow know what type of document to make -- what
		vocabulary. For now, we'll default to HTML.
	@return The element base.
	*/
	protected AbstractElement createDefaultRoot()
	{
		return super.createDefaultRoot(); //G***testing
/*G***fix
		final XMLCSSStyleDeclaration blockCSSStyle=new XMLCSSStyleDeclaration(); //create a new style declaration
		blockCSSStyle.setDisplay(XMLCSSConstants.CSS_DISPLAY_BLOCK);	//make the style declaration display: block
		final MutableAttributeSet htmlAttributeSet=createAttributeSet("html", null, blockCSSStyle);  //G***testing; comment; use a constant; fix namespace
		final MutableAttributeSet bodyAttributeSet=createAttributeSet("body", null, blockCSSStyle);  //G***testing; comment; use a constant; fix namespace
		final MutableAttributeSet pAttributeSet=createAttributeSet("p", null, blockCSSStyle);  //G***testing; comment; use a constant; fix namespace
//G***del		XMLCSSStyleConstants.setParagraphView(pAttributeSet, true);	//show that the paragraph element should have a paragraph view


		writeLock();  //grab a write-lock for this initialization and abandon it during initialization so in normal operation we can detect an illegitimate attempt to mutate attributes
		final Element[] buff=new Element[1];  //create an element array for insertion of elements

		final BranchElement section=new SectionElement(); //create a new section
		final BranchElement html=new BranchElement(section, htmlAttributeSet); //create a new paragraph to represent the document
		final BranchElement body=new BranchElement(html, bodyAttributeSet); //create a new paragraph to represent the HTML body
		final BranchElement p=new BranchElement(body, pAttributeSet); //create a new paragraph to represent the paragraph
		final LeafElement leaf=new LeafElement(p, null, 0, 1);  //create the leaf element
		buff[0]=leaf; //insert the leaf
		p.replace(0, 0, buff);
		buff[0]=p;  //insert the p
		body.replace(0, 0, buff);
		buff[0]=body;  //insert the body
		html.replace(0, 0, buff);
*/

/*G***del

			BranchElement paragraph = new BranchElement(section, null);

			LeafElement brk = new LeafElement(paragraph, null, 0, 1);
			buff[0] = brk;
			paragraph.replace(0, 0, buff);

			final Element[] sectionBuffer=new Element[2];  //G***testing
			sectionBuffer[0] = html;
			sectionBuffer[1] = paragraph;
			section.replace(0, 0, sectionBuffer);
*/
/*G***fix
		buff[0]=html;  //insert the html
		section.replace(0, 0, buff);
		writeUnlock();
		return section;
*/
  }

	/**Creates an attribute set for the described element.
	@param elementNamespaceURI The namespace of the XML element.
	@param elementQName The qualified name of the XML element.
	@return An attribute set reflecting the CSS attributes of the element.
	*/
	public static MutableAttributeSet createAttributeSet(final URI elementNamespaceURI, final String elementQName)
	{
		return createAttributeSet(elementNamespaceURI, elementQName, null);  //create an attribute set with no style
	}

	/**Creates an attribute set for the described element.
	@param elementNamespaceURI The namespace of the XML element.
	@param elementQName The qualified name of the XML element.
	@param style The CSS style to be used for the attribute set.
	@return An attribute set reflecting the CSS attributes of the element.
	*/
	public static MutableAttributeSet createAttributeSet(final URI elementNamespaceURI, final String elementQName, final CSSStyleDeclaration style)
	{
		final SimpleAttributeSet attributeSet=new SimpleAttributeSet();	//create a new attribute for this element
		XMLStyleUtilities.setXMLElementName(attributeSet, elementQName);	//store the element's name in the attribute set
		if(elementNamespaceURI!=null)  //if the element has a namespace URI specified
			XMLStyleUtilities.setXMLElementNamespaceURI(attributeSet, elementNamespaceURI.toString());	//store the element's namespace URI in the attribute set
		final String localName=XMLUtilities.getLocalName(elementQName);  //get the element's local name from the qualified name
		XMLStyleUtilities.setXMLElementLocalName(attributeSet, localName);	//store the element's local name in the attribute set
		if(style!=null) //if style was given G***should we instead do this unconditionally?
			XMLCSSStyleUtilities.setXMLCSSStyle(attributeSet, style);	//store the CSS style in the attribute set
		return attributeSet;	//return the attribute set we created
	}

	/**Gets the font from an attribute set using CSS names instead of the default
		Swing names. The actual font is obtained from the document's attribute context,
		similar to <code>DefaultStyledDocument#getFont</code> (which this function
		overrides), except that this function	may decide to change the font size
		before actually creating the font using the attribute context. If the
		attribute set specified several fonts, the first available one is used.
	@param attributeSet The attribute set from which to retrieve values.
	@return The constructed font.
	@see DefaultStyledDocument.getFont
	@see XMLStyleContext#getFont
	*/
	public Font getFont(AttributeSet attributeSet)
	{
//G***del		Debug.trace("Getting font from attributes: ", com.garretwilson.swing.text.AttributeSetUtilities.getAttributeSetString(attributeSet));  //G***del

//G***maybe use		sun.java2d.SunGraphicsEnvironment.getLocalGraphicsEnvironment()

/*G***maybe use
		final Graphics2D graphics2D=(Graphics2D)g;  //cast to the 2D version of graphics
		final FontRenderContext fontRenderContext=graphics2D.getFontRenderContext();  //get the font rednering context
*/

		int style=Font.PLAIN;	//start out assuming we'll have a plain font
		if(XMLCSSStyleUtilities.isBold(attributeSet))	//if the attributes specify bold (use XMLCSSStyleConstants so we'll recognize CSS values in the attribute set)
			style|=Font.BOLD;	//add bold to our font style
//G***del Debug.trace("is bold: ", new Boolean(XMLCSSStyleConstants.isBold(attributeSet)));  //G***del
		if(XMLCSSStyleUtilities.isItalic(attributeSet))	//if the font attributes specify italics (use XMLCSSStyleConstants so we'll recognize CSS values in the attribute set)
			style|=Font.ITALIC;	//add italics to our font style
		String family=null; //show that we haven't found a font family
		final String[] fontFamilyNameArray=XMLCSSStyleUtilities.getFontFamilyNames(attributeSet); //get the array of font family names
		for(int i=0; i<fontFamilyNameArray.length; ++i) //look at each of the specified fonts
		{
		  final String fontFamilyName=fontFamilyNameArray[i]; //get this font family name
//G***del Debug.trace("Looking for font family name: ", fontFamilyName);
		  if(fontFamilyName.equals("monospace"))  //G***fix all this; tidy; comment
			{
				family="Monospaced";
				break;
			}
		  else if(fontFamilyName.equals("serif"))  //G***fix all this; tidy; comment
			{
				family="Serif";
				break;
			}
		  else if(fontFamilyName.equals("sans-serif"))  //G***fix all this; tidy; comment
			{
//G***fix				family="Lucinda Sans Regular";
				family="SansSerif";
				break;
			}
		  else if(fontFamilyName.equals("symbol"))  //G***fix all this; tidy; comment
			{
				family="Symbol";
				break;
			}
			//G***maybe fix for "Symbol"
				//see if we have the specified font
			final int fontFamilyNameIndex=Arrays.binarySearch(sortedAvailableFontFamilyNameArray, fontFamilyName);
			if(fontFamilyNameIndex>=0)  //if we have the specified font
			{
				family=fontFamilyName;  //show that we found a font family
				break;  //stop searching
			}
		}
		if(family==null)  //if we didn't find a font family
//G***del			family="Code2000";   //G***testing
//G***fix			family="Baraha Devanagari Unicode";   //G***testing
			family="Serif";   //use the default G***use a constant; maybe use a different default
//G***del Debug.trace("Decided on font family: ", family); //G***del
//G***del when works		final String family=StyleConstants.getFontFamily(attributeSet);	//get the font family from the attributes (use XMLCSSStyleConstants so we'll recognize CSS values in the attribute set) G***change to use CSS attributes
		float size=XMLCSSStyleUtilities.getFontSize(attributeSet);	//get the font size from the attributes (use XMLCSSStyleConstants so we'll recognize CSS values in the attribute set)

/*G***put this in the style instead
		//if the attributes specify either superscript or subscript (use XMLCSSStyleConstants so we'll recognize CSS values in the attribute set)
		if(StyleConstants.isSuperscript(attributeSet) || StyleConstants.isSubscript(attributeSet))	//G***change to use CSS attributes
			size-=2;	//reduce the font size by two
*/
		final float zoomFactor=DocumentUtilities.getZoom(this, DocumentConstants.DEFAULT_ZOOM);  //get the zoom factor, assuming a default value if no value is specified
/*G***del
		  //we'll try to get the zoom factor from the text pane we're embedded in
		final float zoomFactor=DocumentUtilities.hasZoomFactor(this)
			  ? DocumentUtilities.getZoomFactor(this) //get the zoom factor value
				: 1.20f;   //assume a zoom of 120%
*/
/*G***del
		final Object zoomFactorObject=getProperty(ZOOM_FACTOR_PROPERTY); //get the zoom factor from the document
		if(zoomFactorObject instanceof Float) //if this is a float, as we expect
			zoomFactor=((Float)zoomFactorObject).floatValue();  //get the zoom factor value
		else  //if we don't get a valid zoom factor back
			zoomFactor=1.20f; //assume a zoom of 120%
*/
		size*=zoomFactor;	//increase the font size by the specified amout

		StyleContext styleContext=((StyleContext)getAttributeContext());  //get the style context to be used for retrieving fonts



		Font font=styleContext.getFont(family, style, Math.round(size));	//use the attribute context to get the font based upon the specifications we just got from attribute set
/*G***fix
Debug.trace("after getting font, font is: "+font);
	if(family.equalsIgnoreCase("Code2000") && CODE2000_FONT!=null)  //G***testing
	{
		Debug.trace("Trying to use code2000 font");
		font=CODE2000_FONT.deriveFont(style, size);
	}
*/
/*G***del when works, remove segment parameter
//G***fix Debug.trace("Font has Devanagari ka: "+font.canDisplay((char)0x0915));
//G***put in TextLayout /*G***fix
		//now that we've found a font, make sure the font can show all of the characters in the given segment, if any
		if(segment!=null) //if a segment of text is available
		{
Debug.trace("Looking at text: "+Integer.toHexString(segment.array[segment.getBeginIndex()]));
			  //find the first character this font can't display
			final int undisplayableCharIndex=font.canDisplayUpTo(segment.array, segment.getBeginIndex(), segment.getEndIndex());
		  if(undisplayableCharIndex>=0) //if there's a character this font can't display
			{
				final char undisplayableChar=segment.array[undisplayableCharIndex]; //get the character that can't be displayed
Debug.trace("Found undisplayable character in font: "+Integer.toHexString(undisplayableChar));
				  //G***don't hard-code the ranges in
				if(undisplayableChar>=0x0600 && undisplayableChar<=0x06FF)  //if this is an Arabic character
		  		font=styleContext.getFont("Lucinda Sans Regular", style, Math.round(size));	//use the font which we know has Arabic characters G***use a constant here G***don't round each time; do it only once
			}
		}
*/
//G***del Debug.trace("Finally chose font family name: ", font.getFamily()); //G***del
		return font;  //return the font we found
//G***del		return styleContext.getFont("Lucinda Sans Regular", style, Math.round(size));	//use the font which we know has Arabic characters G***use a constant here G***don't round each time; do it only once
//G***del		return styleContext.getFont("Serif", 0, Math.round(size));	//use the font which we know has Arabic characters G***use a constant here G***don't round each time; do it only once

	}

	/**Gets a specific font based upon the font name, style, and size. This
		implementation passes the request on to the associated
		<code>StyleContext</code> which provides a cache of fonts. This method is
		used indirectly through the associated view by
		<code>TextLayoutStrategy</code> to get a font for characters that cannot
		be displayed in the specified font.
	@param family The font family (such as "Monospaced")
	@param style The style of the font (such as <code>Font.PLAIN</code>).
	@param size The point size (>=1)
	@return The new font.
	@see DefaultStyledDocument.getFont
	@see XMLStyleContext#getFont
	*/
	public Font getFont(final String family, final int style, final int size)
	{
		//use the attribute context to get the font based upon the specifications
		return ((StyleContext)getAttributeContext()).getFont(family, style, size);
	}

	/**Gets a new font for the specified character by searching all available fonts.
		Fonts are cached by Unicode block and character, for faster searching in
		future queries.
	@param c The character for which a font should be returned.
	@param style The style of the font (such as <code>Font.PLAIN</cod>).
	@param size The point size (>=1).
	@return The new font, or <code>null</code> if a font could not be found to
		display this character.
	@see XMLCSSStyleContext#getFont
	*/
	public Font getFont(final char c, final int style, final int size)
	{
		return ((XMLCSSStyleContext)getAttributeContext()).getFont(c, style, size);  //return a font that supports the character
	}

	/**Returns a sorted array of names of available fonts. The returned array is
		shared by other objects in the system, and should not be modified.
	@return A sorted array of names of available fonts.
	@see XMLCSSStyleContext#getAvailableFontFamilyNames
	*/
/*G***del when works
	public String[] getAvailableFontFamilyNames()
	{
		return ((XMLCSSStyleContext)getAttributeContext()).getAvailableFontFamilyNames();  //return the array of font family names from the syle context
	}
*/

	/**Gets a particular resource from the given location.
	@param href
G***comment
*/
/*G***fix
	public Object getResource(final String href)
*/

	/**Gets a particular resource from the given location. If the resource is
		cached, the cached copy will be returned. If the document is loaded, it will
		be stored in the local weak cache.
	The return types for particular media types are as follows:
	<ul>
		<li>image/* - <code>java.awt.Image</code> The image may not be loaded.</li>
		<li>audio/* - <code>javax.sound.sampled.Line</code> Usually this will be
			of type <code>javax.sound.sampled.Clip</code> and will have been opened.</li>
	</ul>
	@param href The specified location of the resource.
	@return The specified resource.
	@exception URISyntaxException Thrown if the given location results in a syntactically incorrect URI.
	@exception IOException Thrown if the specified resource cannot be retrieved.
	*/
	public Object getResource(final String href) throws URISyntaxException, IOException
	{
Debug.trace("Inside XMLDocument.getResource() with href: ", href);	//G***del
		final MediaType mediaType=getResourceMediaType(href);	//get the media type of the resource
Debug.trace("Inside XMLDocument.getResource() with media type: ", mediaType);	//G***del
		if(mediaType!=null)	//if we think we know the media type of the file involved
		{
			final URI resourceURI=getResourceURI(href);	//create a URI based upon the base URI and the given file location
		  return getResource(resourceURI, mediaType); //get the resource from its URI and its media type
		}
		else
			throw new IOException(href+" has an unrecognized media type.");	//G***i18n
	}

	/**Gets a particular resource from the given location. If the resource is
		cached, the cached copy will be returned. If the document is loaded, it will
		be stored in the local weak cache.
	The return types for particular media types are as follows:
	<ul>
		<li>image/* - <code>java.awt.Image</code> The image may not be loaded.</li>
		<li>audio/* - <code>javax.sound.sampled.Line</code> Usually this will be
			of type <code>javax.sound.sampled.Clip</code> and will have been opened.</li>
	</ul>
	@param uri The URI location of the resource.
	@param mediaType The media type of the resource.
	@return The specified resource.
	@exception IOException Thrown if the specified resource cannot be retrieved.
	*/
	protected Object getResource(final URI uri, final MediaType mediaType) throws IOException
	{
		Object resource=getCachedResource(uri); //see if the resource is cached
		if(resource!=null)  //if the resource was cached
		{
			if(resource instanceof Clip)  //if this resource is a clip G***hack; fix to have a special getClip() method
			{
				final Clip clip=(Clip)resource; //cast the resource to a clip
				if(clip.isRunning())  //if the clip is already running
					clip.stop();  //stop playing the clip
				clip.setFramePosition(0); //start at the beginning of the clip
			}
			return resource;  //return the resource
		}
		else  //if the resource wasn't cached
			return loadResource(uri, mediaType);  //load and return the resource
	}

	/**Gets the URI of a particular resource. If the given <code>href</code> is
		relative, it is correctly normalized to an absolute URI. This version
		assumes relative locations are relative to the base URI unless the base
		URI is <code>null</code>, in which case the href is assumed to be
		absolute.
	@param href The specified location of the resource.
	@return The URI of the specified resource.
	@exception URISyntaxException Thrown if the a URI could not be created.
	@see #getBaseURI
	*/
	public URI getResourceURI(final String href) throws URISyntaxException
	{
		return URIUtilities.createURI(getBaseURI(), href);	//create and return a URI based upon the base URI, if any, and the given file location
	}

	/**Gets the media type of a particular resource.
	@param href The specified location of the resource.
	@return The media type of the specified resource, or <code>null</code> if
		the media type cannot be determined.
	*/
	public MediaType getResourceMediaType(final String href)
	{
//G***del Debug.trace("Getting ready to get media type for: ", href);  //G***del
//G***fix with FileUtilities; fix uppercase/lowercase for file extensions		FileUtilities.getMediaType()
		return FileUtilities.getMediaType(href);  //return the media type from the extension of the href, if any
/*G***del; changed to FileUtilities
		  //G***change all this to use the FileUtilites.getMediaType()
		final int extensionSeparatorIndex=href.lastIndexOf('.');	//find the extension separator character, if there is one
		if(extensionSeparatorIndex!=-1)	//if there is an extension
		{
			final String extension=href.substring(extensionSeparatorIndex+1);	//get the extension
Debug.trace("extension: ", extension);  //G***del
			return MediaType.getMediaType(extension);	//return the media type associated with this extension, if there is one
		}
		return null;	//show that, since this file doesn't have an extension, we don't even want to guess about what media type it represents
*/
	}

	/**Opens an input stream to the given location, based upon the document's
		base URI. The input stream should be closed when it is no longer needed.
	@param href The specified location of the resource.
	@return An open input stream to the resource.
	@exception URISyntaxException Thrown if the given location results in a syntactically incorrect URI.
	@exception IOException Thrown if an input stream to the specified resource
		cannot be created.
	@see #getBaseURI
//G***check about returning null if the resource is not found
	*/
	public InputStream getResourceAsInputStream(final String href) throws URISyntaxException, IOException
	{
		final URI resourceURI=getResourceURI(href);	//create a URI based upon the base URI and the given file location
		return getResourceAsInputStream(resourceURI); //get an input stream from this URI
	}

	/**Opens an input stream to the given URI. The input stream should be closed
		when it is no longer needed.
	@param uri The specified location of the resource.
	@return An open input stream to the resource.
	@exception IOException Thrown if an input stream to the specified resource
		cannot be created.
//G***check about returning null if the resource is not found
	*/
	public InputStream getResourceAsInputStream(final URI uri) throws IOException
	{
		final URIInputStreamable uriInputStreamable=getURIInputStreamable();  //see if we have an input stream locator
		if(uriInputStreamable!=null)  //if we have an input stream locator (if we're reading from a zip file, for instance)
		{
Debug.trace("found input stream locator, getting input stream to URI: ", uri); //G***del
			return uriInputStreamable.getInputStream(uri);  //get an input stream from the URI
		}
		else  //if we don't have an input stream locator
		{
			return uri.toURL().openConnection().getInputStream();	//open a connection to the URI (converted to a URL) and return an input stream to that connection
		}
	}

	/**Returns an input stream from given URI.
		This method is supplied to fulfill the requirements of
		<code>URIInputStreamLocator</code>; this method simply delegates to
		<code>getResourceAsInputStream()</code>, which may be overridden in child
		classes to retrieve input streams differently.
	@param uri A complete URI to a file.
	@return An input stream to the contents of the file represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	@see URIInputStreamLocator
	@see #getResourceAsInputStream
	*/
	public final InputStream getInputStream(final URI uri) throws IOException
	{
		return getResourceAsInputStream(uri); //use the method we already had
	}

	/**Loads a particular resource from the given location. The loaded resource
		will be stored in the local weak cache.
	The return types for particular media types are as follows:
	<ul>
		<li>image/* - <code>java.awt.Image</code> The image may not be loaded.</li>
		<li>audio/* - <code>javax.sound.sampled.Line</code> Usually this will be
			of type <code>javax.sound.sampled.Clip</code> and will have been opened.</li>
	</ul>
	@param resourceURI The specified location of the resource.
	@param mediaType The media type of the resource.
	@return The specified resource.
	@exception IOException Thrown if the specified resource cannot be retrieved.
	*/
	protected Object loadResource(final URI resourceURI, final MediaType mediaType) throws IOException  //G***change this to loadImage, loadClip, etc.
	{
		Object resource;  //this will be assigned if we run into no errors
		if(mediaType.getTopLevelType().equals(MediaTypeConstants.IMAGE))	//if this is an image
		{
			final String mediaSubType=mediaType.getSubtype(); //get the media sub-type
				//if this is a GIF, JPEG, PNG G***fix, or X_BITMAP image
			if(mediaSubType.equals(MediaTypeConstants.GIF) || mediaSubType.equals(MediaTypeConstants.JPEG) || mediaSubType.equals(MediaTypeConstants.PNG)/*G***fix || mediaSubType.equals(MediaTypeConstants.X_BITMAP)*/)
			{
				//G***since we're opening directly from a file, maybe there is a better way to do this
/*G***this works; fix to use our own caching
				final ImageIcon imageIcon=new javax.swing.ImageIcon(resourceURL);	//create an ImageIcon from the file
				resource=imageIcon.getImage();	//G***change to return an image later
*/
/*G***del when works
				final Toolkit toolkit=Toolkit.getDefaultToolkit(); //get the default toolkit
				final Image image=toolkit.createImage(resourceURL);  //G***testing; does this return null if it doesn't exist?
*/
				final InputStream resourceInputStream=getResourceAsInputStream(resourceURI);  //get an input stream to the resource
				try
				{
					final byte[] imageBytes=InputStreamUtilities.getBytes(resourceInputStream);  //read the bytes from the input stream
					final Toolkit toolkit=Toolkit.getDefaultToolkit();	//get the default toolkit
					final Image image=toolkit.createImage(imageBytes);  //create an image from the bytes
					resource=image; //G***testing
				}
				finally
				{
					resourceInputStream.close();  //always close the input stream after we're finished with it
				}

//G***del when works				ImageUtilities.loadImage(image);  //load the image
			}
			else	//if we don't recognize this image type
				throw new IOException("Unrecognized image type: \""+mediaType.getSubtype()+"\"; only \""+MediaType.JPEG+"\", \""+MediaType.PNG+"\", and \""+MediaType.GIF+"\" are currently supported.");	//G***i18n G***fix for other image types
		}
		else if(MediaTypeUtilities.isAudio(mediaType))	//if this is an audio media type
		{
			final InputStream inputStream=new BufferedInputStream(getResourceAsInputStream(resourceURI));	//get a buffered input stream to the audio
//G***we should really close the input stream if something goes wrong
			try
			{
				final Clip clip=(Clip)SampledSoundUtilities.getDataLine(inputStream, Clip.class);	//get a clip from the input stream
				resource=clip;	//return the clip
//G***del				return clip;	//return the clip without caching it, because caching a clip doesn't allow it to be played again
			}
			catch(UnsupportedAudioFileException unsupportedAudioFileException)
			{
				throw (IOException)new IOException("The format of "+resourceURI+" of type "+mediaType+" is unsupported.").initCause(unsupportedAudioFileException);	//G***i18n
			}
			catch(LineUnavailableException lineUnavailableException)
			{
				throw (IOException)new IOException("There is no line available to the audio file "+resourceURI+" of type "+mediaType+".").initCause(lineUnavailableException);	//G***i18n
			}
		}
		else	//if we don't recognize this media type
			throw new IOException("Unrecognized media type: "+mediaType);	//G***i18n
		putCachedResource(resourceURI, resource); //cache the resource in case we need to use it again
		return resource;  //return the resource we found
	}

	/**Gets the location against which to resolve relative URIs. By default this
		will be the document's URI if the document was loaded from a URI.
	@return The location against which to resolve relative URIs, or <code>null</code>
		if there is no base URI.
//G***del	@exception URISyntaxException Thrown if the a URI could not be created.
	@see Document#StreamDescriptionProperty
	*/
/*G***del when works
	public URI getBaseURI()	//G***del throws URISyntaxException
	{
//TODO store the base URI in some other property, so we won't have to switch back and forth between URI and URL

		final Object streamDescription=getProperty(StreamDescriptionProperty); //get the stream description property value
			//G***maybe create a URIUtilities method to do this
		if(streamDescription instanceof URI)	//if the stream description is a URI
			return (URI)streamDescription;	//return the stream description as-is
		else if(streamDescription instanceof URL)	//if the stream description is a URL
		{
			try
			{
				return new URI(streamDescription.toString());	//create a URI from the stream description URL
			}
			catch(URISyntaxException uriSyntaxException)	//if we couldn't create a URI from the URL
			{
				Debug.error(uriSyntaxException);	//if it's a URL, we expect to be able to create a URI from it
				return null;	//don't return any URI
			}
		}
		else if (streamDescription instanceof File)	//if the stream description is a File
			return ((File)streamDescription).toURI();		//convert the File to a URI
		else	//if we don't recognize the string description (or if there isn't one)
			return null;	//show that there is no base URI
	}
*/

	/**Sets the location against which to resolve relative URIs. By default this
		will be the document's URI.
	@param baseURI The new location against which to resolve relative URIs.
	@see #BASE_URI_PROPERTY
	*/
	public void setBaseURI(final URI baseURI)
	{
		DocumentUtilities.setBaseURI(this, baseURI);	//store the base URI
	}

	/**Gets the location against which to resolve relative URIs.
	@return The location against which to resolve relative URIs, or <code>null</code>
		if there is no base URI.
//G***del	@exception URISyntaxException Thrown if the a URI could not be created.
	@see #BASE_URI_PROPERTY
	*/
	public URI getBaseURI()
	{
		return DocumentUtilities.getBaseURI(this);	//return the value of the base URI property
	}

	/**Inserts a group of new elements into the document
	@param offset the starting offset
	@data the element data
	@exception BadLocationException for an invalid starting offset
	@see StyledDocument#insert
	@exception BadLocationException  if the given position does not
	represent a valid location in the associated document.
	*/
	//G***why do we override this?
	protected void insert(int offset, ElementSpec[] data) throws BadLocationException
	{
		super.insert(offset, data);
	}

		/**
		 * Updates document structure as a result of text insertion.  This
		 * will happen within a write lock.  This implementation simply
		 * parses the inserted content for line breaks and builds up a set
		 * of instructions for the element buffer.
		 *
		 * @param chng a description of the document change
		 * @param attr the attributes
		 */
		protected void insertUpdate(DefaultDocumentEvent chng, AttributeSet attr)
		{
Debug.trace("inside XMLDocumet insertupdate");
/*G***del; testing bidiarray
Debug.trace("XMLDocument.insertUpdate()");
        final int chngStart = chng.getOffset();
        final int chngEnd =  chngStart + chng.getLength();
Debug.trace("change start: "+chngStart+" change end: "+chngEnd);
        final int firstPStart = getParagraphElement(chngStart).getStartOffset();
        final int lastPEnd = getParagraphElement(chngEnd).getEndOffset();
Debug.trace("first paragrah start: "+firstPStart+" last paragraph end: "+lastPEnd);

*/


/*G***fix
	if(attr == null) {
			attr = contentAttributeSet;
	}

	// If this is the composed text element, merge the content attribute to it
	else if (attr.isDefined(StyleConstants.ComposedTextAttribute)) {
			((MutableAttributeSet)attr).addAttributes(contentAttributeSet);
	}
*/




	super.insertUpdate(chng, attr);
//G***del		applyxStyles(); //G***testing; put in the correct place, and make sure this gets called when repaginating, if we need to

		}

	/**Initialize the document to reflect the given element structure
		(i.e. the structure reported by the <code>getDefaultRootElement</code>
		method. If the document contained any data it will first be removed.
	<p>This version is given public access so that it can be accessed by the editor kit.</p>
	@param elementSpecs The array of element specifications that define the document.
	@see XMLEditorKit#setXML(org.w3c.dom.Document[], URI[], MediaType[], XMLDocument)
	*/
	public void create(ElementSpec[] elementSpecs)
	{
		super.create(elementSpecs);	//create the document normally
		try		//remove the ending dumming '\n' added by Swing
		{
			if(getLength()>0)	//if we have any characters
			{
				final String text=getText(getLength()-1, 1);	//TODO comment
				if("\n".equals(text))	//if the document ends with an end-of-line character
				{
					remove(getLength()-1, 1);	//remove the last end-of-line character
				}
			}
/*G***del when works
			if(getLength()>1)	//if we have more than one character
			{
				final String text=getText(getLength()-2, 2);
				if("\n\n".equals(text))	//if the document ends with two end-of-line characters
				{
					remove(getLength()-1, 1);	//remove the last end-of-line character
				}
			}
*/
		}
		catch(BadLocationException badLocationException)
		{
			throw (AssertionError)new AssertionError(badLocationException.getMessage()).initCause(badLocationException);
		}

//	G***fix		applyStyles(); //G***testing; put in the correct place, and make sure this gets called when repaginating, if we need to

/*G***fix
		writeLock();	//lock the document for writing G***do we really need to do this, as applying styles doesn't modify the document?
		final Element rootSwingElement=getRootElements()[0]; //get the first root element of the document -- this contains an element tree for each document loaded
		final int swingDocumentElementCount=rootSwingElement.getElementCount(); //find out how many root elements there are
		for(int swingDocumentElementIndex=0; swingDocumentElementIndex<swingDocumentElementCount; ++swingDocumentElementIndex) //look at each root element, each of which represents an XML document
		{
			final Element swingDocumentElement=rootSwingElement.getElement(swingDocumentElementIndex);  //get the first element, which is the root of the document tree
			insertBlockElementEnds(swingDocumentElement);	//G***testing
		}
		writeUnlock();	//release the document writing lock
*/
	}


	protected void insertBlockElementEnds(final Element element)	//G***testing
	{
		Element previousChildElement=null;	//keep track of the last child element
		AttributeSet previousChildAttributeSet=null;	//keep track of the last child element's attributes
		boolean isPreviousChildElementInline=false;	//keep track of whether the last child element was inline
		final int childElementCount=element.getElementCount(); //find out how many child elements there are
		for(int childElementIndex=0; childElementIndex<childElementCount; ++childElementIndex) //look at each child element
		{
			final Element childElement=element.getElement(childElementIndex);  //get this child element
			final AttributeSet childAttributeSet=childElement.getAttributes();	//get the attributes of the child
			final CSSStyleDeclaration childCSSStyle=XMLCSSStyleUtilities.getXMLCSSStyle(childElement.getAttributes()); //get the CSS style of the element (this method makes sure the attributes are present)
			//see if this element is inline (text is always inline, regardless of what the display property says) G***probably make some convenience method for this, and update XMLViewFactory
			final boolean isInline=XMLCSSUtilities.isDisplayInline(childCSSStyle) || AbstractDocument.ContentElementName.equals(childElement.getName());
			if(!isInline)	//if this element is not inline, add an element end character
			{
				try
				{
//G***del					insertString(childElement.getEndOffset(), XMLEditorKit.ELEMENT_END_STRING, childAttributeSet);	//G***testing
					insertString(childElement.getEndOffset(), ELEMENT_END_STRING, null);	//G***testing
						//if an inline child came before a block child, it will make an anonymous view so add an end to it as well
					if(previousChildElement!=null && isPreviousChildElementInline)
					{
//G***del						insertString(previousChildElement.getEndOffset(), XMLEditorKit.ELEMENT_END_STRING, previousChildAttributeSet);	//G***testing
						insertString(previousChildElement.getEndOffset(), ELEMENT_END_STRING, null);	//G***testing
					}
				}
				catch (BadLocationException e)
				{
					Debug.error(e);	//G***fix
				}
			}
			insertBlockElementEnds(childElement);	//insert block ends for this child element's children
			previousChildElement=childElement;						//the current child element now becomes the previous child element
			previousChildAttributeSet=childAttributeSet;	//
			isPreviousChildElementInline=isInline;				//
		}
	}

    /**
     * Notifies all listeners that have registered interest for
     * notification on this event type.  The event instance
     * is lazily created using the parameters passed into
     * the fire method.
     *
     * @param e the event
     * @see EventListenerList
     */
    protected void fireInsertUpdate(DocumentEvent e) {
//G***fix; right now this is only done when the text is first placed in the document		applyStyles(); //G***testing; put in the correct place, and make sure this gets called when repaginating, if we need to
			super.fireInsertUpdate(e);
    }


    /**
     * Calculate the levels array for a range of paragraphs.
     */
/*G***del; testing bidiarray
    private byte[] calculateBidiLevels( int firstPStart, int lastPEnd ) {

        byte levels[] = new byte[ lastPEnd - firstPStart ];
        int  levelsEnd = 0;
	Boolean defaultDirection = null;
	Object d = getProperty(TextAttribute.RUN_DIRECTION);
	if (d instanceof Boolean) {
	    defaultDirection = (Boolean) d;
	}

        // For each paragraph in the given range of paragraphs, get its
        // levels array and add it to the levels array for the entire span.
        for(int o=firstPStart; o<lastPEnd; ) {
            Element p = getParagraphElement( o );
            int pStart = p.getStartOffset();
            int pEnd = p.getEndOffset();

	    // default run direction for the paragraph.  This will be
	    // null if there is no direction override specified (i.e.
	    // the direction will be determined from the content).
            Boolean direction = defaultDirection;
	    d = p.getAttributes().getAttribute(TextAttribute.RUN_DIRECTION);
	    if (d instanceof Boolean) {
		direction = (Boolean) d;
	    }

Debug.trace("updateBidi: paragraph start = " + pStart + " paragraph end = " + pEnd);

            // Create a Bidi over this paragraph then get the level
            // array.
            String pText;
            try {
                pText = getText(pStart, pEnd-pStart);
            } catch (BadLocationException e ) {
                throw new Error("Internal error: " + e.toString());
            }
            // REMIND(bcb) we should really be using a Segment here.
            Bidi bidiAnalyzer;
	    if (direction != null) {
		boolean ltr = direction.equals(TextAttribute.RUN_DIRECTION_LTR);
		bidiAnalyzer = new Bidi(pText.toCharArray(), ltr);
	    } else {
		bidiAnalyzer = new Bidi( pText.toCharArray() );
	    }
            byte[] pLevels = bidiAnalyzer.getLevels();

Debug.trace("Ready to do Bidi arraycopy with pLevels of length: "+pLevels.length+" levels of length: "+levels.length+" levelsEnd: "+levelsEnd);


            System.arraycopy( pLevels, 0, levels, levelsEnd, pLevels.length );
            levelsEnd += pLevels.length;

            o =  p.getEndOffset();
        }

        // REMIND(bcb) remove this code when debugging is done.
        if( levelsEnd != levels.length )
            throw new Error("levelsEnd assertion failed.");

        return levels;
    }
*/

    /**
     * Initialize the document to reflect the given element
     * structure (i.e. the structure reported by the
     * <code>getDefaultRootElement</code> method.  If the
     * document contained any data it will first be removed.
     */
/*G***fix
    protected void create(ElementSpec[] data) {
	try {
	    if (getLength() != 0) {
		remove(0, getLength());
	    }
	    writeLock();

	    // install the content
	    Content c = getContent();
	    int n = data.length;
	    StringBuffer sb = new StringBuffer();
	    for (int i = 0; i < n; i++) {
		ElementSpec es = data[i];
		if (es.getLength() > 0) {
		    sb.append(es.getArray(), es.getOffset(),  es.getLength());
		}
	    }
	    UndoableEdit cEdit = c.insertString(0, sb.toString());

	    // build the event and element structure
	    int length = sb.length();
	    DefaultDocumentEvent evnt =
		new DefaultDocumentEvent(0, length, DocumentEvent.EventType.INSERT);
	    evnt.addEdit(cEdit);
	    buffer.create(length, data, evnt);

	    // update bidi (possibly)
	    super.insertUpdate(evnt, null);

	    // notify the listeners
	    evnt.end();
	    fireInsertUpdate(evnt);
	    fireUndoableEditUpdate(new UndoableEditEvent(this, evnt));
	} catch (BadLocationException ble) {
	    throw new StateInvariantError("problem initializing");
	} finally {
	    writeUnlock();
	}

    }
*/




	/**Finds the element with the matching attribute.
	@param attribute The attribute to compare.
	@param value The value to match.
	@return The element with the matching attribute, or <code>null</code> if none
		could be found.
	*/
//G***maybe make this protected and add a function that only looks for the target ID
	public Element getElement(Object attribute, Object value)
	{
		return getElement(getDefaultRootElement(), attribute, value);	//start searching from the root element
	}

	/**Returns the child element of the specified element that contains the
		desired attribute with the given value, or <code>null</code> if no element
		has an attribute with the desired value. This function is not thread-safe.
//G***del if not needed		If <code>searchLeafAttributes</code> is true, and the element is a leaf,
//G***del if not needed     * a leaf, any attributes that are instances of HTML.Tag with a
//G***del if not needed     * value that is an AttributeSet will also be checked.
	@param element The element on which to start the search
	@param attribute The attribute to compare.
	@param value The value to match.
	@return The element with the matching attribute, or <code>null</code> if none
		could be found.
	*/
	protected Element getElement(Element element, Object attribute, Object value/*G***del if not needed, boolean searchLeafAttributes*/)
	{
Debug.trace("XMLDocument.getElement() comparing value: ", value);
		final AttributeSet attributeSet=element.getAttributes();	//get the attributes of this element
		if(attributeSet!=null && attributeSet.isDefined(attribute))	//if there are attributes and this attribute is defined
		{
Debug.trace("comparing to: ", attributeSet.getAttribute(attribute));	//G***del
	    if(value.equals(attributeSet.getAttribute(attribute)))	//if the value matches
				return element;	//return this element
/*G***del when works; recheck exactly what this kludge was doing
			else	//if the value doesn't match, we'll see if they are trying to match the target ID G***this is a big kludge to get linking to work with OEB in the short term
				//G***this kludge checks to see if we're looking for a target ID; if so,
				//	and we're looking for a file (not a fragment), see if the part before
				//	the '#' matches (the first element, for now, should have at least the
				//	full path for the target ID
			{
Debug.trace("element doesn't match: ", attributeSet.getAttribute(attribute));
				if(attribute.equals(XMLStyleConstants.TARGET_ID_PATH_ATTRIBUTE_NAME))	//if they are looking for the target ID
				{
					final String compareValue=(String)value;	//cast to a string the attribute that we're comparing
Debug.trace("comparing with: ", compareValue);
					if(compareValue.indexOf('#')==-1)	//if we're looking for an absolute target ID (not a fragment)
					{
						String thisValue=(String)attributeSet.getAttribute(attribute);	//get the attribute we're comparing with
						final int poundIndex=thisValue.indexOf('#');	//get the index of any pound symbol in this attribute
						if(poundIndex!=-1)	//if this attribute has a '#'
							thisValue=thisValue.substring(0, poundIndex);	//remove the pound sign and everything after it
				    if(compareValue.equals(thisValue))	//if the value matches
							return element;	//return this element
					}
				}
			}
*/
		}
//G***del if not needed		if(!element.isLeaf())	//if the
//G***del if not needed		{
		for(int elementIndex=0, maxElementIndex=element.getElementCount(); elementIndex<maxElementIndex; ++elementIndex)	//look at each child element
		{
				//see if the child element can find the attribute
			final Element childReturnValue=getElement(element.getElement(elementIndex), attribute, value/*G***del, searchLeafAttributes*/);
			if(childReturnValue!=null)	//if the child find a matching attribute
				return childReturnValue;	//return what the child's found
    }
		return null;	//if we couldn't find matches, return null
	}
/*G***del if not needed
	else if (searchLeafAttributes && attr != null) {
	    // For some leaf elements we store the actual attributes inside
	    // the AttributeSet of the Element (such as anchors).
	    Enumeration names = attr.getAttributeNames();
	    if (names != null) {
		while (names.hasMoreElements()) {
		    Object name = names.nextElement();
		    if ((name instanceof HTML.Tag) &&
			(attr.getAttribute(name) instanceof AttributeSet)) {

			AttributeSet check = (AttributeSet)attr.
			                     getAttribute(name);
			if (check.isDefined(attribute) &&
			    value.equals(check.getAttribute(attribute))) {
			    return e;
			}
		    }
		}
	    }
	}
	return null;
    }
*/

	/**Gets the paragraph element at the offset <code>pos</code>.
		<p>The paragraph elements of <code>XMLDocument</code> can have multiple
		sub-layers of elements, representing nested XML elements such as
		<code>&lt;strong&gt;</code>; these will be translated into a single layer
		of views for each string of content.</p>
		<p>The <code>DefaultStyledDocument</code> version of this element, on the
		other hand, assumes that each paragraph will only have one single layer of
		content elements, so it simply finds the correct content element and returns
		its parent.</p>
		<p>This version finds the first element up the chain that is not an inline
		element. If all elements up the chain are inline, this method
		functions identical to that of <code>DefaultStyledDocument</code>.</p>
		<p>This version of the method is crucial; without it,
		<code>AbstractDocument.calculateBidiLevels()</code> can receive incorrect
		paragraph beginning and ending information and throw an
		<code>ArrayIndexOutOfBoundsException</code>. Editing also requires the
		functionality in this method.</p>
	@param pos The starting offset (>=0);
	@return The element with the paragraph view attribute set, or if none is set,
		the parent element of the leaf element at the given position.
	*/
	public Element getParagraphElement(int pos)
	{
Debug.trace("pos: ", pos);  //G***del
		final Element defaultParagraphElement=super.getParagraphElement(pos); //get the default paragraph element
		final Element rootElement=getDefaultRootElement();  //get the default root element so we'll know when to stop looking up the chain
		Element paragraphElement=defaultParagraphElement; //we'll check the default paragraph element -- perhaps it really is a paragraph element
		while(paragraphElement!=null && paragraphElement!=rootElement)  //stop looking when we've reached the root element or run out of elements
		{
			final AttributeSet paragraphAttributeSet=paragraphElement.getAttributes();  //get the paragraph's attributes
			assert paragraphAttributeSet!=null : "Paragraph has no attributes.";
Debug.trace("this paragraph attribute set: ", com.garretwilson.swing.text.AttributeSetUtilities.getAttributeSetString(paragraphAttributeSet));  //G***del; use relative class name
		  final CSSStyleDeclaration paragraphCSSStyle=XMLCSSStyleUtilities.getXMLCSSStyle(paragraphAttributeSet); //get the CSS style of the element (this method makes sure the attributes are present)
		  if(!XMLCSSUtilities.isDisplayInline(paragraphCSSStyle))  //if this element is marked as a paragraph
//G***del whenw orks			if(XMLStyleConstants.isParagraphView(paragraphAttributeSet))  //if this element is not marked as a paragraph
			{
				Debug.trace("paragraph is paragraph");  //G***del
				return paragraphElement;  //return the paragraph element
			}
			paragraphElement=paragraphElement.getParentElement(); //since this element wasn't a paragraph element, try the one above it
		}
		return defaultParagraphElement; //we couldn't find anything marked as a paragraph, so return the default
	}

	/**Returns true if the text in the range <code>p0</code> to <code>p1</code>
		is left to right.
		Imported from javax.swing.AbstractDocument.text version 1.112 02/02/00 by
		Timothy Prinzing because that version has class access and cannot be called
		from the revised com.garretwilson.swing.text.GlyphPainter, which in turn
		has been taken out of its package so that it can be created by
		com.garretwilson.swing.text.TextLayoutStrategy, which had to be rewritten
		to allow antialised text because of a JDK 1.3.x bug that caused a
		<code>Graphic/code> object not to correctly create a
		<code>FontRenderContext</code> that recognized the antialised font property.
	*/
	public boolean isLeftToRight(int p0, int p1)
	{
		if(!getProperty(JavaConstants.I18N_PROPERTY_NAME).equals(Boolean.TRUE))
		{
	    return true;
		}
		Element bidiRoot = getBidiRootElement();
		int index = bidiRoot.getElementIndex(p0);
		Element bidiElem = bidiRoot.getElement(index);  //G***is this causing problems with our innovations for inline elements?
		if(bidiElem.getEndOffset() >= p1)
		{
			AttributeSet bidiAttrs = bidiElem.getAttributes();
			return ((StyleConstants.getBidiLevel(bidiAttrs) % 2) == 0);
		}
		return true;
	}

	/**Discovers any referenced styles to this document, loads the stylesheets,
		and applies the styles to the Swing element attributes.
	@param swingDocument The Swing document containing the data.
	*/
	public void applyStyles()
	{
Debug.trace("Ready to applystyles");  //G***fix
//G***important; fix		writeLock();  //get a lock on the document
		try
		{
Debug.trace("looking at first root element");  //G***fix
			final Element rootSwingElement=getRootElements()[0]; //get the first root element of the document -- this contains an element tree for each document loaded
	//G***del		for(int swingRootElementIndex=0; swingRootElementIndex<rootSwingElement.getElementCount(); ++swingRootElementIndex) //look at each root element
//G***del			Debug.assert(rootSwingElement.getElementCount()>0, "No Swing root element.");  //assert there is at least one root element
	//G***del		if(rootSwingElement.getElementCount()>0)  //if there is at least one root element
		  final int swingDocumentElementCount=rootSwingElement.getElementCount(); //find out how many root elements there are
		  for(int swingDocumentElementIndex=0; swingDocumentElementIndex<swingDocumentElementCount; ++swingDocumentElementIndex) //look at each root element, each of which represents an XML document
			{
				final Element swingDocumentElement=rootSwingElement.getElement(swingDocumentElementIndex);  //get the child element, which is the root of the document tree
				final AttributeSet documentAttributeSet=swingDocumentElement.getAttributes();	//get the attribute set of the document element
				final URI documentBaseURI=XMLStyleUtilities.getBaseURI(documentAttributeSet);  //get the URI of this document
				final MediaType documentMediaType=XMLStyleUtilities.getMediaType(documentAttributeSet); //see what media type this document is
					//get all styelsheets for this document
				final CSSStyleSheet[] styleSheets=getStylesheetApplier().getStylesheets(swingDocumentElement, documentBaseURI, documentMediaType);
				//apply the stylesheets
				for(int i=0; i<styleSheets.length; ++i) //look at each stylesheet
				{
				  //prepare a progress message: "Applying stylesheet X to XXXXX.html"
					final String progressMessage=MessageFormat.format("Applying stylesheet {0} to {1}", new Object[]{new Integer(i+1), documentBaseURI!=null ? documentBaseURI.toString() : "unknown"}); //G***i18n; fix documentURI if null
Debug.trace(progressMessage); //G***del
					fireMadeProgress(new ProgressEvent(this, APPLY_STYLESHEET_TASK, progressMessage, swingDocumentElementIndex, swingDocumentElementCount));	//fire a progress message saying that we're applying a stylesheet
//G***del System.out.println("applying stylesheet: "+i+" of "+styleSheetList.getLength());  //G***del
					final CSSStyleSheet cssStyleSheet=styleSheets[i];  //get a reference to this stylesheet, assuming that it's a CSS stylesheet (that's all that's currently supported)
					getStylesheetApplier().applyStyleSheet(cssStyleSheet, swingDocumentElement);  //apply the stylesheet to the document
//G***fix					applyStyleSheet(cssStyleSheet, swingDocumentElement);  //apply the stylesheet to the document
				}
Debug.trace("applying local styles"); //G***del
				fireMadeProgress(new ProgressEvent(this, APPLY_STYLESHEET_TASK, "Applying local styles", swingDocumentElementIndex, swingDocumentElementCount));	//fire a progress message saying that we're applying local styles G***i18n
			}
		}
		finally
		{
//G***important; fix			writeUnlock();  //always release the lock on the document
		}
	}

	/**Adds a progress listener.
	@param listener The listener to be notified of progress.
	*/
	public void addProgressListener(ProgressListener listener)
	{
		progressListenerList.add(ProgressListener.class, listener);	//add this listener
	}

	/**Removes a progress listener.
	@param listener The listener that should no longer be notified of progress.
	*/
	public void removeProgressListener(ProgressListener listener)
	{
		progressListenerList.remove(ProgressListener.class, listener);
	}

	/**Notifies all listeners that have registered interest for progress that
		progress has been made.
	@param status The status to display.
	*/
	protected void fireMadeProgress(final ProgressEvent progressEvent)
	{
//G***del if not needed		final ProgressEvent progressEvent=new ProgressEvent(this, status);	//create a new progress event
		final Object[] listeners=progressListenerList.getListenerList();	//get the non-null array of listeners
		for(int i=listeners.length-2; i>=0; i-=2)	//look at each listener, from last to first
		{
			if(listeners[i]==ProgressListener.class)	//if this is a progress listener (it should always be)
				((ProgressListener)listeners[i+1]).madeProgress(progressEvent);
     }
	}

	/**Class to apply styles to Swing elements.
	@author Garret Wilson
	*/
	protected class SwingXMLCSSStylesheetApplier extends AbstractXMLCSSStylesheetApplier
	{

		/**Returns an input stream for the given URI.
		<p>The calling class has the responsibility for closing the input stream.</p>
		@param uri A URI to a resource.
		@return An input stream to the contents of the resource represented by the given URI.
		@exception IOException Thrown if an I/O error occurred.
		*/
		public InputStream getInputStream(final URI uri) throws IOException
		{
			return getResourceAsInputStream(uri);	//ask the Swing document for the URI TODO maybe later change getResourceAsInputStream() to getInputStream() so that the XMLDocument is URIInputStreamable, if it isn't already
		}

		/**Returns the object that represents the root element of the given document.
		@param The object representing the XML document.
		@return The object representing the root element of the XML document.
		*/
		protected Object getDocumentElement(final Object document)
		{
			return document;	//in Swing the XML document is represented by the root element in the document hierarchy---in this implementation, the document element hierarchy is a direct descendant of the section element
		}

		/**Retrieves processing instructions from the given document.
		@param document The document that might contain XML processing instructions.
		@return A non-<code>null</code> array of name-value pairs representing
			processing instructions.
		*/
		protected NameValuePair[] getDocumentProcessingInstructions(final Object document)
		{
			return XMLStyleUtilities.getXMLProcessingInstructions(((Element)document).getAttributes());  //get the processing instructions from the attributes of the document, which is really a Swing element			
		}

		/**Retrieves the namespace URI of the given element.
		@param element The element for which the namespace URI should be returned.
		@return The namespace URI of the given element.
		*/
		protected String getElementNamespaceURI(final Object element)
		{
			return XMLStyleUtilities.getXMLElementNamespaceURI(((Element)element).getAttributes());	//return the element's namespace URI from the Swing element's attributes
		}

		/**Retrieves the local name of the given element.
		@param element The element for which the local name should be returned.
		@return The local name of the given element.
		*/
		protected String getElementLocalName(final Object element)
		{
			return XMLStyleUtilities.getXMLElementLocalName(((Element)element).getAttributes());	//return the element's local name from the Swing element's attributes
		}

		/**Retrieves the value of one of the element's attributes.
		@param element The element owner of the attributes.
		@param attributeNamespaceURI The namespace of the attribute to find.
		@param attributeLocalName The local name of the attribute to find.
		@return The value of the specified attribute, or <code>null</code> if there
			is no such attribute.
		*/
		protected String getElementAttributeValue(final Object element, final String attributeNamespaceURI, final String attributeLocalName)
		{
			return XMLStyleUtilities.getXMLAttributeValue(((Element)element).getAttributes(), attributeNamespaceURI, attributeLocalName);	//return the XML attribute value from the element's attributes
		}

		/**Retrieves the parent element for the given element.
		@param element The element for which a parent should be found.
		@return The element's parent, or <code>null</code> if no parent could be found.
		 */
		protected Object getParentElement(final Object element)
		{
			final Element parentElement=((Element)element).getParentElement(); //get this element's parent
			return parentElement instanceof SectionElement ? null : parentElement;	//return the parent element, unless we've reached the parent section element 
		}
	
		/**Determines the number of child elements the given element has.
		@param element The parent element.
		@return The number of child elements this element has.
		*/
		protected int getChildElementCount(final Object element)
		{
			return ((Element)element).getElementCount();	//return the number of child elements
		}
		
		/**Retrieves the given indexed child of an element.
		@param element The parent element.
		@param index The zero-based index of the child.
		@return The child of the element at the given index.
		*/
		protected Object getChildElement(final Object element, final int index)
		{
			return ((Element)element).getElement(index);	//return the child element at the given index
		}

		/**Retrieves all child text of the given element.
		@param element The element for which text should be returned.
		@return The text content of the element.
		*/
		protected String getElementText(final Object element)
		{
			try
			{
				return SwingTextUtilities.getText((Element)element);  //return the text of the element
			}
			catch(BadLocationException badLocationException)	//we should never get a bad location exception
			{
				throw (AssertionError)new AssertionError(badLocationException.getMessage()).initCause(badLocationException);
			}
		}
	
		/**Imports style information into that already gathered for the given element.
		@param element The element for which style information should be imported
		@param cssStyle The style information to import.	
		*/
		protected void importCSSStyle(final Object element, final CSSStyleDeclaration cssStyle)
		{
			final AttributeSet attributeSet=((Element)element).getAttributes();	//get the element's attributes
			CSSStyleDeclaration elementStyle=(XMLCSSStyleDeclaration)XMLCSSStyleUtilities.getXMLCSSStyle(attributeSet);  //get this element's style
			if(elementStyle==null) //if there is no existing style (usually the editor kit will have supplied one already to reduce the performance hit here)
			{
				elementStyle=new XMLCSSStyleDeclaration();  //create an empty default style TODO use standard DOM classes if we can
				assert attributeSet instanceof MutableAttributeSet : "Attribute set not mutable";
				XMLCSSStyleUtilities.setXMLCSSStyle((MutableAttributeSet)attributeSet, elementStyle);	//put the style in the attributes
			}
//G***del					Debug.trace("style rule is of type: ", cssStyleRule.getClass().getName());  //G***del
			importStyle(elementStyle, cssStyle);	//import the style
		}
	}



/*G***fix
	public void emphasis()	//G***testing
	{
		writeLock();  //G***testing

		


//G***fix		final Element[] buff=new Element[1];  //create an element array for insertion of elements
		final Element characterElement=getCharacterElement(60);
//G***fix		final AttributeSet emAttributeSet=createAttributeSet("em", XHTMLConstants.XHTML_NAMESPACE_URI.toString());	//G***testirng
		final AttributeSet emAttributeSet=createAttributeSet(XHTMLConstants.XHTML_NAMESPACE_URI, "em");	//G***testirng
//G***fix		final Element branchElement=createBranchElement(characterElement.getParentElement(), emAttributeSet);
//G***fix		buff[0]=branchElement;

	final List elementSpecList=new ArrayList();	//create an array to hold our element specs
	elementSpecList.add(new DefaultStyledDocument.ElementSpec(emAttributeSet, DefaultStyledDocument.ElementSpec.StartTagType));
appendElementSpecListContent(elementSpecList, "test", null, null);	//G***fix
	elementSpecList.add(new DefaultStyledDocument.ElementSpec(emAttributeSet, DefaultStyledDocument.ElementSpec.EndTagType));

	final DefaultStyledDocument.ElementSpec[] elementSpecs=(DefaultStyledDocument.ElementSpec[])elementSpecList.toArray(new DefaultStyledDocument.ElementSpec[elementSpecList.size()]);


DefaultDocumentEvent evnt =	new DefaultDocumentEvent(60, 4, DocumentEvent.EventType.INSERT);
//G***fix		evnt.addEdit(cEdit);
//G***fix		buffer.create(1, buff, evnt);
*/
/*G***fix

	try
	{
		insert(60, elementSpecs);
	}
	catch (BadLocationException e)
	{
		Debug.error(e);
	}
*/
/*G***fix
buffer.insert(60, 4, elementSpecs, evnt);

// update bidi (possibly)
insertUpdate(evnt, null);

// notify the listeners
evnt.end();
fireInsertUpdate(evnt);
fireUndoableEditUpdate(new UndoableEditEvent(this, evnt));

*/

/*G***fix
		// update bidi (possibly)
		super.insertUpdate(evnt, null);

		// notify the listeners
		evnt.end();
		fireInsertUpdate(evnt);
		fireUndoableEditUpdate(new UndoableEditEvent(this, evnt));
*/

/*G***del
		final Element[] buff=new Element[1];  //create an element array for insertion of elements

		createBranchElement()

		final BranchElement section=new SectionElement(); //create a new section
		final BranchElement html=new BranchElement(section, htmlAttributeSet); //create a new paragraph to represent the document
		final BranchElement body=new BranchElement(html, bodyAttributeSet); //create a new paragraph to represent the HTML body
		final BranchElement p=new BranchElement(body, pAttributeSet); //create a new paragraph to represent the paragraph
		final LeafElement leaf=new LeafElement(p, null, 0, 1);  //create the leaf element
		buff[0]=leaf; //insert the leaf
		p.replace(0, 0, buff);
		buff[0]=p;  //insert the p
		body.replace(0, 0, buff);
		buff[0]=body;  //insert the body
		html.replace(0, 0, buff);

			BranchElement paragraph = new BranchElement(section, null);

			LeafElement brk = new LeafElement(paragraph, null, 0, 1);
			buff[0] = brk;
			paragraph.replace(0, 0, buff);

			final Element[] sectionBuffer=new Element[2];  //G***testing
			sectionBuffer[0] = html;
			sectionBuffer[1] = paragraph;
			section.replace(0, 0, sectionBuffer);
*/
/*G***fix
		buff[0]=html;  //insert the html
		section.replace(0, 0, buff);
		writeUnlock();
		return section;
*/

/*G***fix
		writeUnlock();
		
	}
*/

	/**Inserts an XML element into the document around the indicated selection.
	@param offset The offset in the document (>=0).
	@param length The length (>=0).
	@param elementNamespaceURI The namespace of the XML element.
	@param elementQName The qualified name of the XML element.
	*/
/*G***fix
	public void insertXMLElement(final int offset, final int length, final URI elementNamespaceURI, final String elementQName)
	{
		writeLock();  //lock the document for writing
		final Element characterElement=getCharacterElement(offset);	//get the element at the offset
		final AttributeSet elementAttributeSet=createAttributeSet(elementNamespaceURI, elementQName);	//create an attribute set for the element
		final List elementSpecList=new ArrayList();	//create an array to hold our element specs
		elementSpecList.add(new DefaultStyledDocument.ElementSpec(elementAttributeSet, DefaultStyledDocument.ElementSpec.StartTagType));
			//TODO use another Unicode character that has replacement semantics, just to make this neater and more readable
		appendElementSpecListContent(elementSpecList, StringUtilities.makeString('*', length), null, null);	//G***fix; comment
		elementSpecList.add(new DefaultStyledDocument.ElementSpec(elementAttributeSet, DefaultStyledDocument.ElementSpec.EndTagType));
		final DefaultStyledDocument.ElementSpec[] elementSpecs=(DefaultStyledDocument.ElementSpec[])elementSpecList.toArray(new DefaultStyledDocument.ElementSpec[elementSpecList.size()]);

		DefaultDocumentEvent evnt=new DefaultDocumentEvent(offset, length, DocumentEvent.EventType.INSERT);
		buffer.insert(offset, length, elementSpecs, evnt);	//insert the element's specifications
	//G***fix	insertUpdate(evnt, null);	//update after the insert
		evnt.end();	//end the editing
		fireInsertUpdate(evnt);	//notify listeners of the insert
		applyStyles();	//G***testing
		fireUndoableEditUpdate(new UndoableEditEvent(this, evnt));	//notify listeners of the undoable edit
		writeUnlock();	//unlock the document
	}
*/



}