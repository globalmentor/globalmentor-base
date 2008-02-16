package com.garretwilson.io;


/**Constant values related to MIME media types, as originally defined in
	<a href="http://www.rfc-editor.org/rfc/rfc2046.txt">RFC 2046</a>,
	"MIME Part 2: Media Types".
@author Garret Wilson
@see javax.mail.internet.ContentType
@see http://www.rfc-editor.org/rfc/rfc2046.txt
@see http://www.w3.org/TR/2002/NOTE-xhtml-media-types-20020430/
*/
public class ContentTypeConstants	//TODO transfer these constants to specific relevant classes and remove this class
{
		//subtype suffixes
	/**The suffix for XML application types, as defined in <a href="http://www.ietf.org/rfc/rfc3023.txt">RFC 3023</a>, "XML Media Types".*/
	public final static String XML_SUBTYPE_SUFFIX="xml";
	/**The suffix for XML external parsed entity subtyes (not yet formally defined).*/
	public final static String XML_EXTERNAL_PARSED_ENTITY_SUBTYPE_SUFFIX="xml-external-parsed-entity";
	/**The suffix for RDF application types.*/
	public final static String RDF_XML_SUBTYPE_SUFFIX="rdf"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX;
	/**The suffix for TURF application types.*/
	public final static String TURF_SUBTYPE_SUFFIX="turf";
	/**The suffix for URF TURF application types.*/
	public final static String URF_TURF_SUBTYPE_SUFFIX="urf"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+TURF_SUBTYPE_SUFFIX;
	/**The suffix for zip application types.*/
	public final static String ZIP_SUBTYPE_SUFFIX="zip";

		//text media types
	/**An OEB 1.0 document.*/
	public final static String X_OEB1_CSS_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"oeb1-css";
	public final static String X_OEB1_DOCUMENT_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"oeb1-document";
	public final static String CALENDAR_SUBTYPE="calendar";
	/**A Cascading Style Sheet document.*/
	public final static String CSS_SUBTYPE="css";
	public final static String DIRECTORY_SUBTYPE="directory";
	public final static String HTML_SUBTYPE="html";
	public final static String JAVASCRIPT_SUBTYPE="javascript";
	public final static String PLAIN_SUBTYPE="plain";
	/**A <code>text/uri-list</code> content type as defined in <a href="http://www.ietf.org/rfc/rfc2483.txt">RFC 2483</a>, "URI Resolution Services Necessary for URN Resolution".*/
	public final static String URI_LIST_SUBTYPE="uri-list";
	/**The wildcard subtype, matching any subtype.*/
	public final static String WILDCARD_SUBTYPE=String.valueOf(ContentTypes.TYPE_WILDCARD_CHAR);
	public final static String XML_SUBTYPE="xml";
	/**An XML external parsed entity, as defined in <a href="http://www.ietf.org/rfc/rfc3023.txt">RFC 3023</a>, "XML Media Types".*/
	public final static String XML_EXTERNAL_PARSED_ENTITY_SUBTYPE="xml-external-parsed-entity";

		//image media types
	/**A GIF image.*/
	public final static String GIF_SUBTYPE="gif";
	/**A JPEG image.*/
	public final static String JPEG_SUBTYPE="jpeg";
	/**A PNG image.*/
	public final static String PNG_SUBTYPE="png";
	/**An SVG image.*/
	public final static String SVG_XML_SUBTYPE="svg"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX;
	/**A TIFF image.*/
	public final static String TIFF_SUBTYPE="tiff";
	/**A bitmap image.*/
	public final static String X_BITMAP_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"bitmap";

		//video media types
	/**An MPEG video, as wel as MPEG 2 layer 3 (MP3) audio with audio/mpeg; see <a href="http://www.rfc-editor.org/rfc/rfc3003.txt">RFC 3003</a>.*/
	public final static String MPEG_SUBTYPE="mpeg";

		//audio media types
	/**Single channel audio encoded using 8-bit ISDN mu-law [PCM] at a sample rate of 8000 Hz.*/
	public final static String BASIC_SUBTYPE="basic";
	/**Microsoft Windows Wave audio format.*/
	public final static String X_WAV_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"wav";

		//application media types
	/**A stream of bytes.*/
	public final static String OCTET_STREAM_SUBTYPE="octet-stream";
	/**A Dictionary Ontology (Dicto) dictionary.*/
	public final static String X_DICTO_RDF_XML_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"dicto"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**A Java application.*/
	public final static String JAVA_SUBTYPE="java";
	/**A  MathML application.*/
	public final static String MATHML_XML_SUBTYPE="mathml"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX;
	/**A Java JNLP file.*/
	public final static String X_JAVA_JNLP_FILE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"java-jnlp-file";
	/**A Java object.*/
	public final static String X_JAVA_OBJECT=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"java-object";
	/**A MAQRO activity.*/
	public final static String X_MAQRO_RDF_XML_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"maqro"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**A MAQRO question.*/
	public final static String X_QRO_RDF_XML_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"qro"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**A Microsoft Word document; see <a href="http://www.iana.org/assignments/media-types/application/msword">http://www.iana.org/assignments/media-types/application/msword</a>.*/
	public final static String MSWORD_SUBTYPE="msword";
	/**An OEB 1.x publication zip file.*/
	public final static String X_OEB_PUBLICATION_ZIP_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"oeb-publication"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+ZIP_SUBTYPE_SUFFIX;
	/**An OEB 1.x package file.*/
	public final static String X_OEB1_PACKAGE_XML_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"oeb1-package"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX;
	/**An Ogg Vorbis file; see <a href="http://www.rfc-editor.org/rfc/rfc3534.txt">RFC 3534</a>.*/
	public final static String OGG_SUBTYPE="ogg";
	/**An Adobe PDF file.*/
	public final static String PDF_SUBTYPE="pdf";
	/**A Rar compressed file.*/	
	public final static String X_RAR_COMPRESSED_SUBTYPTE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"rar-compressed";
	/**A TURF data instance.*/
	public final static String TURF_SUBTYPE="turf";
	/**A Macromedia Flash object.*/
	public final static String X_SHOCKWAVE_FLASH_SUBTYPE="x-shockwave-flash";
	/**Submitted URL-encoded form data; see <a href="http://www.rfc-editor.org/rfc/rfc1867.txt">RFC 1867</a>.*/
	public final static String X_WWW_FORM_URLENCODED=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"www-form-urlencoded";
	/**An XML application.*/
//G***fix	public final static String APPLICATION_XML=APPLICATION+DIVIDER+XML;
	/**An XEB book file, <code>x-xebook+rdf+xml</code>.*/
	public final static String X_XEBOOK_RDF_XML_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"xebook"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**An XEB book zip file, <code>x-xebook+rdf+xml+zip</code>.*/
	public final static String X_XEBOOK_RDF_XML_ZIP_SUBTYPE=X_XEBOOK_RDF_XML_SUBTYPE+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+ZIP_SUBTYPE_SUFFIX;
	/**An XHTML application.*/
	public final static String XHTML_XML_SUBTYPE="xhtml"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX;
	/**An XHTML fragment (not yet formally defined).*/
	public final static String XHTML_XML_EXTERNAL_PARSED_ENTITY_SUBTYPE="xhtml"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_EXTERNAL_PARSED_ENTITY_SUBTYPE_SUFFIX;
	/**A Zip file.*/
	public final static String ZIP_SUBTYPE="zip";

		//multipart media types
	/**Submitted form data; see <a href="http://www.rfc-editor.org/rfc/rfc1867.txt">RFC 1867</a>.*/
	public final static String FORM_DATA_SUBTYPE="form-data";
}