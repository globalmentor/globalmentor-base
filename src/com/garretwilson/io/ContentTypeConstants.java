package com.garretwilson.io;

/**Constant values related to MIME media types, as originally defined in
	<a href="http://www.rfc-editor.org/rfc/rfc2046.txt">RFC 2046</a>,
	"MIME Part 2: Media Types".
@author Garret Wilson
@see javax.mail.internet.ContentType
@see http://www.rfc-editor.org/rfc/rfc2046.txt
@see http://www.w3.org/TR/2002/NOTE-xhtml-media-types-20020430/
*/
public class ContentTypeConstants
{
	/**The divider character for media type strings.*/
	public final static char DIVIDER='/';
	/**The delimiter character separating parameters from the base content type and from each other.*/
	public final static char PARAMETER_DELIMITER_CHAR=';';
	/**The character used to assign parameter values.*/
	public final static char PARAMETER_ASSIGNMENT_CHAR='=';

		//discrete top-level media types
	public final static String TEXT="text";
	public final static String IMAGE="image";
	public final static String AUDIO="audio";
	public final static String VIDEO="video";
	public final static String APPLICATION="application";
		//composite top-level media types
	public final static String MULTIPART="multipart";
	public final static String MESSAGE="message";

	/**The separator character that delimits a subtype suffix.*/
	public final static char SUBTYPE_SUFFIX_DELIMITER_CHAR='+';

		//subtype suffixes
	/**The suffix for XML application types.*/
	public final static String XML_SUBTYPE_SUFFIX="xml";
	/**The suffix for RDF application types.*/
	public final static String RDF_XML_SUBTYPE_SUFFIX="rdf"+SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX;
	/**The suffix for zip application types.*/
	public final static String ZIP_SUBTYPE_SUFFIX="zip";

	/**The character set parameters.*/
	public final static String CHARSET_PARAMETER="charset";

		//text media types
	/**An OEB 1.0 document.*/
	public final static String X_OEB1_CSS_SUBTYPE="x-oeb1-css";
	public final static String X_OEB1_DOCUMENT_SUBTYPE="x-oeb1-document";
	public final static String CALENDAR_SUBTYPE="calendar";
	/**A Cascading Style Sheet document.*/
	public final static String CSS_SUBTYPE="css";
	public final static String DIRECTORY_SUBTYPE="directory";
	public final static String HTML_SUBTYPE="html";
	public final static String JAVASCRIPT_SUBTYPE="javascript";
	public final static String PLAIN_SUBTYPE="plain";
	public final static String XML_SUBTYPE="xml";

		//image media types
	/**A GIF image.*/
	public final static String GIF_SUBTYPE="gif";
	/**A JPEG image.*/
	public final static String JPEG_SUBTYPE="jpeg";
	/**A PNG image.*/
	public final static String PNG_SUBTYPE="png";
	/**An SVG image.*/
	public final static String SVG_XML_SUBTYPE="svg"+SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX;
	/**A TIFF image.*/
	public final static String TIFF_SUBTYPE="tiff";
	/**A bitmap image.*/
	public final static String X_BITMAP_SUBTYPE="x-bitmap";

		//video media types
	/**An MPEG video.*/
	public final static String MPEG_SUBTYPE="mpeg";

		//audio media types
	/**Single channel audio encoded using 8-bit ISDN mu-law [PCM] at a sample rate of 8000 Hz.*/
	public final static String BASIC_SUBTYPE="basic";
	/**MPEG 2 layer 3 (MP3); see <a href="http://www.rfc-editor.org/rfc/rfc3003.txt">RFC 3003</a>.*/
//G***fix	public final static String AUDIO_MPEG=AUDIO+DIVIDER+MPEG;
	/**Microsoft Windows Wave audio format.*/
	public final static String X_WAV_SUBTYPE="x-wav";

		//application media types

	/**A stream of bytes.*/
	public final static String OCTET_STREAM_SUBTYPE="octet-stream";
	/**A Dictionary Ontology (Dicto) dictionary.*/
	public final static String X_DICTO_RDF_XML_SUBTYPE="x-dicto"+SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**A Java application.*/
	public final static String JAVA_SUBTYPE="java";
	/**A  MathML application.*/
	public final static String MATHML_XML_SUBTYPE="mathml"+SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX;
	/**A Java JNLP file.*/
	public final static String X_JAVA_JNLP_FILE="x-java-jnlp-file";
	/**A MAQRO activity.*/
	public final static String X_MAQRO_RDF_XML_SUBTYPE="x-maqro"+SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**A MAQRO question.*/
	public final static String X_QRO_RDF_XML_SUBTYPE="x-qro"+SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**A Microsoft Word document; see <a href="http://www.iana.org/assignments/media-types/application/msword">http://www.iana.org/assignments/media-types/application/msword</a>.*/
	public final static String MSWORD_SUBTYPE="msword";
	/**An OEB 1.x publication zip file.*/
	public final static String X_OEB_PUBLICATION_ZIP_SUBTYPE="x-oeb-publication"+SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX+SUBTYPE_SUFFIX_DELIMITER_CHAR+ZIP_SUBTYPE_SUFFIX;
	/**An OEB 1.x package file.*/
	public final static String X_OEB1_PACKAGE_XML_SUBTYPE="x-oeb1-package"+SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX;
	/**An Ogg Vorbis file; see <a href="http://www.rfc-editor.org/rfc/rfc3534.txt">RFC 3534</a>.*/
	public final static String OGG_SUBTYPE="ogg";
	/**A Rar compressed file.*/	
	public final static String X_RAR_COMPRESSED_SUBTYPTE="x-rar-compressed";
	/**An Adobe PDF file.*/
	public final static String PDF_SUBTYPE="pdf";
	/**An XML application.*/
//G***fix	public final static String APPLICATION_XML=APPLICATION+DIVIDER+XML;
	/**An XEB book file, <code>x-xebook+rdf+xml</code>.*/
	public final static String X_XEBOOK_RDF_XML_SUBTYPE="x-xebook"+SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**An XEB book zip file, <code>x-xebook+rdf+xml+zip</code>.*/
	public final static String X_XEBOOK_RDF_XML_ZIP_SUBTYPE=X_XEBOOK_RDF_XML_SUBTYPE+SUBTYPE_SUFFIX_DELIMITER_CHAR+ZIP_SUBTYPE_SUFFIX;
	/**An XHTML application.*/
	public final static String XHTML_XML_SUBTYPE="xhtml"+SUBTYPE_SUFFIX_DELIMITER_CHAR+XML_SUBTYPE_SUFFIX;
	/**A Zip file.*/
	public final static String ZIP_SUBTYPE="zip";

	//multipart media types
	/**Submitted form data; see <a href="http://www.rfc-editor.org/rfc/rfc1867.txt">RFC 1867</a>.*/
	public final static String FORM_DATA_SUBTYPE="form-data";

}